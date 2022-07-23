{-|
Module      : Css
Description : Simple Css generator library
Copyright   : (c) Alexey Seledkov, 2022
License     : GPL-3
Maintainer  : qyutou@gmail.com
Stability   : experimental
Portability : portable

This library provides an eDSL for creating the CSS files. It uses the Writer
Monad to fill the CSS AST-like representation, which then can be rendered as a
text.

Note: OverloadedStrings are necessary for comfort work

>>> :set -XOverloadedStrings

To create any Css rule you can use the 'rule' function, which takes
the name of the selector as an argument.

It also can be used for creating media queries.

>>> rule ".wrapper" (maxWidth "72rem")
.wrapper {
    max-width: 72rem;
}

Or, if you prefer infix notation:

>>> ".wrapper" ? (maxWidth "72rem")
.wrapper {
    max-width: 72rem;
}

Or, without any function if the type is specified:

>>> ".wrapper" (maxWidth "72rem") :: Css ()
.wrapper {
    max-width: 72rem;
}

Rules may be nested in other rules:

>>> rule "@media only screen and (min-width: 48rem)" (rule ".wrapper" (maxWidth "72rem"))
@media only screen and (min-width: 48rem) {
    .wrapper {
        max-width: 72rem;
    }
}

Css Monad also has a Semigroup and Monoid instance, so the elements are
juxtaposed via semigroup's append:

>>> rule "body" (background "#000000") <> rule ".wrapper" (width "90vw" <> maxWidth "72rem")
body {
    background: #000000;
}
.wrapper {
    width: 90vw;
    max-width: 72rem;
}

Same example using do-notation:

> -- This example requires OverloadedStrings and BlockArguments extensions
>
> sampleStyles :: Css ()
> sampleStyles = do
>     "body" do
>         background "#000000"
>
>     ".wrapper" do
>         width "90vw"
>         maxWidth "72rem"

If some property is not provided in 'Css.Properties' you can create a new with
either declaration or infix (|>):

>>> declaration "width" "90vw"
width: 90vw;
>>> "width" |> "90vw"
width: 90vw;

Or without functions if the type is specified:

> -- Requires OverloadedStrings and BlockArguments
>
> sampleStyles :: Css ()
> sampleStyles = do
> "body" do
>     "background" "#000000"
>

-}

module Css
    ( -- * Types
      Rule(..)
    , Css(..)
      -- * Rendering
    , Config(..)
    , pretty
    , compact
    , renderRule
    , renderRuleWith
    , renderCss
    , renderCssWith
    , putCss
    , putCssWith
    , renderCssToFile
    , renderCssToFileWith
      -- * eDSL
    , rule
    , (?)
    , declaration
    , (|>)
      -- | This module contains (almost) all CSS3 properties
    , module Css.Properties
    ) where

import           Css.Internal
import           Css.Properties
