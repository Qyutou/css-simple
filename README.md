# css-simple
Simple eDSL for writing CSS

## Usage

For normal work the library requires the OverloadedStrings language extension.

There are three main ways to create the CSS Rules: `rule`, infix `?` or
without any function using the OverloadedStrings if the type is specified.
```haskell
>>> :set -XOverloadedStrings
>>> rule ".wrapper" (maxWidth "72rem")
>>> ".wrapper" ? (maxWidth "72rem")
>>> ".wrapper" (maxWidth "72rem") :: Css ()
```

```css
.wrapper {
    max-width: 72rem
}
```

Css monad has the Semigroup and Monoid instances, so the elements are
juxtaposed via semigroups's append:
```haskell
>>> "body" (background "#000000") <> ".wrapper" (width "90vw" <> maxWidth "72rem") :: Css ()
```

```css
body {
    background: #000000
}
.wrapper {
    max-width: 72rem
}
```

Rules may be nested in other rules, this is also used for creating the media
queries:
```haskell
>>> "@media only screen and (min-width: 48rem)" (".wrapper" (maxWidth "72rem")) :: Css ()
```

```css
@media only screen and (min-width: 48rem) {
    .wrapper {
        max-width: 72rem
    }
}
```

If any property is not provided by default, the new one can be created with
any of the following methods:
```haskell
>>> declaration "property" "value"
>>> "property" |> "value"
>>> "property" "value" :: Css ()
```

```css
property: value
```

An example of the CSS in the function:

```haskell

{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

import Css

sampleStyle :: Css ()
sampleStyle = do
    "body" do
        background "#000000"

    ".wrapper" do
        width      "90vw"
        maxWidth   "72rem"

```
