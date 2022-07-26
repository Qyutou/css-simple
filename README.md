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
  width: 90vw;
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

## Rendering

Rendering configuration define how the CSS Document shall be rendered. It must
be the Config data type. Currently there are two configurations:
- `pretty` - Pretty, human-readable configuration, used everywhere by default,
  but works slightly slower than the `compact`
- `compact` - As compact rendering as possible, render as a one-line with the
  least possible amount of the spacing

The rendering of the CSS documents can be done with the functions:
- `renderCss` - Used to render the Css as a Data.Text with default rendering
  configuration
- `renderCssWith` - Used to render the Css as a Data.Text with certain
  rendering configuration
- `putCss` - Used to render the CSS as a Data.Text and print it to stdout with
  default rendering configuration
- `putCssWith` Used to render the CSS as a Data.Text with certain rendering
  configuration and print it to the stdout
- `renderCssToFile` - Used to render the Css as a Data.Text and save it to
  FilePath with default rendering configuration
- `renderCssToFileWith` - Used to render the Css as a Data.Text and save it to
  FilePath with certain rendering configuration

Here are example of different rendering of the example in the previous
section.

Pretty (default) configuration:

```haskell
>>> putCss sampleStyle
```

```css
body {
  background: #000000
}
.wrapper {
  width: 90vw;
  max-width: 72rem
}
```

Compact configuration:

```haskell
>>> putCssWith sampleStyle compact
```

```css
body{background:#000000}.wrapper{width:90vw;max-width:72rem}
```
