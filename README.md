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

```haskell

-- For normal work requires the OverloadedStrings language extension, and
-- the BlockArguments extension may be used to make the code prettier.

-- This Css monad has the Semigroup and Monoid instances, so the elements
-- can be juxtaposed via semigroup's append:
>>> "body" (background "#000000") <> ".wrapper" (width "90vw" <> maxWidth "72rem") :: Css ()
body {
  background: #000000;
}
.wrapper {
  max-width: 72rem;
}

-- Rules may be nested in other rules:
>>> "@media only screen and (min-width: 48rem)" (".wrapper" (maxWidth "72rem")) :: Css ()
@media only screen and (min-width: 48rem) {
  .wrapper {
    max-width: 72rem;
  }
}

-- If some property is not provided by default, it is possible to create a new
-- using either `declaration` or infix version `|>`
>>> declaration "property" "value"
>>> "property" |> "value"
property: value;

-- Here are some bigger code example, this example requires 
-- OverloadedStrings and BlockArguments
sampleStyle :: Css ()
sampleStyle = do
    "body" do
        background "#000000"

    ".wrapper" do
        width      "90vw"
        maxWidth   "72rem"

```
