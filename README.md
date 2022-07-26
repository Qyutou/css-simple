# css-simple
Simple eDSL for writing CSS

## Usage

```haskell

-- For normal work requires the OverloadedStrings language extension, and
-- the BlockArguments extension may be used to make the code prettier.

-- To create the CSS rule there are three main methods, `rule`, infix `?`, or
-- nothing if the type is specified (top-level declaration is enough):
>>> rule ".wrapper" (maxWidth "72rem")
>>> ".wrapper" ? (maxWidth "72rem")
>>> ".wrapper" (maxWidth "72rem") :: Css ()
.wrapper {
    max-width: 72rem;
}

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
