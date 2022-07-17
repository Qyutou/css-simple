# css-simple - Simple eDSL for CSS
## Installation

Add the `css-simple` to dependencies at `package.yaml` or `*.cabal`.

## Usage

```haskell

-- For normal work requires the OverloadedStrings language extension, and
-- the BlockArguments extension may be used to make the code prettier.

-- To create the CSS rule there are three main methods, `rule`, infix `?`, or
-- nothing if the type is specified (top-level declaration is enough):
>>> rule ".wrapper" (maxWidth "72rem")
.wrapper {
        max-width: 72rem;
}

```
