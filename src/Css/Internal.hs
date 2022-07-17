{-|
Module      : Css.Internal
Description : Semi-public functions for the css-simple library
Copyright   : (c) Alexey Seledkov, 2022
License     : GPL-3
Maintainer  : qyutou@gmail.com
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GADTs                      #-}

module Css.Internal
    ( -- * Types
      Rule(..)
    , Css(..)
      -- * Rendering
    , Config(..)
    , pretty
    , compact
    , renderRule
    , renderRuleWith
    , renderRuleWith'
    , renderCss
    , renderCssWith
    , putCss
    , putCssWith
    , renderCssToFile
    , renderCssToFileWith
      -- * eDSL
    , (?)
    , rule
    , (|>)
    , declaration
      -- * Utility
    , getRules
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Builder ( Builder
                    , run
                    , text
                    )
import Control.Monad.Writer ( Writer
                            , MonadWriter
                            , runWriter
                            , tell
                            , listen
                            , pass
                            )
import Data.String (IsString(fromString))

-- * Types

-- | AST-like CSS Rule representation
data Rule
    -- | Rule: selector, inner rules/declarations, next
    = Rule !Text !Rule !Rule
    -- | Declaration: property, value, next
    | Declaration !Text !Text !Rule
    -- | Leaf: text
    -- This is used to allow creating the declarations without using of
    -- functions
    | Leaf !Text
    -- | Empty
    | Empty

-- | Semigroup instance for CSS Rule
instance Semigroup Rule where
    Rule selector inner next <> a =
        Rule selector inner (next <> a)
    Declaration property value next <> a =
        Declaration property value (next <> a)
    _ <> a = a

-- | Monoid instance for CSS Rule
instance Monoid Rule where
    mempty = Empty

-- | Show instance for the CSS Rule
instance Show Rule where
    show = T.unpack . renderRule

-- | Css monad - newtype wrapper around Control.Monad.Writer.Writer monad
newtype Css a = Css
    { runCss :: Writer Rule a }
    deriving (Functor, Applicative, Monad)

-- | MonadWriter instance for the Css monad
instance MonadWriter Rule Css where
    tell = Css . tell
    listen = Css . listen . runCss
    pass = Css . pass . runCss

-- | Semigroup instance for the Css monad
instance Semigroup (Css ()) where
    css <> a = tell $ getRules css <> getRules a

-- | Monoid instance for the Css monad
instance Monoid (Css ()) where
    mempty = tell Empty

-- | IsString instance used to allow the creation of declarations
-- NOTE: This is only for creating the Declarations, it doesn't do anything
-- else.
instance IsString (Css ()) where
    fromString :: String -> Css ()
    fromString = tell . Leaf . T.pack
    {-# INLINE fromString #-}

-- | IsString instance used to create the rules
instance (a ~ Css (), b ~ Css ()) => IsString (a -> b) where
    fromString :: String -> Css () -> Css ()
    fromString strSelector css = case getRules css of
        Leaf txt -> declaration (T.pack strSelector) txt
        inner -> rule (T.pack strSelector) (tell inner)
    {-# INLINE fromString #-}

-- | Show instance for the Css monad
instance Show (Css ()) where
    show = T.unpack . renderCss

-- * eDSL

-- | Create new rule
--
-- ==== __Examples__
--
-- >>> rule "body" (background "#000000") <> rule ".wrapper" (width "90vw" <> maxWidth "72rem")
-- body {
--     background: #000000;
-- }
-- .wrapper {
--     width: 90vw;
--     max-width: 72rem;
-- }
rule
    :: Text   -- ^ Selector
    -> Css () -- ^ Inner Css
    -> Css () -- ^ Return in Css Monad
rule selector inner = 
    tell $ Rule selector (getRules inner) mempty
{-# INLINE rule #-}

-- | Infix version of 'rule'
(?)
    :: Text   -- ^ Selector
    -> Css () -- ^ Inner Css
    -> Css () -- ^ Return in Css Monad
(?) = rule
{-# INLINE (?) #-}

-- | Create new declaration
--
-- ==== __Examples__
--
-- >>> declaration "width" "90vw"
-- width: 90vw;
declaration
    :: Text   -- ^ Property
    -> Text   -- ^ Value
    -> Css () -- ^ Return in Css Monad
declaration property value =
    tell $ Declaration property value mempty
{-# INLINE declaration #-}

-- | Infix version of 'declaration'
(|>)
    :: Text   -- ^ Property
    -> Text   -- ^ Value
    -> Css () -- ^ Return in Css Monad
(|>) = declaration
{-# INLINE (|>) #-}

-- * Rendering

-- | Rendering configuration
data Config = Config
    { _newline :: !Text -- ^ Newline symbol
    , _indent  :: !Text -- ^ Indentation
    , _spacing :: !Text -- ^ Small spacing
    }

-- | Pretty render configuration
pretty :: Config
pretty = Config "\n" "  " " "

-- | Compact render configuration
compact :: Config
compact = Config "" "" ""

-- | Used to render the Css Rules with pretty config
renderRule
    :: Rule -- ^ Rule to render
    -> Text -- ^ Return as a Text
renderRule = (`renderRuleWith` pretty)

-- | Render the Css Rules with certain configuration
renderRuleWith
    :: Rule   -- ^ Rule to render
    -> Config -- ^ Rendering configuration
    -> Text   -- ^ Return as a Text
renderRuleWith rule config =
    run $ renderRuleWith' rule config ""

-- | Helper function for render rules
-- Actually this is the main rendering functions, while the rest of the
-- functions are something like front-end
renderRuleWith'
    :: Rule    -- ^ Rule to render
    -> Config  -- ^ Rendering configuration
    -> Builder -- ^ Prefix
    -> Builder -- ^ Return as a Builder
renderRuleWith' (Rule selector inner Empty) config prefix =
    prefix <> text selector <> (text . _spacing) config <> "{"
    <> (text . _newline) config
    <> renderRuleWith' inner config (prefix <> (text . _indent) config)
    <> (text . _newline) config <> prefix <> "}"
renderRuleWith' (Rule selector inner next) config prefix =
    prefix <> text selector <> (text . _spacing) config <> "{"
    <> (text . _newline) config
    <> renderRuleWith' inner config (prefix <> (text . _indent) config)
    <> (text . _newline) config <> prefix <> "}" <> (text . _newline) config
    <> renderRuleWith' next config prefix
renderRuleWith' (Declaration property value Empty) config prefix =
    prefix <> text property <> ":" <> (text . _spacing) config <> text value
    <> ";"
renderRuleWith' (Declaration property value next) config prefix =
    prefix <> text property <> ":" <> (text . _spacing) config <> text value
    <> ";" <> (text . _newline) config
    <> renderRuleWith' next config prefix
renderRuleWith' _ _ _ = ""

-- | Render the Css Monad with pretty config as Text
renderCss
    :: Css () -- ^ Css to render
    -> Text   -- ^ Return as a Text
renderCss = renderRule . getRules

-- | Render the Css Monad and print it to stdout
putCss
    :: Css () -- ^ Css to render
    -> IO ()  -- ^ Print to stdoup
putCss = TIO.putStrLn . renderRule . getRules

-- | Render the Css Monad and save it to the filePath
renderCssToFile
    :: Css ()   -- ^ Css to render
    -> FilePath -- ^ Path/to/file
    -> IO ()    -- ^ Save the rendered Css to the file path
renderCssToFile css filePath =
    TIO.writeFile filePath . renderRule . getRules $ css

-- | Render the Css Monad with certain config as Text
renderCssWith
    :: Css () -- ^ Css to render
    -> Config -- ^ Configuration
    -> Text   -- ^ Return as a Text
renderCssWith = renderRuleWith . getRules

-- | Render the CSS with certain configuration and print it to stdout
putCssWith
    :: Css () -- ^ Css to render
    -> Config -- ^ Configuration
    -> IO ()  -- ^ Print to stdout
putCssWith css config = TIO.putStrLn (renderRuleWith (getRules css) config)

-- | Render the Css Monad with certain config and save it to the filepath
renderCssToFileWith
    :: Css ()   -- ^ Css to render
    -> Config   -- ^ Configuration
    -> FilePath -- ^ Path/to/file
    -> IO ()    -- ^ Save the Css to the file
renderCssToFileWith css config filePath =
    TIO.writeFile filePath (renderRuleWith (getRules css) config)

-- * Utility

-- | Utility function to extract the Css Rule from Css Monad
getRules :: Css () -> Rule
getRules = snd . runWriter . runCss
{-# INLINE getRules #-}
