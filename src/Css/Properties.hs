{-|
Module      : Css ().Properties
Description : Module which contain (almost) all CSS3 properties
Copyright   : (c) Alexey Seledkov, 2022
License     : GPL-3
Maintainer  : qyutou@gmail.com
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE OverloadedStrings #-}

module Css.Properties where

import           Css.Internal (Css, declaration)
import           Data.Text    (Text)

-- | CSS Property "accent-color"
accentColor
    :: Text   -- ^ Value
    -> Css () -- ^ Return as CSS
accentColor = declaration "accent-color"

-- | CSS Property "align-content"
alignContent
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
alignContent = declaration "align-content"

-- | CSS Property "align-items"
alignItems
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
alignItems = declaration "align-items"

-- | CSS Property "align-self"
alignSelf
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
alignSelf = declaration "align-self"

-- | CSS Property "all"
all
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
all = declaration "all"

-- | CSS Property "animation"
animation
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
animation = declaration "animation"

-- | CSS Property "animation-delay"
animationDelay
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
animationDelay = declaration "animation-delay"

-- | CSS Property "animation-direction"
animationDirection
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
animationDirection = declaration "animation-direction"

-- | CSS Property "animation-duration"
animationDuration
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
animationDuration = declaration "animation-duration"

-- | CSS Property "animation-fill-mode"
animationFillMode
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
animationFillMode = declaration "animation-fill-mode"

-- | CSS Property "animation-iteration-count"
animationIterationCount
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
animationIterationCount = declaration "animation-iteration-count"

-- | CSS Property "animation-name"
animationName
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
animationName = declaration "animation-name"

-- | CSS Property "animation-play-state"
animationPlayState
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
animationPlayState = declaration "animation-play-state"

-- | CSS Property "animation-timing-function"
animationTimingFunction
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
animationTimingFunction = declaration "animation-timing-function"

-- | CSS Property "backdrop-filter"
backdropFilter
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
backdropFilter = declaration "backdrop-filter"

-- | CSS Property "backface-visibility"
backfaceVisibility
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
backfaceVisibility = declaration "backface-visibility"

-- | CSS Property "background"
background
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
background = declaration "background"

-- | CSS Property "background-attachment"
backgroundAttachment
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
backgroundAttachment = declaration "background-attachment"

-- | CSS Property "background-blend-mode"
backgroundBlendMode
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
backgroundBlendMode = declaration "background-blend-mode"

-- | CSS Property "background-clip"
backgroundClip
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
backgroundClip = declaration "background-clip"

-- | CSS Property "background-color"
backgroundColor
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
backgroundColor = declaration "background-color"

-- | CSS Property "background-image"
backgroundImage
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
backgroundImage = declaration "background-image"

-- | CSS Property "background-origin"
backgroundOrigin
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
backgroundOrigin = declaration "background-origin"

-- | CSS Property "background-position"
backgroundPosition
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
backgroundPosition = declaration "background-position"

-- | CSS Property "background-repeat"
backgroundRepeat
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
backgroundRepeat = declaration "background-repeat"

-- | CSS Property "background-size"
backgroundSize
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
backgroundSize = declaration "background-size"

-- | CSS Property "border"
border
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
border = declaration "border"

-- | CSS Property "border-bottom"
borderBottom
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderBottom = declaration "border-bottom"

-- | CSS Property "border-bottom-color"
borderBottomColor
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderBottomColor = declaration "border-bottom-color"

-- | CSS Property "border-bottom-left-radius"
borderBottomLeftRadius
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderBottomLeftRadius = declaration "border-bottom-left-radius"

-- | CSS Property "border-bottom-right-radius"
borderBottomRightRadius
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderBottomRightRadius = declaration "border-bottom-right-radius"

-- | CSS Property "border-bottom-style"
borderBottomStyle
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderBottomStyle = declaration "border-bottom-style"

-- | CSS Property "border-bottom-width"
borderBottomWidth
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderBottomWidth = declaration "border-bottom-width"

-- | CSS Property "border-collapse"
borderCollapse
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderCollapse = declaration "border-collapse"

-- | CSS Property "border-color"
borderColor
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderColor = declaration "border-color"

-- | CSS Property "border-image"
borderImage
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderImage = declaration "border-image"

-- | CSS Property "border-image-outset"
borderImageOutset
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderImageOutset = declaration "border-image-outset"

-- | CSS Property "border-image-repeat"
borderImageRepeat
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderImageRepeat = declaration "border-image-repeat"

-- | CSS Property "border-image-slice"
borderImageSlice
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderImageSlice = declaration "border-image-slice"

-- | CSS Property "border-image-source"
borderImageSource
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderImageSource = declaration "border-image-source"

-- | CSS Property "border-image-width"
borderImageWidth
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderImageWidth = declaration "border-image-width"

-- | CSS Property "border-left"
borderLeft
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderLeft = declaration "border-left"

-- | CSS Property "border-left-color"
borderLeftColor
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderLeftColor = declaration "border-left-color"

-- | CSS Property "border-left-style"
borderLeftStyle
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderLeftStyle = declaration "border-left-style"

-- | CSS Property "border-left-width"
borderLeftWidth
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderLeftWidth = declaration "border-left-width"

-- | CSS Property "border-radius"
borderRadius
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderRadius = declaration "border-radius"

-- | CSS Property "border-right"
borderRight
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderRight = declaration "border-right"

-- | CSS Property "border-right-color"
borderRightColor
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderRightColor = declaration "border-right-color"

-- | CSS Property "border-right-style"
borderRightStyle
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderRightStyle = declaration "border-right-style"

-- | CSS Property "border-right-width"
borderRightWidth
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderRightWidth = declaration "border-right-width"

-- | CSS Property "border-spacing"
borderSpacing
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderSpacing = declaration "border-spacing"

-- | CSS Property "border-style"
borderStyle
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderStyle = declaration "border-style"

-- | CSS Property "border-top"
borderTop
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderTop = declaration "border-top"

-- | CSS Property "border-top-color"
borderTopColor
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderTopColor = declaration "border-top-color"

-- | CSS Property "border-top-left-radius"
borderTopLeftRadius
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderTopLeftRadius = declaration "border-top-left-radius"

-- | CSS Property "border-top-right-radius"
borderTopRightRadius
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderTopRightRadius = declaration "border-top-right-radius"

-- | CSS Property "border-top-style"
borderTopStyle
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderTopStyle = declaration "border-top-style"

-- | CSS Property "border-top-width"
borderTopWidth
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderTopWidth = declaration "border-top-width"

-- | CSS Property "border-width"
borderWidth
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
borderWidth = declaration "border-width"

-- | CSS Property "bottom"
bottom
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
bottom = declaration "bottom"

-- | CSS Property "box-decoration-break"
boxDecorationBreak
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
boxDecorationBreak = declaration "box-decoration-break"

-- | CSS Property "box-shadow"
boxShadow
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
boxShadow = declaration "box-shadow"

-- | CSS Property "box-sizing"
boxSizing
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
boxSizing = declaration "box-sizing"

-- | CSS Property "break-after"
breakAfter
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
breakAfter = declaration "break-after"

-- | CSS Property "break-before"
breakBefore
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
breakBefore = declaration "break-before"

-- | CSS Property "break-inside"
breakInside
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
breakInside = declaration "break-inside"

-- | CSS Property "caption-side"
captionSide
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
captionSide = declaration "caption-side"

-- | CSS Property "caret-color"
caretColor
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
caretColor = declaration "caret-color"

-- | CSS Property "@charset"
charset
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
charset = declaration "@charset"

-- | CSS Property "clear"
clear
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
clear = declaration "clear"

-- | CSS Property "clip"
clip
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
clip = declaration "clip"

-- | CSS Property "color"
color
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
color = declaration "color"

-- | CSS Property "column-count"
columnCount
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
columnCount = declaration "column-count"

-- | CSS Property "column-fill"
columnFill
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
columnFill = declaration "column-fill"

-- | CSS Property "column-gap"
columnGap
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
columnGap = declaration "column-gap"

-- | CSS Property "column-rule"
columnRule
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
columnRule = declaration "column-rule"

-- | CSS Property "column-rule-color"
columnRuleColor
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
columnRuleColor = declaration "column-rule-color"

-- | CSS Property "column-rule-style"
columnRuleStyle
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
columnRuleStyle = declaration "column-rule-style"

-- | CSS Property "column-rule-width"
columnRuleWidth
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
columnRuleWidth = declaration "column-rule-width"

-- | CSS Property "column-span"
columnSpan
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
columnSpan = declaration "column-span"

-- | CSS Property "column-width"
columnWidth
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
columnWidth = declaration "column-width"

-- | CSS Property "columns"
columns
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
columns = declaration "columns"

-- | CSS Property "content"
content
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
content = declaration "content"

-- | CSS Property "counter-increment"
counterIncrement
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
counterIncrement = declaration "counter-increment"

-- | CSS Property "counter-reset"
counterReset
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
counterReset = declaration "counter-reset"

-- | CSS Property "cursor"
cursor
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
cursor = declaration "cursor"

-- | CSS Property "direction"
direction
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
direction = declaration "direction"

-- | CSS Property "display"
display
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
display = declaration "display"

-- | CSS Property "empty-cells"
emptyCells
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
emptyCells = declaration "empty-cells"

-- | CSS Property "filter"
filter
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
filter = declaration "filter"

-- | CSS Property "flex"
flex
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
flex = declaration "flex"

-- | CSS Property "flex-basis"
flexBasis
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
flexBasis = declaration "flex-basis"

-- | CSS Property "flex-direction"
flexDirection
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
flexDirection = declaration "flex-direction"

-- | CSS Property "flex-flow"
flexFlow
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
flexFlow = declaration "flex-flow"

-- | CSS Property "flex-grow"
flexGrow
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
flexGrow = declaration "flex-grow"

-- | CSS Property "flex-shrink"
flexShrink
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
flexShrink = declaration "flex-shrink"

-- | CSS Property "flex-wrap"
flexWrap
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
flexWrap = declaration "flex-wrap"

-- | CSS Property "float"
float
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
float = declaration "float"

-- | CSS Property "font"
font
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
font = declaration "font"

-- | CSS Property "@font-face"
fontFace
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
fontFace = declaration "@font-face"

-- | CSS Property "font-family"
fontFamily
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
fontFamily = declaration "font-family"

-- | CSS Property "font-feature-settings"
fontFeatureSettings
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
fontFeatureSettings = declaration "font-feature-settings"

-- | CSS Property "@font-feature-values"
fontFeatureValues
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
fontFeatureValues = declaration "@font-feature-values"

-- | CSS Property "font-kerning"
fontKerning
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
fontKerning = declaration "font-kerning"

-- | CSS Property "font-language-override"
fontLanguageOverride
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
fontLanguageOverride = declaration "font-language-override"

-- | CSS Property "font-size"
fontSize
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
fontSize = declaration "font-size"

-- | CSS Property "font-size-adjust"
fontSizeAdjust
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
fontSizeAdjust = declaration "font-size-adjust"

-- | CSS Property "font-stretch"
fontStretch
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
fontStretch = declaration "font-stretch"

-- | CSS Property "font-style"
fontStyle
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
fontStyle = declaration "font-style"

-- | CSS Property "font-synthesis"
fontSynthesis
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
fontSynthesis = declaration "font-synthesis"

-- | CSS Property "font-variant"
fontVariant
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
fontVariant = declaration "font-variant"

-- | CSS Property "font-variant-alternates"
fontVariantAlternates
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
fontVariantAlternates = declaration "font-variant-alternates"

-- | CSS Property "font-variant-caps"
fontVariantCaps
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
fontVariantCaps = declaration "font-variant-caps"

-- | CSS Property "font-variant-east-asian"
fontVariantEastAsian
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
fontVariantEastAsian = declaration "font-variant-east-asian"

-- | CSS Property "font-variant-ligatures"
fontVariantLigatures
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
fontVariantLigatures = declaration "font-variant-ligatures"

-- | CSS Property "font-variant-numeric"
fontVariantNumeric
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
fontVariantNumeric = declaration "font-variant-numeric"

-- | CSS Property "font-variant-position"
fontVariantPosition
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
fontVariantPosition = declaration "font-variant-position"

-- | CSS Property "font-weight"
fontWeight
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
fontWeight = declaration "font-weight"

-- | CSS Property "gap"
gap
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
gap = declaration "gap"

-- | CSS Property "grid"
grid
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
grid = declaration "grid"

-- | CSS Property "grid-area"
gridArea
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
gridArea = declaration "grid-area"

-- | CSS Property "grid-auto-columns"
gridAutoColumns
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
gridAutoColumns = declaration "grid-auto-columns"

-- | CSS Property "grid-auto-flow"
gridAutoFlow
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
gridAutoFlow = declaration "grid-auto-flow"

-- | CSS Property "grid-auto-rows"
gridAutoRows
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
gridAutoRows = declaration "grid-auto-rows"

-- | CSS Property "grid-column"
gridColumn
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
gridColumn = declaration "grid-column"

-- | CSS Property "grid-column-end"
gridColumnEnd
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
gridColumnEnd = declaration "grid-column-end"

-- | CSS Property "grid-column-gap"
gridColumnGap
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
gridColumnGap = declaration "grid-column-gap"

-- | CSS Property "grid-column-start"
gridColumnStart
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
gridColumnStart = declaration "grid-column-start"

-- | CSS Property "grid-gap"
gridGap
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
gridGap = declaration "grid-gap"

-- | CSS Property "grid-row"
gridRow
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
gridRow = declaration "grid-row"

-- | CSS Property "grid-row-end"
gridRowEnd
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
gridRowEnd = declaration "grid-row-end"

-- | CSS Property "grid-row-gap"
gridRowGap
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
gridRowGap = declaration "grid-row-gap"

-- | CSS Property "grid-row-start"
gridRowStart
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
gridRowStart = declaration "grid-row-start"

-- | CSS Property "grid-template"
gridTemplate
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
gridTemplate = declaration "grid-template"

-- | CSS Property "grid-template-areas"
gridTemplateAreas
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
gridTemplateAreas = declaration "grid-template-areas"

-- | CSS Property "grid-template-columns"
gridTemplateColumns
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
gridTemplateColumns = declaration "grid-template-columns"

-- | CSS Property "grid-template-rows"
gridTemplateRows
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
gridTemplateRows = declaration "grid-template-rows"

-- | CSS Property "hanging-punctuation"
hangingPunctuation
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
hangingPunctuation = declaration "hanging-punctuation"

-- | CSS Property "height"
height
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
height = declaration "height"

-- | CSS Property "hyphens"
hyphens
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
hyphens = declaration "hyphens"

-- | CSS Property "image-rendering"
imageRendering
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
imageRendering = declaration "image-rendering"

-- | CSS Property "@import"
import_
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
import_ = declaration "@import"

-- | CSS Property "isolation"
isolation
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
isolation = declaration "isolation"

-- | CSS Property "justify-content"
justifyContent
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
justifyContent = declaration "justify-content"

-- | CSS Property "@keyframes"
keyframes
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
keyframes = declaration "@keyframes"

-- | CSS Property "left"
left
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
left = declaration "left"

-- | CSS Property "letter-spacing"
letterSpacing
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
letterSpacing = declaration "letter-spacing"

-- | CSS Property "line-break"
lineBreak
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
lineBreak = declaration "line-break"

-- | CSS Property "line-height"
lineHeight
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
lineHeight = declaration "line-height"

-- | CSS Property "list-style"
listStyle
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
listStyle = declaration "list-style"

-- | CSS Property "list-style-image"
listStyleImage
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
listStyleImage = declaration "list-style-image"

-- | CSS Property "list-style-position"
listStylePosition
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
listStylePosition = declaration "list-style-position"

-- | CSS Property "list-style-type"
listStyleType
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
listStyleType = declaration "list-style-type"

-- | CSS Property "margin"
margin
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
margin = declaration "margin"

-- | CSS Property "margin-bottom"
marginBottom
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
marginBottom = declaration "margin-bottom"

-- | CSS Property "margin-left"
marginLeft
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
marginLeft = declaration "margin-left"

-- | CSS Property "margin-right"
marginRight
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
marginRight = declaration "margin-right"

-- | CSS Property "margin-top"
marginTop
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
marginTop = declaration "margin-top"

-- | CSS Property "mask"
mask
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
mask = declaration "mask"

-- | CSS Property "mask-clip"
maskClip
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
maskClip = declaration "mask-clip"

-- | CSS Property "mask-composite"
maskComposite
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
maskComposite = declaration "mask-composite"

-- | CSS Property "mask-image"
maskImage
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
maskImage = declaration "mask-image"

-- | CSS Property "mask-mode"
maskMode
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
maskMode = declaration "mask-mode"

-- | CSS Property "mask-origin"
maskOrigin
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
maskOrigin = declaration "mask-origin"

-- | CSS Property "mask-position"
maskPosition
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
maskPosition = declaration "mask-position"

-- | CSS Property "mask-repeat"
maskRepeat
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
maskRepeat = declaration "mask-repeat"

-- | CSS Property "mask-size"
maskSize
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
maskSize = declaration "mask-size"

-- | CSS Property "mask-type"
maskType
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
maskType = declaration "mask-type"

-- | CSS Property "max-height"
maxHeight
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
maxHeight = declaration "max-height"

-- | CSS Property "max-width"
maxWidth
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
maxWidth = declaration "max-width"

-- | CSS Property "@media"
media
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
media = declaration "@media"

-- | CSS Property "min-height"
minHeight
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
minHeight = declaration "min-height"

-- | CSS Property "min-width"
minWidth
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
minWidth = declaration "min-width"

-- | CSS Property "mix-blend-mode"
mixBlendMode
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
mixBlendMode = declaration "mix-blend-mode"

-- | CSS Property "object-fit"
objectFit
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
objectFit = declaration "object-fit"

-- | CSS Property "object-position"
objectPosition
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
objectPosition = declaration "object-position"

-- | CSS Property "opacity"
opacity
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
opacity = declaration "opacity"

-- | CSS Property "order"
order
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
order = declaration "order"

-- | CSS Property "orphans"
orphans
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
orphans = declaration "orphans"

-- | CSS Property "outline"
outline
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
outline = declaration "outline"

-- | CSS Property "outline-color"
outlineColor
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
outlineColor = declaration "outline-color"

-- | CSS Property "outline-offset"
outlineOffset
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
outlineOffset = declaration "outline-offset"

-- | CSS Property "outline-style"
outlineStyle
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
outlineStyle = declaration "outline-style"

-- | CSS Property "outline-width"
outlineWidth
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
outlineWidth = declaration "outline-width"

-- | CSS Property "overflow"
overflow
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
overflow = declaration "overflow"

-- | CSS Property "overflow-wrap"
overflowWrap
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
overflowWrap = declaration "overflow-wrap"

-- | CSS Property "overflow-x"
overflowX
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
overflowX = declaration "overflow-x"

-- | CSS Property "overflow-y"
overflowY
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
overflowY = declaration "overflow-y"

-- | CSS Property "padding"
padding
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
padding = declaration "padding"

-- | CSS Property "padding-bottom"
paddingBottom
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
paddingBottom = declaration "padding-bottom"

-- | CSS Property "padding-left"
paddingLeft
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
paddingLeft = declaration "padding-left"

-- | CSS Property "padding-right"
paddingRight
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
paddingRight = declaration "padding-right"

-- | CSS Property "padding-top"
paddingTop
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
paddingTop = declaration "padding-top"

-- | CSS Property "page-break-after"
pageBreakAfter
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
pageBreakAfter = declaration "page-break-after"

-- | CSS Property "page-break-before"
pageBreakBefore
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
pageBreakBefore = declaration "page-break-before"

-- | CSS Property "page-break-inside"
pageBreakInside
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
pageBreakInside = declaration "page-break-inside"

-- | CSS Property "perspective"
perspective
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
perspective = declaration "perspective"

-- | CSS Property "perspective-origin"
perspectiveOrigin
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
perspectiveOrigin = declaration "perspective-origin"

-- | CSS Property "pointer-events"
pointerEvents
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
pointerEvents = declaration "pointer-events"

-- | CSS Property "position"
position
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
position = declaration "position"

-- | CSS Property "quotes"
quotes
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
quotes = declaration "quotes"

-- | CSS Property "resize"
resize
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
resize = declaration "resize"

-- | CSS Property "right"
right
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
right = declaration "right"

-- | CSS Property "row-gap"
rowGap
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
rowGap = declaration "row-gap"

-- | CSS Property "scroll-behavior"
scrollBehavior
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
scrollBehavior = declaration "scroll-behavior"

-- | CSS Property "tab-size"
tabSize
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
tabSize = declaration "tab-size"

-- | CSS Property "table-layout"
tableLayout
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
tableLayout = declaration "table-layout"

-- | CSS Property "text-align"
textAlign
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
textAlign = declaration "text-align"

-- | CSS Property "text-align-last"
textAlignLast
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
textAlignLast = declaration "text-align-last"

-- | CSS Property "text-combine-upright"
textCombineUpright
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
textCombineUpright = declaration "text-combine-upright"

-- | CSS Property "text-decoration"
textDecoration
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
textDecoration = declaration "text-decoration"

-- | CSS Property "text-decoration-color"
textDecorationColor
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
textDecorationColor = declaration "text-decoration-color"

-- | CSS Property "text-decoration-line"
textDecorationLine
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
textDecorationLine = declaration "text-decoration-line"

-- | CSS Property "text-decoration-style"
textDecorationStyle
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
textDecorationStyle = declaration "text-decoration-style"

-- | CSS Property "text-decoration-thickness"
textDecorationThickness
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
textDecorationThickness = declaration "text-decoration-thickness"

-- | CSS Property "text-emphasis"
textEmphasis
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
textEmphasis = declaration "text-emphasis"

-- | CSS Property "text-indent"
textIndent
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
textIndent = declaration "text-indent"

-- | CSS Property "text-justify"
textJustify
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
textJustify = declaration "text-justify"

-- | CSS Property "text-orientation"
textOrientation
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
textOrientation = declaration "text-orientation"

-- | CSS Property "text-overflow"
textOverflow
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
textOverflow = declaration "text-overflow"

-- | CSS Property "text-shadow"
textShadow
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
textShadow = declaration "text-shadow"

-- | CSS Property "text-transform"
textTransform
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
textTransform = declaration "text-transform"

-- | CSS Property "text-underline-position"
textUnderlinePosition
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
textUnderlinePosition = declaration "text-underline-position"

-- | CSS Property "top"
top
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
top = declaration "top"

-- | CSS Property "transform"
transform
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
transform = declaration "transform"

-- | CSS Property "transform-origin"
transformOrigin
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
transformOrigin = declaration "transform-origin"

-- | CSS Property "transform-style"
transformStyle
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
transformStyle = declaration "transform-style"

-- | CSS Property "transition"
transition
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
transition = declaration "transition"

-- | CSS Property "transition-delay"
transitionDelay
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
transitionDelay = declaration "transition-delay"

-- | CSS Property "transition-duration"
transitionDuration
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
transitionDuration = declaration "transition-duration"

-- | CSS Property "transition-property"
transitionProperty
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
transitionProperty = declaration "transition-property"

-- | CSS Property "transition-timing-function"
transitionTimingFunction
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
transitionTimingFunction = declaration "transition-timing-function"

-- | CSS Property "unicode-bidi"
unicodeBidi
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
unicodeBidi = declaration "unicode-bidi"

-- | CSS Property "user-select"
userSelect
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
userSelect = declaration "user-select"

-- | CSS Property "vertical-align"
verticalAlign
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
verticalAlign = declaration "vertical-align"

-- | CSS Property "visibility"
visibility
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
visibility = declaration "visibility"

-- | CSS Property "white-space"
whiteSpace
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
whiteSpace = declaration "white-space"

-- | CSS Property "widows"
widows
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
widows = declaration "widows"

-- | CSS Property "width"
width
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
width = declaration "width"

-- | CSS Property "word-break"
wordBreak
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
wordBreak = declaration "word-break"

-- | CSS Property "word-spacing"
wordSpacing
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
wordSpacing = declaration "word-spacing"

-- | CSS Property "word-wrap"
wordWrap
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
wordWrap = declaration "word-wrap"

-- | CSS Property "writing-mode"
writingMode
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
writingMode = declaration "writing-mode"

-- | CSS Property "z-index"
zIndex
    :: Text    -- ^ Value
    -> Css ()  -- ^ Return as CSS
zIndex = declaration "z-index"


