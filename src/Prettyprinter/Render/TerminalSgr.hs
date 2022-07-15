module Prettyprinter.Render.TerminalSgr
  ( -- * Styling
    AnsiStyle,
    Color (..),
    fromSgr,
    fromSgrs,

    -- ** Font color
    color,
    colorDull,

    -- ** Background color
    bgColor,
    bgColorDull,

    -- ** Font style
    bold,
    italicized,
    underlined,

    -- * Conversion to ANSI-infused 'Text'
    renderLazy,
    renderStrict,

    -- * Render directly to 'stdout'
    renderIO,

    -- ** Convenience functions
    putDoc,
    hPutDoc,
  )
where

import Prettyprinter.Render.TerminalSgr.Internal
import System.Console.ANSI

color :: Color -> AnsiStyle
color = fromSgr . SetColor Foreground Vivid

colorDull :: Color -> AnsiStyle
colorDull = fromSgr . SetColor Foreground Dull

bgColor :: Color -> AnsiStyle
bgColor = fromSgr . SetColor Background Vivid

bgColorDull :: Color -> AnsiStyle
bgColorDull = fromSgr . SetColor Background Dull

bold :: AnsiStyle
bold = fromSgr $ SetConsoleIntensity BoldIntensity

italicized :: AnsiStyle
italicized = fromSgr $ SetItalicized True

underlined :: AnsiStyle
underlined = fromSgr $ SetUnderlining SingleUnderline