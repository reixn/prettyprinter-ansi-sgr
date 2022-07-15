{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | __Warning:__ Internal module. May change arbitrarily between versions.
module Prettyprinter.Render.TerminalSgr.Internal
  ( -- * Styling
    AnsiStyle,
    fromSgr,
    fromSgrs,

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

import Data.IORef
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Prettyprinter
import Prettyprinter.Render.Util.Panic
import qualified System.Console.ANSI as ANSI
import System.IO (Handle, hPutChar, stdout)

fromSgr :: ANSI.SGR -> AnsiStyle
fromSgr s = SetAnsiStyle [s]

fromSgrs :: [ANSI.SGR] -> AnsiStyle
fromSgrs = SetAnsiStyle

renderLazy :: SimpleDocStream AnsiStyle -> TL.Text
renderLazy =
  let push x = (x :)

      unsafePeek [] = panicPeekedEmpty
      unsafePeek (x : _) = x

      unsafePop [] = panicPoppedEmpty
      unsafePop (x : xs) = (x, xs)

      go :: [AnsiStyle] -> SimpleDocStream AnsiStyle -> TLB.Builder
      go s sds = case sds of
        SFail -> panicUncaughtFail
        SEmpty -> mempty
        SChar c rest -> TLB.singleton c <> go s rest
        SText _ t rest -> TLB.fromText t <> go s rest
        SLine i rest -> TLB.singleton '\n' <> TLB.fromText (T.replicate i " ") <> go s rest
        SAnnPush style rest ->
          let currentStyle = unsafePeek s
              newStyle = style <> currentStyle
           in TLB.fromText (styleToRawText newStyle) <> go (push style s) rest
        SAnnPop rest ->
          let (_currentStyle, s') = unsafePop s
              newStyle = unsafePeek s'
           in TLB.fromText (styleToRawText newStyle) <> go s' rest
   in TLB.toLazyText . go [mempty]

renderIO :: Handle -> SimpleDocStream AnsiStyle -> IO ()
renderIO h sdoc = do
  styleStackRef <- newIORef [mempty]

  let push x = modifyIORef' styleStackRef (x :)
      unsafePeek =
        readIORef styleStackRef >>= \case
          [] -> panicPeekedEmpty
          x : _ -> pure x
      unsafePop =
        readIORef styleStackRef >>= \case
          [] -> panicPoppedEmpty
          x : xs -> writeIORef styleStackRef xs >> pure x

  let go = \case
        SFail -> panicUncaughtFail
        SEmpty -> pure ()
        SChar c rest -> do
          hPutChar h c
          go rest
        SText _ t rest -> do
          T.hPutStr h t
          go rest
        SLine i rest -> do
          hPutChar h '\n'
          T.hPutStr h (T.replicate i (T.singleton ' '))
          go rest
        SAnnPush style rest -> do
          currentStyle <- unsafePeek
          let newStyle = style <> currentStyle
          push newStyle
          T.hPutStr h (styleToRawText newStyle)
          go rest
        SAnnPop rest -> do
          _currentStyle <- unsafePop
          newStyle <- unsafePeek
          T.hPutStr h (styleToRawText newStyle)
          go rest
  go sdoc
  readIORef styleStackRef >>= \case
    [] -> panicStyleStackFullyConsumed
    [_] -> pure ()
    xs -> panicStyleStackNotFullyConsumed (length xs)

panicStyleStackFullyConsumed :: void
panicStyleStackFullyConsumed =
  error
    ( "There is no empty style left at the end of rendering"
        ++ " (but there should be). Please report this as a bug."
    )

panicStyleStackNotFullyConsumed :: Int -> void
panicStyleStackNotFullyConsumed len =
  error
    ( "There are " <> show len <> " styles left at the"
        ++ "end of rendering (there should be only 1). Please report"
        ++ " this as a bug."
    )

newtype AnsiStyle = SetAnsiStyle {styleSgr :: [ANSI.SGR]}
  deriving (Eq, Show)

instance Semigroup AnsiStyle where
  cs1 <> cs2 = SetAnsiStyle (styleSgr cs1 <> styleSgr cs2)

-- | 'mempty' does nothing, which is equivalent to inheriting the style of the
-- surrounding doc, or the terminal’s default if no style has been set yet.
instance Monoid AnsiStyle where
  mempty = SetAnsiStyle []
  mappend = (<>)

styleToRawText :: AnsiStyle -> Text
styleToRawText as = T.pack $ ANSI.setSGRCode (ANSI.Reset : styleSgr as)

-- | @('renderStrict' sdoc)@ takes the output @sdoc@ from a rendering and
-- transforms it to strict text.
renderStrict :: SimpleDocStream AnsiStyle -> Text
renderStrict = TL.toStrict . renderLazy

-- | @('putDoc' doc)@ prettyprints document @doc@ to standard output using
-- 'defaultLayoutOptions'.
--
-- >>> putDoc ("hello" <+> "world")
-- hello world
--
-- @
-- 'putDoc' = 'hPutDoc' 'stdout'
-- @
putDoc :: Doc AnsiStyle -> IO ()
putDoc = hPutDoc stdout

-- | Like 'putDoc', but instead of using 'stdout', print to a user-provided
-- handle, e.g. a file or a socket using 'defaultLayoutOptions'.
--
-- > main = withFile "someFile.txt" (\h -> hPutDoc h (vcat ["vertical", "text"]))
--
-- @
-- 'hPutDoc' h doc = 'renderIO' h ('layoutPretty' 'defaultLayoutOptions' doc)
-- @
hPutDoc :: Handle -> Doc AnsiStyle -> IO ()
hPutDoc h doc = renderIO h (layoutPretty defaultLayoutOptions doc)