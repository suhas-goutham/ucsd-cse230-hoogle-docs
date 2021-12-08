{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
-- module Tui where

import System.Directory
import Control.Concurrent
import Brick.BChan (newBChan, writeBChan)
import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Control.Monad
import Control.Monad.Fix (fix)
import Cursor.Brick.TextField
import Cursor.TextField
import Cursor.Types
import Data.Maybe
import Data.Text (Text, isInfixOf, unpack)
import qualified Data.Text.IO as T
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Graphics.Vty
import Path
import Path.IO
import System.Environment
import System.Exit
import Text.Show.Pretty

import Control.Concurrent        (forkIO, threadDelay)
import Control.Monad             (unless)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Control.Lens
  ( makeFieldsNoPrefix,
    makeLenses,
    (%~),
    (&),
    (?~),
    (^.),
  )

import GHC.Generics (Generic)
import Control.Monad.IO.Class (MonadIO(liftIO))

-- | Brick Imports Start

import qualified Brick.Widgets.Border as Bdr
import qualified Brick.Widgets.Border.Style as BdrS
import qualified Data.Text as Tex
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.ProgressBar as P

-- | Brick Imports End

data TuiState =
  TuiState
    { stateCursor :: TextFieldCursor
    }
  deriving (Eq, Show)

makeLenses ''TuiState

sendMess :: Socket -> String -> IO (Maybe String)
sendMess sock s = do
              sendAll sock $ C.pack s
              return Nothing

main :: IO ()
main = tui

data ServerMessage
  = S_Character Char
  | S_Up
  | S_Down
  | S_Right
  | S_Left
  | S_Enter
  | S_Delete
  | S_Backspace
  | S_Quit
  deriving (Generic)

newtype ConnectionTick = ConnectionTick ServerMessage

tui :: IO ()
tui = do
  args <- getArgs
  case args of
    [] -> die "No argument to choose file to edit."
    (fp:_) -> do
        path <- resolveFile' fp
        maybeContents <- forgivingAbsence $ T.readFile (fromAbsFile path)
        let contents = fromMaybe "" maybeContents
        addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4444")
        let serveraddr = head addrinfos
        sock <- socket (addrFamily serveraddr) Stream defaultProtocol
        connect sock (addrAddress serveraddr)
        initialState <- buildInitialState contents sock
        --   endState <- defaultMain tuiApp initialState
        eventChan <- Brick.BChan.newBChan 10
        let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
        initialVty <- buildVty

        reader <- forkIO $ fix $ \loop -> do
              message <- recvMess sock
              case message of
                "up"        -> do
                                writeBChan eventChan $ ConnectionTick S_Up
                                loop
                "down"      -> do
                                writeBChan eventChan $ ConnectionTick S_Down
                                loop
                "left"      -> do
                                writeBChan eventChan $ ConnectionTick S_Left
                                loop
                "right"     -> do
                                writeBChan eventChan $ ConnectionTick S_Right
                                loop
                "backspace" -> do
                                writeBChan eventChan $ ConnectionTick S_Backspace
                                loop
                "delete"    -> do
                                writeBChan eventChan $ ConnectionTick S_Delete
                                loop
                "enter"     -> do
                                writeBChan eventChan $ ConnectionTick S_Enter
                                loop
                _           -> do
                                writeBChan eventChan $ ConnectionTick (S_Character (head message))
                                loop

        quit <- forkIO $ fix $ \loop -> do
                line <- getChar
                if line == '\ESC'
                    then do
                        writeBChan eventChan $ ConnectionTick S_Quit
                    else do
                        loop

        endState <- customMain initialVty buildVty (Just eventChan) tuiApp initialState
        putStr ( unpack (rebuildTextFieldCursor (stateCursor endState)))
        -- | Edits on the Viewer side are not committed to the document
        -- let contents' = rebuildTextFieldCursor (stateCursor endState)
        -- unless (contents == contents') $ T.writeFile (fromAbsFile path) contents'

recvMess sock = do
    x <- recv sock 1024
    return (C.unpack x)

data ResourceName =
  ResourceName
  deriving (Show, Eq, Ord)

tuiApp :: App TuiState ConnectionTick ResourceName
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty [("text", fg red), ("bg", fg blue)]
    }

buildInitialState :: Text -> Socket -> IO TuiState
buildInitialState contents sock = do
  let tfc = makeTextFieldCursor contents
  pure TuiState {stateCursor = tfc}

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = [addBorder "Editor" (C.center $ vLimitPercent 80 $ hLimitPercent 80 (Widget Fixed Fixed $ do
              ctx <- getContext
              render $ selectedTextFieldCursorWidget ResourceName (stateCursor ts)))]

  -- [ forceAttr "text" $
  --   centerLayer $
  --   border $
  --   padLeftRight 1 $ selectedTextFieldCursorWidget ResourceName (stateCursor ts)
  -- , forceAttr "bg" $ fill ' '
  -- ]

mDo s func = do
                let tfc = stateCursor s
                let tfc' = fromMaybe tfc $ func tfc
                let s' = s {stateCursor = tfc'}
                continue s'

handleTuiEvent :: TuiState -> BrickEvent n ConnectionTick -> EventM n (Next TuiState)
handleTuiEvent s (AppEvent (ConnectionTick csReceived)) =
                                case csReceived of
                                    S_Character mes     -> mDo s $ textFieldCursorInsertChar mes . Just
                                    S_Up                -> mDo s textFieldCursorSelectPrevLine
                                    S_Down              -> mDo s textFieldCursorSelectNextLine
                                    S_Left              -> mDo s textFieldCursorSelectPrevChar
                                    S_Right             -> mDo s textFieldCursorSelectNextChar
                                    S_Backspace         -> mDo s $ dullMDelete . textFieldCursorRemove
                                    S_Delete            -> mDo s $ dullMDelete . textFieldCursorDelete
                                    S_Enter             -> mDo s $ Just . textFieldCursorInsertNewline . Just
                                    S_Quit              -> halt s
handleTuiEvent s _ = do 
    continue s

-- | Helper Functions for UI

-- | Adds a rounded border to a widget with the given label
addBorder :: Tex.Text -> Widget ResourceName -> Widget ResourceName
addBorder t = withBorderStyle BdrS.unicodeRounded . Bdr.borderWithLabel (txt t)