{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
-- module Tui where

import System.Directory
import Control.Concurrent
import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Control.Monad
import Cursor.Brick.TextField
import Cursor.TextField
import Cursor.Types
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as T
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
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

data TuiState =
  TuiState
    { stateCursor :: TextFieldCursor,
      _conn :: Socket
    }
  -- deriving (Eq, Show)

makeLenses ''TuiState

sendMess :: Socket -> String -> IO (Maybe String)
sendMess sock s = do
              sendAll sock $ C.pack s
              return Nothing

main :: IO ()
main = tui

tui :: IO ()
tui = do
  args <- getArgs
  case args of
    [] -> die "No argument to choose file to edit."
    (fp:_) -> do
      path <- resolveFile' fp
      maybeContents <- forgivingAbsence $ T.readFile (fromAbsFile path)
      let contents = fromMaybe "" maybeContents
      initialState <- buildInitialState contents
      endState <- defaultMain tuiApp initialState
      let contents' = rebuildTextFieldCursor (stateCursor endState)
      unless (contents == contents') $ T.writeFile (fromAbsFile path) contents'

data ResourceName =
  ResourceName
  deriving (Show, Eq, Ord)

tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty [("text", fg red), ("bg", fg blue)]
    }

buildInitialState :: Text -> IO TuiState
buildInitialState contents = do
  let tfc = makeTextFieldCursor contents
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4444")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  pure TuiState {stateCursor = tfc, _conn = sock}

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts =
  [ forceAttr "text" $
    centerLayer $
    border $
    padLeftRight 1 $ selectedTextFieldCursorWidget ResourceName (stateCursor ts)
  , forceAttr "bg" $ fill ' '
  ]

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye -> 
      let mDo ::
               (TextFieldCursor -> Maybe TextFieldCursor)
            -> EventM n (Next TuiState)
          mDo func = do
            let tfc = stateCursor s
            let tfc' = fromMaybe tfc $ func tfc
            let sock = s ^. conn
            let s' = s {stateCursor = tfc', _conn = sock}
            continue s'
       in case vtye of
            EvKey (KChar c) [] -> do
              let sock = s ^. conn
              liftIO (sendMess sock [c])
              mDo $ textFieldCursorInsertChar c . Just
            EvKey KUp [] -> do
              let sock = s ^. conn
              liftIO (sendMess sock "up")
              mDo textFieldCursorSelectPrevLine
            EvKey KDown [] -> do
              let sock = s ^. conn
              liftIO (sendMess sock "down")
              mDo textFieldCursorSelectNextLine
            EvKey KRight [] -> do
              let sock = s ^. conn
              liftIO (sendMess sock "right")
              mDo textFieldCursorSelectNextChar
            EvKey KLeft [] -> do
              let sock = s ^. conn
              liftIO (sendMess sock "left")
              mDo textFieldCursorSelectPrevChar
            EvKey KBS [] -> do
              let sock = s ^. conn
              liftIO (sendMess sock "backspace")
              mDo $ dullMDelete . textFieldCursorRemove
            EvKey KDel [] -> do
              let sock = s ^. conn
              liftIO (sendMess sock "delete")
              mDo $ dullMDelete . textFieldCursorDelete
            EvKey KEnter [] -> do
              let sock = s ^. conn
              liftIO (sendMess sock "enter")
              mDo $ Just . textFieldCursorInsertNewline . Just
            EvKey KEsc [] -> halt s
            _ -> continue s
    _ -> continue s