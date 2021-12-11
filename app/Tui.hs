{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- module Tui where

import qualified System.Directory as Dir
import Control.Concurrent
import Brick (txt)
import Brick.AttrMap
import Brick.BChan (newBChan, writeBChan)
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Forms
 ( Form
  , newForm
  , formState
  , formFocus
  , setFieldValid
  , renderForm
  , handleFormEvent
  , invalidFields
  , allFieldsValid
  , focusedFormInputAttr
  , invalidFormInputAttr
  , checkboxField
  , radioField
  , editShowableField
  , editTextField
  , editPasswordField
  , (@@=)
  )
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )
import Brick.Widgets.List as L
import Control.Monad
import Control.Monad.Fix (fix)
import Cursor.Brick.TextField
import Cursor.TextField
import Cursor.Types
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as IO
import qualified Data.Vector as Vec
import Graphics.Vty
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Path
import Path.IO
-- import System.Directory
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
import qualified Data.Text as T
import qualified Brick.Widgets.Center as Ctr
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.ProgressBar as P
import Control.Exception (handle)

-- | Brick Imports End

data TuiState =
  TuiState
    { stateCursor :: TextFieldCursor,
      _currentFile :: FilePath
    }
  deriving (Eq, Show)

makeLenses ''TuiState

data ResourceName =
    ResourceName
  | UsernameField
  | FilenameField
  deriving (Show, Eq, Ord)

-- | Login Form with Username field
data LoginForm =
  LoginForm {
    _uname :: T.Text
  } deriving (Show)
makeLenses ''LoginForm

-- | Login Form with Filename field
data NewFileForm =
  NewFileForm {
    _fname :: T.Text
  } deriving (Show)
makeLenses ''NewFileForm

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

-- | MenuList for file selection
type MenuList = L.List ResourceName T.Text

theMap :: AttrMap
theMap = attrMap defAttr
  [ (E.editAttr, white `on` black)
  , (E.editFocusedAttr, black `on` yellow)
  , (invalidFormInputAttr, white `on` red)
  , (focusedFormInputAttr, black `on` yellow)
  , ("text", fg red)
  , ("bg", fg blue)
  ]

data TEditor =
    EnterPage ((Form LoginForm ConnectionTick ResourceName), Socket)
  | ActionSelectPage (MenuList, T.Text, Socket)
  | NewFilePage ((Form NewFileForm ConnectionTick ResourceName), T.Text, Socket)
  | FileSelectPage (MenuList, T.Text, Socket)
  | EditorPage (TuiState, T.Text, Socket)
  | ViewerPage (TuiState, T.Text, Socket)

sendMess :: Socket -> String -> IO (Maybe String)
sendMess sock s = do
              sendAll sock $ C.pack s
              return Nothing

recvMess sock = do
    x <- recv sock 1024
    return (C.unpack x)

main :: IO ()
main = tui

tui :: IO ()
tui = do
  let args = ["abc.txt"]
  -- args <- getArgs
  case args of
    [] -> die "No argument to choose file to edit."
    (fp:_) -> do
      let initForm = mkForm (LoginForm {_uname = ""})
      let buildVty = do
                      v <- mkVty =<< standardIOConfig
                      setMode (outputIface v) Mouse True
                      return v
      initialVty <- buildVty
      sock <- openConnection

      eventChan <- Brick.BChan.newBChan 10

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

      endState <- customMain initialVty buildVty (Just eventChan) app (EnterPage (initForm, sock))
      return ()


-- %%%%%%%%%%%%%%%%%%%%%
-- Actual Editor App
-- %%%%%%%%%%%%%%%%%%%%%

app :: App TEditor ConnectionTick ResourceName
app = App{
          appDraw = drawEditor
        , appHandleEvent = handleTEditorEvent
        , appChooseCursor = showFirstCursor
        , appStartEvent = return
        , appAttrMap = const theMap

}

buildInitialState :: Text -> IO TuiState
buildInitialState contents = do
  let tfc = makeTextFieldCursor contents
  pure TuiState {stateCursor = tfc, _currentFile = ""}

handleTEditorEvent :: TEditor -> BrickEvent ResourceName ConnectionTick -> EventM ResourceName (Next TEditor)
handleTEditorEvent gs ev = case gs of
  EnterPage lf -> handleLoginEvent lf ev
  ActionSelectPage ops -> handleActionSelectEvent ops ev
  NewFilePage ff -> handleNewFileEvent ff ev
  FileSelectPage l -> handleFileSelectEvent l ev
  EditorPage ts -> handleTuiOwnerEvent ts ev
  ViewerPage ts -> handleTuiViewerEvent ts ev

readFileTui :: T.Text -> IO TuiState
readFileTui fname =  do
  let fp = "shared_files/" ++ T.unpack fname
  path <- resolveFile' fp
  maybeContents <- forgivingAbsence $ IO.readFile (fromAbsFile path)
  let contents = fromMaybe "" maybeContents
  buildInitialState contents

openConnection :: IO Socket
openConnection = do
                  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4545")
                  let serveraddr = head addrinfos
                  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
                  connect sock (addrAddress serveraddr)
                  return sock

handleLoginEvent :: (Form LoginForm ConnectionTick ResourceName, Socket) -> BrickEvent ResourceName ConnectionTick -> EventM ResourceName (Next TEditor)
handleLoginEvent (s, sock) e = do
                        let name = ""
                        case e of
                              VtyEvent EvResize {}          -> continue (EnterPage (s,sock))
                              VtyEvent (EvKey KEsc [])      -> do
                                                                liftIO (sendMess sock "quit")
                                                                halt (EnterPage (s,sock))
                              VtyEvent (EvKey KEnter [])    -> do
                                                                let name = formState s ^. uname
                                                                let text2 = ["Create New File", "Edit/View Existing File"]
                                                                continue (ActionSelectPage ((L.list ResourceName (Vec.fromList text2) 2),name, sock))
                              VtyEvent (EvKey (KChar c) []) -> do
                                                                s' <- handleFormEvent e s
                                                                continue (EnterPage (s', sock))
                              _                             -> do
                                                                continue (EnterPage (s, sock))

handleActionSelectEvent :: (MenuList, T.Text, Socket) -> BrickEvent ResourceName ConnectionTick  -> EventM ResourceName (Next TEditor)
handleActionSelectEvent (s,name, sock) (VtyEvent e) = do
                                          let initForm = mkForm (LoginForm {_uname = ""})
                                          let initNewForm = mkNewFileForm (NewFileForm {_fname = ""})
                                          case e of 
                                            (EvKey KEsc  []) -> do
                                                                  continue (EnterPage (initForm, sock))
                                            (EvKey KEnter []) 
                                                   | Just i <- L.listSelected s -> case i of 
                                                     0 -> continue (NewFilePage (initNewForm, name, sock))
                                                     1 -> do
                                                            text1 <- liftIO getFileList
                                                            let text2 = map T.pack text1
                                                            let length_t = length text2
                                                            continue (FileSelectPage ((L.list ResourceName (Vec.fromList text2) length_t), name, sock))
                                            ev -> do
                                              ss2 <- L.handleListEvent ev s
                                              continue (ActionSelectPage (ss2, name, sock))

handleActionSelectEvent (s,name, sock) _   = do continue (ActionSelectPage (s, name, sock))

handleNewFileEvent :: (Form NewFileForm ConnectionTick ResourceName, T.Text, Socket) -> BrickEvent ResourceName ConnectionTick -> EventM ResourceName (Next TEditor)
handleNewFileEvent (s, name, sock) e = do
                          let filname = ""
                          case e of
                                VtyEvent EvResize {}          -> continue (NewFilePage (s,name, sock))
                                VtyEvent (EvKey KEsc [])      -> do
                                                                  let text2 = ["Create New File", "Edit/View Existing File"]
                                                                  continue (ActionSelectPage ((L.list ResourceName (Vec.fromList text2) 2),name, sock))
                                VtyEvent (EvKey KEnter [])    -> do
                                                                  let filname = T.pack (T.unpack name ++ "_" ++ T.unpack (formState s ^. fname) ++ ".txt")
                                                                  tuiSt <- liftIO (readFileTui filname)
                                                                  let tfc = stateCursor tuiSt
                                                                  let current = T.unpack filname
                                                                  let s' = tuiSt {stateCursor = tfc, _currentFile = current}
                                                                  continue (EditorPage (s',name, sock))
                                                                  
                                VtyEvent (EvKey (KChar c) []) -> do
                                                                  s' <- handleFormEvent e s
                                                                  continue (NewFilePage (s',name, sock))
                                _                             -> do
                                                                  continue (NewFilePage (s,name, sock))




handleFileSelectEvent :: (MenuList, T.Text, Socket) -> BrickEvent n e -> EventM ResourceName (Next TEditor)
handleFileSelectEvent (s,name, sock) (VtyEvent e) = do
                            let initForm = mkForm (LoginForm {_uname = ""})
                            case e of
                              (EvKey KEsc []) -> do
                                                  continue (EnterPage (initForm, sock))
                              (EvKey KEnter [])
                                    | Just i <- L.listSelectedElement s -> do
                                        tuiSt <- liftIO (readFileTui (snd i))
                                        let tfc = stateCursor tuiSt
                                        let current = T.unpack (snd i)
                                        let filename = snd i
                                        let s' = tuiSt {stateCursor = tfc, _currentFile = current}
                                        if (T.isPrefixOf name filename)
                                          then continue (EditorPage (s',name, sock))
                                          else continue (ViewerPage (s',name, sock))
                              ev -> do
                                ss2 <- L.handleListEvent ev s
                                continue (FileSelectPage (ss2, name, sock))
handleFileSelectEvent (s,name, sock) _ = do
                                          continue (FileSelectPage (s, name, sock))

-- handleTuiEvent :: (TuiState,T.Text, Socket, Bool) -> BrickEvent n e -> EventM ResourceName (Next TEditor)
-- handleTuiEvent (s, name, sock, isOwner) e = case isOwner of
--   False -> handleOwnerEvent (s, name, sock) e
--   True -> handleOwnerEvent (s, name, sock) e

handleTuiViewerEvent :: (TuiState,T.Text, Socket) -> BrickEvent n ConnectionTick -> EventM ResourceName (Next TEditor)
handleTuiViewerEvent (s, name, sock) (AppEvent (ConnectionTick csReceived)) = do
                                case csReceived of
                                    S_Character mes     -> mDo (s, name, sock) $ textFieldCursorInsertChar mes . Just
                                    S_Up                -> mDo (s, name, sock) textFieldCursorSelectPrevLine
                                    S_Down              -> mDo (s, name, sock) textFieldCursorSelectNextLine
                                    S_Left              -> mDo (s, name, sock) textFieldCursorSelectPrevChar
                                    S_Right             -> mDo (s, name, sock) textFieldCursorSelectNextChar
                                    S_Backspace         -> mDo (s, name, sock) $ dullMDelete . textFieldCursorRemove
                                    S_Delete            -> mDo (s, name, sock) $ dullMDelete . textFieldCursorDelete
                                    S_Enter             -> mDo (s, name, sock) $ Just . textFieldCursorInsertNewline . Just
                                    S_Quit              -> do
                                                            let text2 = ["Create New File", "Edit/View Existing File"]
                                                            continue (ActionSelectPage ((L.list ResourceName (Vec.fromList text2) 2),name, sock))
handleTuiViewerEvent (s, name, sock) (VtyEvent e) = do 
                                case e of
                                  (EvKey KEsc []) -> do
                                                      -- liftIO (close sock)
                                                      -- sock <- liftIO openConnection
                                                      let text2 = ["Create New File", "Edit/View Existing File"]
                                                      continue (ActionSelectPage ((L.list ResourceName (Vec.fromList text2) 2),name, sock))
                                  _               ->  continue (ViewerPage (s, name, sock))

handleTuiOwnerEvent :: (TuiState,T.Text, Socket) -> BrickEvent n e -> EventM ResourceName (Next TEditor)
handleTuiOwnerEvent (s, name, sock) e = do
  case e of
    VtyEvent vtye ->
      let mDo ::
               (TextFieldCursor -> Maybe TextFieldCursor)
            -> EventM n (Next TEditor)
          mDo func = do
            let tfc = stateCursor s
            let tfc' = fromMaybe tfc $ func tfc
            let s' = s {stateCursor = tfc'}
            continue (EditorPage (s',name, sock))
       in case vtye of
            EvKey (KChar c) [] -> do
              liftIO (sendMess sock [c])
              mDo $ textFieldCursorInsertChar c . Just
            EvKey KUp [] -> do
              liftIO (sendMess sock "up")
              mDo textFieldCursorSelectPrevLine
            EvKey KDown [] -> do
              liftIO (sendMess sock "down")
              mDo textFieldCursorSelectNextLine
            EvKey KRight [] -> do
              liftIO (sendMess sock "right")
              mDo textFieldCursorSelectNextChar
            EvKey KLeft [] -> do
              liftIO (sendMess sock "left")
              mDo textFieldCursorSelectPrevChar
            EvKey KBS [] -> do
              liftIO (sendMess sock "backspace")
              mDo $ dullMDelete . textFieldCursorRemove
            EvKey KDel [] -> do
              liftIO (sendMess sock "delete")
              mDo $ dullMDelete . textFieldCursorDelete
            EvKey KEnter [] -> do
              liftIO (sendMess sock "enter")
              mDo $ Just . textFieldCursorInsertNewline . Just
            EvKey KEsc [] -> do
                              let contents' = rebuildTextFieldCursor (stateCursor s)
                              liftIO (putStrLn (T.unpack contents'))
                              let fp = "shared_files/" ++ (s ^. currentFile)
                              path <- resolveFile' fp
                              liftIO (IO.writeFile (fromAbsFile path) contents')

                              -- liftIO (close sock)

                              -- sock <- liftIO openConnection

                              let text2 = ["Create New File", "Edit/View Existing File"]
                              continue (ActionSelectPage ((L.list ResourceName (Vec.fromList text2) 2),name, sock))
                              
            _ -> continue (EditorPage (s,name, sock))
    _ -> continue (EditorPage (s,name, sock))

-- | Helper Functions for appDraw

borderWidth :: Int
borderWidth = 0
borderHeight :: Int
borderHeight = 0

drawEditor :: TEditor -> [Widget ResourceName]
drawEditor g = case g of
  EnterPage  (loginForm, sock) -> drawForm loginForm
  ActionSelectPage (actionList, name, sock) -> drawList actionList
  NewFilePage (f,name, sock) -> drawFileForm f
  FileSelectPage (fileList, name, sock) -> drawList fileList
  EditorPage (tuiState, name, sock)  -> drawTui tuiState
  ViewerPage (tuiState, name, sock)  -> drawTui tuiState

-- | Draw Text Editor UI
drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = [addBorder "Editor" (Ctr.center $ vLimitPercent 80 $ hLimitPercent 80 (Widget Fixed Fixed $ do
              ctx <- getContext
              render $ selectedTextFieldCursorWidget ResourceName (stateCursor ts)))]

-- | Adds a rounded border to a widget with the given label
addBorder :: T.Text -> Widget ResourceName -> Widget ResourceName
addBorder t = withBorderStyle BdrS.unicodeRounded . Bdr.borderWithLabel (txt t)

-- | Draws Login Form
drawForm :: Form LoginForm e ResourceName -> [Widget ResourceName]
drawForm f = [Ctr.vCenter $ Ctr.hCenter form <=> Ctr.hCenter help]
    where
        form = Bdr.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        help = padTop (Pad 1) $ Bdr.borderWithLabel (str "Help") body
        body = str "- Name is free-form text\n"

-- | Draws Login Form
drawFileForm :: Form NewFileForm e ResourceName -> [Widget ResourceName]
drawFileForm f = [Ctr.vCenter $ Ctr.hCenter form <=> Ctr.hCenter help]
    where
        form = Bdr.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        help = padTop (Pad 1) $ Bdr.borderWithLabel (str "Help") body
        body = str "- Please enter file names without extension\n"

-- | Make Login Form
mkForm :: LoginForm -> Form LoginForm e ResourceName
mkForm =
  let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "User Name" @@=
                   editTextField uname UsernameField (Just 1)
               ]

-- | Make New File Form
mkNewFileForm :: NewFileForm -> Form NewFileForm e ResourceName
mkNewFileForm =
  let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "File Name" @@=
                   editTextField fname FilenameField (Just 1)
               ]

-- | Renders a list widget 
drawList :: MenuList -> [Widget ResourceName]
drawList l = [listWidget]
  where
    listWidget = L.renderList listDrawElement True l

-- | Draws an element of a list based on if it is selected
listDrawElement :: Bool -> T.Text -> Widget n
listDrawElement sel t = Ctr.hCenter $ txt symbol <+> txt t
  where
    symbol =
      if sel
        then "-> "
        else "  "

-- Gets the current list of files in the database
getFileList :: IO [String]
getFileList = do
  Dir.setCurrentDirectory "shared_files"
  _cd <- Dir.getCurrentDirectory
  _file <- Dir.getDirectoryContents _cd
  onlyFiles <- filterM Dir.doesFileExist _file
  Dir.setCurrentDirectory ".."
  return onlyFiles

mDo :: (TuiState,T.Text, Socket) -> (TextFieldCursor -> Maybe TextFieldCursor) -> EventM n (Next TEditor)
mDo (s, name, sock) func = do
                let tfc = stateCursor s
                let tfc' = fromMaybe tfc $ func tfc
                let s' = s {stateCursor = tfc'}
                continue (ViewerPage (s', name, sock))