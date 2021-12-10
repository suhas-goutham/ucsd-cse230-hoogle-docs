{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
-- module Tui where

import System.Directory
import Control.Concurrent
import Brick (txt)
import Brick.AttrMap
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
import Cursor.Brick.TextField
import Cursor.TextField
import Cursor.Types
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as IO
import Graphics.Vty
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

-- | Brick Imports Start

import qualified Brick.Widgets.Border as Bdr
import qualified Brick.Widgets.Border.Style as BdrS
import qualified Data.Text as T
import qualified Brick.Widgets.Center as Ctr
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.ProgressBar as P

-- | Brick Imports End

data TuiState =
  TuiState
    { stateCursor :: TextFieldCursor,
      _conn :: Socket
    }
  deriving (Eq, Show)

makeLenses ''TuiState

data ResourceName =
    ResourceName
  | UsernameField
  deriving (Show, Eq, Ord)

-- | Login Form with Username and Filename fields
data LoginForm =
  LoginForm {
    _uname :: T.Text
  } deriving (Show)
makeLenses ''LoginForm

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
    EnterPage (Form LoginForm () ResourceName)
  | EditorPage TuiState

sendMess :: Socket -> String -> IO (Maybe String)
sendMess sock s = do
              sendAll sock $ C.pack s
              return Nothing

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
      endForm <- customMain initialVty buildVty Nothing app (EnterPage initForm)
      return ()
      -- endState <- defaultMain tuiApp initialState
      -- let contents' = rebuildTextFieldCursor (stateCursor endState)
      -- unless (contents == contents') $ IO.writeFile (fromAbsFile path) contents'


-- %%%%%%%%%%%%%%%%%%%%%
-- Actual Editor App
-- %%%%%%%%%%%%%%%%%%%%%

app :: App TEditor () ResourceName
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
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4444")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  pure TuiState {stateCursor = tfc, _conn = sock}

handleTEditorEvent :: TEditor -> BrickEvent ResourceName () -> EventM ResourceName (Next TEditor)
handleTEditorEvent gs ev = case gs of
  EnterPage lf -> handleLoginEvent lf ev
  EditorPage ts -> handleTuiEvent ts ev

readFileTui :: IO TuiState
readFileTui =  do
  let fp = "abc.txt"
  path <- resolveFile' fp
  maybeContents <- forgivingAbsence $ IO.readFile (fromAbsFile path)
  let contents = fromMaybe "" maybeContents
  buildInitialState contents

handleLoginEvent :: Form LoginForm () ResourceName -> BrickEvent ResourceName () -> EventM ResourceName (Next TEditor)
handleLoginEvent s e = do
                        let name = ""
                        case e of
                              VtyEvent EvResize {}          -> continue (EnterPage s)
                              VtyEvent (EvKey KEsc [])      -> halt (EnterPage s)
                              VtyEvent (EvKey KEnter [])    -> do
                                                                let name = formState s ^. uname
                                                                tuiSt <- liftIO readFileTui
                                                                let sock = tuiSt ^. conn
                                                                liftIO (sendMess sock (T.unpack name))
                                                                continue (EditorPage tuiSt)
                              VtyEvent (EvKey (KChar c) []) -> do
                                                                s' <- handleFormEvent e s
                                                                continue (EnterPage s')
                              _                             -> do
                                                                continue (EnterPage s)

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM ResourceName (Next TEditor)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      let mDo ::
               (TextFieldCursor -> Maybe TextFieldCursor)
            -> EventM n (Next TEditor)
          mDo func = do
            let tfc = stateCursor s
            let tfc' = fromMaybe tfc $ func tfc
            let sock = s ^. conn
            let s' = s {stateCursor = tfc', _conn = sock}
            continue (EditorPage s')
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
            EvKey KEsc [] -> halt (EditorPage s)
            _ -> continue (EditorPage s)
    _ -> continue (EditorPage s)


-- | Helper Functions for appDraw

borderWidth :: Int
borderWidth = 0
borderHeight :: Int
borderHeight = 0

drawEditor :: TEditor -> [Widget ResourceName]
drawEditor g = case g of
  EnterPage  loginForm -> drawForm loginForm
  EditorPage tuiState  -> drawTui tuiState

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

-- | Make Login Form
mkForm :: LoginForm -> Form LoginForm e ResourceName
mkForm =
  let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "User Name" @@=
                   editTextField uname UsernameField (Just 1)
               ]