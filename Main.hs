module Main where
  import qualified Graphics.UI.Threepenny as UI
  import Graphics.UI.Threepenny.Core
  import Control.Monad (replicateM)
  import Data.List (intersperse)
  import Data.List.Split (chunksOf)
  import System.Environment (getArgs)

  updateValue :: UI.Element -> UI.Element -> a -> UI ()
  updateValue t b _ = do
    txt <- get value t
    element b # set UI.text (if txt == "" then " " else txt)
    element b # set UI.value txt
    element b # set (attr "tile") (if txt == "" then " " else txt)
    return ()

  setup :: Int -> Int -> Window -> UI ()
  setup w h window = do
    return window # set title "Roguelike Map Editor"

    tileLabel <- UI.span # set UI.text "Current Tile:"
    tileButton <- UI.input # set UI.value "."

    convertToCurrentButton <- UI.button # set UI.text "Convert Tiles To Current Tile Char"

    fileNameLabel <- UI.span # set UI.text "level name:"
    fileName <- UI.input # set UI.value "level1"

    tileButtons <- replicateM (w * h) (UI.button # set UI.text "." # set UI.value " " # set UI.height 5 # set UI.width 5)

    saveButton <- UI.button # set UI.text "Save Map"

    getBody window #+ [
                        column [
                                element tileLabel,
                                element tileButton
                               ], 
                        column [
                                element fileNameLabel,
                                element fileName,
                                grid (chunksOf w (map element tileButtons))
                               ], 
                        column [
                                element saveButton
                               ], 
                        column [
                                element convertToCurrentButton
                               ]
                      ]

    {- These next two lines are magic and I forget what they do -}
    let buttonsWithCallbacks = map (updateValue tileButton) tileButtons
    let buttonsCallbacksWithButtons = zip tileButtons buttonsWithCallbacks

    {- 
    This is another one of those "magic" lines I don't quite understand.
    All I know is it maps the function to update its value to the current tile whenever its clicked
    -}
    mapM_ (\(but, zipMap) -> on UI.click but zipMap) buttonsCallbacksWithButtons

    {- Save Button Callback -}
    on UI.click saveButton $ \_ -> do
      tiles <- mapM (\b -> get UI.value b) tileButtons
      targetFile <- get UI.value fileName
      liftIO $ do
        writeFile (targetFile ++ ".txt") ""
        mapM_ (appendFile (targetFile ++ ".txt")) (concat $ intersperse ["\n"] (chunksOf w tiles))

    {- Callback to turn every tile to the current tile character (for selecting your own "empty" tile, etc) -}
    on UI.click convertToCurrentButton $ \_ -> do
      txt <- get value tileButton
      mapM_ (\b -> element b # set UI.text txt) tileButtons
      return ()

  main :: IO ()
  main = do
    sz <- fmap head getArgs
    sz' <- fmap (head . drop 1) getArgs
    let w = read sz :: Int
    let h  = read sz' :: Int
    startGUI defaultConfig (setup w h)