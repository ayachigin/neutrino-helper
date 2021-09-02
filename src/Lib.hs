module Lib (
    translate
)where

import Control.Monad ( forM_ )
import System.Exit ( ExitCode )
import System.FilePath ( pathSeparator, (</>), normalise, takeBaseName )
import System.Directory ( doesDirectoryExist, listDirectory, createDirectoryIfMissing)
import System.Process ( system )
import System.IO (hFlush, stdout)

label :: String
label = normalise "score/label"

data Option = Option
    { baseName :: String
    , styleShift :: Float
    , pitchShift :: Float
    , formantShift :: Float
    , labelFull :: String
    , labelMono :: String 
    } deriving (Show, Eq)

translate :: IO ()
translate = do
    createDirectoryIfMissing True "wav"
    models <- modelYouUse
    songs <- songToTranslate
    print songs
    forM_ songs $ \(song, songPath) -> do
        let baseName = takeBaseName songPath
            option = Option
                { baseName = baseName
                , styleShift = 0
                , pitchShift = 1.0
                , formantShift = 1.0
                , labelFull = label </> "full" </> baseName ++ ".lab"
                , labelMono = label </> "mono" </> baseName ++ ".lab"
                }
        translate_ models option songPath

translate_
    models 
    (Option baseName styleShift pitchShift formantShift labelFull labelMono)
    songPath = do
    putStrLn $ "songPath on translate: " ++ songPath
    forM_ models $ \model -> do
        let modelPath = "model" </> model ++ [pathSeparator]
        musicXMLtoLabel songPath labelFull labelMono
        neutrino labelFull baseName modelPath
        world baseName pitchShift formantShift model
        nsf labelFull baseName model

musicXMLtoLabel :: String -> String -> String -> IO ExitCode
musicXMLtoLabel songPath labelFull labelMono = do
    system "echo %date% %time% : musicxmlをラベルに変換しています..."
    system . unwords  $
        [ "bin" </> "musicXMLtoLabel.exe"
        , songPath
        , labelFull
        , labelMono ]

neutrino :: String -> String -> String -> IO ExitCode
neutrino labelFull baseName modelPath = do
    system "echo %date% %time% : NEUTRINOの変換を開始..."
    system . unwords $
        [ "bin" </> "NEUTRINO.exe"
        , labelFull
        , label </> "timing" </> baseName ++ ".lab" 
        , "output" </> baseName ++ ".f0"
        , "output" </> baseName ++ ".mgc"
        , "output" </> baseName ++ ".bap"
        , modelPath
        , "-m -t"  ]
world :: String -> Float -> Float -> String -> IO ExitCode
world baseName pitchShift formantShift model = do
    system "echo %date% %time% : WORLD実行中..."
    system . unwords $
        [ "bin" </> "WORLD.exe"
        , "output" </> baseName ++ ".f0"
        , "output" </> baseName ++ ".mgc"
        , "output" </> baseName ++ ".bap"
        , "-f " ++ show pitchShift
        , "-m " ++ show formantShift
        , "-o"
        , "wav" </> baseName ++ "-" ++ model ++ "_syn.wav"
        , "-t" ]

nsf :: String -> String -> String -> IO ExitCode
nsf labelFull baseName model = do
    system "echo %date% %time% : NSF実行中..."
    system . unwords $
        [ "bin" </> "NSF_IO.exe"
        , labelFull
        , label </> "timing" </> baseName ++ ".lab"
        , "output" </> baseName ++ ".f0"
        , "output" </> baseName ++ ".mgc"
        , "output" </> baseName ++ ".bap"
        , model
        , "wav" </> baseName ++ "-" ++ model ++ "_nsf.wav"
        , "-t"]


getThePath :: String -> String -> IO [String] -> IO (String, String)
getThePath message dir action = do
    xs <- action
    n <- prompt message $ 
            mapM_ putStrLn . zipWith f [1..] $ xs
    let name = xs !! (n-1)
    return (name, normalise $ dir </> name)
    where f a b = show a ++ ": " ++ b

modelYouUse :: IO [String]
modelYouUse = do
    ms <- models
    n <- prompt "使いたいモデルを選んで数字を入力してください" $ 
        mapM_ putStrLn . zipWith f [0..] $ ("全モデル一括変換": ms)
    case n of
        0 -> return ms
        _ -> return [ms !!(n - 1)]
    where
        f a b = show a ++ ": " ++ b
        models = listDirectory "model"

songToTranslate :: IO [(String, String)]
songToTranslate = do
    (name, path) <- getThePath
        "変換する曲を選んで数字を入力してください"
        musicxml
        songs
    isDirectory <- doesDirectoryExist path
    if isDirectory
    then do
        ps <- listDirectory path
        return $ map (\x -> (takeBaseName x, musicxml </> name </> normalise x)) ps
    else return [(name, path)]
    where
        songs = listDirectory musicxml
        musicxml = normalise "score/musicxml"

prompt :: (Read a) => String -> IO () -> IO a
prompt message action = do 
    putStrLn message
    action
    putStr "数字を半角で入力> "
    hFlush stdout
    readLn
