{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
import ClassyPrelude.Conduit hiding (hash)
import Text.XML as X hiding (writeFile)
import Text.XML.Cursor
import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString.Base16 as B16
import System.Directory (createDirectoryIfMissing, removeFile, doesFileExist)
import System.FilePath

root :: FilePath
root = "extracted"

main :: IO ()
main = runResourceT $ do
    generated <-
           sourceDirectoryDeep False "generated-xml"
        $$ awaitForever handleXML
        =$ foldMapC (asSet . singletonSet)
    sourceDirectoryDeep False root
        $= filterC (\fp -> takeExtension fp == ".hs")
        $$ mapM_C (\fp -> do
            unless (fp `member` generated) $ liftIO $ removeFile fp)

handleXML :: MonadIO m => FilePath -> Producer m FilePath
handleXML fp = do
    doc <- liftIO $ X.readFile def
        { psDecodeEntities = decodeHtmlEntities
        } fp
    let cursor = fromDocument doc
        snippets0 = map (filter (/= '\r')) $ cursor $// element "programlisting" &/ content
        snippets
            | isContinuous fp
                = unlines (filter (not . hasPath) snippets0)
                : filter hasPath snippets0
            | otherwise = snippets0
    let fileMap :: Map FilePath ByteString
        fileMap = asMap $ map (encodeUtf8 . filter (/= '\r') . unlines) $ unionsWith (++)
          $ map (maybe
                    mempty
                    (\(x, y) -> singletonMap x [y])
                . getFileName)
            snippets
    forM_ (mapToList fileMap) $ \(fp, code') -> do
        liftIO $ whenM (fileChanged fp code') $ do
            createDirectoryIfMissing True $ takeDirectory fp
            writeFile fp code'
        yield fp

-- | One of the chapters where all code snippets must be concatenated together.
isContinuous :: FilePath -> Bool
isContinuous fp =
    takeBaseName fp `member` names
  where
    names = asSet $ setFromList
        [ "blog-example-advanced"
        ]

fileChanged :: FilePath -> ByteString -> IO Bool
fileChanged fp new
    | isHashName fp = not <$> doesFileExist fp
    | otherwise = do
        eold <- tryIO $ ClassyPrelude.Conduit.readFile fp
        return $ case eold of
            Left _ -> True
            Right old -> old /= new

isHashName :: FilePath -> Bool
isHashName t = "Extracted_" `isInfixOf` t

hasPath = ("-- @" `isPrefixOf`)

getFileName :: Text -> Maybe (FilePath, Text)
getFileName orig
    | Just fp <- listToMaybe (mapMaybe (stripPrefix "-- @") lorig) =
        Just (root </> unpack fp, unlines $ filter (not . isFileName) lorig)
    | all (not . isMain) lorig = Nothing
    | any isImport lorig = Just (hashfp, unlines $ go lorig)
    | otherwise = Just (hashfp, unlines $ header : lorig)
  where
    lorig = lines orig

    isFileName = ("-- @" `isPrefixOf`)

    isMain = ("main = " `isPrefixOf`)

    name = "Extracted_" ++ (decodeUtf8 $ B16.encode $ hash $ encodeUtf8 orig)

    hashfp = root </> unpack name <.> "hs"

    go [] = []
    go (x:xs)
        | "import " `isPrefixOf` x = header : x : xs
        | otherwise = x : go xs

    isImport = ("import " `isPrefixOf`)

    header = "module " ++ name ++ " where"
