{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
import ClassyPrelude.Conduit hiding (hash)
import Text.XML as X hiding (writeFile)
import Text.XML.Cursor
import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString.Base16 as B16
import Filesystem (createTree, removeFile, isFile)

root :: FilePath
root = "extracted"

main :: IO ()
main = runResourceT $ do
    generated <-
           sourceDirectoryDeep False "generated-xml"
        $$ awaitForever handleXML
        =$ foldMapC (asSet . singletonSet)
    sourceDirectoryDeep False root
        $= filterC (\fp -> hasExtension fp "hs")
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
            | isContinuous fp = [unlines snippets0]
            | otherwise = snippets0
    let fileMap = asMap $ map unlines $ unionsWith (++)
          $ map (maybe
                    mempty
                    (\(x, y) -> singletonMap x (opoint y))
                . getFileName)
            snippets
    forM_ (mapToList fileMap) $ \(fp, code') -> do
        liftIO $ whenM (fileChanged fp code') $ do
            createTree $ directory fp
            writeFile fp $ filter (/= '\r') code'
        yield fp

-- | One of the chapters where all code snippets must be concatenated together.
isContinuous :: FilePath -> Bool
isContinuous fp =
    basename fp `member` names
  where
    names = asSet $ setFromList
        [ "blog-example-advanced"
        ]

fileChanged fp new
    | isHashName fp = not <$> isFile fp
    | otherwise = do
        eold <- tryIO $ ClassyPrelude.Conduit.readFile fp
        return $ case eold of
            Left _ -> True
            Right old -> old /= new

isHashName :: FilePath -> Bool
isHashName t = "Extracted_" `isInfixOf` fpToText t

getFileName :: Text -> Maybe (FilePath, Text)
getFileName orig
    | Just fp <- listToMaybe (mapMaybe (stripPrefix "-- @") lorig) =
        Just (root </> fpFromText fp, unlines $ filter (not . isFileName) lorig)
    | all (not . isMain) lorig = Nothing
    | any isImport lorig = Just (hashfp, unlines $ go lorig)
    | otherwise = Just (hashfp, unlines $ header : lorig)
  where
    lorig = lines orig

    isFileName = ("-- @" `isPrefixOf`)

    isMain = ("main = " `isPrefixOf`)

    name = "Extracted_" ++ (decodeUtf8 $ B16.encode $ hash $ encodeUtf8 orig)

    hashfp = root </> fpFromText name <.> "hs"

    go [] = []
    go (x:xs)
        | "import " `isPrefixOf` x = header : x : xs
        | otherwise = x : go xs

    isImport = ("import " `isPrefixOf`)

    header = "module " ++ name ++ " where"
