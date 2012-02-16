{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit
import qualified Data.Conduit.List as CL
import Network.HTTP.Conduit
import Text.HTML.TagStream
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as S8
import Data.Time
import System.Locale
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Filesystem as F
import qualified Filesystem.Path.CurrentOS as F
import qualified Data.Yaml as Y

dropWhile' :: Resource m => (a -> Bool) -> Conduit a m a -- FIXME add to conduit
dropWhile' f =
    conduitState True push close
  where
    push False x = return $ StateProducing False [x]
    push True x
        | f x = return $ StateProducing True []
        | otherwise = return $ StateProducing False [x]
    close _ = return []

takeWhile' :: Resource m => (a -> Bool) -> Conduit a m a -- FIXME add to conduit
takeWhile' f =
    conduitState True push close
  where
    push False x = return $ StateProducing False []
    push True x
        | f x = return $ StateProducing True [x]
        | otherwise = return $ StateProducing False []
    close _ = return []

main :: IO ()
main = withManager $ \m -> do
    req <- parseUrl "http://www.yesodweb.com/blog-archive"
    Response _ _ body <- http req m
    x <- body
        $$ tokenStream
        =$ dropWhile' (/= TagOpen "nav" [("class", "toc")] False)
        =$ takeWhile' (/= TagClose "nav")
        =$ sequenceSink () getPost
        =$ CL.mapM (grab m)
        =$ CL.consume
    liftIO $ Y.encodeFile "posts.yaml" x

data Post = Post
    { postTime :: UTCTime
    , postAuthor :: T.Text
    , postTitle :: T.Text
    , postFP :: F.FilePath
    }
    deriving Show

instance Y.ToJSON Post where
    toJSON (Post time a title fp) = Y.object
        [ "time" Y..= show time
        , "author" Y..= a
        , "title" Y..= title
        , "path" Y..= F.encodeString fp
        ]

grab m (daytime, href, titleBS) = runResourceT $ do
    let title = decodeUtf8 titleBS
    let slug = snd $ T.breakOnEnd "/" $ decodeUtf8 href
    let mutc = parseTime defaultTimeLocale "%B %e, %Y %l:%M %P" $ S8.unpack daytime
    utc <-
        case mutc of
            Nothing -> error $ "Invalid timestamp: " ++ show daytime
            Just utc -> return utc

    let (year, month, _) = toGregorian $ utctDay utc

    let fp = F.decodeString (show year) F.</>
             F.decodeString (show month) F.</>
             F.fromText slug F.<.> "html"

    liftIO $ F.createTree $ F.directory fp

    req <- parseUrl $ S8.unpack href
    Response _ _ body <- http req m
    x <- body
        $$ tokenStream
        =$ dropWhile' (not . isAvatar)
        =$ takeWhile' (not . isDisqus)
        =$ CL.consume
    author <-
        case x of
            TagOpen "img" as _:_ ->
                case lookup "src" as of
                    Just src ->
                        case src of
                            "http://www.gravatar.com/avatar/bad65d3d7319025d73e065d7a29ee22a?s=100&amp;d=identicon" -> return "greg"
                            "http://www.gravatar.com/avatar/71596bb1ca3ba3aa4400c3f407baec9f?s=100&amp;d=identicon" -> return "michael"
                            _ -> error $ "Unknown author: " ++ show src
                    Nothing -> error "Unknown author"
            _ -> error "Unknown author"
    let content = encode $ drop 1 $ dropWhile (not . isScriptClose) x

    liftIO $ S8.writeFile (F.encodeString fp) content

    return $ Post utc author (replace title) fp
  where
    isAvatar (TagOpen "img" as _) = lookup "alt" as == Just "Avatar"
    isAvatar _ = False
    isScriptClose (TagClose "script") = True
    isScriptClose _ = False
    isArticleClose (TagClose "article") = True
    isArticleClose _ = False
    isDisqus (TagOpen "div" [("id", "disqus")] _) = True
    isDisqus _ = False
    replace = T.replace "&#39;" "'"
            . T.replace "&quot;" "\""
            . T.replace "&amp;" "&"
            . T.replace "&lt;" "<"
            . T.replace "&gt;" ">"

getPost () = do
    mx <- CL.head
    case mx of
        Just (TagOpen "a" [("title", daytime), ("href", href)] False) -> do
            Just (Text title) <- CL.head
            return $ Emit () [(daytime, href, title)]
        Nothing -> return Stop
        _ -> getPost ()
