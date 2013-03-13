{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
import ClassyPrelude.Conduit
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Text.XML as X
import qualified Filesystem.Path.CurrentOS as F
import Control.Exception (handle, SomeException, throw)
import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Data.ByteString.Lazy as L
import Data.Monoid (mconcat)
import System.IO (hPutStrLn, stderr)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Data.Char (isUpper, isSpace)
import Data.Text.Lazy.Builder (fromText, Builder, toLazyText, fromLazyText)

data Block = BlockText Text
           | BlockLiterate Text
           | BlockHaskell Text
           | BlockCode Text
           | BlockPara [Inline]
           | BlockTitle Int [Inline]
           | BlockHtml Text [Block]
           | BlockImage Text

data Inline = InlineText Text
            | InlineCode Text
            | InlineLink Text [Inline]
            | InlineHtml Text [Inline]
            | InlineWrap Text [Inline]

renderBlock :: Block -> Builder
renderBlock (BlockText t) = "\n\n" ++ fromText t ++ "\n\n"
renderBlock (BlockLiterate t) = "\n\n```haskell active\n{-# START_FILE main.lhs #-}\n" ++ fromText t ++ "\n```\n\n"
renderBlock (BlockHaskell t) = "\n\n```haskell" ++ active ++ web ++ "\n" ++ fromText t' ++ "\n```\n\n"
  where
    t' = T.replace "warpDebug 3000" "warpEnv" t
    web = if t /= t' then " web" else ""
    active = if "\nmain " `isInfixOf` t then " active" else ""
renderBlock (BlockCode t) = "\n\n```\n" ++ fromText t ++ "\n```\n\n"
renderBlock (BlockPara inlines) = "\n\n" ++ strip' (renderInlines inlines) ++ "\n\n"
  where
    strip' = fromLazyText . TL.strip . toLazyText
renderBlock (BlockTitle 1 _) = ""
renderBlock (BlockTitle depth inlines) = "\n\n" ++ fromText (replicate depth "#") ++ " " ++ renderInlines inlines ++ "\n"
renderBlock (BlockImage href) = "\n\n![](" ++ fromText href ++ ")\n\n"
renderBlock (BlockHtml tag blocks) = concat
    [ "\n\n<"
    , fromText tag
    , ">\n\n"
    , concat $ map renderBlock blocks
    , "\n\n</"
    , fromText tag
    , ">\n\n"
    ]

renderInlines = concat . map renderInline

renderInline (InlineText "") = empty
renderInline (InlineText t) = fromText $ begin ++ unwords (words t) ++ end
  where
    begin = if isSpace (T.head t) then " " else ""
    end = if isSpace (T.last t) then " " else ""
renderInline (InlineCode t) = "<code>" ++ fromText (concatMap escape t) ++ "</code>"
  where
    escape '<' = "&lt;"
    escape '&' = "&amp;"
    escape c = singleton c
renderInline (InlineLink href text) = "[" ++ renderInlines text ++ "](" ++ fromText href ++ ")"
renderInline (InlineHtml tag inlines) = concat
    [ "<"
    , fromText tag
    , ">"
    , renderInlines inlines
    , "</"
    , fromText tag
    , ">"
    ]
renderInline (InlineWrap t inlines) = concat
    [ fromText t
    , renderInlines inlines
    , fromText t
    ]

main = do
    [fp] <- getArgs
    Document _ (Element _ _ ns) _ <- X.readFile def $ repack fp
    let blocks = concatMap (goNode 1) ns
    TLIO.putStrLn $ toLazyText $ concat $ map renderBlock blocks
  where
    goNode :: Int -> Node -> [Block]
    goNode d (NodeElement e) = goBlock d e
    goNode _ (NodeContent t) = [BlockText t]
    goNode _ _ = []

    goInline :: Node -> [Inline]
    goInline (NodeElement e) = goInlineE e
    goInline (NodeContent t) = [InlineText t]
    goInline _ = []

    goInlineE (Element "codeph" as [NodeContent t]) = [InlineCode t]
    goInlineE (Element "cmdname" as [NodeContent t]) = [InlineCode t]
    goInlineE (Element "xref" as cs) = [goXref as cs]
    goInlineE (Element "term" as cs) = [InlineWrap "__" $ concatMap goInline cs]
    goInlineE (Element "i" as cs) = [InlineWrap "*" $ concatMap goInline cs]
    goInlineE (Element "cite" as cs) = [InlineWrap "_" $ concatMap goInline cs]
    goInlineE (Element "b" as cs) = [InlineWrap "**" $ concatMap goInline cs]
    goInlineE (Element "apiname" as [NodeContent t]) = [goApiname t]
    goInlineE (Element n as cs)
        | n `elem` ["q"] = [InlineHtml (nameLocalName n) $ concatMap goInline cs]
    goInlineE e = error $ "\n\ngoInlineE: " ++ show e

    goBlock :: Int -> Element -> [Block]
    goBlock _ (Element "codeblock" as [NodeContent t])
        | Just "lhaskell" <- lookup "outputclass" as = [BlockLiterate t]
        | Just "haskell" <- lookup "outputclass" as = [BlockHaskell $ goStartStop t]
        | otherwise = [BlockCode t]
    goBlock d (Element n as cs) | Just wrapper <- lookup n (containers d) = [wrapper $ concatMap goInline cs]
    goBlock d (Element "concept" _ cs) = concatMap (goNode $ d + 1) cs
    goBlock d (Element "ul" _ cs) = concatMap (goList "* ") cs
    goBlock d (Element "ol" _ cs) = concatMap (goList "1. ") cs
    goBlock d (Element "note" _ cs) = [BlockPara $ concatMap goInline (concatMap stripParas cs)]
    goBlock d (Element "dl" _ cs) = [BlockHtml "dl" $ concatMap (goNode d) cs]
    goBlock d (Element "dt" _ cs) = [BlockHtml "dt" [BlockPara $ concatMap goInline cs]]
    goBlock d (Element "dd" _ cs) = [BlockHtml "dd" [BlockPara $ concatMap goInline cs]]
    goBlock d (Element "lq" _ cs) = [BlockHtml "blockquote" [BlockPara $ concatMap goInline cs]]
    goBlock d (Element "fig" _ cs) = concatMap (goNode d) $ filter (not . isTitle) cs
    goBlock d (Element "image" as cs) = [goImage as cs]
    goBlock d (Element n as cs) | n `elem` stripped = concatMap (goNode d) cs
    goBlock _ e@(Element n as cs) =
        case n of
            _ -> error $ "\n\nUnknown: " ++ show e
        {-
        | nameLocalName n `Set.member` unchanged = [NodeElement $ Element n as cs']
        | Just n' <- Map.lookup n simples = [NodeElement $ Element n' as cs']
        | Just (n', clazz) <- Map.lookup n classes = [NodeElement $ Element n' (Map.insert "class" clazz as) cs']
        | n `Set.member` deleted = []
        | n `Set.member` stripped = cs'
        | n == "codeblock" = [NodeElement $ Element "pre" as [NodeElement $ Element "code" Map.empty cs']]
        | n == "xref" = goXref as cs'
        | n == "image" = goImage as cs'
        -}

    goList start (NodeElement (Element "li" _ cs)) = [BlockPara $ InlineText start : concatMap goInline (concatMap stripParas cs)]
      where
    goList _ _ = []

    stripParas (NodeElement (Element "p" _ cs)) = cs
    stripParas x = [x]

    isTitle (NodeElement (Element "title" _ _)) = True
    isTitle _ = False

    containers depth = Map.fromList
        [ ("p", BlockPara)
        , ("title", BlockTitle depth)
        ]

    stripped = Set.fromList
        [ "conbody"
        , "dlentry"
        ]

    {-
    goElem _ (Element "apiname" _ [NodeContent t]) = goApiname t

    unchanged = Set.fromList $ T.words "p ul li i ol b dl dt dd cite q"

    simples = Map.fromList
        [ ("concept", "section")
        , ("codeph", "code")
        , ("title", "h1")
        , ("lq", "blockquote")
        , ("simpletable", "table")
        , ("sthead", "thead")
        , ("stentry", "td")
        , ("strow", "tr")
        , ("fig", "figure")
        ]

    classes = Map.fromList
        [ ("note", ("aside", "note"))
        , ("term", ("span", "term"))
        , ("cmdname", ("span", "cmdname"))
        , ("filepath", ("span", "filepath"))
        , ("userinput", ("span", "userinput"))
        , ("varname", ("span", "varname"))
        , ("msgblock", ("pre", "msgblock"))
        ]

    deleted = Set.fromList
        [
        ]
-}

    goApiname t =
        InlineLink href [InlineText text]
      where
        (href, text) =
            case T.split (== ':') t of
                [x] -> (T.append "http://hackage.haskell.org/package/" x, x)
                [x, y] -> (T.concat
                    [ "http://hackage.haskell.org/packages/archive/"
                    , x
                    , "/latest/doc/html/"
                    , T.replace "." "-" y
                    , ".html"
                    ], y)
                [x, y, z] -> (T.concat
                    [ "http://hackage.haskell.org/packages/archive/"
                    , x
                    , "/latest/doc/html/"
                    , T.replace "." "-" y
                    , ".html#"
                    , if isUpper (T.head z) then "t:" else "v:"
                    , z
                    ], z)
                xs -> error $ show xs

    goXref as cs =
        InlineLink href' (concatMap goInline cs)
      where
        href' =
            case Map.lookup "href" as of
                Just href
                    | "/" `T.isPrefixOf` href || "://" `T.isInfixOf` href -> href
                    | otherwise ->
                        let name = T.takeWhile (/= '.') href
                            suffix = T.dropWhile (/= '#') href
                         in T.append name suffix
                Nothing -> error "xref without href"

    goStartStop =
        T.unlines . map go . T.lines
      where
        go "-- START" = "-- show"
        go "-- STOP" = "-- /show"
        go t = t

    goImage as cs =
        BlockImage href'
      where
        href' =
            case Map.lookup "href" as of
                Just href ->
                    let name = either id id $ F.toText $ F.basename $ F.fromText href
                     in T.append "http://www.yesodweb.com/book/image/" name
                Nothing -> error "image without href"

data LHask t = LHCode t | LHText t

instance Functor LHask where
    fmap f (LHCode x) = LHCode (f x)
    fmap f (LHText x) = LHText (f x)

lhLine :: Text -> LHask [Text]
lhLine t =
    case T.stripPrefix "> " t of
        Nothing -> LHText [t]
        Just s -> LHCode [s]

toBlocks :: [LHask [Text]] -> [LHask [Text]]
toBlocks [] = []
toBlocks (LHCode x:LHCode y:rest) = toBlocks $ LHCode (x ++ y) : rest
toBlocks (LHText x:LHText y:rest) = toBlocks $ LHText (x ++ y) : rest
toBlocks (x:rest) = x : toBlocks rest
