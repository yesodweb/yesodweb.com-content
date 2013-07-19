main = interact $ unlines . go . lines

go [] = []
go (l:ls)
    | Just l' <- stripLit l =
        let (ls1, ls2) = mapMaybeWhile stripLit ls
         in "[source, haskell]"
            : "----"
            : l' : ls1
            ++ "----" : go ls2
    | otherwise = l : go ls

stripLit ('>':' ':x) = Just x
stripLit _ = Nothing

mapMaybeWhile f =
    go id
  where
    go front [] = (front [], [])
    go front (x:xs) =
        case f x of
            Nothing -> (front [], x:xs)
            Just x' -> go (front . (x':)) xs
