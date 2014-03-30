{-# LANGUAGE OverloadedStrings #-}
import System.IO (stdin)
import Data.Monoid ((<>))
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Map.Lazy as M

import Data.Text as T hiding (map, repeat, take)
import Data.Text.IO as T
import Data.Conduit (($$))
import Data.Conduit.Binary (sourceHandle)
import Text.XML as X
import Text.XML.Cursor as X

main = do
    doc <- sourceHandle stdin $$ X.sinkDoc def 
    T.putStrLn $ showdoc doc

showdoc :: X.Document -> T.Text
showdoc doc = 
    let tops =  anyElement $ fromDocument doc
    in  T.concat $ catMaybes $ map (showElement 0) tops


type Depth = Int
showElement :: Depth -> Cursor -> Maybe T.Text
showElement d cur = do
    let prefix = return $ T.concat $ take (d+1) $ repeat "    "
    let children = child >=> anyElement $ cur
    ch <- mapM ((prefix <>) . showElement (d+1)) children
    te <- showNode (node cur)
    return $ T.intercalate "\n" $ te : ch

showNode :: Node -> Maybe T.Text
showNode (NodeElement el) =
    let Name ln ns pr = elementName el
        nm = M.lookup "name" $ elementAttributes el
    in  pr <> return ":" <> return ln  <> return "   " <> nm
showNode (NodeContent c) = Just $ "\"" <> c <> "\""
showNode (NodeComment c) = Just $ "-- " <> c
showNode _ = Nothing
