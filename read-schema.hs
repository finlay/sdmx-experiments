{-# LANGUAGE OverloadedStrings #-}
import System.IO (stdin)
import Data.Text as T
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
    let cursor = fromDocument doc
    in  T.concat $ cursor $/ element "xs:element" &/ attribute "name"

