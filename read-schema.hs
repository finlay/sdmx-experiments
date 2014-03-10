{-# LANGUAGE OverloadedStrings #-}
import System.IO (stdin)
import Data.Monoid ((<>))
import Data.Maybe (fromJust, catMaybes)

import Data.Text as T hiding (map)
import Data.Text.IO as T
import Data.Conduit (($$))
import Data.Conduit.Binary (sourceHandle)
import Text.XML as X
import Text.XML.Cursor as X

main = do
    doc <- sourceHandle stdin $$ X.sinkDoc def 
    T.putStrLn $ showdoc doc

showdoc :: X.Document -> T.Text
showdoc doc = showElement 0 $ documentRoot doc

rep i = Prelude.take i . Prelude.repeat

type Depth = Int
showElement :: Depth -> Element -> T.Text
showElement d el = 
    let te = showName $ elementName el
        selectElements (NodeElement el) = Just el
        selectElements _ = Nothing
        children = catMaybes $ map selectElements $ elementNodes el
        prefix :: Int -> Text -> Text
        prefix d txt = T.concat $ rep d "    " ++ [txt]
        ch = map (prefix d . showElement (d + 1)) children
    in T.intercalate "\n" $ te : ch

showName :: Name -> T.Text
showName (Name ln ns pr) = fromJust (pr <> return ":" <> return ln )
