{-#LANGUAGE FlexibleInstances #-}
module Network.N2O.Jq where

import Language.HJavaScript.Syntax

data El = El deriving (Show)
data Doc = Doc deriving (Show)

instance IsClass El
instance IsClass Doc

doc :: Exp Doc
doc = JConst "document"

qi :: String -> Exp El
qi s = JCall (JConst "qi") (JString s)

jqSetProp obj prop val = ExpStmt $ JAssign (JDerefVar obj prop) val

jqHide id = jqSetProp (qi id) "style.display" $ JString "none"
jqShow id = jqSetProp (qi id) "style.display" $ JString "block"

setSid sid = jqSetProp doc "cookie" . JString $ "n2o_sid=" ++ sid
