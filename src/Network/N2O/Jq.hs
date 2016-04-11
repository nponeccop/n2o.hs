{-#LANGUAGE FlexibleInstances #-}
module Network.N2O.Jq where

import Language.HJavaScript.Syntax

data El = El deriving (Show)

instance IsClass El

qi :: String -> Exp El
qi s = JCall (JConst "qi") (JString s)

jqSetProp obj prop val = ExpStmt $ JAssign (JDerefVar (qi "aaa") prop) val

jqHide id = jqSetProp (qi id) "style.display" $ JString "none"
jqShow id = jqSetProp (qi id) "style.display" $ JString "block"

