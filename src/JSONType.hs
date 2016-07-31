{-# LANGUAGE MultiParamTypeClasses #-}
module JSONType where
import Typeclasses 
import Test.QuickCheck
import Text.PrettyPrint.HughesPJ

data JSONType = JBool
              | JString
              | JNumber
              | JObject [JSONType]
              | JArray JSONType
              deriving (Ord, Eq)

toDocJSONType :: JSONType -> Doc
toDocJSONType JBool        = text "Bool"
toDocJSONType JString      = text "String"
toDocJSONType JNumber      = text "Number"
toDocJSONType (JArray t)   = text "[" <> toDocJSONType t <> text "]"
toDocJSONType (JObject ts) = text "{" $+$ nest 4 (vcat (map (toDocJSONType) ts)) $+$ text "}"

instance Show JSONType where
    show = show . toDocJSONType

data JSONValue = JBoolV Bool
               | JStringV String
               | JNumberV Float
               | JObjectV [JSONValue]
               | JArrayV [JSONValue]
               deriving (Ord, Eq)

toDocJSONValue :: JSONValue -> Doc
toDocJSONValue (JBoolV b)    = text $ show b
toDocJSONValue (JStringV s)  = text $ show s
toDocJSONValue (JNumberV f)  = text $ show f
toDocJSONValue (JArrayV a)   = text $ show a
toDocJSONValue (JObjectV ts) = text "{" $+$ nest 4 (vcat (map (toDocJSONValue) ts)) $+$ text "}"

instance Show JSONValue where
    show = show . toDocJSONValue

instance Implements JSONValue JSONType where
    implement JBool       = fmap JBoolV arbitrary
    implement JString     = fmap JStringV arbitrary
    implement JNumber     = fmap JNumberV arbitrary
    implement (JObject t) = do
                                obj <- sequence $ map implement t
                                return $ JObjectV obj
    implement (JArray t)  = sized $ \n -> fmap JArrayV $ sequence $ replicate n $ implement t

instance Checks JSONType JSONValue where
    check JBool (JBoolV _)     = True
    check JString (JStringV _) = True
    check JNumber (JNumberV _) = True
    check (JObject tps) (JObjectV vls) = and $ zipWith check tps vls
    check (JArray t) (JArrayV vls) = and $ map (check t) vls
    check _ _ = False
