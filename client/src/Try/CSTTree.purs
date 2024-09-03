module Try.CSTBuilder where

import Control.Alt
import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (length, null)
import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Debug (spy, traceM)
import Effect.Aff (Aff)
import Foreign (F, Foreign, ForeignError(..), fail, readArray)
import Foreign.Generic (defaultOptions, genericDecode)
import Foreign.Generic.Class (class Decode, decode, encode)
import Foreign.Index (readProp)
import Foreign.Object (Object)
import Foreign.Object (toArrayWithKey)
import Foreign.Object as Object
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Unsafe.Coerce (unsafeCoerce)

foreign import toString :: Foreign -> String

data Tree = NodeArr (Maybe String) (Array Tree)
          | NodeObject (Maybe String) (Object Tree)
          | Leaf Foreign

readMTag :: Foreign -> Maybe String
readMTag fgn = hush $ runExcept do  
      t <- readProp "tag" fgn
      decode t

decodeArr :: Foreign -> F Tree
decodeArr fgn = do 
    let mTag = readMTag fgn
    case mTag of 
      Just tag -> do 
                  arr <- readProp "contents" fgn >>= readArray
                  NodeArr mTag <$> (traverse decode arr)
      Nothing -> do 
          arr <- readArray fgn 
          NodeArr Nothing <$> (traverse decode arr)


decodeObject :: Foreign -> F Tree
decodeObject fgn = do 
    let mTag = readMTag fgn
    case mTag of 
      Just tag -> do 
                  obj <- readProp "contents" fgn
                  NodeObject mTag <$> (decode obj)
      Nothing -> NodeObject Nothing <$> (decode fgn)

instance Decode Tree where 
      decode fgn = decodeArr fgn <|> (decodeObject fgn) <|> (pure $ Leaf fgn)

component :: forall q i o. H.Component q Tree o Aff
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
  }
  where 
    initialState i = i
    render :: Tree -> H.ComponentHTML Unit () Aff
    render  tree = renderTree tree




createNested :: forall w i .Maybe String -> Array (HH.HTML w i) -> HH.HTML w i
createNested mTag children = case mTag of 
                    Just tag -> HH.details_ [ HH.summary_ [ HH.text tag ] , content]
                    Nothing -> content
              where 
                    content
                      | null children = HH.text "[]"
                      | otherwise = HH.div
                                [ HP.class_ $ HH.ClassName "tree-content" ]
                                children


renderArr :: forall w i. Maybe String -> Array Tree -> HH.HTML w i
renderArr tag children = createNested tag (renderTree <$> children)

renderObj :: forall w i. Maybe String -> Object Tree -> HH.HTML w i
renderObj tag children = createNested tag (toArrayWithKey renderKeyVal children)

renderKeyVal :: forall w i. String -> Tree -> HH.HTML w i
renderKeyVal key valTree = 
    HH.div_
    [ HH.i_ [ HH.text (key <> ": ") ]
    , renderTree valTree
    ]

renderTree :: forall w i. Tree -> HH.HTML w i
renderTree (NodeArr tag children) = renderArr tag children
renderTree (NodeObject tag children) = renderObj tag children
renderTree (Leaf fgn) = HH.text (toString fgn)
    








