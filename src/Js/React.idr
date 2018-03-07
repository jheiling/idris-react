module Js.React

import Control.Monad.Syntax
import Data.Foldable.Extras
import Js
import Js.Array
import Js.Object

%default total
%access export



public export
data Element = MkElement Ptr

Class Element where
    ptr (MkElement p) = p

public export
data Child = ChildElement Element
           | Text String



private
childrenArray : Foldable f => f Child -> JS_IO Ptr
childrenArray = createWith appendChild >=> pure . ptr
    where appendChild : Child -> Array -> JS_IO ()
          appendChild (ChildElement (MkElement p)) = append p
          appendChild (Text t) = append t

%inline
private
jsElement : (ty : Type) -> {auto fty : FTy FFI_JS [] ty} -> ty
jsElement ty = js "React.createElement(%0, %1, ...%2)" ty

tag : (Class p, Foldable f) => (name : String) -> (props : p) -> (children : f Child) -> JS_IO Element
tag name props = childrenArray >=> jsElement (String -> Ptr -> Ptr -> JS_IO Ptr) name (ptr props) >=> pure . MkElement

simple : Member a => (display : a -> JS_IO Element) -> (arg : a) -> JS_IO Element
simple display arg = assert_total inner
    where inner = MkElement <$> js "React.createElement(%0, %1)"
                                   (JsFn (Ptr -> JS_IO Ptr) -> Ptr -> JS_IO Ptr)
                                   (MkJsFn $ get "v" . MkObject >=> (display >=> pure . ptr))
                                   (ptr !(wrap "v" arg))

class' : (Class c, Class p, Foldable f) => (cl : c) -> (props : p) -> (children : f Child) -> JS_IO Element
class' cl props = childrenArray >=> jsElement (Ptr -> Ptr -> Ptr -> JS_IO Ptr) (ptr cl) (ptr props) >=> pure . MkElement



div : (Class p, Foldable f) => (props : p) -> (children : f Child) -> JS_IO Element
div = tag "div"

button : (Class p, Foldable f) => (props : p) -> (children : f Child) -> JS_IO Element
button = tag "button"
