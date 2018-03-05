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

public export
data Child = ChildElement Element
           | Text String

public export
data Component = Tag String
               | Simple (Object -> JS_IO Element)
               | Class Ptr



Cast Element Ptr where
    cast (MkElement ptr) = ptr

Cast (JS_IO Ptr) (JS_IO Element) where
    cast x = pure $ MkElement !x

Cast (JS_IO Element) (JS_IO Ptr) where
    cast x = pure $ cast !x



private
childrenArray : Foldable f => (children : f Child) -> JS_IO Ptr
childrenArray children =
    do array <- Js.Array.empty
       iter (append' array) children
       pure $ cast array
    where append' : Array -> Child -> JS_IO ()
          append' array (ChildElement (MkElement ptr)) = append ptr array
          append' array (Text t) = append t array

%inline
private
jsElement : (ty : Type) -> {auto fty : FTy FFI_JS [] ty} -> ty
jsElement ty = js "React.createElement(%0, %1, ...%2)" ty

tag : Foldable f => (name : String) -> (props : Object) -> (children : f Child) -> JS_IO Element
tag name props children = cast $ jsElement (String -> Ptr -> Ptr -> JS_IO Ptr) name (cast props) !(childrenArray children)

simple : Member a => (display : a -> JS_IO Element) -> (arg : a) -> JS_IO Element
simple display arg = assert_total inner
    where inner = cast $ js "React.createElement(%0, %1)"
                            (JsFn (Ptr -> JS_IO Ptr) -> Ptr -> JS_IO Ptr)
                            (MkJsFn $ get "v" . MkObject >=> cast . display)
                            (cast !(wrap "v" arg))

class' : Foldable f => (ptr : Ptr) -> (props : Object) -> (children : f Child) -> JS_IO Element
class' ptr props children = cast $ jsElement (Ptr -> Ptr -> Ptr -> JS_IO Ptr) ptr (cast props) !(childrenArray children)



div : Foldable f => (props : Object) -> (children : f Child) -> JS_IO Element
div = tag "div"

button : Foldable f => (props : Object) -> (children : f Child) -> JS_IO Element
button = tag "button"
