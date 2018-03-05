module JS.React

import Control.Monad.Syntax
import JS
import JS.Array
import JS.Object
import Data.Foldable.Extras

%default total



public export
data Element = MkElement Ptr

public export
data Child = ChildElement Element
           | Text String

public export
data Component = Tag String
               | Simple (Object -> JS_IO Element)
               | Class Ptr



public export
Cast Element Ptr where
    cast (MkElement ptr) = ptr



%inline
jsElement : (ty : Type) -> {auto fty : FTy FFI_JS [] ty} -> ty
jsElement ty = js "React.createElement(%0, %1, ...%2)" ty

childrenArray : (Functor f, Foldable f) => (children : f Child) -> JS_IO Ptr
childrenArray children =
    do array <- JS.Array.empty
       iter (append array) children
       pure $ cast array
    where append : Array -> Child -> JS_IO ()
          append array (ChildElement c) = appendPtr (cast c) array
          append array (Text t) = appendString t array



export
tag : Functor f => Foldable f => (name : String) -> (props : Object) -> (children : f Child) -> JS_IO Element
tag name props children = pure $ MkElement !(jsElement (String -> Ptr -> Ptr -> JS_IO Ptr) name (cast props) !(childrenArray children))

export
simple : (toJs : a -> JS_IO Object) -> (fromJs : Object -> JS_IO a) -> (display : a -> JS_IO Element) -> (arg : a) -> JS_IO Element
simple toJs fromJs display arg = assert_total inner
    where inner = pure $ MkElement !(js "React.createElement(%0, %1)"
                                        (JsFn (Ptr -> JS_IO Ptr) -> Ptr -> JS_IO Ptr)
                                        (MkJsFn $ (fromJs >=> display) . MkObject >=> pure . cast)
                                        (cast !(toJs arg)))

export
class' : Functor f => Foldable f => (ptr : Ptr) -> (props : Object) -> (children : f Child) -> JS_IO Element
class' ptr props children = pure $ MkElement !(jsElement (Ptr -> Ptr -> Ptr -> JS_IO Ptr) ptr (cast props) !(childrenArray children))



export
div : Functor f => Foldable f => (props : Object) -> (children : f Child) -> JS_IO Element
div = tag "div"
