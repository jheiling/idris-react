module Js.React.Element

import Control.Monad.Syntax
import Data.Foldable.Extras
import Js
import Js.Array
import Js.Object

%default total
%access export



public export
data Component = MkComponent Ptr

Class Component where
    ptr (MkComponent p) = p

public export
data Element = MkElement Ptr

Class Element where
    ptr (MkElement p) = p

public export
data Child = Elem Element
           | Text String

public export
record ComponentInfo p s where
   constructor MkComponentInfo
   initState : (Member p, Member s) => p -> s
   render : Member s => s -> JS_IO Element



%inline
componentInfo : (Member p, Member s) => (initState : p -> s) -> (render : s -> JS_IO Element) -> ComponentInfo p s
componentInfo initState render = MkComponentInfo initState render

component : (Member p, Member s) => (info : ComponentInfo p s) -> JS_IO Component
component info =
    do object <- Js.Object.empty
       setFuncPtr "getInitialState" (props >>= get "v" . MkObject >>= wrap "v" . initState info >>= pure . ptr) object
       setFuncPtr "render" (state >>= get "v" . MkObject >>= render info >>= pure . ptr) object
       MkComponent <$> js "createReactClass(%0)" (Ptr -> JS_IO Ptr) (ptr object)
    where %inline
          props : JS_IO Ptr
          props = js "this.props" (JS_IO Ptr)
          %inline
          state : JS_IO Ptr
          state = js "this.state" (JS_IO Ptr)
          setFuncPtr : String -> JS_IO Ptr -> Object -> JS_IO ()
          setFuncPtr member func object = assert_total $ jsSet (String -> JsFn (() -> JS_IO Ptr) -> Ptr -> JS_IO ()) member (MkJsFn $ \() => func) (ptr object)



private
childrenArray : Foldable f => f Child -> JS_IO Ptr
childrenArray = createWith appendChild >=> pure . ptr
    where appendChild : Child -> Array -> JS_IO ()
          appendChild (Elem (MkElement p)) = append p
          appendChild (Text t) = append t

%inline
private
jsElement : (ty : Type) -> {auto fty : FTy FFI_JS [] ty} -> ty
jsElement ty = js "React.createElement(%0, %1, ...%2)" ty

%inline
tag : (Class p, Foldable f) => (name : String) -> (props : p) -> (children : f Child) -> JS_IO Element
tag name props = childrenArray >=> jsElement (String -> Ptr -> Ptr -> JS_IO Ptr) name (ptr props) >=> pure . MkElement

%inline
simple : Member a => (display : a -> JS_IO Element) -> (arg : a) -> JS_IO Element
simple display arg = assert_total inner
    where inner = MkElement <$> js "React.createElement(%0, %1)"
                                   (JsFn (Ptr -> JS_IO Ptr) -> Ptr -> JS_IO Ptr)
                                   (MkJsFn $ get "v" . MkObject >=> (display >=> pure . ptr))
                                   (ptr !(wrap "v" arg))

%inline
fromComponent : (Class p, Foldable f) => (component : Component) -> (props : p) -> (children : f Child) -> JS_IO Element
fromComponent component props = childrenArray >=> jsElement (Ptr -> Ptr -> Ptr -> JS_IO Ptr) (ptr component) (ptr props) >=> pure . MkElement

%inline
fromComponent' : Member p => (component : Component) -> (arg : p) -> JS_IO Element
fromComponent' component arg = fromComponent component !(wrap "v" arg) []



%inline
div : (Class p, Foldable f) => (props : p) -> (children : f Child) -> JS_IO Element
div = tag "div"

%inline
button : (Class p, Foldable f) => (props : p) -> (children : f Child) -> JS_IO Element
button = tag "button"
