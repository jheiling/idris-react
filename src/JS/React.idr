module JS.React

import Control.Monad.Syntax

import JS
import JS.Array
import JS.Object
import Data.Foldable.Extras

%default total
%access public export



data UtChild = UtChildElement Ptr
             | UtText String

data UtComponent = UtTag String
                 | UtSimple (JsFn (Ptr -> JS_IO Ptr))
                 | UtClass Ptr

data Element = MkElement Ptr

data Child = ChildElement Element
           | Text String

data Component = Tag String
               | Simple (Object -> JS_IO Element)
               | Class Ptr

Cast Element Ptr where
    cast (MkElement ptr) = ptr

Cast Child UtChild where
    cast (ChildElement e) = UtChildElement $ cast e
    cast (Text t) = UtText t

Cast Component UtComponent where
    cast (Tag t) = UtTag t
    cast (Simple f) = UtSimple $ MkJsFn $ (f . MkObject) >=> (pure . cast)
    cast (Class c) = UtClass c



utClass : Ptr -> JS_IO Ptr
utClass = js "createReactClass(%0)" (Ptr -> JS_IO Ptr)



utAppendChild : UtChild -> Ptr -> JS_IO ()
utAppendChild (UtChildElement e) = utAppendPtr e
utAppendChild (UtText t) = utAppendString t

utChildren : Foldable f => f UtChild -> JS_IO Ptr
utChildren xs = do arr <- JS.Array.utEmpty
                   iter (flip utAppendChild arr) xs
                   pure arr

jsElement : (ty : Type) -> {auto fty : FTy FFI_JS [] ty} -> ty
jsElement ty = js "React.createElement(%0)" ty

jsElementP : (ty : Type) -> {auto fty : FTy FFI_JS [] ty} -> ty
jsElementP ty = js "React.createElement(%0, %1)" ty

jsElementC : (ty : Type) -> {auto fty : FTy FFI_JS [] ty} -> ty
jsElementC ty = js "React.createElement(%0, null, ...%1)" ty

jsElementPC : (ty : Type) -> {auto fty : FTy FFI_JS [] ty} -> ty
jsElementPC ty = js "React.createElement(%0, %1, ...%2)" ty

utElement : UtComponent -> JS_IO Ptr
utElement (UtTag t) = jsElement (String -> JS_IO Ptr) t
utElement (UtSimple f) = assert_total $ jsElement (JsFn (Ptr -> JS_IO Ptr) -> JS_IO Ptr) f
utElement (UtClass c) = jsElement (Ptr -> JS_IO Ptr) c

utElementP : UtComponent -> Ptr -> JS_IO Ptr
utElementP (UtTag t) props = jsElementP (String -> Ptr -> JS_IO Ptr) t props
utElementP (UtSimple f) props = assert_total $ jsElementP (JsFn (Ptr -> JS_IO Ptr) -> Ptr -> JS_IO Ptr) f props
utElementP (UtClass c) props = jsElementP (Ptr -> Ptr -> JS_IO Ptr) c props

utElementC : Foldable f => UtComponent -> f UtChild -> JS_IO Ptr
utElementC (UtTag t) children = jsElementC (String -> Ptr -> JS_IO Ptr) t !(utChildren children)
utElementC (UtSimple f) children = assert_total $ jsElementC (JsFn (Ptr -> JS_IO Ptr) -> Ptr -> JS_IO Ptr) f !(utChildren children)
utElementC (UtClass c) children = jsElementC (Ptr -> Ptr -> JS_IO Ptr) c !(utChildren children)

utElementPC : Foldable f => UtComponent -> Ptr -> f UtChild -> JS_IO Ptr
utElementPC (UtTag t) props children = jsElementPC (String -> Ptr -> Ptr -> JS_IO Ptr) t props !(utChildren children)
utElementPC (UtSimple f) props children = assert_total $ jsElementPC (JsFn (Ptr -> JS_IO Ptr) -> Ptr -> Ptr -> JS_IO Ptr) f props !(utChildren children)
utElementPC (UtClass c) props children = jsElementPC (Ptr -> Ptr -> Ptr -> JS_IO Ptr) c props !(utChildren children)

utSimple : (a -> JS_IO Ptr) -> (Ptr -> JS_IO a) -> (a -> JS_IO Ptr) -> a -> JS_IO Ptr
utSimple toJs fromJs func = toJs >=> utElementP (UtSimple $ MkJsFn $ fromJs >=> func)



fromUtElement : JS_IO Ptr -> JS_IO Element
fromUtElement obj = pure $ MkElement !obj

element : Component -> JS_IO Element
element = (utElement . cast) >=> (pure . MkElement)

elementP : Component -> Object -> JS_IO Element
elementP component = (utElementP (cast component) . cast) >=> (pure . MkElement)

elementC : Functor f => Foldable f => Component -> f Child -> JS_IO Element
elementC component = (utElementC (cast component) . map cast) >=> (pure . MkElement)

elementPC : Functor f => Foldable f => Component -> Object -> f Child -> JS_IO Element
elementPC component props = (utElementPC (cast component) (cast props) . map cast) >=> (pure . MkElement)

simple : (a -> JS_IO Object) -> (Object -> JS_IO a) -> (a -> JS_IO Element) -> a -> JS_IO Element
simple toJs fromJs func = toJs >=> elementP (Simple $ fromJs >=> func)



divComponent : Component
divComponent = Tag "div"

div : JS_IO Element
div = element divComponent

divP : Object -> JS_IO Element
divP = elementP divComponent

divC : Functor f => Foldable f => f Child -> JS_IO Element
divC = elementC divComponent

divPC : Functor f => Foldable f => Object -> f Child -> JS_IO Element
divPC = elementPC divComponent
