module Js.React.DOM

import Js
import Js.Object
import Js.DOM.Element
import Js.React.Element

%default total



export
render : (mount : Js.DOM.Element.Element) -> (element : Js.React.Element.Element) -> JS_IO ()
render mount = js "ReactDOM.render(%1, %0)" (Ptr -> Ptr -> JS_IO ()) (ptr mount) . ptr
