module Js.React.DOM

import Js
import Js.Object
import Js.DOM
import Js.React

%default total



export
render : (mount : Js.DOM.Element) -> (element : Js.React.Element) -> JS_IO ()
render mount = js "ReactDOM.render(%1, %0)" (Ptr -> Ptr -> JS_IO ()) (ptr mount) . ptr
