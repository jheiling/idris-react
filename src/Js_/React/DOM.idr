module Js.React.DOM

import Js
import Js.DOM
import Js.React

%default total
%access export



render : (mount : Js.DOM.Element) -> (element : Js.React.Element) -> JS_IO ()
render mount = js "ReactDOM.render(%1, %0)" (Ptr -> Ptr -> JS_IO ()) (cast mount) . cast
