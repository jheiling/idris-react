module JS.React.DOM

import JS
import JS.DOM
import JS.React

%default total
%access export



render : (element : JS.React.Element) -> (mount : JS.DOM.Element) -> JS_IO ()
render element = js "ReactDOM.render(%0, %1)" (Ptr -> Ptr -> JS_IO ()) (cast element) . cast
