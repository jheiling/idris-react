module JS.React.DOM

import JS
import JS.DOM
import JS.React

%default total
%access export



render : (mount : JS.DOM.Element) -> (element : JS.React.Element) -> JS_IO ()
render mount = js "ReactDOM.render(%1, %0)" (Ptr -> Ptr -> JS_IO ()) (cast mount) . cast
