module JS.React.DOM

import JS
import JS.DOM
import JS.React

%default total
%access export



utRender : (element : Ptr)
        -> (mount : Ptr)
        -> JS_IO ()
utRender = js "ReactDOM.render(%0, %1)" (Ptr -> Ptr -> JS_IO ())



render : (element : JS.React.Element)
      -> (mount : JS.DOM.Element)
      -> JS_IO ()
render element = utRender (cast element) . cast
