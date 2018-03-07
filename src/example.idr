import Js
import Js.Object
import Js.DOM.Document
import Js.DOM.Element
import Js.React.DOM
import Js.React.Element

%default total



helloInitState : String -> String
helloInitState = id

helloRender : String -> JS_IO Js.React.Element.Element
helloRender who = div !(wrap "color" "red" >>= wrap "style")
                      [Text $ "Hello, " ++ who ++ "!"]

helloInfo : ComponentInfo String String
helloInfo = MkComponentInfo helloInitState helloRender

helloClass : JS_IO Js.React.Element.Element
helloClass = fromComponent' !(component helloInfo) "Class"

hello : String -> JS_IO Js.React.Element.Element
hello = simple (\who => div !(wrap "color" "blue" >>= wrap "style")
                            [Text $ "Hello, " ++ who ++ "!"])

main : JS_IO ()
main = render !(getElement "root")
              !(div !empty
                    [ Elem !(hello "Idris")
                    , Elem !(hello "React")
                    , Elem !helloClass
                    , Text "... it's working :)"
                    ])
