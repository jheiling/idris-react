import Js
import Js.Object
import Js.DOM.Document
import Js.DOM.Element
import Js.React.DOM
import Js.React.Element

%default total



hello : String -> JS_IO Js.React.Element.Element
hello = simple (\who => div !(wrap "color" "blue" >>= wrap "style")
                            [Text $ "Hello, " ++ who ++ "!"])

main : JS_IO ()
main = render !(getElement "root")
              !(div !empty
                    [ ChildElement !(hello "Idris")
                    , ChildElement !(hello "React")
                    , Text "... it's working :)"
                    ])
