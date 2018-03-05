import JS
import JS.Object
import JS.DOM
import JS.DOM.Document
import JS.React
import JS.React.DOM

%default total



hello : String -> JS_IO JS.React.Element
hello = simple (cast . js "{who: %0}" (String -> JS_IO Ptr))
               (js "%0.who" (Ptr -> JS_IO String) . cast)
               (\who => div !(cast $ js "{style: {color: 'blue'}}" (JS_IO Ptr))
                            [Text $ "Hello, " ++ who ++ "!"])

main : JS_IO ()
main = render !(div !empty
                    [ChildElement !(hello "Idris"),
                     ChildElement !(hello "React"),
                     Text "... it's working :)"])
              !(getElement "root")
