module Main where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Form as Form
import CSS as C

type FormValues = 
  { firstName :: String
  , email :: String
  }


formContent = 
    [ Form.Input 
      { getter: (\fv -> fv.firstName)
      , setter: (\fv firstName -> fv {firstName=firstName})
      , render: Form.textField "First Name"
      }
    , Form.Input 
        { getter: (\fv -> fv.email) 
        , setter: (\fv email -> fv {email=email})
        , render: Form.textField "Email"
        }
    , Form.Submission $ D.button [P.onClick] [D.text "Submit"]
    ]

-- formWidget :: forall a. Widget HTML a

style = C.render $ do
  C.marginLeft (C.px 50.0)


formWidget = do
  D.div [P.style style] [go Nothing]
    where
      go formState = do
        event <- Form.form ({ firstName: "", email: "" }) formState [] formContent
        case event of
          Form.FormChange oldFormState -> go $ Just oldFormState 
          Form.Submit oldFormState -> do
            liftEffect (logShow oldFormState.formData)
            D.div [ P.style { animationDuration: "3s", animationName: "slidein" }] [D.text "Thank you!"]

main :: Effect Unit
main = runWidgetInDom "root" $ formWidget 
