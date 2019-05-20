module Form where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Data.String (length)
import Data.Maybe (Maybe(..))

hasMaxLength maxLength value = 
  { isValid: length value <= maxLength
  , msg: "Length must be less than " <> show maxLength
  }

validations = 
  { firstName: hasMaxLength 8
  , email: hasMaxLength 8
  }

data ValidationState = Valid | Invalid String

applyValidations value = 
  { validationState: if validation.isValid then Valid else Invalid validation.msg
  , value: value
  }
  where validation = validations.firstName $ value




type FormState a = { formData :: a }
data FormEvent a = FormChange (FormState a) | Submit (FormState a)
data FormField events formValues a
  = Input { getter :: formValues -> String, setter :: formValues -> String -> formValues, render :: String -> Widget HTML events } 
  | Submission (Widget HTML a)

    

-- form :: forall a. FormState a -> Array (FormField a) -> Widget HTML (FormEvent a)
form defaultValues maybeFormState reactProps fields = do
  D.form ([Submit formState <$ P.onSubmit] <> reactProps) $ map (renderField formState) fields
  where formState = case maybeFormState of
          Just previousFormState -> previousFormState
          Nothing -> { formData: defaultValues }

-- renderField :: forall a. FormState a -> FormField a -> Widget HTML (FormEvent a)
renderField formState field = case field of
  Input { getter, setter, render } -> (FormChange <<< updateForm setter) <$> render (getter formState.formData)
  Submission render -> Submit formState <$ render
  where updateForm setter value = { formData: setter formState.formData value }


textField label value = D.label' 
  [ D.text label
  , D.input [ P.value value, P.unsafeTargetValue <$> P.onChange ]
  ]
