{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Exercise where

import Import
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail

data ExerciseFormModel = ExerciseFormModel
    { item :: ItemId
    , value :: Int
    }

exerciseForm :: [(Text, ItemId)] -> Html -> MForm Handler (FormResult ExerciseFormModel, Widget)
exerciseForm itemList = renderDivs $ ExerciseFormModel
    <$> areq (selectFieldList itemList) "種目" Nothing
    <*> areq intField "" Nothing

getExerciseR :: Handler Html
getExerciseR = do
    items <- runDB $ selectList [] []
    let itemList = fmap (\it -> (itemName $ entityVal it, entityKey it)) items
    (widget, enctype) <- generateFormPost $ exerciseForm itemList
    defaultLayout $(widgetFile "exercise")

postExerciseR :: Handler Html
postExerciseR = do
    items <- runDB $ selectList [] []
    let itemList = fmap (\it -> (itemName $ entityVal it, entityKey it)) items
    ((result, widget), enctype) <- runFormPost $ exerciseForm itemList
    userId <- requireAuthId
    case result of
        FormSuccess form -> do
          _ <- runDB $ insert $ exerciseFormToExercise form
          defaultLayout $(widgetFile "exercise3")
        FormFailure _ -> do
            let str = "不正なデータが送信されました。" :: Text
            defaultLayout $(widgetFile "exercise")
        FormMissing -> defaultLayout [whamlet|データが送信されませんでした。 |]

exerciseFormToExercise :: ExerciseFormModel -> Exercise
exerciseFormToExercise form = Exercise (item form) (value form)