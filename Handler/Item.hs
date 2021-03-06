{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Item where

import Import
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail

data ItemFormModel = ItemFormModel
    { name :: Text
    , unitName :: Text
    , load :: Int
    , loadUnitName :: Text
    }

itemForm :: Html -> MForm Handler (FormResult ItemFormModel, Widget)
itemForm = renderDivs $ ItemFormModel
    <$> areq textField "種目名" Nothing
    <*> areq textField "単位名" Nothing
    <*> areq intField  "負荷" Nothing
    <*> areq textField  "負荷単位名" Nothing

getItemR :: Handler Html
getItemR = do
    let str = "実体参照に変換<されるよ>" :: Text
    (widget, enctype) <- generateFormPost itemForm
    defaultLayout $(widgetFile "item")

postItemR :: Handler Html
postItemR = do
    ((result, widget), enctype) <- runFormPost itemForm
    userId <- requireAuthId
    case result of
        FormSuccess form -> do
          _ <- runDB $ insert $ itemFormToItem form userId
          defaultLayout $(widgetFile "item3")
        FormFailure _ -> do
            let str = "不正なデータが送信されました。" :: Text
            defaultLayout $(widgetFile "item")
        FormMissing -> defaultLayout [whamlet|データが送信されませんでした。 |]

itemFormToItem :: ItemFormModel -> UserId -> Item
itemFormToItem form userId = Item userId (name form) (unitName form) (load form) (loadUnitName form) True

getItemsR :: Handler Html
getItemsR = do
    userId <- requireAuthId
    items <- runDB $ selectList [ItemUser ==. userId] [Desc ItemId]
    defaultLayout $ do
        setTitle "Items"
        $(widgetFile "items")