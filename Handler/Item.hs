{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Item where

import Import

-- data ExampleFormModel = ExampleFormModel
--     { textVal1        :: Text
--     , textVal2        :: Maybe Text
--     , numVal          :: Int
--     , boolVal         :: Bool
--     }

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
    case result of
        FormSuccess form -> defaultLayout $(widgetFile "item3")
        FormFailure _ -> do
            let str = "不正なデータが送信されました。" :: Text
            defaultLayout $(widgetFile "item")
        FormMissing -> defaultLayout [whamlet|データが送信されませんでした。 |]