module Handler.Template where

import Import
import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )

-- This is a handler function for the GET request method on the TemplateR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getTemplateR :: Handler Html
getTemplateR = do
    (formWidget, formEnctype) <- generateFormPost templateForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getTemplateR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Write a template!"
        $(widgetFile "templates")

postTemplateR :: Handler Html
postTemplateR = do
    ((result, formWidget), formEnctype) <- runFormPost templateForm
    let handlerName = "postTemplateR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

templateForm :: Form (FileInfo, Text)
templateForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField (withSmallInput "What's on the file?") Nothing

