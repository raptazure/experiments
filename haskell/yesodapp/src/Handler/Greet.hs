{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Greet where

import Import

{-
getGreetR :: Text -> Handler Html
getGreetR name = do
  defaultLayout $ do
    setTitle "Hello there"
    [whamlet|<p>Hello there #{name}</p>|]
-}

getGreetR :: Text -> Handler TypedContent
getGreetR name = selectRep $ do
  provideRep $ pure $ object 
    ["name" .= name]
  provideRep $ defaultLayout $ do
    setTitle "Hello there"
    [whamlet|<p>Hello there #{name}</p>|]