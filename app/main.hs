{-# Language FlexibleContexts #-}
{-# Language GADTs #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language QuasiQuotes #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language ViewPatterns #-}
import           Control.Applicative ((<$>))
import           Control.Monad.Logger (runStdoutLoggingT)
import           Data.Char (toLower, toUpper)
import           Data.List (nub)
import           Data.Text (Text, unpack)
import qualified Data.Text as T (head, intersperse, map, null, tail)
import           Database.Persist.Sqlite (ConnectionPool, SqlBackend,
                                          createSqlitePool, runMigration,
                                          runSqlPersistMPool, runSqlPool)
import           System.Random (randomRIO)
import           Text.Blaze (text)
import           Yesod
import           Yesod.Static

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
HangmanWord
   spelling Text
Game
   word    HangmanWordId
   lives   Int
   missing String
|]

data Hangman = Hangman {
    connPool    :: ConnectionPool,
    getStatic   :: Static
  }

staticFiles "static"

mkYesod "Hangman" [parseRoutes|
/             HomeR  GET
/word         AddHangmanWordR GET POST
/words        HangmanWordsR   GET
/game         NewGameR        GET
/games        GamesR          GET
/game/#GameId GameR           GET POST
/static StaticR Static getStatic
|]
instance Yesod Hangman where
  defaultLayout inside = do
    mmsg <- getMessage
    pc <- widgetToPageContent $ do
      toWidget [lucius|
body {
  width: 760px;
  margin: 1em auto;
  font-family: sans-serif;
}
#message {
  color: #900;
}
|]
      inside
    withUrlRenderer [hamlet|
$doctype 5
<html>
  <head>
    <title>#{pageTitle pc}
    ^{pageHead pc}
  <body>
    <h1>
      #{pageTitle pc}
    $maybe msg <- mmsg
      <div #message>#{msg}
    ^{pageBody pc}
|]

instance YesodPersist Hangman where
  type YesodPersistBackend Hangman = SqlBackend
  runDB f = do
    master <- getYesod
    let pool = connPool master
    runSqlPool f pool

type Form x = Html -> MForm Handler (FormResult x, Widget)

instance RenderMessage Hangman FormMessage where
  renderMessage _ _ = defaultFormMessage

hangmans :: [StaticRoute]
hangmans = [img_hangman0_svg, img_hangman1_svg, img_hangman2_svg,
            img_hangman3_svg, img_hangman4_svg, img_hangman5_svg,
            img_hangman6_svg]

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Yesod Hangman"
    [whamlet|
<p>Welcome to Hangman
<ul>
  <li>
    <a href=@{HangmanWordsR}>Hangman words
  <li>
    <a href=@{AddHangmanWordR}>New Hangman word
  <li>
    <a href=@{GamesR}>Hangman games
  <li>
    <a href=@{NewGameR}>New game
|]

wordForm :: Form HangmanWord
wordForm = renderDivs $ HangmanWord
  <$> areq textField "Hangman Word" Nothing

getAddHangmanWordR :: Handler Html
getAddHangmanWordR = postAddHangmanWordR

postAddHangmanWordR :: Handler Html
postAddHangmanWordR = do
  ((result, wordWidget), enctype) <- runFormPost wordForm
  case result of
    FormSuccess word -> do
      _ <- runDB $ insert word
      setMessage $ text $ "Added new word " <> hangmanWordSpelling word <> "."
      redirect HangmanWordsR
    FormFailure _ -> setMessage "Please choose another word."
    _             -> return ()
  defaultLayout $ do
    setTitle "Add a new word"
    [whamlet|
<form method=post enctype=#{enctype}>
  ^{wordWidget}
  <div>
    <input type=submit value="Add word">
    |]

getHangmanWordsR :: Handler Html
getHangmanWordsR = do
  hangmanWords <- runDB $ selectList [] [Asc HangmanWordSpelling]
  defaultLayout $ do
    setTitle "Available Hangman Words"
    [whamlet|
<ul>
  $forall Entity _ word <- hangmanWords
    <li>
      #{hangmanWordSpelling word}
<ul>
  <li>
    <a href=@{AddHangmanWordR}>New Hangman word
  <li>
    <a href=@{HomeR}>Home page
    |]

getGameR :: GameId -> Handler Html
getGameR = postGameR

charForm :: Form Char
charForm = renderDivs $
  T.head <$> areq charField "Character" Nothing
  where
    charField = check validateChar textField
    validateChar :: Text -> Either Text Text
    validateChar t
      | T.null t          = Left "No character provided!"
      | T.null (T.tail t) = Right t
      | otherwise         = Left "Only one character is allowed!"

postGameR :: GameId -> Handler Html
postGameR game = do
  Game word lives missing <- runDB $ get404 game
  hangmanWord <- runDB $ get404 word
  if lives < 1 then lost hangmanWord missing
    else
    if null missing then won hangmanWord lives
      else do
      ((result, charWidget), enctype) <- runFormPost charForm
      (missing', lives') <- case result of
        FormSuccess char ->
          if char `elem` missing
            then do
            let missing' = filter (toLower char /=) $ filter (toUpper char /=) missing
            _ <- runDB $ update game [GameMissing =. missing']
            if null missing' then redirect $ GameR game
              else return (missing', lives)
            else do
            _ <- runDB $ update game [GameLives -=. 1]
            if lives <= 1 then redirect $ GameR game
              else return (missing, lives - 1)
        _ -> return (missing, lives)
      defaultLayout $ do
        setTitle "Hangman"
        [whamlet|
<img src=@{StaticR $ hangmans !! lives'} />
<pre>
  #{formatWord missing' hangmanWord}
<p>
  You have #{lives'} lives left.
<p>
  Choose a character
<form method=post enctype=#{enctype}>
  ^{charWidget}
  <div>
    <input type=submit value="Guess Character">
      |]

formatWord :: String -> HangmanWord -> Text
formatWord missing hangmanWord =
  T.intersperse ' ' $ replaceMissing `T.map` hangmanWordSpelling hangmanWord
  where
    replaceMissing c
      | c `elem` missing = '_'
      | otherwise        = c

getGamesR :: Handler Html
getGamesR = do
  games <- runDB $ selectList [] [Asc GameId]
  defaultLayout $ do
    setTitle "Games"
    [whamlet|
<ul>
  $forall Entity gid game <- games
    <li>
      #{gameLives game} 
      <a href=@{GameR gid}>
        $if active game
          Continue
        $else
          Finished
<p>
  <a href=@{NewGameR}>New game
    |]
  where
    active game =
      gameLives game > 0 && not (null $ gameMissing game)

lost :: HangmanWord -> String -> Handler Html
lost hangmanWord missing =
  defaultLayout $ do
    setTitle "Hangman Game lost!"
    [whamlet|
<p>Sorry, you failed guessing
<pre>#{formatWord missing hangmanWord}
<p>right and died.
<img src=@{StaticR $ img_hangman0_svg} />
<ul>
  <li>
    <a href=@{HomeR}>Home page
    |]

won :: HangmanWord -> Int -> Handler Html
won hangmanWord lives =
  defaultLayout $ do
    setTitle "Hangman Game won!"
    [whamlet|
<p>Congratulations you guessed
<pre>#{hangmanWordSpelling hangmanWord}
<p>right and survived with #{show lives} lives.
<img src=@{StaticR $ hangmans !! lives} />
<ul>
  <li>
    <a href=@{HomeR}>Home page
    |]

getNewGameR :: Handler Html
getNewGameR = do
  hangmanWords <- runDB $ selectList [] []
  wordPos <- liftIO $ randomRIO (0, length hangmanWords - 1)
  let Entity wordId word = hangmanWords !! wordPos
  game <- runDB $ insert $ Game wordId 6 $ nub $ unpack $ hangmanWordSpelling word
  redirect $ GameR game

main :: IO ()
main = do
    pool <- runStdoutLoggingT $ createSqlitePool "hangman.db3" 10
    runSqlPersistMPool (runMigration migrateAll) pool
    static' <- static "static"
    warp 3001 $ Hangman pool static'
