{-# LANGUAGE ScopedTypeVariables #-}
-- | Лабораторна робота №1, варіант 8.
-- Інформаційна система "Пакети прикладних програм, розроблених на факультеті".
-- Haskell + MySQL, робота з таблицями побудована на класах типів.
module Main (main) where

import           Control.Exception       (catch, throwIO)
import           Control.Monad           (forM_)
import           Data.Maybe              (fromMaybe)
import           Data.Proxy              (Proxy (..))
import           Database.MySQL.Base     (Option (CharsetName))
import           Database.MySQL.Simple
import           System.Environment      (lookupEnv)
import           System.IO
import           System.IO.Error         (isEOFError)

import Classes
import Input
import Instances ()
import Menu
import Model
import Reports

-- | Параметри підключення можна змінити змінними середовища DB_HOST, DB_USER, DB_PASSWORD
connectDb :: IO Connection
connectDb = do
  host <- fromMaybe "127.0.0.1" <$> lookupEnv "DB_HOST"
  user <- fromMaybe "root"      <$> lookupEnv "DB_USER"
  pass <- fromMaybe ""          <$> lookupEnv "DB_PASSWORD"
  connect defaultConnectInfo
    { connectHost     = host
    , connectUser     = user
    , connectPassword = pass
    , connectDatabase = "ppp_faculty"
    , connectOptions  = [CharsetName "utf8mb4"]
    }

-- | Одне й те саме підменю для будь-якої таблиці. Конкретну таблицю
-- визначає тип a, а всю роботу з нею роблять екземпляри класів
tableMenu :: forall a. Record a => Connection -> Proxy a -> IO ()
tableMenu conn p = menu (tableTitle p) "Назад"
  [ ("Показати всі записи", showAll)
  , ("Додати запис",        addRecord)
  , ("Редагувати запис",    editRecord)
  , ("Видалити запис",      deleteRecord)
  ]
  where
    showAll = selectAll conn p >>= printTable (headers p) . map cells

    addRecord = do
      putStrLn "Введіть дані нового запису:"
      x <- edit conn (Nothing :: Maybe a)
      newId <- insertEntity conn x
      putStrLn ("Запис додано, його id = " ++ show newId)

    editRecord = do
      i <- askInt "id запису, який редагуємо" Nothing
      found <- selectById conn p i
      case found of
        Nothing  -> putStrLn "Запису з таким id немає."
        Just old -> do
          putStrLn "Натисніть Enter, щоб залишити значення без змін."
          new <- edit conn (Just old)
          updateEntity conn new
          putStrLn "Зміни збережено."

    deleteRecord = do
      i <- askInt "id запису, який видаляємо" Nothing
      found <- selectById conn p i
      case found of
        Nothing -> putStrLn "Запису з таким id немає."
        Just x  -> do
          printTable (headers p) [cells x]
          ok <- confirm "Точно видалити цей запис?"
          if ok
            then deleteEntity conn p i >> putStrLn "Запис видалено."
            else putStrLn "Видалення скасовано."

mainMenu :: Connection -> IO ()
mainMenu conn = menu "Пакети прикладних програм факультету" "Вихід"
  [ (tableTitle (Proxy :: Proxy Person),        tableMenu conn (Proxy :: Proxy Person))
  , (tableTitle (Proxy :: Proxy Package),       tableMenu conn (Proxy :: Proxy Package))
  , (tableTitle (Proxy :: Proxy Developer),     tableMenu conn (Proxy :: Proxy Developer))
  , (tableTitle (Proxy :: Proxy Seminar),       tableMenu conn (Proxy :: Proxy Seminar))
  , (tableTitle (Proxy :: Proxy Participant),   tableMenu conn (Proxy :: Proxy Participant))
  , (tableTitle (Proxy :: Proxy Demonstration), tableMenu conn (Proxy :: Proxy Demonstration))
  , (tableTitle (Proxy :: Proxy TestResult),    tableMenu conn (Proxy :: Proxy TestResult))
  , ("Пошук і звіти",                           reportsMenu conn)
  ]

main :: IO ()
main = do
  forM_ [stdin, stdout, stderr] (`hSetEncoding` utf8)
  hSetBuffering stdout NoBuffering
  conn <- connectDb
  putStrLn "Підключення до бази ppp_faculty встановлено."
  mainMenu conn `catch` \(e :: IOError) ->
    if isEOFError e then putStrLn "\nВведення завершено." else throwIO e
  close conn
  putStrLn "Роботу завершено. До зустрічі!"
