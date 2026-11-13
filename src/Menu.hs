{-# LANGUAGE ScopedTypeVariables #-}
-- | Універсальне текстове меню і зрозуміла обробка помилок бази даних
module Menu (menu, safely) where

import           Control.Exception            (Handler (..), catches)
import           Control.Monad                (forM_)
import qualified Database.MySQL.Base          as Base
import           Database.MySQL.Simple        (FormatError, QueryError)
import           Database.MySQL.Simple.Result (ResultError)
import           Text.Printf                  (printf)
import           Text.Read                    (readMaybe)

import Input (prompt)

-- | Меню з пронумерованими пунктами. Працює, доки користувач не введе 0
menu :: String -> String -> [(String, IO ())] -> IO ()
menu title exitLabel items = loop
  where
    loop = do
      putStrLn ("\n========== " ++ title ++ " ==========")
      forM_ (zip [1 :: Int ..] items) $ \(i, (name, _)) -> printf "%d. %s\n" i name
      putStrLn ("0. " ++ exitLabel)
      choice <- prompt "Ваш вибір"
      case readMaybe choice :: Maybe Int of
        Just 0 -> pure ()
        Just n | n >= 1 && n <= length items -> safely (snd (items !! (n - 1))) >> loop
        _ -> putStrLn "Такого пункту немає, спробуйте ще раз." >> loop

-- | Виконує дію і, якщо щось пішло не так у БД, пояснює це людською мовою,
-- а програма працює далі
safely :: IO () -> IO ()
safely action = action `catches`
  [ Handler $ \(e :: Base.MySQLError) -> putStrLn ("Помилка MySQL: " ++ explain e)
  , Handler $ \(e :: ResultError)     -> putStrLn ("Помилка читання даних: " ++ show e)
  , Handler $ \(e :: FormatError)     -> putStrLn ("Помилка у запиті: " ++ show e)
  , Handler $ \(e :: QueryError)      -> putStrLn ("Помилка у запиті: " ++ show e)
  ]
  where
    explain e = case Base.errNumber e of
      1062 -> "такий запис уже є (порушено унікальність)."
      1451 -> "запис використовується в інших таблицях, спочатку видаліть або змініть пов'язані записи."
      1452 -> "немає запису, на який ви посилаєтесь (неправильний id)."
      3819 -> "значення не проходить перевірку CHECK."
      _    -> Base.errMessage e
