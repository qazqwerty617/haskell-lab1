{-# LANGUAGE ScopedTypeVariables #-}
-- | Допоміжні функції для діалогу з користувачем: введення з перевіркою
-- і акуратний вивід таблиць.
module Input
  ( prompt, askText, askOptional, askInt, askDay, askTime, askEnum, askRef
  , confirm, printTable, fmtDay, fmtTime
  ) where

import           Control.Monad    (forM_)
import           Data.Char        (isSpace)
import           Data.List        (intercalate, transpose)
import           Data.Proxy       (Proxy)
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Time
import           Database.MySQL.Simple (Connection)
import           System.IO        (hFlush, stdout)
import           Text.Read        (readMaybe)

import Classes

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

-- | Показує підказку і читає рядок
prompt :: String -> IO String
prompt label = do
  putStr (label ++ ": ")
  hFlush stdout
  trim <$> getLine

-- | Підказка з поточним значенням у дужках (коли редагуємо запис)
labelWith :: String -> Maybe String -> String
labelWith label Nothing    = label
labelWith label (Just old) = label ++ " [" ++ old ++ "]"

-- | Загальна схема: читаємо рядок, порожній рядок означає "залишити як було",
-- некоректне значення - питаємо ще раз
askWith :: (String -> Maybe a) -> (a -> String) -> String -> String -> Maybe a -> IO a
askWith parse shw hint label old = loop
  where
    loop = do
      s <- prompt (labelWith label (shw <$> old))
      case (s, old) of
        ("", Just v) -> pure v
        _ -> maybe (putStrLn ("  Некоректне значення. " ++ hint) >> loop) pure (parse s)

-- | Обов'язковий текст
askText :: String -> Maybe Text -> IO Text
askText = askWith nonEmpty T.unpack "Поле не може бути порожнім."
  where nonEmpty s = if null s then Nothing else Just (T.pack s)

-- | Необов'язковий текст, "-" очищає поле
askOptional :: String -> Maybe Text -> IO Text
askOptional label old = do
  s <- prompt (labelWith label (T.unpack <$> old) ++ " (необов'язково, \"-\" очистить)")
  pure $ case s of
    ""  -> maybe T.empty id old
    "-" -> T.empty
    _   -> T.pack s

askInt :: String -> Maybe Int -> IO Int
askInt = askWith readMaybe show "Потрібне ціле число."

askDay :: String -> Maybe Day -> IO Day
askDay label = askWith (parseTimeM True defaultTimeLocale "%d.%m.%Y") fmtDay
  "Формат дати: ДД.ММ.РРРР, наприклад 25.11.2026." (label ++ " (ДД.ММ.РРРР)")

askTime :: String -> Maybe TimeOfDay -> IO TimeOfDay
askTime label = askWith (parseTimeM True defaultTimeLocale "%H:%M") fmtTime
  "Формат часу: ГГ:ХХ, наприклад 13:30." (label ++ " (ГГ:ХХ)")

-- | Вибір значення перелічуваного типу за номером
askEnum :: forall a. DbEnum a => String -> Maybe a -> IO a
askEnum label old = do
  let options = allValues :: [a]
  putStrLn ("  " ++ intercalate ", "
    [show i ++ " - " ++ T.unpack (uaLabel o) | (i, o) <- zip [1 :: Int ..] options])
  askWith (pick options) (T.unpack . uaLabel) "Введіть номер зі списку." label old
  where
    pick options s = case readMaybe s of
      Just i | i >= 1 && i <= length options -> Just (options !! (i - 1))
      _ -> Nothing

-- | Посилання на запис іншої таблиці (зовнішній ключ).
-- Спочатку показуємо, з чого можна вибирати, потім перевіряємо, що такий id існує
askRef :: forall a. (Entity a, Tabular a) => Connection -> Proxy a -> String -> Maybe Int -> IO Int
askRef conn p label old = do
  rows <- selectAll conn p
  putStrLn ("  Доступні записи: " ++ intercalate "; "
    [show (entityId r) ++ " - " ++ short r | r <- rows])
  let loop = do
        i <- askInt label old
        exists <- selectById conn p i
        case exists of
          Just _  -> pure i
          Nothing -> putStrLn "  Запису з таким id немає, оберіть зі списку вище." >> loop
  loop

confirm :: String -> IO Bool
confirm q = do
  s <- prompt (q ++ " (т/н)")
  pure (s `elem` ["т", "Т", "так", "y", "Y"])

fmtDay :: Day -> String
fmtDay = formatTime defaultTimeLocale "%d.%m.%Y"

fmtTime :: TimeOfDay -> String
fmtTime = formatTime defaultTimeLocale "%H:%M"

-- | Друкує таблицю з вирівняними стовпцями. Довгі значення обрізаються
printTable :: [String] -> [[String]] -> IO ()
printTable hs rows
  | null rows = putStrLn "  (записів немає)"
  | otherwise = do
      let cut s   = if length s > 42 then take 39 s ++ "..." else s
          table   = hs : map (map cut) rows
          widths  = map (maximum . map length) (transpose table)
          line r  = "  " ++ intercalate " | " [c ++ replicate (w - length c) ' ' | (c, w) <- zip r widths]
          divider = "  " ++ intercalate "-+-" [replicate w '-' | w <- widths]
      putStrLn (line hs)
      putStrLn divider
      forM_ (tail table) (putStrLn . line)
      putStrLn ("  Усього записів: " ++ show (length rows))
