{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Класи типів, на яких тримається вся програма, і узагальнені
-- (generic) операції з базою даних, написані один раз для всіх таблиць.
module Classes
  ( DbEnum (..), allValues, fromDbCode, convertEnum
  , Entity (..), Tabular (..), Editable (..), Record
  , proxyOf, selectAll, selectById, insertEntity, updateEntity, deleteEntity
  ) where

import Control.Exception                  (evaluate, throw)
import Data.ByteString                    (ByteString)
import Data.List                          (intercalate)
import Data.Maybe                         (listToMaybe)
import Data.Proxy                         (Proxy (..))
import Data.String                        (fromString)
import Data.Text                          (Text)
import Database.MySQL.Base.Types          (Field)
import Database.MySQL.Simple
import Database.MySQL.Simple.Param        (Action, render)
import Database.MySQL.Simple.QueryResults (QueryResults)
import Database.MySQL.Simple.Result       (ResultError (..), convert)

------------------------------------------------------------------------
-- Перелічувані типи (роль, статус, тип заходу...)

-- | Тип з фіксованим набором значень, який зберігається в БД як рядок.
-- dbCode - як значення записане в таблиці, uaLabel - як його бачить користувач
class (Eq a, Enum a, Bounded a) => DbEnum a where
  dbCode  :: a -> Text
  uaLabel :: a -> Text

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound .. maxBound]

fromDbCode :: DbEnum a => Text -> Maybe a
fromDbCode t = lookup t [(dbCode x, x) | x <- allValues]

-- | Спільна логіка для instance Result будь-якого DbEnum
convertEnum :: DbEnum a => String -> Field -> Maybe ByteString -> a
convertEnum typeName f v =
  case fromDbCode raw of
    Just x  -> x
    Nothing -> throw (ConversionFailed "VARCHAR" typeName "" ("невідоме значення " ++ show raw))
  where
    raw = convert f v :: Text

------------------------------------------------------------------------
-- Таблиці

-- | Тип, який відповідає таблиці БД з ключем id
class QueryResults a => Entity a where
  tableName  :: Proxy a -> String     -- ім'я таблиці в MySQL
  tableTitle :: Proxy a -> String     -- назва для меню
  columns    :: Proxy a -> [String]   -- стовпці, крім id
  entityId   :: a -> Int
  toRow      :: a -> [Action]         -- значення стовпців у тому ж порядку, що й columns

-- | Як показати запис у вигляді рядка таблиці на екрані
class Tabular a where
  headers :: Proxy a -> [String]
  cells   :: a -> [String]
  -- | Короткий опис запису для підказок (за замовчуванням другий стовпець)
  short   :: a -> String
  short x = cells x !! 1

-- | Введення нового запису (Nothing) або редагування наявного (Just старий).
-- Connection потрібен, щоб підказати користувачу допустимі id з інших таблиць
class Editable a where
  edit :: Connection -> Maybe a -> IO a

-- | Усе, що потрібно для повноцінної роботи з таблицею в меню
type Record a = (Entity a, Tabular a, Editable a)

proxyOf :: a -> Proxy a
proxyOf _ = Proxy

------------------------------------------------------------------------
-- Узагальнений CRUD: одна реалізація на всі сім таблиць

allColumns :: Entity a => Proxy a -> String
allColumns p = intercalate ", " ("id" : columns p)

-- | Строгі поля + evaluate: помилки перетворення ловимо тут, а не пізніше
forceAll :: [a] -> IO [a]
forceAll = mapM evaluate

selectAll :: forall a. Entity a => Connection -> Proxy a -> IO [a]
selectAll conn p =
  query_ conn (fromString sql) >>= forceAll
  where
    sql = "SELECT " ++ allColumns p ++ " FROM " ++ tableName p ++ " ORDER BY id"

selectById :: forall a. Entity a => Connection -> Proxy a -> Int -> IO (Maybe a)
selectById conn p i =
  listToMaybe <$> (query conn (fromString sql) (Only i) >>= forceAll)
  where
    sql = "SELECT " ++ allColumns p ++ " FROM " ++ tableName p ++ " WHERE id = ?"

-- | Додає запис і повертає id, який йому видала база
insertEntity :: forall a. Entity a => Connection -> a -> IO Int
insertEntity conn x = do
  _ <- execute conn (fromString sql) (toRow x)
  fromIntegral <$> insertID conn
  where
    p    = proxyOf x
    cols = columns p
    sql  = "INSERT INTO " ++ tableName p ++ " (" ++ intercalate ", " cols ++ ") VALUES ("
        ++ intercalate ", " (map (const "?") cols) ++ ")"

updateEntity :: forall a. Entity a => Connection -> a -> IO ()
updateEntity conn x = do
  _ <- execute conn (fromString sql) (toRow x ++ [render (entityId x)])
  pure ()
  where
    p   = proxyOf x
    sql = "UPDATE " ++ tableName p ++ " SET "
       ++ intercalate ", " [c ++ " = ?" | c <- columns p] ++ " WHERE id = ?"

-- | Повертає кількість видалених рядків (0, якщо такого id немає)
deleteEntity :: forall a. Entity a => Connection -> Proxy a -> Int -> IO Int
deleteEntity conn p i =
  fromIntegral <$> execute conn (fromString sql) (Only i)
  where
    sql = "DELETE FROM " ++ tableName p ++ " WHERE id = ?"
