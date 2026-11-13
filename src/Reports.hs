{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Пошук і звіти. Тут запити з JOIN по кількох таблицях,
-- щоб замість голих id користувач бачив імена і назви.
module Reports (reportsMenu) where

import           Data.Proxy            (Proxy (..))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Time             (Day, TimeOfDay)
import           Database.MySQL.Simple

import Classes
import Input
import Instances ()
import Menu
import Model

reportsMenu :: Connection -> IO ()
reportsMenu conn = menu "Пошук і звіти" "Назад"
  [ ("Пошук ППП за словом у назві, анотації або виді", searchPackages conn)
  , ("Пошук людей за ПІБ або групою / кафедрою",      searchPeople conn)
  , ("Команда розробників обраного ППП",              packageTeam conn)
  , ("Над якими ППП працює обрана людина",            personProjects conn)
  , ("Графік семінарів за період",                    seminarSchedule conn)
  , ("Список учасників семінару",                     seminarParticipants conn)
  , ("План демонстрацій і випробувань (заплановані)", demoPlan conn)
  , ("Результати випробувань обраного ППП",           testReport conn)
  , ("Зведена статистика по всіх ППП",                packageStats conn)
  ]

str :: Text -> String
str = T.unpack

likePattern :: String -> Text
likePattern w = T.pack ("%" ++ w ++ "%")

label :: DbEnum a => a -> String
label = T.unpack . uaLabel

searchPackages :: Connection -> IO ()
searchPackages conn = do
  w <- prompt "Що шукаємо"
  let p = likePattern w
  rows <- query conn
    "SELECT id, title, kind, version, annotation FROM packages \
    \WHERE title LIKE ? OR annotation LIKE ? OR kind LIKE ? ORDER BY title" (p, p, p)
  printTable ["id", "Назва", "Вид", "Версія", "Анотація"]
    [ [show i, str t, str k, str v, str a] | (i :: Int, t, k, v, a) <- rows ]

searchPeople :: Connection -> IO ()
searchPeople conn = do
  w <- prompt "Частина ПІБ, група або кафедра"
  let p = likePattern w
  rows <- query conn
    "SELECT p.id, p.full_name, p.role, p.unit, COUNT(d.id) \
    \FROM people p LEFT JOIN package_developers d ON d.person_id = p.id \
    \WHERE p.full_name LIKE ? OR p.unit LIKE ? \
    \GROUP BY p.id, p.full_name, p.role, p.unit ORDER BY p.full_name" (p, p)
  printTable ["id", "ПІБ", "Роль", "Група / кафедра", "Кількість ППП"]
    [ [show i, str n, label (r :: Role), str u, show (c :: Int)] | (i :: Int, n, r, u, c) <- rows ]

packageTeam :: Connection -> IO ()
packageTeam conn = do
  pkg <- askRef conn (Proxy :: Proxy Package) "id ППП" Nothing
  rows <- query conn
    "SELECT p.full_name, p.role, p.unit, d.contribution \
    \FROM package_developers d JOIN people p ON p.id = d.person_id \
    \WHERE d.package_id = ? ORDER BY p.role DESC, p.full_name" (Only pkg)
  printTable ["ПІБ", "Роль", "Група / кафедра", "Внесок"]
    [ [str n, label (r :: Role), str u, str c] | (n, r, u, c) <- rows ]

personProjects :: Connection -> IO ()
personProjects conn = do
  per <- askRef conn (Proxy :: Proxy Person) "id людини" Nothing
  rows <- query conn
    "SELECT pk.title, pk.version, pk.kind, d.contribution \
    \FROM package_developers d JOIN packages pk ON pk.id = d.package_id \
    \WHERE d.person_id = ? ORDER BY pk.title" (Only per)
  printTable ["ППП", "Версія", "Вид", "Внесок"]
    [ [str t, str v, str k, str c] | (t, v, k, c) <- rows ]

seminarSchedule :: Connection -> IO ()
seminarSchedule conn = do
  from <- askDay "Від" Nothing
  to   <- askDay "До" Nothing
  rows <- query conn
    "SELECT s.seminar_date, s.start_time, s.room, pk.title, pr.full_name, s.topic, \
    \       (SELECT COUNT(*) FROM seminar_participants sp WHERE sp.seminar_id = s.id) \
    \FROM seminars s \
    \JOIN packages pk ON pk.id = s.package_id \
    \JOIN people pr ON pr.id = s.presenter_id \
    \WHERE s.seminar_date BETWEEN ? AND ? \
    \ORDER BY s.seminar_date, s.start_time" (from, to)
  printTable ["Дата", "Час", "Ауд.", "ППП", "Доповідач", "Тема", "Учасників"]
    [ [fmtDay d, fmtTime t, str r, str pk, str pr, str tp, show (c :: Int)]
    | (d :: Day, t :: TimeOfDay, r, pk, pr, tp, c) <- rows ]

seminarParticipants :: Connection -> IO ()
seminarParticipants conn = do
  sem <- askRef conn (Proxy :: Proxy Seminar) "id семінару" Nothing
  rows <- query conn
    "SELECT p.full_name, p.role, p.unit FROM seminar_participants sp \
    \JOIN people p ON p.id = sp.person_id WHERE sp.seminar_id = ? ORDER BY p.full_name" (Only sem)
  printTable ["ПІБ", "Роль", "Група / кафедра"]
    [ [str n, label (r :: Role), str u] | (n, r, u) <- rows ]

demoPlan :: Connection -> IO ()
demoPlan conn = do
  rows <- query_ conn
    "SELECT d.event_date, d.event_type, pk.title, d.place, p.full_name \
    \FROM demonstrations d \
    \JOIN packages pk ON pk.id = d.package_id \
    \JOIN people p ON p.id = d.responsible_id \
    \WHERE d.status = 'planned' ORDER BY d.event_date"
  printTable ["Дата", "Захід", "ППП", "Місце", "Відповідальний"]
    [ [fmtDay d, label (k :: EventKind), str t, str pl, str n] | (d, k, t, pl, n) <- rows ]

testReport :: Connection -> IO ()
testReport conn = do
  pkg <- askRef conn (Proxy :: Proxy Package) "id ППП" Nothing
  rows <- query conn
    "SELECT d.event_date, r.criterion, r.outcome, r.comment \
    \FROM test_results r JOIN demonstrations d ON d.id = r.demonstration_id \
    \WHERE d.package_id = ? ORDER BY d.event_date, r.id" (Only pkg)
  printTable ["Дата", "Критерій", "Результат", "Коментар"]
    [ [fmtDay d, str c, label o, str cm] | (d, c, o :: Outcome, cm) <- rows ]
  let passed = length [ () | (_ :: Day, _ :: Text, Passed, _ :: Text) <- rows ]
  if null rows
    then putStrLn "  Для цього ППП ще немає результатів випробувань."
    else putStrLn ("  Пройдено " ++ show passed ++ " з " ++ show (length rows) ++ " перевірок.")

packageStats :: Connection -> IO ()
packageStats conn = do
  rows <- query_ conn
    "SELECT pk.title, \
    \  (SELECT COUNT(*) FROM package_developers d WHERE d.package_id = pk.id), \
    \  (SELECT COUNT(*) FROM seminars s WHERE s.package_id = pk.id), \
    \  (SELECT COUNT(*) FROM seminar_participants sp JOIN seminars s ON s.id = sp.seminar_id \
    \     WHERE s.package_id = pk.id), \
    \  (SELECT COUNT(*) FROM demonstrations d WHERE d.package_id = pk.id), \
    \  (SELECT COUNT(*) FROM test_results r JOIN demonstrations d ON d.id = r.demonstration_id \
    \     WHERE d.package_id = pk.id AND r.outcome = 'passed'), \
    \  (SELECT COUNT(*) FROM test_results r JOIN demonstrations d ON d.id = r.demonstration_id \
    \     WHERE d.package_id = pk.id) \
    \FROM packages pk ORDER BY pk.title"
  printTable ["ППП", "Розробників", "Семінарів", "Слухачів", "Заходів", "Тестів пройдено"]
    [ [str t, show dv, show sm, show sl, show ev, show ok ++ " з " ++ show total]
    | (t, dv :: Int, sm :: Int, sl :: Int, ev :: Int, ok :: Int, total :: Int) <- rows ]
