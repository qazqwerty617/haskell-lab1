{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Екземпляри (instances) класів для всіх типів предметної області.
-- Тут описано, як кожен тип читається з БД, записується в БД,
-- показується на екрані і вводиться з клавіатури.
module Instances () where

import           Data.Proxy                         (Proxy (..))
import qualified Data.Text                          as T
import           Database.MySQL.Simple.Param        (Param (..))
import           Database.MySQL.Simple.QueryResults (QueryResults (..), convertError)
import           Database.MySQL.Simple.Result       (Result (..))

import Classes
import Input
import Model

------------------------------------------------------------------------
-- Перелічувані типи: код у БД, підпис для людини, читання і запис

instance DbEnum Role where
  dbCode Student = "student"
  dbCode Teacher = "teacher"
  uaLabel Student = "студент"
  uaLabel Teacher = "викладач"

instance DbEnum EventKind where
  dbCode Demo = "demo"
  dbCode Test = "test"
  uaLabel Demo = "демонстрація"
  uaLabel Test = "тестове випробування"

instance DbEnum EventStatus where
  dbCode Planned   = "planned"
  dbCode Done      = "done"
  dbCode Cancelled = "cancelled"
  uaLabel Planned   = "заплановано"
  uaLabel Done      = "проведено"
  uaLabel Cancelled = "скасовано"

instance DbEnum Outcome where
  dbCode Passed = "passed"
  dbCode Failed = "failed"
  uaLabel Passed = "пройдено"
  uaLabel Failed = "не пройдено"

instance Result Role        where convert = convertEnum "Role"
instance Result EventKind   where convert = convertEnum "EventKind"
instance Result EventStatus where convert = convertEnum "EventStatus"
instance Result Outcome     where convert = convertEnum "Outcome"

instance Param Role        where render = render . dbCode
instance Param EventKind   where render = render . dbCode
instance Param EventStatus where render = render . dbCode
instance Param Outcome     where render = render . dbCode

label :: DbEnum a => a -> String
label = T.unpack . uaLabel

------------------------------------------------------------------------
-- Person (people)

instance QueryResults Person where
  convertResults [fa, fb, fc, fd, fe] [va, vb, vc, vd, ve] =
    Person (convert fa va) (convert fb vb) (convert fc vc) (convert fd vd) (convert fe ve)
  convertResults fs vs = convertError fs vs 5

instance Entity Person where
  tableName  _ = "people"
  tableTitle _ = "Студенти та викладачі"
  columns    _ = ["full_name", "role", "unit", "email"]
  entityId     = personId
  toRow p      = [render (personName p), render (personRole p), render (personUnit p), render (personEmail p)]

instance Tabular Person where
  headers _ = ["id", "ПІБ", "Роль", "Група / кафедра", "E-mail"]
  cells p   = [show (personId p), T.unpack (personName p), label (personRole p),
               T.unpack (personUnit p), T.unpack (personEmail p)]

instance Editable Person where
  edit _ old = do
    name  <- askText "ПІБ" (personName <$> old)
    role  <- askEnum "Роль" (personRole <$> old)
    unit  <- askText "Група (для студента) або кафедра (для викладача)" (personUnit <$> old)
    email <- askText "Електронна пошта" (personEmail <$> old)
    pure (Person (maybe 0 personId old) name role unit email)

------------------------------------------------------------------------
-- Package (packages)

instance QueryResults Package where
  convertResults [fa, fb, fc, fd, fe, ff] [va, vb, vc, vd, ve, vf] =
    Package (convert fa va) (convert fb vb) (convert fc vc) (convert fd vd) (convert fe ve) (convert ff vf)
  convertResults fs vs = convertError fs vs 6

instance Entity Package where
  tableName  _ = "packages"
  tableTitle _ = "Пакети прикладних програм"
  columns    _ = ["title", "annotation", "kind", "version", "release_date"]
  entityId     = packageId
  toRow p      = [render (packageTitle p), render (packageAnnotation p), render (packageKind p),
                  render (packageVersion p), render (packageRelease p)]

instance Tabular Package where
  headers _ = ["id", "Назва", "Вид", "Версія", "Випущено", "Анотація"]
  cells p   = [show (packageId p), T.unpack (packageTitle p), T.unpack (packageKind p),
               T.unpack (packageVersion p), fmtDay (packageRelease p), T.unpack (packageAnnotation p)]

instance Editable Package where
  edit _ old = do
    title <- askText "Назва ППП" (packageTitle <$> old)
    ann   <- askText "Анотація" (packageAnnotation <$> old)
    kind  <- askText "Вид (навчальний, науковий, прикладний...)" (packageKind <$> old)
    ver   <- askText "Версія" (packageVersion <$> old)
    rel   <- askDay "Дата випуску" (packageRelease <$> old)
    pure (Package (maybe 0 packageId old) title ann kind ver rel)

------------------------------------------------------------------------
-- Developer (package_developers)

instance QueryResults Developer where
  convertResults [fa, fb, fc, fd] [va, vb, vc, vd] =
    Developer (convert fa va) (convert fb vb) (convert fc vc) (convert fd vd)
  convertResults fs vs = convertError fs vs 4

instance Entity Developer where
  tableName  _ = "package_developers"
  tableTitle _ = "Розробники ППП"
  columns    _ = ["package_id", "person_id", "contribution"]
  entityId     = devId
  toRow d      = [render (devPackage d), render (devPerson d), render (devContribution d)]

instance Tabular Developer where
  headers _ = ["id", "id ППП", "id розробника", "Внесок у проєкт"]
  cells d   = [show (devId d), show (devPackage d), show (devPerson d), T.unpack (devContribution d)]

instance Editable Developer where
  edit conn old = do
    pkg <- askRef conn (Proxy :: Proxy Package) "id ППП" (devPackage <$> old)
    per <- askRef conn (Proxy :: Proxy Person) "id розробника" (devPerson <$> old)
    con <- askText "Внесок у проєкт" (devContribution <$> old)
    pure (Developer (maybe 0 devId old) pkg per con)

------------------------------------------------------------------------
-- Seminar (seminars)

instance QueryResults Seminar where
  convertResults [fa, fb, fc, fd, fe, ff, fg] [va, vb, vc, vd, ve, vf, vg] =
    Seminar (convert fa va) (convert fb vb) (convert fc vc) (convert fd vd)
            (convert fe ve) (convert ff vf) (convert fg vg)
  convertResults fs vs = convertError fs vs 7

instance Entity Seminar where
  tableName  _ = "seminars"
  tableTitle _ = "Семінари з ознайомлення з ППП"
  columns    _ = ["package_id", "presenter_id", "seminar_date", "start_time", "room", "topic"]
  entityId     = semId
  toRow s      = [render (semPackage s), render (semPresenter s), render (semDate s),
                  render (semTime s), render (semRoom s), render (semTopic s)]

instance Tabular Seminar where
  headers _ = ["id", "Тема", "id ППП", "id доповідача", "Дата", "Час", "Ауд."]
  cells s   = [show (semId s), T.unpack (semTopic s), show (semPackage s), show (semPresenter s),
               fmtDay (semDate s), fmtTime (semTime s), T.unpack (semRoom s)]

instance Editable Seminar where
  edit conn old = do
    pkg   <- askRef conn (Proxy :: Proxy Package) "id ППП" (semPackage <$> old)
    pres  <- askRef conn (Proxy :: Proxy Person) "id доповідача" (semPresenter <$> old)
    day   <- askDay "Дата семінару" (semDate <$> old)
    time  <- askTime "Час початку" (semTime <$> old)
    room  <- askText "Аудиторія" (semRoom <$> old)
    topic <- askText "Тема" (semTopic <$> old)
    pure (Seminar (maybe 0 semId old) pkg pres day time room topic)

------------------------------------------------------------------------
-- Participant (seminar_participants)

instance QueryResults Participant where
  convertResults [fa, fb, fc] [va, vb, vc] =
    Participant (convert fa va) (convert fb vb) (convert fc vc)
  convertResults fs vs = convertError fs vs 3

instance Entity Participant where
  tableName  _ = "seminar_participants"
  tableTitle _ = "Учасники семінарів"
  columns    _ = ["seminar_id", "person_id"]
  entityId     = partId
  toRow p      = [render (partSeminar p), render (partPerson p)]

instance Tabular Participant where
  headers _ = ["id", "id семінару", "id учасника"]
  cells p   = [show (partId p), show (partSeminar p), show (partPerson p)]

instance Editable Participant where
  edit conn old = do
    sem <- askRef conn (Proxy :: Proxy Seminar) "id семінару" (partSeminar <$> old)
    per <- askRef conn (Proxy :: Proxy Person) "id учасника" (partPerson <$> old)
    pure (Participant (maybe 0 partId old) sem per)

------------------------------------------------------------------------
-- Demonstration (demonstrations)

instance QueryResults Demonstration where
  convertResults [fa, fb, fc, fd, fe, ff, fg] [va, vb, vc, vd, ve, vf, vg] =
    Demonstration (convert fa va) (convert fb vb) (convert fc vc) (convert fd vd)
                  (convert fe ve) (convert ff vf) (convert fg vg)
  convertResults fs vs = convertError fs vs 7

instance Entity Demonstration where
  tableName  _ = "demonstrations"
  tableTitle _ = "Демонстрації та тестові випробування"
  columns    _ = ["package_id", "event_type", "event_date", "place", "responsible_id", "status"]
  entityId     = demoId
  toRow d      = [render (demoPackage d), render (demoKind d), render (demoDate d),
                  render (demoPlace d), render (demoResponsible d), render (demoStatus d)]

instance Tabular Demonstration where
  headers _ = ["id", "Захід", "id ППП", "Дата", "Місце", "id відповідального", "Статус"]
  cells d   = [show (demoId d), label (demoKind d), show (demoPackage d), fmtDay (demoDate d),
               T.unpack (demoPlace d), show (demoResponsible d), label (demoStatus d)]
  short d   = label (demoKind d) ++ " " ++ fmtDay (demoDate d)

instance Editable Demonstration where
  edit conn old = do
    pkg    <- askRef conn (Proxy :: Proxy Package) "id ППП" (demoPackage <$> old)
    kind   <- askEnum "Тип заходу" (demoKind <$> old)
    day    <- askDay "Дата" (demoDate <$> old)
    place  <- askText "Місце проведення" (demoPlace <$> old)
    resp   <- askRef conn (Proxy :: Proxy Person) "id відповідального" (demoResponsible <$> old)
    status <- askEnum "Статус" (demoStatus <$> old)
    pure (Demonstration (maybe 0 demoId old) pkg kind day place resp status)

------------------------------------------------------------------------
-- TestResult (test_results)

instance QueryResults TestResult where
  convertResults [fa, fb, fc, fd, fe] [va, vb, vc, vd, ve] =
    TestResult (convert fa va) (convert fb vb) (convert fc vc) (convert fd vd) (convert fe ve)
  convertResults fs vs = convertError fs vs 5

instance Entity TestResult where
  tableName  _ = "test_results"
  tableTitle _ = "Результати випробувань"
  columns    _ = ["demonstration_id", "criterion", "outcome", "comment"]
  entityId     = resId
  toRow r      = [render (resDemo r), render (resCriterion r), render (resOutcome r), render (resComment r)]

instance Tabular TestResult where
  headers _ = ["id", "id заходу", "Критерій", "Результат", "Коментар"]
  cells r   = [show (resId r), show (resDemo r), T.unpack (resCriterion r),
               label (resOutcome r), T.unpack (resComment r)]

instance Editable TestResult where
  edit conn old = do
    demo <- askRef conn (Proxy :: Proxy Demonstration) "id заходу" (resDemo <$> old)
    crit <- askText "Критерій перевірки" (resCriterion <$> old)
    out  <- askEnum "Результат" (resOutcome <$> old)
    com  <- askOptional "Коментар" (resComment <$> old)
    pure (TestResult (maybe 0 resId old) demo crit out com)
