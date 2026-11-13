{-# LANGUAGE OverloadedStrings #-}
-- | Типи даних предметної області "Пакети прикладних програм факультету".
-- Кожен тип відповідає одній таблиці бази даних.
module Model where

import Data.Text (Text)
import Data.Time (Day, TimeOfDay)

-- Перелічувані значення, які в БД зберігаються як короткий рядок

data Role = Student | Teacher
  deriving (Eq, Show, Enum, Bounded)

data EventKind = Demo | Test
  deriving (Eq, Show, Enum, Bounded)

data EventStatus = Planned | Done | Cancelled
  deriving (Eq, Show, Enum, Bounded)

data Outcome = Passed | Failed
  deriving (Eq, Show, Enum, Bounded)

-- Записи таблиць. Поля строгі (!), щоб помилки перетворення даних
-- з'являлися одразу під час читання, а не десь пізніше

-- | Таблиця people
data Person = Person
  { personId    :: !Int
  , personName  :: !Text
  , personRole  :: !Role
  , personUnit  :: !Text      -- група або кафедра
  , personEmail :: !Text
  }

-- | Таблиця packages
data Package = Package
  { packageId         :: !Int
  , packageTitle      :: !Text
  , packageAnnotation :: !Text
  , packageKind       :: !Text
  , packageVersion    :: !Text
  , packageRelease    :: !Day
  }

-- | Таблиця package_developers
data Developer = Developer
  { devId           :: !Int
  , devPackage      :: !Int
  , devPerson       :: !Int
  , devContribution :: !Text
  }

-- | Таблиця seminars
data Seminar = Seminar
  { semId        :: !Int
  , semPackage   :: !Int
  , semPresenter :: !Int
  , semDate      :: !Day
  , semTime      :: !TimeOfDay
  , semRoom      :: !Text
  , semTopic     :: !Text
  }

-- | Таблиця seminar_participants
data Participant = Participant
  { partId      :: !Int
  , partSeminar :: !Int
  , partPerson  :: !Int
  }

-- | Таблиця demonstrations
data Demonstration = Demonstration
  { demoId          :: !Int
  , demoPackage     :: !Int
  , demoKind        :: !EventKind
  , demoDate        :: !Day
  , demoPlace       :: !Text
  , demoResponsible :: !Int
  , demoStatus      :: !EventStatus
  }

-- | Таблиця test_results
data TestResult = TestResult
  { resId        :: !Int
  , resDemo      :: !Int
  , resCriterion :: !Text
  , resOutcome   :: !Outcome
  , resComment   :: !Text
  }
