-- Лабораторна робота №1, варіант 8
-- Інформаційна система "Пакети прикладних програм, розроблених на факультеті"
-- Схема бази даних MySQL і початкові дані для перевірки

SET NAMES utf8mb4;

DROP DATABASE IF EXISTS ppp_faculty;
CREATE DATABASE ppp_faculty CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
USE ppp_faculty;

-- 1. Студенти та викладачі, які займаються розробкою ППП
CREATE TABLE people (
    id         INT AUTO_INCREMENT PRIMARY KEY,
    full_name  VARCHAR(120) NOT NULL,
    role       VARCHAR(10)  NOT NULL CHECK (role IN ('student', 'teacher')),
    unit       VARCHAR(80)  NOT NULL,   -- група студента або кафедра викладача
    email      VARCHAR(80)  NOT NULL UNIQUE
);

-- 2. Самі пакети прикладних програм
CREATE TABLE packages (
    id           INT AUTO_INCREMENT PRIMARY KEY,
    title        VARCHAR(100) NOT NULL UNIQUE,
    annotation   TEXT         NOT NULL,
    kind         VARCHAR(60)  NOT NULL,   -- навчальний, науковий, прикладний...
    version      VARCHAR(20)  NOT NULL,
    release_date DATE         NOT NULL
);

-- 3. Хто і що робить у кожному пакеті (зв'язок "багато до багатьох")
CREATE TABLE package_developers (
    id           INT AUTO_INCREMENT PRIMARY KEY,
    package_id   INT NOT NULL,
    person_id    INT NOT NULL,
    contribution VARCHAR(120) NOT NULL,
    UNIQUE (package_id, person_id),
    FOREIGN KEY (package_id) REFERENCES packages(id),
    FOREIGN KEY (person_id)  REFERENCES people(id)
);

-- 4. Графік семінарів для ознайомлення з ППП
CREATE TABLE seminars (
    id           INT AUTO_INCREMENT PRIMARY KEY,
    package_id   INT  NOT NULL,
    presenter_id INT  NOT NULL,
    seminar_date DATE NOT NULL,
    start_time   TIME NOT NULL,
    room         VARCHAR(20)  NOT NULL,
    topic        VARCHAR(150) NOT NULL,
    FOREIGN KEY (package_id)   REFERENCES packages(id),
    FOREIGN KEY (presenter_id) REFERENCES people(id)
);

-- 5. Хто записався на семінар
CREATE TABLE seminar_participants (
    id         INT AUTO_INCREMENT PRIMARY KEY,
    seminar_id INT NOT NULL,
    person_id  INT NOT NULL,
    UNIQUE (seminar_id, person_id),
    FOREIGN KEY (seminar_id) REFERENCES seminars(id),
    FOREIGN KEY (person_id)  REFERENCES people(id)
);

-- 6. План демонстрацій та тестових випробувань
CREATE TABLE demonstrations (
    id             INT AUTO_INCREMENT PRIMARY KEY,
    package_id     INT  NOT NULL,
    event_type     VARCHAR(10) NOT NULL CHECK (event_type IN ('demo', 'test')),
    event_date     DATE NOT NULL,
    place          VARCHAR(60) NOT NULL,
    responsible_id INT  NOT NULL,
    status         VARCHAR(10) NOT NULL CHECK (status IN ('planned', 'done', 'cancelled')),
    FOREIGN KEY (package_id)     REFERENCES packages(id),
    FOREIGN KEY (responsible_id) REFERENCES people(id)
);

-- 7. Результати тестових випробувань за окремими критеріями
CREATE TABLE test_results (
    id               INT AUTO_INCREMENT PRIMARY KEY,
    demonstration_id INT NOT NULL,
    criterion        VARCHAR(100) NOT NULL,
    outcome          VARCHAR(10)  NOT NULL CHECK (outcome IN ('passed', 'failed')),
    comment          VARCHAR(200) NOT NULL DEFAULT '',
    FOREIGN KEY (demonstration_id) REFERENCES demonstrations(id)
);

-- Початкові дані

INSERT INTO people (full_name, role, unit, email) VALUES
('Коваленко Ірина Василівна',   'teacher', 'Кафедра прикладної математики',  'kovalenko@fcs.edu.ua'),
('Шевчук Олег Петрович',        'teacher', 'Кафедра програмної інженерії',   'shevchuk@fcs.edu.ua'),
('Мельник Андрій Сергійович',   'student', 'КН-31',                          'melnyk.a@stud.fcs.edu.ua'),
('Бондар Софія Олегівна',       'student', 'КН-31',                          'bondar.s@stud.fcs.edu.ua'),
('Ткаченко Максим Ігорович',    'student', 'ПІ-22',                          'tkachenko.m@stud.fcs.edu.ua'),
('Кравчук Дарина Андріївна',    'student', 'ПІ-22',                          'kravchuk.d@stud.fcs.edu.ua'),
('Олійник Віктор Миколайович',  'teacher', 'Кафедра інформаційних систем',   'oliinyk@fcs.edu.ua'),
('Савченко Юлія Романівна',     'student', 'ІС-41',                          'savchenko.y@stud.fcs.edu.ua');

INSERT INTO packages (title, annotation, kind, version, release_date) VALUES
('StatLab',      'Пакет для статистичної обробки експериментальних даних: описова статистика, регресія, перевірка гіпотез',      'науковий',   '2.1', '2025-03-15'),
('ГрафАналітик', 'Побудова та аналіз графів: пошук найкоротших шляхів, мінімальних остовів і компонент зв''язності',               'навчальний', '1.4', '2024-11-02'),
('MatrixKit',    'Бібліотека чисельних методів лінійної алгебри з наочною візуалізацією кожного кроку обчислень',                 'навчальний', '0.9', '2025-09-20'),
('EduTest',      'Система для створення тестів, автоматичної перевірки відповідей і збору статистики успішності студентів',        'прикладний', '3.0', '2023-05-10'),
('OptiPlan',     'Оптимізація розкладу занять з урахуванням побажань викладачів і завантаженості аудиторій',                      'прикладний', '1.0', '2026-02-01');

INSERT INTO package_developers (package_id, person_id, contribution) VALUES
(1, 1, 'Керівник проєкту, математичні моделі'),
(1, 3, 'Модуль регресійного аналізу'),
(1, 4, 'Інтерфейс користувача'),
(2, 2, 'Керівник проєкту'),
(2, 5, 'Алгоритми пошуку шляхів'),
(3, 1, 'Науковий консультант'),
(3, 6, 'Візуалізація обчислень'),
(4, 7, 'Керівник проєкту, архітектура'),
(4, 8, 'Модуль статистики успішності'),
(5, 2, 'Алгоритм оптимізації'),
(5, 5, 'Робота з базою даних'),
(5, 8, 'Тестування');

INSERT INTO seminars (package_id, presenter_id, seminar_date, start_time, room, topic) VALUES
(1, 1, '2026-11-18', '10:00:00', '305', 'Перші кроки в StatLab: імпорт даних і описова статистика'),
(2, 2, '2026-11-20', '13:30:00', '214', 'Алгоритми на графах у ГрафАналітику'),
(4, 7, '2026-11-25', '11:50:00', '118', 'Як створити власний тест в EduTest'),
(3, 6, '2026-12-02', '10:00:00', '305', 'Метод Гаусса крок за кроком у MatrixKit'),
(5, 2, '2026-12-09', '15:10:00', '214', 'OptiPlan: як скласти розклад без конфліктів');

INSERT INTO seminar_participants (seminar_id, person_id) VALUES
(1, 4), (1, 5), (1, 6), (1, 8),
(2, 3), (2, 6),
(3, 1), (3, 3), (3, 4),
(4, 3), (4, 5),
(5, 7), (5, 6);

INSERT INTO demonstrations (package_id, event_type, event_date, place, responsible_id, status) VALUES
(4, 'test', '2026-10-14', 'Лабораторія 118',   7, 'done'),
(2, 'demo', '2026-10-28', 'Актова зала',       2, 'done'),
(1, 'test', '2026-11-27', 'Лабораторія 305',   1, 'planned'),
(3, 'demo', '2026-12-10', 'Актова зала',       1, 'planned'),
(5, 'test', '2026-12-16', 'Лабораторія 214',   2, 'planned'),
(5, 'demo', '2026-12-22', 'Онлайн, Google Meet', 5, 'planned');

INSERT INTO test_results (demonstration_id, criterion, outcome, comment) VALUES
(1, 'Коректність автоматичної перевірки',  'passed', 'Перевірено на 500 відповідях'),
(1, 'Швидкодія при 200 користувачах',      'passed', 'Середній відгук 0,4 с'),
(1, 'Експорт статистики в Excel',          'failed', 'Неправильне кодування кирилиці, передано на доопрацювання'),
(2, 'Візуалізація графа на 1000 вершин',   'passed', ''),
(2, 'Алгоритм Дейкстри на тестових графах', 'passed', 'Усі 30 тестів пройдено');
