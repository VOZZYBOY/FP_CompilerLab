# Минимальный функциональный язык программирования

Этот проект представляет собой компилятор и интерпретатор для минимального функционального языка программирования, разработанного на языке F#. Язык поддерживает базовые функциональные конструкции, такие как переменные, арифметические операции, условные выражения и создание новых переменных.

## Описание языка

### Поддерживаемые конструкции:

1. **Переменные**: 
   - Пример: `(let x = 5 ...)` — создание переменной `x` со значением `5`.

2. **Арифметические операции**:
   - **Сложение**: `(add <expr1> <expr2>)` — сложение двух выражений.
     - Пример: `(add 5 10)` — результатом будет `15`.
   - **Умножение**: `(mul <expr1> <expr2>)` — умножение двух выражений.
     - Пример: `(mul 5 10)` — результатом будет `50`.

3. **Условные выражения**:
   - Пример: `(if <cond> <expr1> <expr2>)` — если условие `cond` истинно (не равно 0), вычисляется выражение `expr1`; иначе вычисляется `expr2`.
     - Пример: `(if (mul x 2) (add x 5) 0)` — если результат умножения `x` на 2 не равен 0, то возвращается `x + 5`, иначе — `0`.

## Пример программы

Программа, которая демонстрирует основные возможности языка:

```plaintext
(let x = (add 5 10) (if (mul x 2) (add x 5) 0))

#### Структура проекта
**Компилятор: Парсит входной код, создавая абстрактное синтаксическое дерево (AST).**
Интерпретатор: Выполняет программу, вычисляя результат на основе AST.
Особенности
Простая реализация для минимального функционального языка.
Поддержка переменных, базовых арифметических операций и условных операторов.
Минимальный и легко расширяемый синтаксис.
Авторы
Андриянов Эрик Вячеславович, M80-212Б-22Б

