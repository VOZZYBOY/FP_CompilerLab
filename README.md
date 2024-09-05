# Функциональный язык программирования на F#

Этот репозиторий содержит простой функциональный язык программирования, разработанный на F#. Язык основан на лямбда-исчислении и поддерживает рекурсию, базовые арифметические операции, условные выражения и пользовательские функции. Язык разработан с минималистичным синтаксисом и фокусом на принципах функционального программирования.

## Возможности

- **Именованные переменные (`let`)**: Определение переменных в локальной области видимости.
- **Рекурсия (`letrec`)**: Поддержка рекурсивных функций.
- **Лямбда-функции**: Анонимные функции могут быть определены с помощью ключевого слова `lambda`.
- **Применение функций**: Поддержка применения функций к аргументам.
- **Условные выражения**: Простые условные конструкции с использованием `if`.
- **Базовые арифметические операции**: Поддерживаются `+`, `-`, `*`, `/`.
- **Целочисленные значения**: Операции с целыми числами.

## Примеры программ на пвесдоязыке


### Функция факториала

Функция факториала может быть определена с использованием рекурсии:

```text
letrec fact = lambda n -> 
    if (= n 0) 1 
    (* n (fact (- n 1)))
in fact 5
#### Лямбда функция
(lambda (x) (+ x 1)) 3
''''
##### Рекурсия(Фибоначи)
letrec fib = lambda n -> 
    if (= n 0) 0
    (if (= n 1) 1
    (+ (fib (- n 1)) (fib (- n 2))))
in fib 10 ```
Работал Андриянов Эрик Вячеславович совместно с chatgpt4o,copilot,яндекс алиса

