module Main where

salutations = "Hello"
person = "Marco"
greetings =
  salutations <> " " <> person

makeGreeting salutation person' =
  salutation <> " " <> person'


makeThruple a b c = (a,b,c)
lambdaThruple a b = \c -> (a,b,c)
lambdaThruple' = \a -> \b -> \c -> (a,b,c)

greetPerson = makeGreeting "Hello "

enthusiasticGreeting salutation =
  makeGreeting (salutation <> "!")

greetGeorge = (`makeGreeting` "George")

greetGeorge' = flip makeGreeting "George"

main = print greetings