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

extendedGreeting person = 
  let
    joinWithNewLines a b = a <> "\n" <> b
    joined = joinWithNewLines hello goodbye
    hello = makeGreeting salutations person
    goodbye = makeGreeting "See you later" person
  in
    joined

extendedGreeting' person = 
  let
    joinWithNewLines a b = a <> "\n" <> b
    helloAndGoodBye hello goodbye =
      let
        hello' = makeGreeting hello person
        goodbye' = makeGreeting goodbye person
      in
        joinWithNewLines hello' goodbye'
  in
    helloAndGoodBye "Hello" "Goodbye"

letWhereGreeting name place =
  let
    salutation= "Hello " <> name
    meetingInfo = location "Tuesday"
  in
    salutation <> " " <> meetingInfo
  where 
    location day = "we met at " <> place <> " on a " <> day

extendedGreeting'' person =
  helloAndGoodBye "Hello" "Goodbye"
  where
    helloAndGoodBye hello goodbye = 
      joinWithNewLines hello' goodbye'
      where
        hello' = "Hello"
        goodbye' = "Goodbye"
    joinWithNewLines a b = a <> "\n" <> b

printSmallNumber num =
  let
    msg = if num < 10
      then show num
      else "The number is too big!"
  in
    print msg

guardSize num
  | num < 3 = ecxlaim "small"
  | num < 10 = ecxlaim "tmedium"
  | num < 100 = ecxlaim "pretty big"
  | num < 1000 = ecxlaim"giant"
  | otherwise = ecxlaim "unfathomalbly big"  
  where
    ecxlaim message = "That's a " <> message <> " number!"


main = print greetings