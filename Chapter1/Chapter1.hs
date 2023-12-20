module Chapter1 where
    
addOne num = num + 1
timesTwo num = num *2
squared num = num * num
minusFive num = num - 5

findResult num = minusFive ( squared (timesTwo (addOne num)))

timesTwoPlusOne = timesTwo . addOne

timesEight = timesTwo . timesTwo . timesTwo

-- Pointful -> Pointfree
makeGreeting' salutation person = salutation <> " " <> person
makeGreeting'' salutation = ((salutation <> " ") <>)
makeGreeting''' salutation = (<>) (salutation <> " ")
makeGreeting'''' = (<>) . (\salutation -> salutation <> " ")
makeGreeting''''' = (<>) . (<> " ") 