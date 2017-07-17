module Game.Helpers exposing (..)

setIfInRange: Int -> Int -> Int -> Int -> Int
setIfInRange newVal currentValue minValue maxValue =
    if minValue <= newVal && newVal <= maxValue then
        newVal
    else
        currentValue

toIntOrDefault default string =
    Result.withDefault default (String.toInt string)

seq: Int -> List Int
seq max =
    List.range 0 (max-1)

counter : List a -> (a -> Bool) -> Int
counter items fn =
    List.foldr (\item accum -> if (fn item) then accum + 1 else accum) 0 items

