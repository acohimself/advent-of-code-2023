{-

   Andreas Christian Olsen
   aco@acohimself.com

   https://adventofcode.com/2023/day/2

-}


module Main exposing (main)

import Browser
import Char exposing (isDigit)
import Html exposing (Html, a, button, div, form, h2, i, input, label, text, textarea)
import Html.Attributes exposing (action, class, for, placeholder, required, rows, style, type_)
import Html.Events exposing (onClick, onInput)
import List exposing (head, map, sum, tail)
import Maybe exposing (Maybe, withDefault)
import Regex
import String exposing (dropLeft, fromList, lines, reverse, startsWith, toInt, uncons)



-- input is the input from
-- from https://adventofcode.com/2023/day/2/input


type alias Model =
    { input : String
    , part1Value : Int
    , part2Value : Int
    }


initialModel : Model
initialModel =
    { input = ""
    , part1Value = 0
    , part2Value = 0
    }


type Msg
    = InputFrequencyText String
    | Solve
    | LoadFromCache


type alias GameSet =
    { red : Int
    , green : Int
    , blue : Int
    }


type alias Game =
    { id : Int
    , sets : List GameSet
    }


parseSet : String -> GameSet
parseSet string =
    let
        colors =
            string
                |> String.split ","
                |> map String.words

        red =
            colors
                |> map
                    (\l ->
                        case l of
                            x :: "red" :: _ ->
                                Maybe.withDefault 0 (toInt x)

                            _ ->
                                0
                    )
                |> sum

        green =
            colors
                |> map
                    (\l ->
                        case l of
                            x :: "green" :: _ ->
                                Maybe.withDefault 0 (toInt x)

                            _ ->
                                0
                    )
                |> sum

        blue =
            colors
                |> map
                    (\l ->
                        case l of
                            x :: "blue" :: _ ->
                                Maybe.withDefault 0 (toInt x)

                            _ ->
                                0
                    )
                |> sum
    in
    GameSet red green blue


parseGame : String -> Game
parseGame string =
    let
        id =
            string
                |> dropLeft 5
                |> String.split ":"
                |> head
                |> Maybe.withDefault "0"
                |> toInt
                |> Maybe.withDefault 0

        sets =
            string
                |> String.split ":"
                |> tail
                |> Maybe.withDefault []
                |> head
                |> Maybe.withDefault ""
                |> String.split ";"
                |> map parseSet
    in
    Game id sets


gameValue : Int -> Int -> Int -> Game -> Int
gameValue red green blue game =
    let
        possible : Int -> Int -> Int -> GameSet -> Bool
        possible r g b set =
            case set.red > r of
                True ->
                    False

                False ->
                    case set.green > g of
                        True ->
                            False

                        False ->
                            case set.blue > b of
                                True ->
                                    False

                                False ->
                                    True
    in
    if
        game.sets
            |> map (possible red green blue)
            |> List.member False
    then
        0

    else
        game.id


fewestCubePowered : Game -> Int
fewestCubePowered game =
    let
        minimum red green blue gamesets =
            case gamesets of
                [] ->
                    red * green * blue

                set :: sets ->
                    minimum (max red set.red) (max green set.green) (max blue set.blue) sets
    in
    game.sets
        |> minimum 0 0 0


part1 : String -> Int
part1 input =
    let
        games =
            input
                |> lines
                |> map parseGame
    in
    games
        |> map (gameValue 12 13 14)
        |> sum


part2 : String -> Int
part2 input =
    let
        games =
            input
                |> lines
                |> map parseGame
    in
    games
        |> map fewestCubePowered
        |> sum


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputFrequencyText text ->
            { model | input = text }

        Solve ->
            { model
                | part1Value = part1 model.input
                , part2Value = part2 model.input
            }

        LoadFromCache ->
            { model | input = inputString }


view : Model -> Html Msg
view model =
    div []
        [ div [ class "container" ]
            [ div [ class "w-1/2 mx-auto" ]
                [ h2 [ class "text-xl" ] [ text "Day2: Cube Conundrum" ]
                ]
            , div [ class "flex space-x-8 justify-center" ]
                [ a
                    [ class "inline-block px-7 py-3 bg-blue-600 text-white font-medium text-sm leading-snug uppercase rounded shadow-md hover:bg-blue-700 hover:shadow-lg focus:bg-blue-700 focus:shadow-lg focus:outline-none focus:ring-0 active:bg-blue-800 active:shadow-lg transition duration-150 ease-in-out"
                    , onClick LoadFromCache
                    ]
                    [ text "Load data" ]
                , a
                    [ class "inline-block px-7 py-3 bg-blue-600 text-white font-medium text-sm leading-snug uppercase rounded shadow-md hover:bg-blue-700 hover:shadow-lg focus:bg-blue-700 focus:shadow-lg focus:outline-none focus:ring-0 active:bg-blue-800 active:shadow-lg transition duration-150 ease-in-out"
                    , onClick Solve
                    ]
                    [ text "Find solutions" ]
                ]
            , form [ action "#" ]
                [ div [ class "mdl-textfield mdl-js-textfield", style "padding" "16px" ]
                    [ textarea
                        [ class "form-control block w-full px-3 py-1.5 text-base font-normal text-gray-700 bg-white bg-clip-padding border border-solid border-gray-300 rounded transition ease-in-out m-0 focus:text-gray-700 focus:bg-white focus:border-blue-600 focus:outline-none"
                        , rows 3
                        , placeholder "Paste input text here"
                        , required True
                        , onInput InputFrequencyText
                        ]
                        [ text model.input ]
                    ]
                , div [ class "flex space-x-8 justify-center" ]
                    [ div [ class "textarea_label" ] [ text "Part1: " ]
                    , text <| String.fromInt model.part1Value
                    , div [ class "textarea_label" ] [ text "Part2: " ]
                    , text <| String.fromInt model.part2Value
                    ]
                ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


inputString : String
inputString =
    """Game 1: 2 blue, 3 red; 3 green, 3 blue, 6 red; 4 blue, 6 red; 2 green, 2 blue, 9 red; 2 red, 4 blue
Game 2: 4 red, 1 green; 3 red; 13 green, 5 red, 3 blue; 3 green, 2 red; 3 blue, 5 red, 3 green; 2 red, 3 blue, 12 green
Game 3: 4 red, 1 green, 1 blue; 1 red, 1 blue; 6 red, 1 green; 6 red, 3 blue, 1 green; 4 red
Game 4: 4 blue, 12 red, 4 green; 6 green, 3 blue, 19 red; 3 blue, 2 red, 2 green
Game 5: 1 red, 5 blue, 16 green; 1 red, 6 green, 3 blue; 2 red, 12 blue; 17 blue, 3 green; 7 green, 2 red, 6 blue
Game 6: 3 green, 1 blue, 5 red; 5 green, 5 red; 2 green, 2 blue, 3 red; 5 green, 2 red; 3 green, 6 red, 3 blue; 5 green, 4 red
Game 7: 15 blue, 1 red, 6 green; 4 blue, 7 green, 2 red; 14 blue, 5 green, 2 red
Game 8: 6 blue, 3 green, 10 red; 2 blue, 1 green, 5 red; 6 blue, 3 green, 12 red; 11 red, 1 green, 1 blue; 5 blue, 14 red, 3 green; 3 red
Game 9: 15 red, 3 blue; 1 blue, 16 red; 1 red, 3 blue; 1 blue, 1 green, 9 red
Game 10: 1 red; 1 blue, 7 green; 1 green, 5 blue; 3 blue, 3 green; 1 green
Game 11: 19 blue, 13 green; 19 blue, 2 green; 10 blue, 3 red, 12 green; 11 blue, 1 red, 6 green
Game 12: 7 green, 5 blue; 6 green, 3 red, 6 blue; 2 red, 5 blue, 15 green; 2 red, 1 blue, 1 green; 4 red, 4 green, 2 blue; 3 blue, 6 green
Game 13: 9 red, 2 blue, 2 green; 1 blue, 2 red, 15 green; 9 green, 2 blue, 9 red; 5 blue, 8 green, 5 red; 2 blue, 11 green, 5 red
Game 14: 9 blue, 1 red; 10 blue, 4 green, 3 red; 2 red, 6 blue; 4 green, 2 blue, 1 red; 5 green, 2 red, 11 blue; 12 blue, 2 red, 1 green
Game 15: 9 blue, 7 green, 12 red; 9 red, 17 green, 8 blue; 6 red, 4 blue, 4 green; 5 red, 17 green
Game 16: 5 green, 4 red; 3 blue, 3 red, 14 green; 6 red, 5 blue, 12 green
Game 17: 8 blue, 5 green, 2 red; 6 red, 6 blue; 9 red; 5 blue, 2 green, 8 red; 13 red, 4 blue, 4 green; 9 blue, 3 green, 5 red
Game 18: 8 green, 1 red, 2 blue; 4 green, 4 red, 1 blue; 6 blue, 2 red
Game 19: 3 green, 9 blue; 4 blue, 10 red; 6 red, 3 green, 3 blue; 6 red, 4 green, 9 blue
Game 20: 11 green, 3 blue; 6 green; 3 green, 6 blue; 1 red, 5 green; 6 blue, 7 green
Game 21: 1 green, 1 blue, 12 red; 6 red, 2 blue; 5 green, 4 red, 2 blue; 11 red, 8 green, 1 blue
Game 22: 10 red; 1 red, 13 green, 9 blue; 6 blue, 12 red, 12 green; 10 red, 8 blue, 11 green; 2 green, 1 red, 3 blue; 7 red, 1 blue, 8 green
Game 23: 11 red, 15 blue; 10 blue, 16 red, 1 green; 14 blue, 5 red; 1 green, 9 red, 9 blue; 1 red, 7 blue, 3 green; 6 red, 2 green, 3 blue
Game 24: 6 blue, 11 red; 16 green, 2 red, 1 blue; 8 red, 7 blue; 14 blue, 9 green, 9 red; 13 green, 4 red, 8 blue; 2 red, 7 blue, 1 green
Game 25: 2 green, 12 blue, 1 red; 10 blue, 5 red, 5 green; 2 blue, 9 red, 3 green; 5 blue, 4 red, 2 green
Game 26: 7 blue, 6 red, 1 green; 2 blue, 3 green, 12 red; 2 blue, 6 red, 5 green
Game 27: 2 green, 3 red; 4 green; 2 red, 1 blue, 1 green; 2 red, 1 green, 2 blue
Game 28: 11 blue, 1 red, 5 green; 2 blue, 2 red, 4 green; 10 blue, 4 red, 1 green
Game 29: 6 blue, 17 red, 1 green; 8 blue, 4 red; 14 blue, 1 red, 3 green
Game 30: 2 blue, 4 green; 7 green, 1 blue, 1 red; 1 blue, 8 green
Game 31: 15 blue, 9 green, 2 red; 5 green, 4 blue, 1 red; 1 green, 15 red, 7 blue; 5 red, 2 blue
Game 32: 1 blue, 5 red, 3 green; 3 green, 8 red, 1 blue; 5 green, 1 red; 4 green, 3 blue, 15 red; 2 green, 1 blue; 4 blue, 15 red, 4 green
Game 33: 3 red, 10 blue; 4 red, 9 blue; 1 green, 10 blue
Game 34: 3 blue, 1 green, 9 red; 4 green, 2 red, 9 blue; 7 blue, 3 red; 6 blue, 13 red; 4 green, 13 blue, 9 red
Game 35: 14 red, 1 green; 1 red, 2 green, 4 blue; 3 blue, 10 red, 6 green; 5 blue, 6 red, 7 green; 7 blue, 5 red
Game 36: 2 blue, 8 red, 9 green; 9 green, 3 red, 10 blue; 6 red, 8 blue, 1 green; 6 green, 8 red, 4 blue
Game 37: 10 green, 3 red, 6 blue; 2 blue, 9 red, 5 green; 13 green, 9 red, 10 blue; 2 blue, 4 green, 9 red
Game 38: 4 red, 14 blue, 12 green; 6 red, 12 green, 18 blue; 6 green, 1 blue, 1 red
Game 39: 5 red, 1 blue, 3 green; 1 blue, 3 green, 8 red; 15 red, 1 blue, 5 green; 3 green, 5 red; 1 blue, 14 red; 3 green, 1 blue, 12 red
Game 40: 8 green, 4 blue; 5 blue, 7 red, 8 green; 5 blue, 8 green; 6 green, 3 red, 12 blue; 14 blue, 7 green, 2 red; 1 green, 7 red, 5 blue
Game 41: 7 red, 10 green; 10 red, 6 green; 9 red, 7 green, 1 blue; 3 red, 1 blue
Game 42: 3 green, 2 blue, 13 red; 1 blue, 3 red; 11 green, 16 red; 3 green, 1 blue, 16 red; 5 red, 8 green
Game 43: 12 blue, 9 red; 16 blue, 2 red, 7 green; 4 red, 1 blue, 11 green; 15 blue, 4 red, 9 green
Game 44: 17 green, 5 blue, 2 red; 9 green, 11 blue, 1 red; 20 green, 3 blue, 8 red; 2 red, 13 green, 9 blue; 15 green, 12 blue; 4 blue, 7 green, 9 red
Game 45: 5 green; 5 green, 1 red; 3 green, 2 blue; 1 green, 1 blue, 1 red
Game 46: 10 red, 11 green; 16 green, 8 blue, 12 red; 9 green, 9 blue
Game 47: 20 green, 17 red, 1 blue; 16 red, 2 blue, 11 green; 3 blue, 19 red, 1 green; 3 blue, 17 red, 17 green; 12 green, 2 blue, 7 red
Game 48: 1 red, 4 blue, 6 green; 19 green, 1 red, 1 blue; 16 green, 3 blue, 1 red; 3 blue, 17 green; 4 blue, 12 green
Game 49: 13 green, 2 blue, 1 red; 1 green, 8 red, 2 blue; 11 red, 11 green, 3 blue; 7 red, 8 green, 4 blue
Game 50: 11 blue, 1 red, 2 green; 1 green, 10 blue; 1 blue; 6 blue; 1 green, 2 blue
Game 51: 3 red, 3 green, 1 blue; 3 green, 3 red; 10 green, 4 red; 3 red, 2 green
Game 52: 1 red, 4 blue; 1 green, 11 blue; 1 green, 3 red, 6 blue; 4 red, 1 green, 4 blue; 9 blue, 1 green; 10 blue, 1 green
Game 53: 2 blue, 4 green, 1 red; 8 blue, 4 red, 7 green; 9 red, 7 blue, 6 green; 3 red, 7 green, 1 blue; 2 red, 9 blue, 5 green; 1 green, 7 red, 10 blue
Game 54: 1 red, 1 blue, 5 green; 2 red, 1 green, 2 blue; 3 green, 3 blue, 2 red; 4 red; 12 red, 5 green, 2 blue
Game 55: 2 red, 11 blue; 16 green, 7 red, 16 blue; 4 blue, 11 green, 7 red; 8 green, 18 blue, 8 red
Game 56: 2 blue, 2 green, 1 red; 1 red, 1 green; 1 red; 4 green; 1 blue; 1 blue, 7 green
Game 57: 4 blue, 3 green; 16 green, 2 red, 5 blue; 1 red, 13 green, 2 blue; 3 blue, 12 green, 2 red; 2 red, 5 blue, 4 green; 10 green, 2 blue
Game 58: 3 blue, 8 green; 4 green, 3 blue; 7 green, 5 blue, 5 red; 8 green; 3 red, 6 blue, 9 green; 2 red, 10 green, 4 blue
Game 59: 7 blue, 6 green, 5 red; 7 red, 2 blue; 5 red, 11 green, 14 blue; 8 green, 17 red
Game 60: 3 green, 8 blue, 2 red; 4 green, 7 blue, 6 red; 13 blue, 8 green, 2 red; 10 red, 6 blue, 5 green; 11 green, 3 blue, 4 red; 9 red, 5 green, 9 blue
Game 61: 4 red, 18 blue, 13 green; 9 green, 5 red, 3 blue; 4 green, 3 blue, 4 red; 8 red, 4 green, 7 blue; 8 red, 4 blue, 6 green; 10 green, 5 red, 14 blue
Game 62: 12 red, 14 blue, 9 green; 9 blue, 6 red, 4 green; 2 red, 5 blue; 1 red, 12 blue
Game 63: 11 blue, 13 red, 11 green; 4 blue, 9 green; 8 blue, 9 red; 7 red, 11 green, 7 blue
Game 64: 10 blue, 8 red, 12 green; 10 red, 12 blue, 9 green; 3 green, 17 red; 12 green, 15 blue, 16 red; 6 green, 15 blue, 1 red; 9 red, 6 blue, 10 green
Game 65: 7 red, 7 blue; 3 blue, 1 red, 1 green; 3 red, 8 blue
Game 66: 1 blue, 3 red; 10 green, 5 blue; 4 green; 3 red, 11 green; 3 blue, 15 green, 3 red
Game 67: 1 red; 2 blue, 2 green, 1 red; 6 green, 1 blue
Game 68: 7 red, 4 blue; 4 blue, 6 red, 7 green; 2 green, 19 red, 11 blue; 11 green, 9 red
Game 69: 4 blue, 3 green, 1 red; 7 blue, 1 red, 3 green; 5 blue, 1 green; 2 blue, 10 green, 2 red; 2 red, 6 green, 5 blue; 1 red, 4 green, 2 blue
Game 70: 9 blue, 7 red, 6 green; 19 blue, 4 red, 5 green; 6 blue, 7 red, 4 green; 3 blue, 4 red, 2 green
Game 71: 6 green, 12 blue, 4 red; 11 red, 10 green, 11 blue; 3 red, 14 blue, 13 green; 4 blue, 3 green
Game 72: 2 green, 1 blue, 9 red; 10 red, 3 green, 1 blue; 11 red, 2 green; 2 green, 1 blue, 5 red; 1 red, 1 blue, 3 green; 13 red, 4 blue, 1 green
Game 73: 11 green, 6 blue; 7 green, 6 blue, 7 red; 12 green, 8 blue, 11 red; 4 red, 2 blue, 9 green; 4 green, 7 blue, 2 red
Game 74: 3 blue, 7 red; 3 blue, 5 green, 2 red; 5 red, 1 green, 3 blue; 8 green, 2 blue, 11 red; 3 blue, 8 green, 10 red
Game 75: 2 green; 5 blue; 1 blue, 1 red; 1 red, 9 blue, 2 green; 2 blue, 2 green
Game 76: 12 blue, 13 green; 5 red, 11 blue, 9 green; 12 green, 6 red
Game 77: 1 blue, 15 green, 12 red; 15 green, 5 blue; 14 green, 3 blue, 8 red
Game 78: 11 green, 8 blue, 1 red; 9 green, 8 blue, 1 red; 13 green, 5 red, 6 blue; 5 red, 7 green, 20 blue; 10 blue, 5 red
Game 79: 3 blue; 6 blue, 5 red; 4 red, 1 green, 4 blue; 7 blue, 6 red; 7 red, 1 blue; 1 red, 1 blue, 1 green
Game 80: 11 green, 3 red, 8 blue; 2 red, 15 green, 2 blue; 5 green, 8 blue, 2 red; 8 blue, 14 green; 2 blue, 13 green
Game 81: 9 red, 4 green; 7 green, 4 red; 2 red, 4 blue, 6 green; 6 red, 4 blue, 9 green; 1 green, 3 red; 6 green, 1 blue, 8 red
Game 82: 5 blue, 3 red, 3 green; 5 red; 2 red, 3 green, 8 blue
Game 83: 10 green, 1 red, 1 blue; 3 red, 1 green, 1 blue; 4 red, 10 green
Game 84: 16 red, 2 green, 6 blue; 6 red, 3 green, 8 blue; 3 green, 10 red, 5 blue; 4 blue, 3 green; 15 red
Game 85: 3 green, 2 red; 5 green, 4 blue; 5 green, 8 red, 3 blue
Game 86: 7 green, 16 blue, 7 red; 1 green, 12 red, 2 blue; 15 green, 16 blue, 7 red
Game 87: 1 red, 6 green, 5 blue; 2 green, 1 blue; 2 green, 1 red, 1 blue; 5 green, 4 blue
Game 88: 3 green, 3 red, 4 blue; 1 red, 1 green; 6 blue, 9 red, 1 green; 1 green, 11 red, 3 blue; 7 red, 6 blue
Game 89: 2 blue, 3 red, 4 green; 5 red, 7 blue, 14 green; 8 blue, 5 red, 16 green; 2 blue, 5 red, 7 green; 5 green, 9 blue, 1 red
Game 90: 1 blue, 3 red, 7 green; 11 green, 4 red, 1 blue; 1 red, 1 blue, 6 green; 2 blue, 2 green; 8 green, 2 blue; 3 red, 2 blue, 4 green
Game 91: 6 blue, 4 red, 1 green; 8 red, 3 blue, 3 green; 1 green, 2 blue, 5 red; 1 blue, 3 green
Game 92: 8 green, 1 red, 5 blue; 2 green, 7 blue; 11 blue, 5 green, 8 red; 7 blue, 3 red, 4 green
Game 93: 3 green, 1 red, 9 blue; 13 red, 5 blue, 8 green; 5 green, 2 red, 7 blue
Game 94: 4 green, 10 blue, 8 red; 4 red, 10 blue, 2 green; 2 green, 10 blue, 5 red; 5 green, 2 red, 10 blue
Game 95: 5 green, 1 blue; 3 blue, 11 green, 8 red; 8 blue, 2 red, 12 green; 4 green, 4 blue, 4 red
Game 96: 1 blue, 13 green; 8 blue, 3 red, 4 green; 1 red, 3 blue, 10 green
Game 97: 18 green, 4 red; 1 blue, 2 red, 9 green; 6 red, 3 blue, 10 green; 3 blue, 15 green, 4 red
Game 98: 2 blue, 3 green, 6 red; 1 green, 1 blue, 8 red; 8 red, 3 green, 1 blue; 2 blue; 8 red, 2 green, 2 blue
Game 99: 1 green, 2 red, 1 blue; 8 green, 4 blue, 1 red; 7 blue, 1 red, 11 green; 9 green, 3 blue; 1 red, 2 blue; 1 red, 6 blue
Game 100: 7 blue, 9 green, 2 red; 5 red, 9 green; 1 blue, 8 red, 13 green"""
