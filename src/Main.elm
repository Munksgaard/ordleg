port module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events
import Element exposing (Attribute, Element, centerX, centerY, column, el, height, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Json.Decode as Json
import Json.Encode as Encode
import List.Extra as List
import Random
import Task
import Time
import WordList exposing (wordList)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = Element.layout [] << view
        }



-- MODEL


type alias Model =
    { error : Maybe String
    , currentGuess : String
    , dayAndSolution : Maybe ( Int, String )
    , guesses : List String
    }


emptyModel : Model
emptyModel =
    Model Nothing "" Nothing []


init : Json.Value -> ( Model, Cmd Msg )
init json =
    ( emptyModel
    , Time.now
        |> Task.map (initialize json)
        |> Task.perform SetModel
    )


initialize : Json.Value -> Time.Posix -> Model
initialize json time =
    let
        day =
            ordlegDay time
    in
    case Json.decodeValue decodeModel json of
        Ok encodedModel ->
            if
                Maybe.map ((==) day << Tuple.first) encodedModel.dayAndSolution
                    |> Maybe.withDefault False
            then
                encodedModel

            else
                { emptyModel | dayAndSolution = Just <| getSolution day }

        Err _ ->
            { emptyModel | dayAndSolution = Just <| getSolution day }


getSolution : Int -> ( Int, String )
getSolution day =
    hashDay (Array.length wordList - 1) day
        |> modBy (Array.length wordList)
        |> flip Array.get wordList
        |> Maybe.withDefault "SUPPE"
        |> Tuple.pair day


ordlegDay : Time.Posix -> Int
ordlegDay d =
    Time.posixToMillis d
        // (1000 * 60 * 60 * 24)
        -- Days from 1970-01-01 to 2022-01-17
        - 19006


hashDay : Int -> Int -> Int
hashDay n d =
    Random.step (Random.int 0 n) (Random.initialSeed d)
        |> Tuple.first



-- UPDATE


type Msg
    = KeyDown Key
    | SetModel Model
    | StorageChanged Json.Value
    | Share


type Key
    = Character Char
    | Control String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetModel new_model ->
            ( new_model
            , Cmd.none
            )

        KeyDown (Character c0) ->
            let
                c =
                    Char.toUpper c0
            in
            ( { model
                | currentGuess =
                    if not (solutionFound model) && isAlphaÆØÅ c then
                        String.left 5 <|
                            model.currentGuess
                                ++ String.fromChar c

                    else
                        model.currentGuess
                , error = Nothing
              }
            , Cmd.none
            )

        KeyDown (Control "Backspace") ->
            ( { model
                | currentGuess =
                    if not (solutionFound model) then
                        String.dropRight 1 model.currentGuess

                    else
                        model.currentGuess
                , error = Nothing
              }
            , Cmd.none
            )

        KeyDown (Control "Enter") ->
            if not (solutionFound model) && List.length model.guesses < 6 then
                if String.length model.currentGuess == 5 then
                    if binSearch wordList model.currentGuess then
                        let
                            new_model =
                                { model
                                    | currentGuess = ""
                                    , guesses = model.currentGuess :: model.guesses
                                    , error = Nothing
                                }
                        in
                        ( new_model, setStorage <| encodeModel new_model )

                    else
                        ( { model | error = Just "Ordet findes ikke i ordbogen." }
                        , Cmd.none
                        )

                else
                    ( { model | error = Just "Ordet skal være fem bogstaver langt." }
                    , Cmd.none
                    )

            else
                ( model, Cmd.none )

        KeyDown (Control _) ->
            ( { model | error = Nothing }
            , Cmd.none
            )

        StorageChanged json ->
            ( case Json.decodeValue decodeModel json of
                Ok new_model ->
                    new_model

                Err _ ->
                    model
            , Cmd.none
            )

        Share ->
            ( { model | error = Just "Kopieret til udklipsholderen." }
            , share model
            )


solutionFound : Model -> Bool
solutionFound model =
    List.head model.guesses == Maybe.map Tuple.second model.dayAndSolution


share : Model -> Cmd msg
share model =
    case model.dayAndSolution of
        Just ( day, solution ) ->
            let
                guessNumber =
                    if solutionFound model then
                        String.fromInt <| List.length model.guesses

                    else
                        "x"
            in
            "Ordleg #"
                ++ String.fromInt day
                ++ " "
                ++ guessNumber
                ++ "/6"
                ++ "\n"
                ++ String.join "\n"
                    (List.reverse model.guesses
                        |> List.map
                            (charColors solution
                                >> List.map (Tuple.first >> feedbackUnicode)
                                >> String.fromList
                            )
                    )
                |> setClipboard

        Nothing ->
            Cmd.none



-- VIEW


view : Model -> Element Msg
view model =
    case model.dayAndSolution of
        Nothing ->
            Element.none

        Just ( _, solution ) ->
            column
                [ padding 10
                , centerX
                , height Element.fill
                , spacing 20
                , width Element.fill
                ]
                [ el [ centerX, Font.size 30 ] (text "Velkommen til Ordleg!")
                , (List.map (keysCompareDiv solution) (List.reverse model.guesses)
                    ++ [ guessBoxes model ]
                    ++ List.repeat 6 emptyRow
                  )
                    |> List.take 6
                    |> column
                        [ spacing 10
                        , centerX
                        , centerY
                        , padding 15
                        , Element.below <| errorMessage model
                        , Element.above <| endMessage model
                        ]
                , tastatur model
                ]


boxRow : List (Element msg) -> Element msg
boxRow =
    row [ spacing 10 ]


emptyRow : Element msg
emptyRow =
    List.repeat 5 emptyBox |> boxRow


box : List (Attribute msg) -> Element msg -> Element msg
box attrs =
    el (attrs ++ [ width <| Element.px 50, height <| Element.px 50 ])


emptyBox : Element msg
emptyBox =
    box [ Border.color gray, Border.width 1 ] Element.none


guessBoxes : Model -> Element msg
guessBoxes model =
    List.map
        (box [ Border.color black, Border.width 1 ]
            << el [ centerX, centerY ]
            << text
            << String.fromChar
        )
        (String.toList model.currentGuess)
        ++ List.repeat 5 emptyBox
        |> List.take 5
        |> boxRow


black : Element.Color
black =
    rgb255 0 0 0


gray : Element.Color
gray =
    rgb255 180 180 180


lightGray : Element.Color
lightGray =
    rgb255 220 220 220


green : Element.Color
green =
    rgb255 0 255 0


yellow : Element.Color
yellow =
    rgb255 255 255 0


type Feedback
    = Gray
    | Yellow
    | Green


feedbackColor : Feedback -> Element.Color
feedbackColor f =
    case f of
        Gray ->
            gray

        Yellow ->
            yellow

        Green ->
            green


feedbackUnicode : Feedback -> Char
feedbackUnicode f =
    case f of
        Gray ->
            '⬜'

        Yellow ->
            '🟨'

        Green ->
            '🟩'


charColors : String -> String -> List ( Feedback, Char )
charColors solution guess =
    let
        ( correctLetters, correctLetterIndices ) =
            List.map2 Tuple.pair (String.toList guess) (String.toList solution)
                |> List.indexedMap
                    (\i ( x, y ) ->
                        if x == y then
                            Just ( x, i )

                        else
                            Nothing
                    )
                |> List.filterMap identity
                |> List.unzip

        leftoverLetters =
            List.foldl List.remove (String.toList solution) correctLetters
    in
    String.toList guess
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( idx, letter ) ( lettersLeft0, acc ) ->
                let
                    ( color, lettersLeft ) =
                        if List.member idx correctLetterIndices then
                            ( Green, lettersLeft0 )

                        else if List.member letter lettersLeft0 then
                            ( Yellow, List.remove letter lettersLeft0 )

                        else
                            ( Gray, lettersLeft0 )
                in
                ( lettersLeft, ( color, letter ) :: acc )
            )
            ( leftoverLetters, [] )
        |> Tuple.second
        |> List.reverse


keysCompareDiv : String -> String -> Element msg
keysCompareDiv solution guess =
    charColors solution guess
        |> List.map (uncurry keyCompareDiv)
        |> row [ spacing 10 ]


keyCompareDiv : Feedback -> Char -> Element msg
keyCompareDiv feedback c0 =
    let
        c =
            String.fromChar c0
    in
    text c
        |> el [ centerX, centerY ]
        |> box [ Background.color <| feedbackColor feedback ]


tastaturList : List (List ( String, Key ))
tastaturList =
    [ [ ( "Q", Character 'Q' ), ( "W", Character 'W' ), ( "E", Character 'E' ), ( "R", Character 'R' ), ( "T", Character 'T' ), ( "Y", Character 'Y' ), ( "U", Character 'U' ), ( "I", Character 'I' ), ( "O", Character 'O' ), ( "P", Character 'P' ), ( "Å", Character 'Å' ) ]
    , [ ( "A", Character 'A' ), ( "S", Character 'S' ), ( "D", Character 'D' ), ( "F", Character 'F' ), ( "G", Character 'G' ), ( "H", Character 'H' ), ( "J", Character 'J' ), ( "K", Character 'K' ), ( "L", Character 'L' ), ( "Æ", Character 'Æ' ), ( "Ø", Character 'Ø' ) ]
    , [ ( "Enter", Control "Enter" ), ( "Z", Character 'Z' ), ( "X", Character 'X' ), ( "C", Character 'C' ), ( "V", Character 'V' ), ( "B", Character 'B' ), ( "N", Character 'N' ), ( "M", Character 'M' ), ( "⌫", Control "Backspace" ) ]
    ]


tastatur : Model -> Element Msg
tastatur model =
    let
        letterColor : String -> Element.Color
        letterColor c =
            if
                List.any
                    (\guess ->
                        List.map2 Tuple.pair
                            (List.map String.fromChar <| String.toList guess)
                            (Maybe.map Tuple.second model.dayAndSolution
                                |> Maybe.withDefault ""
                                |> String.toList
                                |> List.map String.fromChar
                            )
                            |> List.filter (uncurry (==))
                            |> List.map Tuple.first
                            |> List.any ((==) c)
                    )
                    model.guesses
            then
                green

            else if
                String.concat model.guesses
                    |> String.toList
                    |> List.map String.fromChar
                    |> List.any ((==) c)
            then
                if
                    Maybe.map Tuple.second model.dayAndSolution
                        |> Maybe.withDefault ""
                        |> String.contains c
                then
                    yellow

                else
                    gray

            else
                lightGray
    in
    tastaturList
        |> List.map
            (Element.row
                [ spacing 5
                , centerX
                ]
                << List.map
                    (\( c, k ) ->
                        Input.button
                            [ Background.color <| letterColor c
                            , keyToWidth k
                            , Font.size 16
                            , height <| Element.px 40
                            , Element.focused []
                            , Font.center
                            ]
                            { onPress = Just <| KeyDown k
                            , label = text c
                            }
                    )
            )
        |> Element.column
            [ spacing 5
            , Element.alignBottom
            , centerX
            ]


keyToWidth : Key -> Attribute msg
keyToWidth key =
    case key of
        Character _ ->
            width <| Element.px 27

        Control _ ->
            width <| Element.px 50


errorMessage : Model -> Element msg
errorMessage model =
    case model.error of
        Just error ->
            el [ centerX ] <| text error

        Nothing ->
            Element.none


endMessage : Model -> Element Msg
endMessage model =
    if solutionFound model then
        Element.paragraph [ Font.center ]
            [ text "Godt klaret! "
            , Input.button [ Font.underline ]
                { onPress = Just <| Share
                , label = text "Del"
                }
            ]

    else
        case ( model.dayAndSolution, List.length model.guesses >= 6 ) of
            ( Just ( _, solution ), True ) ->
                Element.paragraph [ Font.center ] <| [ text "Desværre... Ordet var ", el [ Font.bold ] (text solution) ]

            _ ->
                Element.none



-- SUBSCRIPTIONS


keyDecoder : Json.Decoder Key
keyDecoder =
    Json.map toKey (Json.field "key" Json.string)


toKey : String -> Key
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string


subscriptions : model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
            |> Sub.map KeyDown
        , storageChanged StorageChanged
        ]



-- PORTS


port setStorage : Json.Value -> Cmd msg


port storageChanged : (Json.Value -> msg) -> Sub msg


port setClipboard : String -> Cmd msg


encodeModel : Model -> Json.Value
encodeModel model =
    case model.dayAndSolution of
        Just ( day, _ ) ->
            Encode.object
                [ ( "guesses", Encode.list Encode.string model.guesses )
                , ( "day", Encode.int day )
                ]

        Nothing ->
            Encode.null


decodeModel : Json.Decoder Model
decodeModel =
    Json.map2 (Model Nothing "" << (Just << getSolution))
        (Json.field "day" Json.int)
        (Json.field "guesses" (Json.list Json.string))



-- UTILS


flip : (a -> b -> c) -> b -> a -> c
flip f x y =
    f y x


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( x, y ) =
    f x y


isAlphaÆØÅ : Char -> Bool
isAlphaÆØÅ c =
    Char.isAlpha c || c == 'Æ' || c == 'Ø' || c == 'Å'


binSearch : Array comparable -> comparable -> Bool
binSearch arr x =
    let
        binSearchHelper lower upper =
            let
                half =
                    (upper - lower) // 2 + lower
            in
            if half == lower then
                Array.get lower arr == Just x

            else
                case Maybe.map (compare x) <| Array.get half arr of
                    Just LT ->
                        binSearchHelper lower half

                    Just EQ ->
                        True

                    Just GT ->
                        binSearchHelper half upper

                    Nothing ->
                        False
    in
    binSearchHelper 0 <| Array.length arr


identity : x -> x
identity x =
    x
