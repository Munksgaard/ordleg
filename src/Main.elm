module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events
import Element exposing (Attribute, Element, centerX, centerY, column, el, height, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Json.Decode as Json
import Task
import Time



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
    { guesses : List String
    , currentGuess : String
    , solution : Maybe String
    , error : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] "" Nothing Nothing
    , Time.now
        |> Task.map
            (hashDay
                >> modBy (Array.length wordList)
                >> flip Array.get wordList
                >> Maybe.withDefault "SUPPE"
            )
        |> Task.perform SolutionIs
    )


hashDay : Time.Posix -> Int
hashDay t =
    Time.posixToMillis t // (1000 * 60 * 60 * 24)



-- UPDATE


type Msg
    = KeyDown Key
    | SolutionIs String


type Key
    = Character Char
    | Control String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SolutionIs solution ->
            ( { model | solution = Just solution }
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
            ( if not (solutionFound model) then
                if String.length model.currentGuess == 5 then
                    if binSearch wordList model.currentGuess then
                        { model
                            | currentGuess = ""
                            , guesses = model.currentGuess :: model.guesses
                            , error = Nothing
                        }

                    else
                        { model | error = Just "Ordet findes ikke i ordbogen." }

                else
                    { model | error = Just "Ordet skal være fem bogstaver langt." }

              else
                model
            , Cmd.none
            )

        KeyDown (Control _) ->
            ( { model | error = Nothing }
            , Cmd.none
            )


solutionFound : Model -> Bool
solutionFound model =
    List.head model.guesses == model.solution



-- VIEW


view : Model -> Element Msg
view model =
    case model.solution of
        Nothing ->
            Element.none

        Just solution ->
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
                    |> column [ spacing 10, centerX, centerY ]
                , errorMessage model
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


keysCompareDiv : String -> String -> Element msg
keysCompareDiv solution guess =
    row [ spacing 10 ] (List.indexedMap (keyCompareDiv solution) <| String.toList guess)


keyCompareDiv : String -> Int -> Char -> Element msg
keyCompareDiv s n c0 =
    let
        c =
            String.fromChar c0

        color =
            if String.slice n (n + 1) s == c then
                green

            else if String.contains c s then
                yellow

            else
                gray
    in
    box [ Background.color color ] <| el [ centerX, centerY ] <| text c


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
                            (Maybe.withDefault "" model.solution
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
                if String.contains c <| Maybe.withDefault "" model.solution then
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
                        el
                            [ Events.onClick (KeyDown k)
                            , Background.color <| letterColor c
                            , keyToWidth k
                            , Font.size 16
                            , height <| Element.px 40
                            ]
                            (el
                                [ centerY
                                , centerX
                                ]
                                (text c)
                            )
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
    Browser.Events.onKeyDown keyDecoder
        |> Sub.map KeyDown



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


wordList : Array String
wordList =
    Array.fromList <| List.sort [ "ABBED", "ÅBNER", "ABORT", "ACCES", "ADLED", "ADRÆT", "ÅDSEL", "ADVIS", "ÆGIDE", "ÆKLES", "ÆLDRE", "ÆLDST", "ÆNDER", "ÆNDRE", "ÆRBAR", "ÆRGRE", "ÆRLIG", "AEROB", "ÆRØBO", "ÆRØSK", "ÆSTET", "ÆVRED", "AFART", "AFASI", "AFBUD", "AFDØD", "AFGUD", "AFISE", "AFKOG", "AFKOM", "AFLAD", "AFLØB", "AFLYD", "AFSÆT", "AFSKY", "AFSLÅ", "AFSTÅ", "AFTAG", "AFTEN", "AFTES", "AFVEJ", "AGAPE", "AGAVE", "AGENS", "AGENT", "AGERE", "AGERN", "AGNAT", "AGONI", "AGORA", "AGRAR", "AGTEN", "AGTER", "AGURK", "AHORN", "AJOUR", "AKRYL", "AKSEL", "AKTIE", "AKTIV", "AKTOR", "AKTØR", "ALARM", "ALBUE", "ALBUM", "ALDER", "ALENE", "ALERT", "ALGOD", "ALHUS", "ALIAS", "ALIBI", "ALIEN", "ALKYD", "ALMEN", "ALMUE", "ALPIN", "ALRUM", "ALTAN", "ALTER", "ALTID", "ALTSÅ", "ALVOR", "AMBRA", "AMISH", "AMØBE", "AMORF", "AMPEL", "AMPUL", "ANÆMI", "ANDEL", "ANDEN", "ANDET", "ÅNDET", "ÅNDIG", "ANDRE", "ANGER", "ANGLE", "ANGRE", "ANGST", "ANIMA", "ANION", "ANJON", "ANKEL", "ANKER", "ANKRE", "ANLÆG", "ANLØB", "ANNAL", "ANNUA", "ANODE", "ANRÅB", "ANSAT", "ANSET", "ANSLÅ", "ANSTÅ", "ANTAL", "ANTIK", "AORTA", "APATI", "APPEL", "APRIL", "APSIS", "ÅRBOG", "AREAL", "ARENA", "ARGON", "ARGOT", "ARIER", "ÅRING", "ARISK", "ARKIV", "ÅRLIG", "ARMOD", "AROMA", "ARRAK", "ARRET", "ARRIG", "ÅRSAG", "ARSEN", "ARSIS", "ARTIG", "ARVÆV", "ASIAT", "ASIET", "ASKET", "ASPIC", "ÅSTED", "ASTMA", "ATLAS", "ATLET", "ATONI", "ATTAK", "ATTEN", "ATTER", "ATTRÅ", "AUDIO", "AUDIT", "AUGUR", "AUTOR", "AVERS", "AVIND", "AVLER", "BACON", "BADGE", "BADUT", "BÆGER", "BÆLGE", "BÆLLE", "BÆLTE", "BÆNKE", "BÆRER", "BÆRME", "BÆVEN", "BÆVER", "BÆVRE", "BAGBO", "BAGEL", "BAGER", "BAGOM", "BAGPÅ", "BAGUD", "BAJER", "BAKKE", "BAKSE", "BALDE", "BALJE", "BALLE", "BALSA", "BAMLE", "BAMSE", "BANAL", "BANAN", "BANDE", "BÅNDE", "BANDT", "BANDY", "BANGE", "BANJE", "BANJO", "BANKE", "BANKO", "BANTU", "BAPPE", "BARAK", "BARBE", "BARDE", "BARET", "BÅRET", "BAROK", "BARON", "BARRE", "BARSK", "BARYL", "BARYT", "BASAL", "BASAR", "BASIS", "BASKE", "BASSE", "BASTA", "BASTE", "BASUN", "BATAT", "BATCH", "BATIK", "BATTE", "BÅTTE", "BATTS", "BAVLE", "BEÆRE", "BEBOP", "BEDRE", "BEDST", "BEFOR", "BEFRI", "BEGÆR", "BEGAV", "BEGGE", "BEGIK", "BEGLO", "BEHAG", "BEHOV", "BEIGE", "BEJAE", "BEJLE", "BELÆG", "BELØB", "BENET", "BERET", "BERYL", "BESAD", "BESAT", "BESLÅ", "BESØG", "BESTÅ", "BESYV", "BETEL", "BETLE", "BETØD", "BETOG", "BETON", "BETRO", "BEVIS", "BIAVL", "BIBEL", "BIDET", "BIDSK", "BIFAG", "BIFIL", "BIGOT", "BIJOB", "BIKER", "BIKSE", "BILAG", "BILDE", "BILED", "BILLE", "BILOS", "BILYD", "BIMBO", "BIMLE", "BINÆR", "BINDE", "BINGO", "BIORD", "BIPPE", "BISAM", "BISAT", "BISOL", "BISON", "BISSE", "BISTÅ", "BITCH", "BITTE", "BIVEJ", "BIZAR", "BJERG", "BJESK", "BJØRN", "BLADE", "BLÆRE", "BLÆSE", "BLÆST", "BLÅNE", "BLANK", "BLASE", "BLEGE", "BLIDE", "BLIND", "BLINI", "BLINK", "BLIST", "BLITZ", "BLIVE", "BLØDE", "BLOND", "BLOTE", "BLUES", "BLUFF", "BLUND", "BLUSE", "BOBBE", "BOBLE", "BØFFE", "BØGER", "BOGEY", "BOGIE", "BOJAN", "BØJLE", "BOKSE", "BØLGE", "BOLIG", "BOLLE", "BØLLE", "BOLTE", "BOMBE", "BOMME", "BONDE", "BØNNE", "BONUS", "BOOKE", "BOOME", "BOOST", "BOOTE", "BOPÆL", "BORDE", "BORGE", "BORNE", "BORST", "BØRST", "BORTE", "BOSAT", "BOSSE", "BØSSE", "BØTTE", "BOULE", "BØVET", "BØVLE", "BØVSE", "BOWLE", "BOXER", "BRÆGE", "BRAGE", "BRAGT", "BRAND", "BRASE", "BRASK", "BRAST", "BRAVO", "BREAK", "BREDE", "BRIKS", "BRINK", "BRINT", "BRISE", "BRIST", "BRITE", "BRØDE", "BRØLE", "BRØND", "BRØST", "BRUDT", "BRUGE", "BRUGS", "BRUNE", "BRUSE", "BRUSK", "BRYDE", "BRYSK", "BRYST", "BUGGY", "BUGNE", "BUGTE", "BUHKO", "BUKET", "BUKKE", "BUKLE", "BULET", "BULLE", "BULNE", "BUMLE", "BUMPE", "BUMSE", "BUNDE", "BUNDT", "BUNKE", "BURDE", "BURKA", "BURRE", "BUSKE", "BUSTE", "BUTAN", "BUTIK", "BYBUD", "BYBUS", "BYDEL", "BYGAS", "BYGET", "BYGGE", "BYLIV", "BYLTE", "BYNÆR", "BYNKE", "BYRÅD", "BYRDE", "BYRET", "BYRUM", "BYSTE", "BYTTE", "BYTUR", "CÆSAR", "CÆSUR", "CAJUN", "CASTE", "CEDER", "CELLE", "CELLO", "CERUT", "CHARM", "CHECK", "CHILI", "CHIPS", "CIDER", "CIGAR", "CIRKA", "CITAR", "CITAT", "CITER", "CIVIL", "CLIPS", "COACH", "COATE", "CONGA", "CORNY", "COVER", "CRACK", "CRAWL", "CRAZY", "CREDO", "CREME", "CREPE", "CROSS", "CURIE", "CUTTE", "CYKEL", "CYKLE", "CYSTE", "DADEL", "DADLE", "DÅDYR", "DÆGGE", "DÆKKE", "DÆLEN", "DÆMME", "DÆMON", "DÆMPE", "DÆMRE", "DÆNGE", "DAGES", "DÅLAM", "DALBO", "DALER", "DALRE", "DAMET", "DAMPE", "DANBO", "DANDY", "DANER", "DANNE", "DANSE", "DANSK", "DASKE", "DATID", "DATIV", "DATJA", "DAVID", "DAVRE", "DEALE", "DEBAT", "DEBET", "DEBIL", "DEBUT", "DECIM", "DEIST", "DEJSE", "DEKAN", "DEKOR", "DELLE", "DELTA", "DENAR", "DENIM", "DENNE", "DENTE", "DEPOT", "DERAF", "DERBY", "DERES", "DEROM", "DEROP", "DERPÅ", "DERUD", "DESTO", "DETTE", "DEVON", "DEVOT", "DIGEL", "DIGER", "DIGTE", "DIKKE", "DILDO", "DILLE", "DINAR", "DINER", "DINGO", "DIODE", "DIPOL", "DIRKE", "DIRRE", "DISET", "DISKE", "DISKO", "DISSE", "DITTO", "DIVAN", "DJÆRV", "DØBER", "DOBLE", "DØDIS", "DOGME", "DOKKE", "DOLCE", "DØLGE", "DOLKE", "DØLLE", "DOLME", "DOMME", "DØMME", "DONNA", "DØNNE", "DONOR", "DONUT", "DORER", "DORGE", "DORSK", "DØSIG", "DOSIS", "DØTRE", "DOUCE", "DOULA", "DOVEN", "DOVNE", "DOYEN", "DOZER", "DRÅBE", "DRÆBE", "DRÆNE", "DRÆVE", "DRAGE", "DRAGT", "DRAMA", "DRAPA", "DREJE", "DREJL", "DRENG", "DRESS", "DRIFT", "DRINK", "DRIVE", "DROGE", "DRØJE", "DRONE", "DRØNE", "DROPS", "DRUDE", "DUBBE", "DUBLE", "DUFTE", "DUGGE", "DUKAT", "DUKKE", "DULGT", "DULLE", "DULME", "DUMME", "DUMMY", "DUMPE", "DUNET", "DUNKE", "DUNST", "DUPPE", "DURUM", "DUSIN", "DUSØR", "DUTTE", "DVÆLE", "DVÆRG", "DVALE", "DVASK", "DYBDE", "DYBSØ", "DYDIG", "DYKKE", "DYNGE", "DYPPE", "DYRKE", "DYSSE", "DYSTE", "DYTTE", "DYVEL", "EAGLE", "EBOLA", "ECLAT", "EDDER", "EDERS", "EDIKT", "EFFEN", "EFTER", "EGERN", "EGNET", "EKKOE", "EKSEM", "EKSIL", "ELBIL", "ELEGI", "ELGKO", "ELITE", "ELLER", "ELSKE", "ELVER", "EMMER", "EMOJI", "EMSIG", "ENDDA", "ENDNU", "ENDOG", "ENGEL", "ENHED", "ENKEL", "ENLIG", "ENØRE", "ENORM", "ENRUM", "ENSOM", "ENTAL", "ENTEN", "ENTRE", "ENZYM", "EOLIT", "EPISK", "EPOKE", "EPOXY", "ERIKA", "ESROM", "ESSAY", "ESSIG", "ESTER", "ETAGE", "ETAPE", "ETÅRS", "ETBÆR", "ETISK", "ETMÅL", "ETTAL", "ETTER", "ETUDE", "EUNUK", "EVENT", "EVERT", "EXCES", "FABEL", "FABLE", "FACET", "FACIL", "FACIT", "FACON", "FACTS", "FADER", "FADØL", "FÆCES", "FÆDRE", "FÆGTE", "FÆHÅR", "FÆISK", "FÆKAL", "FÆLDE", "FÆLLE", "FÆNGE", "FÆRGE", "FÆRRE", "FÆSTE", "FAGER", "FAGOT", "FAKIR", "FAKTA", "FALDE", "FALLE", "FALME", "FALSE", "FALSK", "FAMLE", "FAMØS", "FANCY", "FANDT", "FANGE", "FARAD", "FARAO", "FARCE", "FAREN", "FÅRET", "FARIN", "FARSI", "FARTE", "FARVE", "FASAN", "FASTE", "FATAL", "FÅTAL", "FATTE", "FATWA", "FAUNA", "FAVNE", "FAVØR", "FEBER", "FEDME", "FEDTE", "FEHÅR", "FEJDE", "FEJLE", "FEJRE", "FEMÅR", "FEMTE", "FEMTI", "FENNE", "FENOL", "FENYL", "FERIE", "FERLE", "FERSK", "FESET", "FESTE", "FIBER", "FICUS", "FIDEL", "FIDUS", "FIFLE", "FIGEN", "FIGHT", "FIGUR", "FIKSE", "FILET", "FILME", "FILTE", "FILUR", "FIMRE", "FIMSE", "FINAL", "FINDE", "FINER", "FINIT", "FINKE", "FINNE", "FINSK", "FINTE", "FIRER", "FIRMA", "FIRME", "FIRTI", "FISET", "FISKE", "FISSE", "FISTE", "FJÆLE", "FJÆRT", "FJANT", "FJASE", "FJELD", "FJERN", "FJERT", "FJOLS", "FJONG", "FJORD", "FLADE", "FLÅDE", "FLÆBE", "FLÆNG", "FLÆSE", "FLÆSK", "FLAGE", "FLAIR", "FLANE", "FLASH", "FLERE", "FLEST", "FLINK", "FLINT", "FLIRE", "FLIRT", "FLISE", "FLØDE", "FLØJL", "FLØJT", "FLOKS", "FLORA", "FLOVE", "FLUGT", "FLUID", "FLUKS", "FLUOR", "FLUTE", "FLYDE", "FLYER", "FLYVE", "FNISE", "FNYSE", "FODER", "FODRE", "FOGED", "FØGET", "FØJTE", "FOKUS", "FOLDE", "FØLER", "FØLGE", "FOLIE", "FOLIO", "FONDS", "FONEM", "FORÅD", "FORAN", "FORÅR", "FORBI", "FORCE", "FORDI", "FOREL", "FØRER", "FORGÅ", "FORKE", "FORMÅ", "FORME", "FORNE", "FØRNE", "FORNY", "FORPÅ", "FORRÅ", "FORSÅ", "FORSE", "FØRST", "FORTE", "FORUD", "FORUM", "FOSSE", "FOTON", "FOYER", "FRÅDE", "FRÆSE", "FRAGÅ", "FRAGT", "FRANC", "FRANK", "FRASE", "FRÅSE", "FREAK", "FREDE", "FRELS", "FREON", "FRIER", "FRISE", "FRISK", "FRIST", "FRONT", "FROST", "FRUGT", "FRYDE", "FRYGT", "FRYNS", "FRYSE", "FUCKE", "FUGTE", "FULDE", "FULGT", "FUMLE", "FUNKE", "FUNKY", "FUPPE", "FURET", "FURIE", "FUSEL", "FUSEN", "FUSER", "FUSKE", "FUTIL", "FUTON", "FUTTE", "FYLDE", "FYLKE", "FYNBO", "FYNSK", "FYORD", "FYRÅB", "FYRIG", "FYRRE", "FYSIK", "FYTIN", "GÅBEN", "GÆKKE", "GÆLDE", "GÆLER", "GÆLLE", "GÆNGE", "GÆNGS", "GÆRDE", "GÆSTE", "GÆTTE", "GAFFE", "GAFLE", "GAGAT", "GÅLÆG", "GALAN", "GALAR", "GALDE", "GALEJ", "GALGE", "GALLA", "GALLE", "GALON", "GALOP", "GALPE", "GAMBE", "GAMET", "GAMMA", "GANGE", "GARDE", "GARVE", "GÅSET", "GASSE", "GÅTID", "GÅTUR", "GAUGE", "GAUSS", "GAVNE", "GEARE", "GEBET", "GEBIS", "GEBYR", "GEDDE", "GEHØR", "GEJLE", "GEJST", "GEKKO", "GELED", "GEMAK", "GEMAL", "GEMEN", "GEMME", "GEMSE", "GEMYT", "GENBO", "GENNE", "GENOM", "GENRE", "GENSE", "GENUA", "GENUS", "GERNE", "GEVÆR", "GEVIR", "GIBBE", "GIFTE", "GIGUE", "GILDE", "GIMPE", "GIPSE", "GIRAF", "GISNE", "GISPE", "GITRE", "GIVEN", "GIVER", "GIVET", "GJALD", "GJORD", "GJORT", "GLÆDE", "GLANE", "GLANS", "GLIBE", "GLIDE", "GLIMT", "GLIOM", "GLOBE", "GLØDE", "GLØGG", "GLOSE", "GNAVE", "GNEJS", "GNIDE", "GNIER", "GNIST", "GNOME", "GODTE", "GØGLE", "GOKKE", "GONGE", "GØNGE", "GOPLE", "GOTER", "GOTIK", "GOUDA", "GRADE", "GRÆDE", "GRÆSK", "GRAMS", "GRAND", "GRÅNE", "GRANT", "GRAPE", "GRAVE", "GREEN", "GREJE", "GRENE", "GREVE", "GRIBE", "GRILL", "GRIME", "GRIND", "GRINE", "GRISE", "GRISK", "GRØDE", "GROFT", "GRØFT", "GRØNT", "GROWL", "GRUBE", "GRUMS", "GRUND", "GRUSE", "GRYDE", "GRYNT", "GUANO", "GUAVA", "GUBBE", "GUFFE", "GUFLE", "GUIDE", "GULNE", "GUMLE", "GUMME", "GUMMI", "GUMPE", "GUNST", "GUPPY", "GYLLE", "GYLPE", "GYNGE", "GYROS", "GYSER", "GYTJE", "GYVEL", "HABIL", "HABIT", "HACKE", "HADSK", "HÆDER", "HÆDRE", "HÆFTE", "HÆGTE", "HÆKLE", "HÆLDE", "HÆLER", "HÆLVT", "HÆMME", "HÆNDE", "HÆNGE", "HÆRDE", "HÆRGE", "HÆTTE", "HÆVDE", "HÆVNE", "HAGLE", "HAIKU", "HAKKE", "HALAL", "HALLO", "HALMA", "HALON", "HALSE", "HALTE", "HALVØ", "HAMAM", "HAMIT", "HAMLE", "HAMRE", "HÅNDE", "HANDY", "HANKE", "HÅNLE", "HÅNSK", "HAPSE", "HARAM", "HAREM", "HÅRET", "HARKE", "HARME", "HARPE", "HARPY", "HARSK", "HARVE", "HASPE", "HASTE", "HAUSA", "HAVÅL", "HAVDE", "HAVNE", "HAVRE", "HEADE", "HEALE", "HEAVY", "HEDDE", "HEDEN", "HEGLE", "HEGNE", "HEILE", "HEJRE", "HEJSA", "HEJSE", "HEKSE", "HELÅR", "HELLE", "HELME", "HELSE", "HELST", "HEMAN", "HENAD", "HENDE", "HENDØ", "HENGÅ", "HENNA", "HENNE", "HENRY", "HENSE", "HENTE", "HEPPE", "HERAF", "HERME", "HEROM", "HEROP", "HEROS", "HERPÅ", "HERRE", "HERSE", "HERTZ", "HERUD", "HERUT", "HETZE", "HEVET", "HIDSE", "HIJAB", "HIKKE", "HIKST", "HILLE", "HILSE", "HIMLE", "HINDE", "HINDI", "HINDU", "HINKE", "HIRSE", "HISSE", "HITTE", "HJÆLP", "HJALD", "HJALP", "HJELM", "HJORD", "HJORT", "HJULE", "HOBBY", "HOBEN", "HØFDE", "HOFTE", "HØJDE", "HØJNE", "HØJRE", "HØJST", "HØKER", "HØLÆS", "HOLDE", "HOLDT", "HØNSE", "HOPLA", "HOPPE", "HOPSA", "HORDE", "HØRER", "HØRME", "HORST", "HOSTE", "HØSTE", "HOTEL", "HØTYV", "HOUSE", "HOVED", "HØVED", "HOVEN", "HØVLE", "HOVNE", "HOVSA", "HUGAF", "HUGGE", "HUGST", "HULKE", "HULLE", "HUMAN", "HUMLE", "HUMME", "HUMOR", "HUMØR", "HUMPE", "HUMUS", "HURRA", "HUSAR", "HUSKE", "HUSLY", "HUTLE", "HVÆLV", "HVÆSE", "HVALP", "HVEDE", "HVENE", "HVEPS", "HVERV", "HVIDE", "HVILE", "HVINE", "HVORI", "HYÆNE", "HYBEL", "HYBEN", "HYGGE", "HYKLE", "HYLDE", "HYLER", "HYLLE", "HYMEN", "HYMNE", "HYNDE", "HYPPE", "HYRDE", "HYSSE", "HYTTE", "ICING", "IDEAL", "IDEEL", "IDIOM", "IDIOT", "IDRÆT", "IFALD", "IFØRE", "IHJEL", "IKKUN", "ILAND", "ILBUD", "ILDER", "ILDHU", "ILDNE", "ILING", "ILSOM", "ILTER", "ILTOG", "IMAGE", "IMENS", "IMMER", "IMMUN", "INBOX", "INDAD", "INDBO", "INDEN", "INDER", "INDGÅ", "INDIE", "INDRE", "INDSE", "INDSØ", "INDVI", "INERT", "INFAM", "INGEN", "INPUT", "INTET", "INTIM", "INTRO", "INUIT", "IRISK", "IRONI", "IRØRE", "IRRET", "ISBÅD", "ISBAR", "ISBOD", "ISFRI", "ISHAV", "ISING", "ISLÆG", "ISLÆT", "ISLAG", "ISLAM", "ISLOM", "ISSUE", "ISSYL", "ISTAP", "ISTID", "IVRIG", "JABBE", "JÆGER", "JÆRPE", "JÆTTE", "JÆVNE", "JAGER", "JAGTE", "JAKET", "JAKKE", "JAMBE", "JAMEN", "JAMME", "JAMRE", "JANTE", "JAPPE", "JASKE", "JAVEL", "JAZZE", "JEANS", "JENKA", "JERES", "JETON", "JIHAD", "JOBBE", "JODLE", "JOGGE", "JOINT", "JØKEL", "JOKER", "JOKKE", "JOLLE", "JOLRE", "JORDE", "JOULE", "JUBEL", "JUBLE", "JUDAS", "JUICE", "JUMBE", "JUNGE", "JUNKE", "JUNTA", "JUVEL", "KABEL", "KABYS", "KADET", "KADRE", "KÆFTE", "KÆLEN", "KÆLKE", "KÆLVE", "KÆMME", "KÆMPE", "KÆRNE", "KÆRRE", "KÆRTE", "KÆVLE", "KAFFE", "KAGLE", "KAHYT", "KAJAK", "KAKAO", "KALAS", "KALDE", "KALIF", "KALKE", "KALLA", "KALOT", "KALVE", "KAMEL", "KAMIK", "KAMIN", "KAMME", "KANAL", "KANDE", "KANEL", "KANIN", "KANON", "KANTE", "KANUT", "KAPEL", "KAPER", "KAPOK", "KAPPA", "KAPPE", "KAPRE", "KAPUN", "KAPUT", "KARAT", "KÅRDE", "KARET", "KARGO", "KARMA", "KARPE", "KARRY", "KARSE", "KARSK", "KARTE", "KARVE", "KASAK", "KASBA", "KASKO", "KASSE", "KASTE", "KASUS", "KATAR", "KAVAJ", "KAZOO", "KEBAB", "KEDEL", "KEFIR", "KEGLE", "KELIM", "KENDE", "KENDT", "KERNE", "KERTE", "KERUB", "KETCH", "KETON", "KHMER", "KIGGE", "KIKKE", "KIKSE", "KILDE", "KIMSE", "KININ", "KINKY", "KIOSK", "KIPER", "KIPPA", "KIPPE", "KIRKE", "KISEL", "KISTE", "KITIN", "KITTE", "KIVES", "KJOLE", "KJOVE", "KLÆBE", "KLÆDE", "KLAGE", "KLANG", "KLAPS", "KLARE", "KLASE", "KLASK", "KLEJN", "KLEMT", "KLERK", "KLIKE", "KLIMA", "KLINE", "KLINK", "KLINT", "KLIPS", "KLIRE", "KLOAK", "KLODE", "KLODS", "KLØER", "KLØFT", "KLOGE", "KLØGT", "KLONE", "KLORE", "KLØVE", "KLOVN", "KLUMP", "KLUNS", "KLYDE", "KLYNK", "KNÆGT", "KNÆLE", "KNAGE", "KNALD", "KNARK", "KNARR", "KNASE", "KNAST", "KNIBE", "KNIKS", "KNIPS", "KNIRK", "KNOLD", "KNUBS", "KNUDE", "KNUGE", "KNUSE", "KNYST", "KOALA", "KOBEN", "KØBER", "KOBLE", "KOBRA", "KØDET", "KOFTE", "KOGEØ", "KOGER", "KOGGE", "KOGLE", "KOKET", "KOKON", "KOKOS", "KOKSE", "KOLBE", "KØLER", "KØLIG", "KOLIK", "KØLLE", "KOLLI", "KØLNE", "KOLON", "KOLOS", "KOMET", "KOMIK", "KOMMA", "KOMME", "KONDI", "KONET", "KONGE", "KONTI", "KONTO", "KONUS", "KOØJE", "KOPAR", "KOPEK", "KOPRA", "KORAL", "KORAN", "KORDE", "KØRER", "KORPS", "KORSE", "KORTE", "KOSAK", "KOSTE", "KØTER", "KRAAL", "KRADS", "KRÆFT", "KRÆGE", "KRÆSE", "KRÆVE", "KRAFT", "KRAGE", "KRANK", "KRANS", "KRÅSE", "KRAVE", "KRAVL", "KREBS", "KREDS", "KREOL", "KRIDT", "KRIMI", "KRISE", "KROAT", "KROGE", "KROLF", "KRONE", "KRUDT", "KRUSE", "KRYBE", "KRYDS", "KRYPT", "KUBIK", "KUBUS", "KUGLE", "KUJON", "KUKKE", "KUKUR", "KULAK", "KULDE", "KULØR", "KULOS", "KULSO", "KUMME", "KUNDE", "KUNNE", "KUNST", "KUPLE", "KUPON", "KUPPE", "KURER", "KURIE", "KURRE", "KURVE", "KUSKE", "KUSSE", "KUTTE", "KVÆDE", "KVÆGE", "KVÆLD", "KVÆLE", "KVÆRK", "KVÆRN", "KVAJE", "KVALM", "KVALT", "KVANT", "KVARK", "KVART", "KVASE", "KVAST", "KVEJL", "KVIDE", "KVINT", "KVIST", "KVOTE", "KYPER", "KYRAS", "KYRIE", "KYSSE", "LABAN", "LABBE", "LABEL", "LABER", "LABIL", "LADER", "LÆDER", "LÆGGE", "LÆGTE", "LÆKAT", "LÆKKE", "LÆMME", "LÆNGE", "LÆNKE", "LÆNSE", "LÆRER", "LÆRKE", "LÆSER", "LÆSKE", "LÆSPE", "LÆSSE", "LAGDE", "LAGEN", "LAGER", "LAGRE", "LAKAJ", "LAKKE", "LALLE", "LAMEL", "LAMME", "LAMPE", "LANDE", "LÅNER", "LANGE", "LANGS", "LANSE", "LAPIS", "LAPPE", "LARGE", "LARGO", "LARME", "LARVE", "LASER", "LASET", "LASKE", "LASSO", "LASTE", "LASUR", "LATEX", "LATIN", "LATTE", "LAVET", "LEASE", "LEBEN", "LEDER", "LEDES", "LEDET", "LEDIG", "LEFLE", "LEGAL", "LEGAT", "LEGIO", "LEJDE", "LEJER", "LEJRE", "LEMMA", "LEMPE", "LEMUR", "LENTO", "LERET", "LETAL", "LETTE", "LEVER", "LEVIT", "LEVNE", "LEVRE", "LIDEN", "LIDSE", "LIERE", "LIGGE", "LIGHT", "LIGNE", "LIKØR", "LILJE", "LILLA", "LILLE", "LIMBO", "LIMIT", "LINDE", "LINER", "LINJE", "LINKE", "LINSE", "LIPID", "LIRKE", "LISTE", "LITER", "LITRA", "LOADE", "LOBBE", "LOBBY", "LØBER", "LØBSK", "LODDE", "LODEN", "LØDIG", "LODSE", "LØFTE", "LOGGE", "LOGIK", "LOGIN", "LOGON", "LOGRE", "LØJER", "LØJET", "LØJPE", "LOKAL", "LOKKE", "LØKKE", "LOKUM", "LOMME", "LOMVI", "LONGE", "LØNNE", "LOOPE", "LOPPE", "LOREN", "LØSEN", "LØSNE", "LOSSE", "LOTTE", "LOTTO", "LOTUS", "LOYAL", "LUBBE", "LUDER", "LUFFE", "LUFTE", "LUGAR", "LUGTE", "LUKAF", "LUKKE", "LULLE", "LUMEN", "LUMRE", "LUMSK", "LUNCH", "LUNDE", "LUNGE", "LUNKE", "LUNTE", "LUPIN", "LURER", "LUSET", "LUSKE", "LUTRE", "LYBSK", "LYDIG", "LYGTE", "LYKKE", "LYMFE", "LYRIK", "LYSÅR", "LYSKE", "LYSNE", "LYSTE", "LYTTE", "MACHO", "MADRO", "MÆCEN", "MÆGLE", "MÆGTE", "MÆLDE", "MÆLKE", "MÆNGE", "MÆRKE", "MÆSKE", "MÆTTE", "MAFIA", "MAGER", "MAGMA", "MAGRE", "MAGTE", "MAILE", "MAJOR", "MAKKE", "MAKRO", "MALAJ", "MALER", "MÅLER", "MALKE", "MALLE", "MALØR", "MALTE", "MAMBA", "MAMBO", "MANDE", "MÅNED", "MÅNEN", "MANGA", "MANGE", "MANGO", "MANGT", "MANKE", "MANKO", "MANNA", "MANNE", "MANUS", "MAORI", "MAPPE", "MARCH", "MARIN", "MARSK", "MARTS", "MASAI", "MASKE", "MÅSKE", "MASSE", "MATCH", "MÅTTE", "MEDGÅ", "MEDIA", "MEDIE", "MEDIO", "MEDSØ", "MEGEN", "MEGET", "MEJER", "MEJSE", "MEKKA", "MELDE", "MELET", "MELIS", "MELON", "MENED", "MENIG", "MENTE", "MERIT", "MESAN", "MESSE", "METAL", "METAN", "METER", "METRA", "METRO", "METYL", "MEZZO", "MIAVE", "MIDJE", "MIDTE", "MIDTI", "MIKSE", "MILJØ", "MIMER", "MIMIK", "MIMRE", "MINDE", "MINØR", "MINUS", "MINUT", "MISSE", "MISTE", "MITRA", "MITTE", "MIXER", "MJAVE", "MOBBE", "MØBEL", "MOBIL", "MODAL", "MODEL", "MODEM", "MODEN", "MODER", "MØDES", "MODGÅ", "MODIG", "MØDIG", "MODNE", "MØDOM", "MØDRE", "MODSØ", "MODUL", "MODUS", "MØFFE", "MØGSO", "MOKKA", "MOKKE", "MOLÆR", "MOLBO", "MOLER", "MØLLE", "MOLOK", "MOMSE", "MØNBO", "MØNJE", "MONNE", "MØNNE", "MØNSK", "MØNTE", "MOPPE", "MORAL", "MOREL", "MØRKE", "MØRNE", "MORSE", "MOSLE", "MOSTE", "MOTEL", "MOTET", "MOTIV", "MOTOR", "MOTTO", "MØVER", "MUDRE", "MUFFE", "MUFTI", "MUGNE", "MUHKO", "MUKKE", "MULAT", "MULIG", "MULKT", "MULLE", "MULTE", "MUMIE", "MUMLE", "MUNDE", "MURBI", "MURER", "MURRE", "MUSIK", "MUZAK", "MYNDE", "MYNTE", "MYOSE", "MYRDE", "MYRRA", "MYRTE", "MYSLI", "NÅDIG", "NADIR", "NÆBES", "NÆBET", "NÆGTE", "NÆLDE", "NÆNNE", "NÆPPE", "NÆRIG", "NÆRME", "NÆRPÅ", "NÆSTE", "NÆVNE", "NÆVNT", "NAFTA", "NAGLE", "NAKKE", "NANDU", "NAPPA", "NAPPE", "NARKO", "NARRE", "NASAL", "NASSE", "NATUR", "NAVER", "NAVLE", "NEDAD", "NEDEN", "NEDOM", "NEDRE", "NEGER", "NEGLE", "NEMME", "NERIE", "NERTZ", "NERVE", "NESTE", "NETOP", "NETTE", "NETTO", "NEVET", "NIÅRS", "NICHE", "NIECE", "NIKKE", "NINJA", "NIØJE", "NIPPE", "NIPSE", "NIQAB", "NISSE", "NITAL", "NITTE", "NOBEL", "NØDIG", "NOGEN", "NØGEN", "NOGET", "NOGLE", "NØGLE", "NØJES", "NØKKE", "NØLER", "NOMEN", "NONET", "NONNE", "NOPPE", "NOPRE", "NØRDE", "NORNE", "NORSK", "NOSSE", "NOTAR", "NOTAT", "NUBRE", "NUDEL", "NULRE", "NULTE", "NUMSE", "NUPPE", "NURSE", "NUSER", "NUSLE", "NUSSE", "NUTID", "NUVEL", "NYDER", "NYHED", "NYKKE", "NYLIG", "NYLON", "NYMFE", "NYNNE", "NYRIG", "NYSNE", "NYSYN", "NYTÅR", "NYTTE", "OASIS", "OBLAT", "OBLIK", "ØBOER", "OCEAN", "ODDER", "ODIØS", "ØDSEL", "ØDSLE", "OFFER", "OKAPI", "OKKER", "OKTAN", "OKTAV", "OKTET", "ØLBAS", "OLDEN", "ØLLET", "ØLVOM", "OLYMP", "OMBUD", "OMBUK", "OMEGA", "OMEGN", "OMEND", "OMGÅS", "ØMHED", "OMLØB", "OMLYD", "OMMER", "OMVEJ", "ONANI", "ONKEL", "ØNSKE", "ONYKS", "OPÆDE", "OPART", "OPBAG", "OPBUD", "OPERA", "OPFEJ", "OPGØR", "OPHAV", "OPHØR", "OPHUG", "OPIAT", "OPIUM", "OPKØB", "OPKOG", "OPLÆG", "OPLAG", "OPLØB", "OPØVE", "OPRÅB", "OPRET", "OPRØR", "OPSAT", "OPSLÅ", "OPSTÅ", "OPSYN", "OPTAG", "OPTIK", "OPTIL", "OPTOG", "OPTUR", "ORDEN", "ORDNE", "ORDRE", "ORGAN", "ORGEL", "ORGIE", "ØRIGE", "ORKAN", "ØRKEN", "ORKIS", "ORLON", "ORLOV", "ORNAT", "ØRRED", "ØSKEN", "OSMAN", "ØSTAT", "ØSTEN", "OSTET", "ØSTOM", "ØSTPÅ", "ØSTRE", "ØSTUD", "OTIUM", "OTTER", "OUNCE", "ØVDAG", "OVENI", "ØVRIG", "PÅBUD", "PACER", "PADDE", "PADLE", "PAGAJ", "PÅHIT", "PÅHØR", "PAKET", "PAKIS", "PAKKE", "PÅLÆG", "PALET", "PALLE", "PALME", "PALPE", "PAMPA", "PANDA", "PANDE", "PANEL", "PANIK", "PANTE", "PANTY", "PAPIL", "PAPIR", "PARAT", "PARIA", "PARKA", "PARRE", "PARSE", "PARTI", "PARTY", "PARYK", "PASHA", "PÅSKE", "PASSE", "PASTA", "PÅSTÅ", "PÅSYN", "PÅTÅR", "PATER", "PATIO", "PATOS", "PATTE", "PAUKE", "PAUSE", "PEAKE", "PEBER", "PEBET", "PEBRE", "PEDAL", "PEDEL", "PEJLE", "PELSE", "PENCE", "PENGE", "PENIS", "PENNY", "PENSA", "PEPPE", "PERLE", "PERSE", "PESTO", "PETIT", "PETTE", "PIANO", "PIBER", "PIBET", "PIBLE", "PIFFE", "PIFTE", "PIGET", "PIKKE", "PILAF", "PILKE", "PILLE", "PILOT", "PIMPE", "PINDE", "PINJE", "PINOL", "PINSE", "PINUP", "PIPPE", "PIQUE", "PIRAT", "PIRKE", "PIROG", "PIROL", "PIRRE", "PISKE", "PISSE", "PIVOT", "PIXEL", "PIZZA", "PJALT", "PJANK", "PJASK", "PJECE", "PJEVS", "PJUSK", "PLADE", "PLADS", "PLÆNE", "PLAGE", "PLAID", "PLANE", "PLANO", "PLASK", "PLAST", "PLEBS", "PLEJE", "PLEJL", "PLIGT", "PLINT", "PLIRE", "PLØJE", "PLØRE", "PLUMP", "PØBEL", "POCHE", "PODIE", "POESI", "POINT", "POKAL", "POKER", "POLÆR", "POLAK", "POLAR", "POLET", "POLIO", "POLKA", "PØLLE", "PØLSE", "POLSK", "POLYP", "POMET", "PØNSE", "POPPE", "PORET", "PORNO", "PORØS", "PORRE", "PORSE", "PORTO", "POSET", "POSØR", "POSTE", "POTTE", "POWER", "PRÆGE", "PRÆKE", "PRÆST", "PRAGT", "PRAJE", "PRALE", "PRENT", "PRIMA", "PRIME", "PRIMO", "PRINS", "PRINT", "PRION", "PRIOR", "PRISE", "PROSA", "PRØVE", "PROVO", "PRUNK", "PRUST", "PRYDE", "PRYGL", "PSYKE", "PUDRE", "PUDSE", "PUFFE", "PUGER", "PUKKE", "PUKLE", "PULJE", "PULLE", "PULSE", "PUMPE", "PUNCH", "PUNGE", "PUNKE", "PUNKT", "PUPIL", "PUPPE", "PURRE", "PURSE", "PUSHE", "PUSLE", "PUSTE", "PUTTE", "PUTTO", "PYGMÆ", "PYLON", "PYLRE", "PYNTE", "PYTON", "RABAT", "RABBI", "RÅBER", "RABLE", "RÅBUK", "RACER", "RADAR", "RÅDIG", "RADIO", "RADIX", "RÅDNE", "RADON", "RADSÅ", "RÅDYR", "RÆKKE", "RÆLIG", "RÆNKE", "RÆSON", "RAFLE", "RAFTE", "RÅHED", "RÅHUS", "RAIDE", "RAITA", "RAJAH", "RAKET", "RÅKID", "RAKKE", "RAKLE", "RAKTE", "RÅLAM", "RALLE", "RALLY", "RAMLE", "RAMME", "RAMPE", "RANCH", "RANDE", "RANDT", "RANGE", "RANKE", "RÅNOK", "RAPID", "RAPPE", "RAPSE", "RASLE", "RASPE", "RASTE", "RATIO", "RÅTRÆ", "RAVER", "RAYON", "REBEL", "REBUS", "RECES", "REDDE", "REDER", "REDET", "REGAL", "REGEL", "REGNE", "REJFE", "REJSE", "REKYL", "REMIS", "REMIX", "REMSE", "RENDE", "RENSE", "RENTE", "REPOS", "RETOR", "RETRO", "RETTE", "RETUR", "REVET", "REVIR", "REVLE", "REVNE", "REVSE", "RIBBE", "RIDSE", "RIGEL", "RIGGE", "RIGID", "RILLE", "RIMPE", "RIMTE", "RINDE", "RINGE", "RINKE", "RIPPE", "RISLE", "RISPE", "RISTE", "RITTE", "RITUS", "RIVAL", "RIVER", "ROBÅD", "ROBOT", "ROCKE", "RODEO", "RODET", "RØDME", "RØDNE", "RØFLE", "RØGER", "RØGET", "RØGTE", "ROKKE", "ROLIG", "ROLLE", "ROMAN", "ROMBE", "ROMER", "RØMME", "ROMMY", "RONDO", "RØNNE", "RØRÆG", "RØRIG", "ROSEN", "ROSET", "ROSIN", "RØSTI", "ROTOR", "ROTTE", "ROUGE", "ROUGH", "RØVER", "ROYAL", "RUBBE", "RUBEL", "RUBIN", "RUCHE", "RUDEL", "RUDER", "RUDET", "RUGBY", "RULAM", "RULLE", "RUMBA", "RUMLE", "RUMME", "RUMPE", "RUNDE", "RUNDT", "RUNGE", "RURAL", "RUSKE", "RUSTE", "RUTTE", "RYDDE", "RYGER", "RYGTE", "RYKKE", "RYNKE", "RYSTE", "RYTME", "SABEL", "SABLE", "SÅDAN", "SADEL", "SADLE", "SÆKKE", "SÆLGE", "SÆNKE", "SÆRBO", "SÆSON", "SÆTER", "SÆTTE", "SAFIR", "SAFTE", "SÅGAR", "SAGDE", "SAGTE", "SAKKE", "SAKSE", "SALÆR", "SALAT", "SALDO", "SALEP", "SALIG", "SALME", "SALON", "SALSA", "SALTE", "SALTO", "SALUT", "SALVE", "SAMBA", "SAMBO", "SAMLE", "SAMME", "SAMSK", "SAMSØ", "SANDE", "SANKE", "SANSE", "SÅPAS", "SÅSÆD", "SÅSOM", "SATAN", "SATIN", "SATSE", "SATTE", "SATYR", "SAUCE", "SAUNA", "SÅVEL", "SAVLE", "SAVNE", "SCENE", "SCONE", "SCOOP", "SCORE", "SEDAN", "SEEDE", "SEGNE", "SEJLE", "SEJRE", "SEKEL", "SEKST", "SELEN", "SELVE", "SEMIT", "SENAT", "SENDE", "SENET", "SENIL", "SEPIA", "SERAF", "SERGE", "SERIE", "SERIF", "SERØS", "SERUM", "SERVE", "SESAM", "SETUP", "SEXET", "SFÆRE", "SHEIK", "SHIIT", "SHINE", "SHUNT", "SIBEN", "SIDDE", "SIDEN", "SIDST", "SIFON", "SIGEL", "SIGMA", "SIGNE", "SIGTE", "SIKRE", "SILDE", "SILKE", "SILUR", "SIMRE", "SINDE", "SINKE", "SINUS", "SIOUX", "SIPPE", "SIRAT", "SIRTS", "SIRUP", "SISAL", "SITAR", "SITIN", "SITRE", "SJÆGT", "SJÆLE", "SJASK", "SJAVS", "SJUFT", "SJUSK", "SKABE", "SKADE", "SKÆLM", "SKÆLV", "SKÆMT", "SKÆND", "SKÆNE", "SKÆNK", "SKÆRE", "SKÆRF", "SKÆRM", "SKÆRV", "SKÆVE", "SKAFT", "SKAKT", "SKALA", "SKÅLE", "SKALK", "SKALP", "SKÅNE", "SKANK", "SKARE", "SKARN", "SKARP", "SKARV", "SKATE", "SKAVE", "SKEDE", "SKEJE", "SKEJS", "SKELE", "SKEMA", "SKIBE", "SKIDE", "SKIDT", "SKIFT", "SKILT", "SKIND", "SKIVE", "SKJUL", "SKØDE", "SKØGE", "SKOLE", "SKØNT", "SKØRT", "SKOSE", "SKOVE", "SKOVL", "SKRAB", "SKRÆK", "SKRÆL", "SKRÆV", "SKRAL", "SKRÅL", "SKRAP", "SKRÅS", "SKRAT", "SKRED", "SKREG", "SKREV", "SKRIG", "SKRIN", "SKRIV", "SKRØD", "SKROG", "SKROT", "SKRUB", "SKRUD", "SKRUE", "SKRUK", "SKUDE", "SKUDT", "SKULE", "SKUNK", "SKURE", "SKURK", "SKURV", "SKVÆT", "SKVAT", "SKVIS", "SKYDE", "SKYET", "SKYLD", "SKYPE", "SKYTS", "SLÆBE", "SLÆDE", "SLÆGT", "SLÅEN", "SLÆNG", "SLÅER", "SLAGS", "SLANG", "SLANK", "SLANT", "SLASK", "SLAVE", "SLESK", "SLIBE", "SLICE", "SLIDE", "SLIDS", "SLIME", "SLIPS", "SLISK", "SLØJD", "SLØJE", "SLØRE", "SLØSE", "SLØVE", "SLUDE", "SLUGE", "SLUGT", "SLUMP", "SLURK", "SLUSE", "SLYNG", "SMÆDE", "SMÆLD", "SMAGE", "SMALL", "SMART", "SMASH", "SMASK", "SMEAR", "SMEDE", "SMELT", "SMIDE", "SMILE", "SMØGE", "SMØLE", "SMØRE", "SMOVS", "SMUDS", "SMULD", "SMULE", "SMULT", "SMURT", "SMYGE", "SNACK", "SNÆRE", "SNÆRT", "SNAGE", "SNAPS", "SNARE", "SNART", "SNASK", "SNAVE", "SNAVS", "SNEGL", "SNERT", "SNIGE", "SNILD", "SNIPE", "SNØFT", "SNOLD", "SNØRE", "SNORK", "SNØVL", "SNØVS", "SNUDE", "SNUSE", "SNUSK", "SNYDE", "SØBAD", "SOBER", "SODET", "SØDME", "SØGER", "SØJLE", "SOLÅR", "SOLDE", "SOLEN", "SØLET", "SOLGT", "SOLID", "SØLLE", "SOLUR", "SØMIL", "SOMME", "SØMME", "SONAR", "SONDE", "SONET", "SONOR", "SOPPE", "SØREN", "SØRET", "SØRGE", "SØRME", "SØSYG", "SØULK", "SOUND", "SØVEJ", "SOVSE", "SPADE", "SPÆDE", "SPÆGE", "SPÆND", "SPÆNE", "SPALT", "SPAND", "SPANG", "SPANT", "SPARE", "SPARK", "SPEAK", "SPEED", "SPEGE", "SPEJL", "SPELT", "SPERM", "SPIDS", "SPILD", "SPILE", "SPIND", "SPION", "SPIRE", "SPISE", "SPJÆT", "SPLID", "SPLIT", "SPØGE", "SPØJS", "SPOLE", "SPORE", "SPORT", "SPOVE", "SPRÆL", "SPRAK", "SPRAY", "SPRIT", "SPRØD", "SPROG", "SPRUT", "SPULE", "SPUNS", "SPURT", "SPURV", "SQUAW", "STÅBI", "STADE", "STADS", "STÆNK", "STÆRK", "STÆSE", "STÆVN", "STAGE", "STALD", "STAMP", "STAND", "STANG", "STANK", "START", "STASE", "STAVE", "STAVN", "STEAK", "STEDE", "STEGE", "STEJL", "STELE", "STEMT", "STENE", "STIFT", "STIGE", "STILE", "STILK", "STILL", "STIME", "STING", "STIVE", "STJAL", "STØBE", "STØDE", "STØDT", "STØJE", "STOLA", "STOLE", "STOLT", "STORK", "STORM", "STORY", "STOUT", "STØVE", "STOVT", "STRÆB", "STRÆK", "STRAF", "STRAM", "STRED", "STREG", "STRID", "STRIK", "STRIP", "STRØG", "STRØM", "STROP", "STRUT", "STRYG", "STUDS", "STUMP", "STUND", "STUNT", "STUTS", "STUVE", "STYLE", "STYNE", "STYRE", "STYRT", "SUDER", "SUGER", "SUITE", "SUJET", "SUKAT", "SUKKE", "SUKRE", "SULFO", "SULKY", "SULTE", "SUMAK", "SUMME", "SUMPE", "SUNDE", "SUPER", "SUPPE", "SURFE", "SURRE", "SUSHI", "SUTTE", "SUTUR", "SVADA", "SVÆLG", "SVÆRD", "SVÆRM", "SVÆVE", "SVAJE", "SVALE", "SVAMP", "SVANE", "SVANG", "SVANS", "SVARE", "SVEDE", "SVEDT", "SVEJF", "SVEND", "SVIDE", "SVIGE", "SVIGT", "SVIME", "SVIND", "SVINE", "SVING", "SVIRE", "SVIRP", "SVØBE", "SVOVL", "SWING", "SYDEN", "SYDOM", "SYDPÅ", "SYDUD", "SYGNE", "SYLTE", "SYNÅL", "SYNDE", "SYNES", "SYNGE", "SYNKE", "SYNSK", "SYREN", "SYRER", "SYRNE", "SYSLE", "SYTØJ", "SYVER", "SYVTI", "TABEL", "TABER", "TÆKKE", "TÆLLE", "TÆMME", "TÆNDE", "TÆNKE", "TÆPPE", "TÆRTE", "TÆSKE", "TÆTNE", "TÆTTE", "TÆVEN", "TÅGET", "TAGGE", "TAJGA", "TAKKE", "TAKLE", "TAKSI", "TAKST", "TALAR", "TÅLED", "TALER", "TALJE", "TALON", "TALTE", "TAMIL", "TAMPE", "TANDE", "TANGA", "TANGE", "TANGO", "TANKE", "TANTE", "TAPAS", "TAPET", "TAPIR", "TAPPE", "TARIF", "TÅRNE", "TAROK", "TAROT", "TASKE", "TASTE", "TATAR", "TATER", "TAVET", "TAVLE", "TAXIE", "TAZET", "TEGNE", "TEINT", "TEIST", "TEJNE", "TEJST", "TEKOP", "TEKST", "TELTE", "TEMPI", "TEMPO", "TENOR", "TEORI", "TERME", "TERNE", "TERPE", "TERRE", "TERTS", "TESKE", "TESTE", "THETA", "THYBO", "TIARA", "TIÅRS", "TIERE", "TIEST", "TIGER", "TIGGE", "TIKKE", "TILDE", "TILGÅ", "TILSÅ", "TILSE", "TILTE", "TIMER", "TIMES", "TIMID", "TINDE", "TINGE", "TINTE", "TIØRE", "TIPPE", "TIRRE", "TISKE", "TISSE", "TITAL", "TITAN", "TITEL", "TITTE", "TJÆRE", "TJALD", "TJALK", "TJANS", "TJAVS", "TJENE", "TJØRN", "TOÅRS", "TOAST", "TOBAK", "TOBIS", "TODDY", "TØFFE", "TØFLE", "TOFTE", "TØJLE", "TØJRE", "TØJTE", "TOLDE", "TOLKE", "TØLTE", "TOMAT", "TOMLE", "TOMME", "TØMME", "TØMRE", "TONAL", "TØNDE", "TONER", "TONIC", "TONSE", "TOPAS", "TOPPE", "TOPTI", "TØRIS", "TØRKE", "TØRNE", "TØRRE", "TORSK", "TORSO", "TØRST", "TØSET", "TØSNE", "TOSSE", "TOTAL", "TOTEM", "TOTNE", "TOTTE", "TOTUR", "TOUCH", "TOUGH", "TRÅDE", "TRÅDT", "TRÆDE", "TRÆET", "TRÆLS", "TRÆNE", "TRÆSK", "TRÆVL", "TRAGT", "TRAMP", "TRANE", "TRANG", "TRAVE", "TRAVL", "TRAWL", "TREER", "TREMA", "TREND", "TRETI", "TREVL", "TRIAS", "TRICK", "TRIND", "TRINE", "TRIOL", "TRIØR", "TRIST", "TRIVI", "TRODS", "TROFÆ", "TRØJE", "TROKÆ", "TROLD", "TRONE", "TROPE", "TRØST", "TRUCK", "TRUMF", "TRUST", "TRYNE", "TUDSE", "TUGTE", "TUKAN", "TULLE", "TULRE", "TUMLE", "TUMOR", "TUMPE", "TUNER", "TUNGE", "TURBO", "TURDE", "TUSCH", "TUSKE", "TUSSE", "TUTOR", "TUTTE", "TUTTI", "TVÆRE", "TVÆRS", "TVÆRT", "TVANG", "TVEBO", "TVEGE", "TVIST", "TVIVL", "TWEED", "TWEEN", "TWEET", "TWILL", "TWIST", "TYFON", "TYFUS", "TYGGE", "TYKKE", "TYKNE", "TYLLE", "TYLVT", "TYNDE", "TYNGE", "TYRAN", "TYSSE", "TYVER", "TYVTE", "UÆGTE", "UANET", "UBLID", "UDBUD", "UDDØD", "UDELT", "UDFRI", "UDHUS", "UDKIG", "UDKIK", "UDKOG", "UDLÆG", "UDLÅN", "UDLØB", "UDLYD", "UDØBT", "UDØRK", "UDØSE", "UDØVE", "UDRÅB", "UDRIV", "UDRØJ", "UDSÆD", "UDSAT", "UDSPY", "UDSTÅ", "UDSYN", "UDTAG", "UDTOG", "UDTUR", "UDVEJ", "UEGAL", "UENIG", "UFIKS", "UFØDT", "UFØRE", "UFRED", "UGIFT", "UGRÆS", "UHELD", "UHØRT", "UHYRE", "UJÆVN", "UKLAR", "UKLOG", "ULÆRD", "ULÆST", "ULAND", "ULAVE", "ULDEN", "ULDET", "ULIGE", "ULØST", "ULYST", "UMAGE", "UMAMI", "UMBRA", "UMILD", "UNÅDE", "UNDER", "UNDGÅ", "UNDRE", "UNDSÅ", "UNDSE", "UNGMØ", "UNIKA", "UNION", "UNODE", "UØVET", "URÆMI", "URBAN", "UREDE", "UREDT", "UREEL", "URØRT", "URREM", "URTID", "URUND", "USAGT", "USAND", "USKET", "USKIK", "USKØN", "USSEL", "USUND", "UTAKT", "UTALT", "UTIDE", "UTING", "UTOPI", "UTRYG", "UTUGT", "UVANE", "UVANT", "UVEJR", "UVORN", "VABEL", "VÅBEN", "VABLE", "VADER", "VÅDTE", "VÆBNE", "VÆDDE", "VÆDRE", "VÆGRE", "VÆGTE", "VÆGUR", "VÆKKE", "VÆKST", "VÆLDE", "VÆLGE", "VÆLIG", "VÆLSK", "VÆLTE", "VÆNGE", "VÆNNE", "VÆRDI", "VÆREN", "VÆRFT", "VÆRGE", "VÆRKE", "VÆRNE", "VÆRRE", "VÆRST", "VÆSEL", "VÆSEN", "VÆSKE", "VÆTTE", "VÆVER", "VÅGEN", "VAGER", "VÅGNE", "VAKLE", "VAKTE", "VALEN", "VALGT", "VALID", "VALKE", "VALLE", "VALME", "VALØR", "VALSE", "VALSK", "VALTE", "VANDE", "VÅNDE", "VANDT", "VANGE", "VANKE", "VANRY", "VANTE", "VARAN", "VARDE", "VARIA", "VARIG", "VARME", "VARPE", "VARTE", "VARYL", "VASAL", "VÅSET", "VASKE", "VATER", "VATRE", "VEDET", "VEDGÅ", "VEGAR", "VEGET", "VEGNE", "VEJER", "VEJLE", "VEJRE", "VELÆR", "VELAN", "VELAR", "VELIN", "VEMOD", "VENDE", "VENØS", "VENTE", "VERFE", "VERST", "VERVE", "VESIR", "VETOE", "VIDDE", "VIDEN", "VIDEO", "VIDJE", "VIDNE", "VIDST", "VIFTE", "VIGØR", "VIGTE", "VIKAR", "VIKKE", "VIKLE", "VIKSE", "VILDT", "VILJE", "VILLA", "VILLE", "VIMRE", "VIMSE", "VINCA", "VINDE", "VINGE", "VINKE", "VINØS", "VINYL", "VIOLA", "VIPPE", "VIRAK", "VIRAL", "VIRIL", "VIRKE", "VIRRE", "VIRUS", "VISER", "VISIR", "VISIT", "VISKE", "VISNE", "VISSE", "VISUM", "VITAL", "VLIES", "VODKA", "VOGTE", "VOILA", "VOKAL", "VOKSE", "VOLDE", "VOLFE", "VOLTE", "VOLUT", "VØLVE", "VORDE", "VORES", "VORTE", "VOTUM", "VOVET", "VOVSE", "VRÆLE", "VRAGE", "VRANG", "VREDE", "VRIDE", "VRIST", "VRØVL", "VUGGE", "VULST", "VUNDE", "VUPTI", "WAGON", "WHIST", "XENON", "YACHT", "YATZY", "YDMYG", "YNDER", "YNDIG", "YNGEL", "YNGLE", "YNGRE", "YNGST", "YPPIG", "YUCCA", "ZAPPE", "ZEBRA", "ZEFYR", "ZELOT", "ZENIT", "ZLOTY", "ZOBEL", "ZOOME", "ZWECK" ]
