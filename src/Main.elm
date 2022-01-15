module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events
import Element exposing (Attribute, Element, centerX, centerY, column, el, height, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
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


view : Model -> Element msg
view model =
    case model.solution of
        Nothing ->
            Element.none

        Just solution ->
            column [ padding 10, centerX, height Element.fill, spacing 20 ]
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
    el (attrs ++ [ width <| Element.px 70, height <| Element.px 70 ])


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


tastaturList : List (List String)
tastaturList =
    [ [ "Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P", "Å" ]
    , [ "A", "S", "D", "F", "G", "H", "J", "K", "L", "Æ", "Ø" ]
    , [ "Enter", "Z", "X", "C", "V", "B", "N", "M", "⌫" ]
    ]


tastatur : Model -> Element msg
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
            (Element.row [ spacing 10, centerX ]
                << List.map
                    (\c ->
                        c
                            |> text
                            |> el [ Background.color <| letterColor c, padding 10 ]
                    )
            )
        |> Element.column [ spacing 10, Element.alignBottom ]


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
    Array.fromList <| List.sort [ "ABBED", "ABORT", "ACCES", "ADLED", "ADRÆT", "ADVIS", "AEROB", "AFART", "AFASI", "AFBUD", "AFDØD", "AFGUD", "AFISE", "AFKOG", "AFKOM", "AFLAD", "AFLYD", "AFLØB", "AFSLÅ", "AFSTÅ", "AFSÆT", "AFTAG", "AFTEN", "AFTES", "AFVEJ", "AGAPE", "AGAVE", "AGENS", "AGENT", "AGERE", "AGERN", "AGNAT", "AGONI", "AGORA", "AGTEN", "AGTER", "AGURK", "AHORN", "AJOUR", "AKRYL", "AKSEL", "AKTIE", "AKTOR", "AKTØR", "ALARM", "ALBUM", "ALDER", "ALENE", "ALERT", "ALGOD", "ALHUS", "ALIAS", "ALIBI", "ALIEN", "ALKYD", "ALMEN", "ALMUE", "ALPIN", "ALRUM", "ALTAN", "ALTER", "ALTID", "ALTSÅ", "ALVOR", "AMBRA", "AMISH", "AMORF", "AMPEL", "AMPUL", "AMØBE", "ANDEL", "ANDEN", "ANDET", "ANDRE", "ANGER", "ANGLE", "ANGRE", "ANIMA", "ANION", "ANJON", "ANKEL", "ANKER", "ANKRE", "ANLÆG", "ANLØB", "ANNAL", "ANNUA", "ANODE", "ANRÅB", "ANSAT", "ANSET", "ANSLÅ", "ANSTÅ", "ANTAL", "ANÆMI", "AORTA", "APATI", "APPEL", "APRIL", "APSIS", "AREAL", "ARENA", "ARGON", "ARGOT", "ARIER", "ARISK", "ARKIV", "ARMOD", "AROMA", "ARRAK", "ARRET", "ARRIG", "ARSEN", "ARSIS", "ARTIG", "ARVÆV", "ASIAT", "ASIET", "ASKET", "ASPIC", "ASTMA", "ATLAS", "ATLET", "ATONI", "ATTAK", "ATTEN", "ATTER", "AUDIO", "AUDIT", "AUGUR", "AUTOR", "AVERS", "AVIND", "AVLER", "BACON", "BADGE", "BADUT", "BAGBO", "BAGEL", "BAGER", "BAGOM", "BAGPÅ", "BAGUD", "BAJER", "BAKSE", "BALDE", "BALJE", "BALSA", "BAMLE", "BAMSE", "BANAL", "BANAN", "BANDT", "BANDY", "BANGE", "BANJE", "BANJO", "BANKO", "BAPPE", "BARAK", "BARBE", "BARDE", "BARET", "BARON", "BARRE", "BARSK", "BARYL", "BARYT", "BASAL", "BASAR", "BASIS", "BASKE", "BASSE", "BASTA", "BASTE", "BASUN", "BATAT", "BATCH", "BATIK", "BATTE", "BATTS", "BAVLE", "BEBOP", "BEDST", "BEFOR", "BEFRI", "BEGAV", "BEGGE", "BEGIK", "BEGLO", "BEGÆR", "BEHAG", "BEIGE", "BEJAE", "BEJLE", "BELÆG", "BELØB", "BENET", "BERET", "BERYL", "BESAD", "BESAT", "BESLÅ", "BESTÅ", "BESYV", "BESØG", "BETEL", "BETLE", "BETOG", "BETON", "BETRO", "BETØD", "BEVIS", "BEÆRE", "BIAVL", "BIBEL", "BIDET", "BIDSK", "BIFAG", "BIFIL", "BIGOT", "BIJOB", "BIKER", "BIKSE", "BILAG", "BILDE", "BILED", "BILLE", "BILOS", "BILYD", "BIMBO", "BIMLE", "BINDE", "BINÆR", "BIORD", "BIPPE", "BISAM", "BISAT", "BISOL", "BISON", "BISTÅ", "BITCH", "BITTE", "BIVEJ", "BIZAR", "BJERG", "BJESK", "BJØRN", "BLADE", "BLANK", "BLASE", "BLEGE", "BLIDE", "BLIND", "BLINI", "BLIST", "BLITZ", "BLIVE", "BLOND", "BLOTE", "BLUFF", "BLUND", "BLUSE", "BLÆSE", "BLÆST", "BLÅNE", "BOBBE", "BOGEY", "BOGIE", "BOJAN", "BOKSE", "BOLIG", "BOLTE", "BOMME", "BONDE", "BONUS", "BOOKE", "BOOME", "BOOST", "BOOTE", "BOPÆL", "BORDE", "BORGE", "BORNE", "BORST", "BORTE", "BOSAT", "BOSSE", "BOULE", "BOXER", "BRAGE", "BRAGT", "BRASK", "BRAVO", "BREAK", "BREDE", "BRIKS", "BRINK", "BRINT", "BRISE", "BRIST", "BRITE", "BRUDT", "BRUGE", "BRUGS", "BRUNE", "BRUSE", "BRUSK", "BRYDE", "BRYSK", "BRÆGE", "BRØDE", "BRØLE", "BRØND", "BRØST", "BUGGY", "BUGNE", "BUGTE", "BUHKO", "BUKET", "BUKKE", "BUKLE", "BULET", "BULLE", "BULNE", "BUMLE", "BUMPE", "BUMSE", "BUNDE", "BUNDT", "BURDE", "BURKA", "BURRE", "BUSKE", "BUSTE", "BUTAN", "BUTIK", "BYBUD", "BYBUS", "BYDEL", "BYGAS", "BYGET", "BYGGE", "BYLIV", "BYLTE", "BYNKE", "BYNÆR", "BYRDE", "BYRET", "BYRUM", "BYRÅD", "BYSTE", "BYTUR", "BÆGER", "BÆLGE", "BÆLLE", "BÆLTE", "BÆNKE", "BÆRER", "BÆRME", "BÆVEN", "BÆVER", "BÆVRE", "BØFFE", "BØGER", "BØJLE", "BØLLE", "BØNNE", "BØRST", "BØTTE", "BØVET", "BØVLE", "BØVSE", "BÅNDE", "BÅRET", "BÅTTE", "CAJUN", "CASTE", "CEDER", "CELLE", "CELLO", "CERUT", "CHARM", "CHECK", "CHILI", "CHIPS", "CIDER", "CIGAR", "CIRKA", "CITAR", "CITAT", "CITER", "CIVIL", "CLIPS", "COACH", "COATE", "CONGA", "CORNY", "COVER", "CRACK", "CRAWL", "CRAZY", "CREDO", "CREPE", "CROSS", "CURIE", "CUTTE", "CYKEL", "CYKLE", "CYSTE", "CÆSAR", "CÆSUR", "DADEL", "DADLE", "DAGES", "DALBO", "DALER", "DALRE", "DAMET", "DAMPE", "DANBO", "DANDY", "DANER", "DANNE", "DANSE", "DASKE", "DATID", "DATIV", "DATJA", "DAVID", "DAVRE", "DEALE", "DEBAT", "DEBET", "DEBIL", "DEBUT", "DECIM", "DEIST", "DEJSE", "DEKAN", "DEKOR", "DELLE", "DELTA", "DENAR", "DENIM", "DENNE", "DENTE", "DEPOT", "DERAF", "DERBY", "DERES", "DEROM", "DEROP", "DERPÅ", "DERUD", "DESTO", "DETTE", "DEVON", "DEVOT", "DIGEL", "DIGER", "DIGTE", "DIKKE", "DILDO", "DILLE", "DINAR", "DINER", "DINGO", "DIODE", "DIPOL", "DIRKE", "DIRRE", "DISET", "DISKO", "DITTO", "DIVAN", "DJÆRV", "DOGME", "DOKKE", "DOLCE", "DOLKE", "DOLME", "DOMME", "DONNA", "DONOR", "DONUT", "DORER", "DORGE", "DORSK", "DOSIS", "DOUCE", "DOULA", "DOVEN", "DOVNE", "DOYEN", "DOZER", "DRAGT", "DRAMA", "DRAPA", "DREJE", "DREJL", "DRENG", "DRESS", "DRIFT", "DRINK", "DROGE", "DRONE", "DROPS", "DRUDE", "DRÆBE", "DRÆNE", "DRÆVE", "DRØJE", "DRØNE", "DRÅBE", "DUBBE", "DUBLE", "DUFTE", "DUGGE", "DUKAT", "DULGT", "DULME", "DUMME", "DUMMY", "DUNET", "DUNKE", "DUNST", "DUPPE", "DUSIN", "DUSØR", "DUTTE", "DVALE", "DVASK", "DVÆLE", "DVÆRG", "DYBDE", "DYBSØ", "DYDIG", "DYKKE", "DYPPE", "DYRKE", "DYSTE", "DYTTE", "DYVEL", "DÆGGE", "DÆLEN", "DÆMME", "DÆMON", "DÆMPE", "DÆMRE", "DÆNGE", "DØBER", "DØDIS", "DØLGE", "DØLLE", "DØMME", "DØNNE", "DØSIG", "DØTRE", "DÅDYR", "DÅLAM", "EAGLE", "EBOLA", "ECLAT", "EDDER", "EDERS", "EDIKT", "EFFEN", "EFTER", "EGERN", "EGNET", "EKKOE", "EKSEM", "EKSIL", "ELBIL", "ELEGI", "ELGKO", "ELITE", "ELLER", "ELSKE", "ELVER", "EMOJI", "EMSIG", "ENDDA", "ENDNU", "ENDOG", "ENGEL", "ENHED", "ENKEL", "ENLIG", "ENORM", "ENRUM", "ENSOM", "ENTAL", "ENTEN", "ENTRE", "ENZYM", "ENØRE", "EOLIT", "EPISK", "EPOKE", "EPOXY", "ERIKA", "ESROM", "ESSAY", "ESSIG", "ETAGE", "ETAPE", "ETBÆR", "ETISK", "ETMÅL", "ETTAL", "ETTER", "ETUDE", "ETÅRS", "EUNUK", "EVENT", "EVERT", "EXCES", "FABEL", "FABLE", "FACET", "FACIL", "FACIT", "FACON", "FACTS", "FADER", "FAGER", "FAGOT", "FAKIR", "FAKTA", "FALDE", "FALLE", "FALME", "FALSE", "FAMLE", "FAMØS", "FANCY", "FANDT", "FARAD", "FARAO", "FARCE", "FAREN", "FARIN", "FARSI", "FARTE", "FASAN", "FATAL", "FATTE", "FATWA", "FAUNA", "FAVNE", "FAVØR", "FEBER", "FEDME", "FEDTE", "FEHÅR", "FEJLE", "FEJRE", "FEMTE", "FEMTI", "FEMÅR", "FENNE", "FENOL", "FENYL", "FERIE", "FERLE", "FERSK", "FESET", "FESTE", "FIBER", "FICUS", "FIDEL", "FIDUS", "FIFLE", "FIGEN", "FIGHT", "FIGUR", "FIKSE", "FILET", "FILME", "FILTE", "FILUR", "FIMRE", "FIMSE", "FINAL", "FINDE", "FINER", "FINKE", "FINNE", "FIRER", "FIRMA", "FIRME", "FIRTI", "FISET", "FISKE", "FISSE", "FISTE", "FJANT", "FJASE", "FJELD", "FJERN", "FJERT", "FJOLS", "FJONG", "FJORD", "FJÆLE", "FJÆRT", "FLAIR", "FLASH", "FLERE", "FLEST", "FLINK", "FLINT", "FLIRE", "FLIRT", "FLISE", "FLOKS", "FLORA", "FLOVE", "FLUGT", "FLUKS", "FLUOR", "FLUTE", "FLYDE", "FLYER", "FLYVE", "FLÆBE", "FLÆNG", "FLÆSE", "FLÆSK", "FLØJL", "FLØJT", "FNISE", "FNYSE", "FODER", "FODRE", "FOGED", "FOKUS", "FOLDE", "FOLIE", "FOLIO", "FONDS", "FONEM", "FORAN", "FORBI", "FORCE", "FORDI", "FOREL", "FORGÅ", "FORKE", "FORME", "FORMÅ", "FORNE", "FORNY", "FORPÅ", "FORRÅ", "FORSE", "FORSÅ", "FORTE", "FORUD", "FORUM", "FORÅD", "FORÅR", "FOSSE", "FOTON", "FOYER", "FRAGT", "FRAGÅ", "FRANC", "FRANK", "FRASE", "FREAK", "FREDE", "FRELS", "FREON", "FRIER", "FRISE", "FRISK", "FRIST", "FRONT", "FROST", "FRUGT", "FRYDE", "FRYGT", "FRYSE", "FRÆSE", "FRÅSE", "FUCKE", "FUGTE", "FULDE", "FULGT", "FUMLE", "FUNKY", "FUPPE", "FURET", "FURIE", "FUSEL", "FUSEN", "FUSER", "FUSKE", "FUTIL", "FUTON", "FUTTE", "FYNBO", "FYORD", "FYRIG", "FYRRE", "FYRÅB", "FYSIK", "FYTIN", "FÆCES", "FÆDRE", "FÆGTE", "FÆHÅR", "FÆISK", "FÆKAL", "FÆLLE", "FÆNGE", "FÆRRE", "FØGET", "FØJTE", "FØLER", "FØRER", "FØRNE", "FØRST", "FÅRET", "FÅTAL", "GAFFE", "GAFLE", "GAGAT", "GALAN", "GALAR", "GALDE", "GALEJ", "GALGE", "GALLA", "GALLE", "GALON", "GALOP", "GALPE", "GAMBE", "GAMET", "GAMMA", "GARDE", "GARVE", "GASSE", "GAUGE", "GAUSS", "GAVNE", "GEARE", "GEBET", "GEBIS", "GEBYR", "GEDDE", "GEHØR", "GEJLE", "GEKKO", "GELED", "GEMAK", "GEMAL", "GEMEN", "GEMSE", "GEMYT", "GENBO", "GENNE", "GENOM", "GENRE", "GENSE", "GENUA", "GENUS", "GERNE", "GEVIR", "GEVÆR", "GIBBE", "GIFTE", "GIGUE", "GIPSE", "GIRAF", "GISNE", "GISPE", "GITRE", "GIVEN", "GIVER", "GIVET", "GJALD", "GJORD", "GJORT", "GLANE", "GLANS", "GLIBE", "GLIDE", "GLIMT", "GLIOM", "GLOBE", "GLOSE", "GLØDE", "GLØGG", "GNAVE", "GNEJS", "GNIDE", "GNIER", "GNIST", "GNOME", "GOKKE", "GONGE", "GOPLE", "GOTER", "GOTIK", "GOUDA", "GRADE", "GRAMS", "GRAND", "GRANT", "GRAPE", "GREEN", "GRENE", "GREVE", "GRIBE", "GRILL", "GRIME", "GRIND", "GRINE", "GRISE", "GRISK", "GROFT", "GROWL", "GRUBE", "GRUMS", "GRUSE", "GRYDE", "GRYNT", "GRÆDE", "GRØDE", "GRØFT", "GRØNT", "GRÅNE", "GUANO", "GUAVA", "GUBBE", "GUFFE", "GUFLE", "GULNE", "GUMLE", "GUMME", "GUMMI", "GUMPE", "GUNST", "GUPPY", "GYLLE", "GYLPE", "GYROS", "GYSER", "GYTJE", "GYVEL", "GÆKKE", "GÆLDE", "GÆLER", "GÆNGE", "GÆNGS", "GÆSTE", "GÆTTE", "GØGLE", "GØNGE", "GÅBEN", "GÅLÆG", "GÅSET", "GÅTID", "GÅTUR", "HABIL", "HABIT", "HACKE", "HADSK", "HAGLE", "HAIKU", "HALAL", "HALLO", "HALMA", "HALON", "HALSE", "HALTE", "HALVØ", "HAMAM", "HAMIT", "HAMLE", "HAMRE", "HANDY", "HANKE", "HAPSE", "HARAM", "HAREM", "HARKE", "HARPE", "HARPY", "HARSK", "HASTE", "HAVDE", "HAVNE", "HAVRE", "HAVÅL", "HEADE", "HEALE", "HEAVY", "HEDDE", "HEDEN", "HEGNE", "HEILE", "HEJSA", "HEJSE", "HEKSE", "HELME", "HELSE", "HELST", "HELÅR", "HEMAN", "HENAD", "HENDE", "HENDØ", "HENGÅ", "HENNA", "HENNE", "HENRY", "HENSE", "HENTE", "HEPPE", "HERAF", "HERME", "HEROM", "HEROP", "HEROS", "HERPÅ", "HERRE", "HERSE", "HERTZ", "HERUD", "HERUT", "HETZE", "HEVET", "HIDSE", "HIJAB", "HIKST", "HILLE", "HILSE", "HIMLE", "HINDE", "HINDI", "HINDU", "HINKE", "HIRSE", "HISSE", "HITTE", "HJALD", "HJALP", "HJELM", "HJORD", "HJORT", "HJULE", "HJÆLP", "HOBBY", "HOBEN", "HOFTE", "HOLDE", "HORDE", "HORST", "HOTEL", "HOUSE", "HOVED", "HOVEN", "HOVNE", "HOVSA", "HUGAF", "HUGGE", "HUGST", "HULKE", "HULLE", "HUMAN", "HUMLE", "HUMME", "HUMOR", "HUMPE", "HUMUS", "HUMØR", "HUSAR", "HUSKE", "HUSLY", "HUTLE", "HVALP", "HVENE", "HVEPS", "HVERV", "HVIDE", "HVINE", "HVORI", "HVÆLV", "HVÆSE", "HYBEL", "HYBEN", "HYKLE", "HYLER", "HYLLE", "HYMEN", "HYMNE", "HYNDE", "HYPPE", "HYRDE", "HYSSE", "HYÆNE", "HÆDER", "HÆDRE", "HÆKLE", "HÆLER", "HÆLVT", "HÆRDE", "HÆRGE", "HÆTTE", "HÆVDE", "HÆVNE", "HØFDE", "HØJDE", "HØJNE", "HØJST", "HØKER", "HØLÆS", "HØNSE", "HØRER", "HØRME", "HØSTE", "HØTYV", "HØVED", "HØVLE", "HÅNDE", "HÅNLE", "HÅNSK", "HÅRET", "ICING", "IDEEL", "IDIOM", "IDIOT", "IDRÆT", "IFALD", "IFØRE", "IHJEL", "IKKUN", "ILAND", "ILBUD", "ILDER", "ILDHU", "ILDNE", "ILING", "ILSOM", "ILTER", "ILTOG", "IMAGE", "IMENS", "IMMER", "IMMUN", "INBOX", "INDAD", "INDBO", "INDEN", "INDER", "INDGÅ", "INDIE", "INDSE", "INDSØ", "INDVI", "INERT", "INFAM", "INGEN", "INPUT", "INTIM", "INTRO", "INUIT", "IRISK", "IRONI", "IRRET", "IRØRE", "ISBAR", "ISBOD", "ISBÅD", "ISFRI", "ISHAV", "ISING", "ISLAG", "ISLAM", "ISLOM", "ISLÆG", "ISLÆT", "ISSUE", "ISSYL", "ISTAP", "ISTID", "IVRIG", "JABBE", "JAGER", "JAGTE", "JAKET", "JAKKE", "JAMBE", "JAMEN", "JAMME", "JAMRE", "JANTE", "JAPPE", "JASKE", "JAVEL", "JAZZE", "JEANS", "JENKA", "JERES", "JETON", "JIHAD", "JODLE", "JOGGE", "JOINT", "JOKER", "JOKKE", "JOLRE", "JORDE", "JOULE", "JUBEL", "JUBLE", "JUDAS", "JUICE", "JUMBE", "JUNGE", "JUNTA", "JUVEL", "JÆGER", "JÆRPE", "JÆTTE", "JÆVNE", "JØKEL", "KABEL", "KABYS", "KADET", "KADRE", "KAFFE", "KAGLE", "KAHYT", "KAJAK", "KAKAO", "KALAS", "KALIF", "KALLA", "KALOT", "KALVE", "KAMEL", "KAMIK", "KAMIN", "KAMME", "KANAL", "KANDE", "KANEL", "KANIN", "KANTE", "KANUT", "KAPEL", "KAPER", "KAPOK", "KAPPA", "KAPRE", "KAPUN", "KAPUT", "KARAT", "KARET", "KARGO", "KARMA", "KARPE", "KARRY", "KARSE", "KARSK", "KARVE", "KASAK", "KASBA", "KASKO", "KASSE", "KASUS", "KATAR", "KAVAJ", "KAZOO", "KEBAB", "KEDEL", "KEFIR", "KEGLE", "KELIM", "KENDT", "KERNE", "KERTE", "KERUB", "KETCH", "KETON", "KHMER", "KIGGE", "KIKKE", "KIKSE", "KIMSE", "KININ", "KINKY", "KIOSK", "KIPER", "KIPPA", "KIPPE", "KIRKE", "KISEL", "KISTE", "KITIN", "KITTE", "KIVES", "KJOLE", "KJOVE", "KLAPS", "KLARE", "KLASE", "KLEJN", "KLEMT", "KLERK", "KLIKE", "KLIMA", "KLINE", "KLINT", "KLIPS", "KLIRE", "KLOAK", "KLODE", "KLOGE", "KLONE", "KLORE", "KLOVN", "KLUMP", "KLUNS", "KLYDE", "KLYNK", "KLÆBE", "KLØER", "KLØFT", "KLØGT", "KLØVE", "KNARK", "KNARR", "KNASE", "KNAST", "KNIKS", "KNIPS", "KNIRK", "KNOLD", "KNUBS", "KNUDE", "KNUGE", "KNUSE", "KNÆGT", "KNÆLE", "KOALA", "KOBEN", "KOBLE", "KOBRA", "KOFTE", "KOGER", "KOGEØ", "KOGGE", "KOKET", "KOKON", "KOKOS", "KOKSE", "KOLBE", "KOLIK", "KOLLI", "KOLON", "KOLOS", "KOMET", "KOMIK", "KOMMA", "KONDI", "KONET", "KONGE", "KONTI", "KONTO", "KONUS", "KOPAR", "KOPEK", "KOPRA", "KORAN", "KORDE", "KORPS", "KORSE", "KORTE", "KOSAK", "KOØJE", "KRAAL", "KRADS", "KRAFT", "KRAGE", "KRANS", "KRAVE", "KRAVL", "KREBS", "KREDS", "KREOL", "KRIDT", "KRIMI", "KRISE", "KROAT", "KROGE", "KROLF", "KRUDT", "KRUSE", "KRYBE", "KRYDS", "KRYPT", "KRÆFT", "KRÆGE", "KRÆSE", "KRÆVE", "KRAAL", "KRÅSE", "KUBIK", "KUBUS", "KUGLE", "KUJON", "KUKKE", "KUKUR", "KULAK", "KULDE", "KULOS", "KULSO", "KULØR", "KUMME", "KUNDE", "KUNNE", "KUNST", "KUPLE", "KUPON", "KUPPE", "KURER", "KURIE", "KUSKE", "KUSSE", "KUTTE", "KVAJE", "KVALT", "KVANT", "KVAST", "KVEJL", "KVIDE", "KVIST", "KVOTE", "KVÆGE", "KVÆLD", "KVÆLE", "KVÆRK", "KVÆRN", "KYPER", "KYRAS", "KYRIE", "KYSSE", "KÆFTE", "KÆLEN", "KÆLKE", "KÆLVE", "KÆMME", "KÆRRE", "KÆRTE", "KØBER", "KØDET", "KØLER", "KØLIG", "KØLLE", "KØLNE", "KØRER", "KØTER", "KÅRDE", "LABAN", "LABBE", "LABER", "LABIL", "LAGDE", "LAGEN", "LAGRE", "LAKAJ", "LAKKE", "LALLE", "LAMEL", "LAMME", "LAMPE", "LANDE", "LANGS", "LANSE", "LAPIS", "LAPPE", "LARME", "LARVE", "LASER", "LASET", "LASSO", "LASTE", "LASUR", "LATEX", "LATIN", "LATTE", "LAVET", "LEASE", "LEBEN", "LEDER", "LEDES", "LEDET", "LEDIG", "LEFLE", "LEGAL", "LEGIO", "LEJDE", "LEJER", "LEJRE", "LEMMA", "LEMUR", "LENTO", "LERET", "LETAL", "LEVER", "LEVIT", "LEVNE", "LEVRE", "LIDEN", "LIDSE", "LIERE", "LIGGE", "LIGHT", "LIGNE", "LIKØR", "LILJE", "LILLA", "LILLE", "LIMIT", "LINDE", "LINER", "LINJE", "LINKE", "LINSE", "LIPID", "LIRKE", "LITER", "LITRA", "LOADE", "LOBBE", "LOBBY", "LODEN", "LODSE", "LOGGE", "LOGIK", "LOGIN", "LOGON", "LOGRE", "LOKAL", "LOKKE", "LOKUM", "LOMME", "LOMVI", "LONGE", "LOOPE", "LOREN", "LOSSE", "LOTTE", "LOTTO", "LOTUS", "LOYAL", "LUBBE", "LUDER", "LUFTE", "LUGAR", "LUGTE", "LUKAF", "LULLE", "LUMEN", "LUMRE", "LUMSK", "LUNCH", "LUNDE", "LUNGE", "LUPIN", "LURER", "LUSET", "LUSKE", "LUTRE", "LYBSK", "LYDIG", "LYGTE", "LYKKE", "LYMFE", "LYRIK", "LYSNE", "LYSTE", "LYSÅR", "LYTTE", "LÆDER", "LÆKAT", "LÆKKE", "LÆMME", "LÆNSE", "LÆRER", "LÆRKE", "LÆSER", "LÆSKE", "LÆSPE", "LÆSSE", "LØBER", "LØBSK", "LØDIG", "LØJER", "LØJET", "LØJPE", "LØKKE", "LØNNE", "LØSEN", "LØSNE", "LÅNER", "MADRO", "MAFIA", "MAGMA", "MAGRE", "MAGTE", "MAILE", "MAJOR", "MAKKE", "MAKRO", "MALAJ", "MALER", "MALKE", "MALLE", "MALTE", "MALØR", "MAMBA", "MAMBO", "MANDE", "MANGE", "MANGO", "MANGT", "MANKE", "MANKO", "MANNA", "MANNE", "MANUS", "MAORI", "MAPPE", "MARCH", "MARIN", "MARSK", "MARTS", "MASAI", "MASSE", "MEDGÅ", "MEDIA", "MEDIE", "MEDIO", "MEDSØ", "MEGEN", "MEGET", "MEJER", "MEJSE", "MELDE", "MELET", "MELIS", "MELON", "MENED", "MENIG", "MENTE", "MERIT", "MESAN", "METAL", "METAN", "METER", "METRA", "METRO", "METYL", "MEZZO", "MIAVE", "MIDJE", "MIDTE", "MIDTI", "MIKSE", "MILJØ", "MIMER", "MIMIK", "MIMRE", "MINUT", "MINØR", "MISSE", "MISTE", "MITRA", "MITTE", "MIXER", "MJAVE", "MOBBE", "MODAL", "MODEL", "MODEM", "MODEN", "MODER", "MODGÅ", "MODIG", "MODNE", "MODSØ", "MODUL", "MODUS", "MOKKA", "MOLBO", "MOLER", "MOLOK", "MOLÆR", "MOMSE", "MONNE", "MOREL", "MORSE", "MOSLE", "MOSTE", "MOTEL", "MOTET", "MOTIV", "MOTOR", "MOTTO", "MUDRE", "MUFFE", "MUFTI", "MUGNE", "MUHKO", "MUKKE", "MULAT", "MULIG", "MULKT", "MULLE", "MULTE", "MUMIE", "MUMLE", "MUNDE", "MURBI", "MURER", "MURRE", "MUSIK", "MUZAK", "MYNDE", "MYNTE", "MYOSE", "MYRDE", "MYRRA", "MYRTE", "MYSLI", "MÆCEN", "MÆGLE", "MÆGTE", "MÆLDE", "MÆNGE", "MÆSKE", "MÆTTE", "MØBEL", "MØDES", "MØDIG", "MØDOM", "MØDRE", "MØFFE", "MØGSO", "MØLLE", "MØNBO", "MØNJE", "MØNNE", "MØNSK", "MØNTE", "MØRKE", "MØRNE", "MØVER", "MÅLER", "MÅNED", "MÅNEN", "MÅSKE", "NADIR", "NAFTA", "NANDU", "NAPPA", "NAPPE", "NARKO", "NARRE", "NASSE", "NATUR", "NEDAD", "NEDEN", "NEDOM", "NEDRE", "NEGER", "NEGLE", "NERIE", "NERTZ", "NERVE", "NESTE", "NETOP", "NETTE", "NETTO", "NEVET", "NICHE", "NIECE", "NINJA", "NIPPE", "NIPSE", "NIQAB", "NISSE", "NITAL", "NIØJE", "NIÅRS", "NOBEL", "NOGEN", "NOGET", "NOGLE", "NOMEN", "NONET", "NONNE", "NOPRE", "NORNE", "NOTAR", "NOTAT", "NUBRE", "NUDEL", "NULRE", "NULTE", "NUMSE", "NUPPE", "NURSE", "NUSER", "NUSLE", "NUSSE", "NUTID", "NUVEL", "NYDER", "NYHED", "NYKKE", "NYLIG", "NYLON", "NYMFE", "NYNNE", "NYRIG", "NYSNE", "NYSYN", "NYTÅR", "NÆBES", "NÆBET", "NÆGTE", "NÆLDE", "NÆNNE", "NÆPPE", "NÆRIG", "NÆRME", "NÆRPÅ", "NÆVNE", "NÆVNT", "NØGEN", "NØJES", "NØKKE", "NØLER", "NØRDE", "NÅDIG", "OASIS", "OBLAT", "OBLIK", "OCEAN", "ODDER", "ODIØS", "OFFER", "OKAPI", "OKKER", "OKTAN", "OKTAV", "OKTET", "OLDEN", "OLYMP", "OMBUD", "OMBUK", "OMEGA", "OMEGN", "OMEND", "OMGÅS", "OMLYD", "OMLØB", "OMMER", "OMVEJ", "ONANI", "ONKEL", "ONYKS", "OPART", "OPBAG", "OPBUD", "OPERA", "OPFEJ", "OPGØR", "OPHAV", "OPHUG", "OPHØR", "OPIAT", "OPIUM", "OPKOG", "OPKØB", "OPLAG", "OPLÆG", "OPLØB", "OPRET", "OPRØR", "OPRÅB", "OPSAT", "OPSLÅ", "OPSTÅ", "OPSYN", "OPTAG", "OPTIL", "OPTOG", "OPTUR", "OPÆDE", "OPØVE", "ORDNE", "ORDRE", "ORGAN", "ORGEL", "ORGIE", "ORKAN", "ORKIS", "ORLON", "ORLOV", "ORNAT", "OSMAN", "OSTET", "OTIUM", "OTTER", "OUNCE", "OVENI", "PACER", "PADDE", "PAGAJ", "PAKET", "PAKIS", "PALET", "PALME", "PALPE", "PAMPA", "PANDA", "PANEL", "PANIK", "PANTE", "PANTY", "PAPIL", "PAPIR", "PARAT", "PARIA", "PARKA", "PARRE", "PARSE", "PARTI", "PARTY", "PARYK", "PASHA", "PASSE", "PASTA", "PATER", "PATIO", "PATOS", "PAUKE", "PEAKE", "PEBET", "PEBRE", "PEDEL", "PEJLE", "PELSE", "PENCE", "PENGE", "PENIS", "PENNY", "PENSA", "PEPPE", "PESTO", "PETIT", "PETTE", "PIBER", "PIBLE", "PIFFE", "PIFTE", "PIGET", "PIKKE", "PILAF", "PILKE", "PILOT", "PIMPE", "PINDE", "PINJE", "PINOL", "PINSE", "PINUP", "PIPPE", "PIQUE", "PIRAT", "PIRKE", "PIROG", "PIROL", "PIRRE", "PISKE", "PISSE", "PIVOT", "PIXEL", "PIZZA", "PJANK", "PJASK", "PJECE", "PJEVS", "PJUSK", "PLADE", "PLADS", "PLAID", "PLANE", "PLANO", "PLAST", "PLEBS", "PLEJL", "PLIGT", "PLINT", "PLIRE", "PLÆNE", "PLØJE", "POCHE", "PODIE", "POESI", "POINT", "POKAL", "POKER", "POLAK", "POLAR", "POLET", "POLIO", "POLKA", "POLSK", "POLYP", "POLÆR", "POMET", "POPPE", "PORET", "PORNO", "PORRE", "PORSE", "PORTO", "PORØS", "POSET", "POSTE", "POSØR", "POWER", "PRAGT", "PRAJE", "PRALE", "PRENT", "PRIMA", "PRIME", "PRIMO", "PRINS", "PRINT", "PRION", "PRIOR", "PROSA", "PROVO", "PRUNK", "PRUST", "PRYDE", "PRYGL", "PRÆGE", "PRÆKE", "PRÆST", "PSYKE", "PUDRE", "PUDSE", "PUFFE", "PUGER", "PUKKE", "PUKLE", "PULJE", "PULLE", "PULSE", "PUNGE", "PUNKE", "PUNKT", "PUPIL", "PUPPE", "PURRE", "PURSE", "PUSHE", "PUSLE", "PUSTE", "PUTTO", "PYGMÆ", "PYLON", "PYLRE", "PYNTE", "PYTON", "PØBEL", "PØLLE", "PØLSE", "PØNSE", "PÅBUD", "PÅHIT", "PÅHØR", "PÅLÆG", "PÅSKE", "PÅSTÅ", "PÅSYN", "PÅTÅR", "RABAT", "RABBI", "RABLE", "RACER", "RADAR", "RADIO", "RADIX", "RADON", "RADSÅ", "RAFLE", "RAFTE", "RAIDE", "RAITA", "RAJAH", "RAKET", "RAKKE", "RAKLE", "RAKTE", "RALLE", "RALLY", "RAMLE", "RAMPE", "RANCH", "RANDE", "RANDT", "RANGE", "RAPID", "RAPSE", "RASLE", "RASPE", "RASTE", "RATIO", "RAVER", "RAYON", "REBEL", "REBUS", "RECES", "REDDE", "REDER", "REDET", "REGAL", "REGEL", "REGNE", "REJFE", "REKYL", "REMIX", "RENSE", "RENTE", "REPOS", "RETOR", "REVET", "REVIR", "REVLE", "REVSE", "RIGEL", "RIGGE", "RIGID", "RIMPE", "RIMTE", "RINDE", "RINKE", "RIPPE", "RISLE", "RISPE", "RISTE", "RITUS", "RIVAL", "RIVER", "ROBOT", "ROBÅD", "ROCKE", "RODEO", "RODET", "ROLIG", "ROLLE", "ROMAN", "ROMBE", "ROMER", "ROMMY", "RONDO", "ROSEN", "ROSET", "ROSIN", "ROTOR", "ROUGE", "ROUGH", "ROYAL", "RUBBE", "RUBEL", "RUBIN", "RUCHE", "RUDEL", "RUDER", "RUDET", "RUGBY", "RULAM", "RUMBA", "RUMLE", "RUMME", "RUMPE", "RUNDT", "RUNGE", "RURAL", "RUSKE", "RUSTE", "RUTTE", "RYDDE", "RYGER", "RYGTE", "RYKKE", "RYSTE", "RYTME", "RÆLIG", "RÆNKE", "RÆSON", "RØDNE", "RØFLE", "RØGER", "RØGTE", "RØNNE", "RØRIG", "RØRÆG", "RØSTI", "RØVER", "RÅBER", "RÅBUK", "RÅDIG", "RÅDNE", "RÅDYR", "RÅHED", "RÅHUS", "RÅKID", "RÅLAM", "RÅNOK", "RÅTRÆ", "SABEL", "SABLE", "SADEL", "SADLE", "SAFIR", "SAFTE", "SAGDE", "SAGTE", "SAKKE", "SAKSE", "SALAT", "SALDO", "SALEP", "SALIG", "SALME", "SALON", "SALSA", "SALTE", "SALTO", "SALUT", "SALÆR", "SAMBA", "SAMLE", "SAMME", "SAMSK", "SAMSØ", "SANDE", "SANKE", "SANSE", "SATAN", "SATIN", "SATSE", "SATTE", "SATYR", "SAUCE", "SAUNA", "SAVLE", "SAVNE", "SCENE", "SCONE", "SCOOP", "SEDAN", "SEEDE", "SEGNE", "SEJLE", "SEJRE", "SEKEL", "SEKST", "SELEN", "SELVE", "SEMIT", "SENAT", "SENDE", "SENET", "SENIL", "SEPIA", "SERAF", "SERGE", "SERIE", "SERIF", "SERUM", "SERVE", "SERØS", "SETUP", "SEXET", "SFÆRE", "SHEIK", "SHIIT", "SHINE", "SHUNT", "SIBEN", "SIDDE", "SIDEN", "SIDST", "SIFON", "SIGEL", "SIGMA", "SIGNE", "SIKRE", "SILDE", "SILKE", "SILUR", "SIMRE", "SINDE", "SINUS", "SIRAT", "SIRTS", "SIRUP", "SISAL", "SITAR", "SITIN", "SITRE", "SJASK", "SJAVS", "SJUFT", "SJUSK", "SJÆGT", "SJÆLE", "SKAFT", "SKAKT", "SKALA", "SKALK", "SKALP", "SKANK", "SKARE", "SKARV", "SKATE", "SKAVE", "SKEDE", "SKEJE", "SKEJS", "SKELE", "SKEMA", "SKIBE", "SKIFT", "SKILT", "SKIND", "SKJUL", "SKOVE", "SKOVL", "SKRAB", "SKRAL", "SKRAP", "SKRAT", "SKREG", "SKREV", "SKRIG", "SKRIN", "SKRIV", "SKROG", "SKROT", "SKRUB", "SKRUD", "SKRUK", "SKRÆK", "SKRÆL", "SKRÆV", "SKRØD", "SKRÅL", "SKRÅS", "SKUDE", "SKUDT", "SKULE", "SKURK", "SKURV", "SKVIS", "SKVÆT", "SKYDE", "SKYET", "SKYLD", "SKYPE", "SKYTS", "SKÆLM", "SKÆLV", "SKÆMT", "SKÆND", "SKÆNE", "SKÆNK", "SKÆRE", "SKÆRF", "SKÆRM", "SKÆRV", "SKÆVE", "SKØGE", "SKØNT", "SKØRT", "SKÅLE", "SKÅNE", "SLAGS", "SLANG", "SLANK", "SLANT", "SLASK", "SLESK", "SLIBE", "SLIDS", "SLIME", "SLIPS", "SLISK", "SLUDE", "SLUGE", "SLUGT", "SLUMP", "SLURK", "SLYNG", "SLÆBE", "SLÆGT", "SLÆNG", "SLØJD", "SLØJE", "SLØRE", "SLØSE", "SLØVE", "SLÅEN", "SLÅER", "SMAGE", "SMALL", "SMART", "SMASH", "SMASK", "SMEAR", "SMEDE", "SMELT", "SMIDE", "SMILE", "SMOVS", "SMUDS", "SMULD", "SMULE", "SMULT", "SMURT", "SMYGE", "SMÆDE", "SMÆLD", "SMØLE", "SNACK", "SNAGE", "SNAPS", "SNARE", "SNART", "SNAVE", "SNEGL", "SNIGE", "SNILD", "SNIPE", "SNOLD", "SNORK", "SNUDE", "SNUSE", "SNUSK", "SNYDE", "SNÆRE", "SNÆRT", "SNØFT", "SNØVL", "SNØVS", "SOBER", "SODET", "SOLDE", "SOLEN", "SOLGT", "SOLID", "SOLUR", "SOLÅR", "SOMME", "SONAR", "SONDE", "SONET", "SONOR", "SOPPE", "SOUND", "SOVSE", "SPALT", "SPANG", "SPANT", "SPARE", "SPARK", "SPEAK", "SPEED", "SPEGE", "SPEJL", "SPELT", "SPERM", "SPILD", "SPILE", "SPION", "SPJÆT", "SPLID", "SPLIT", "SPORT", "SPOVE", "SPRAK", "SPRAY", "SPRIT", "SPROG", "SPRÆL", "SPRØD", "SPULE", "SPUNS", "SPURT", "SPURV", "SPÆDE", "SPÆGE", "SPÆND", "SPÆNE", "SPØGE", "SPØJS", "SQUAW", "STADE", "STALD", "STAMP", "STANG", "START", "STASE", "STAVN", "STEAK", "STEDE", "STEGE", "STEJL", "STELE", "STEMT", "STENE", "STILE", "STILK", "STILL", "STING", "STIVE", "STJAL", "STOLA", "STOLE", "STOLT", "STORK", "STORM", "STORY", "STOUT", "STOVT", "STRAF", "STRAM", "STRED", "STREG", "STROP", "STRUT", "STRYG", "STRÆB", "STRÆK", "STRØM", "STUND", "STUNT", "STUTS", "STUVE", "STYLE", "STYNE", "STYRT", "STÆNK", "STÆRK", "STÆSE", "STÆVN", "STØBE", "STØDE", "STØDT", "STØJE", "STØVE", "STÅBI", "SUDER", "SUGER", "SUITE", "SUJET", "SUKAT", "SUKKE", "SUKRE", "SULFO", "SULKY", "SULTE", "SUMAK", "SUMME", "SUMPE", "SUNDE", "SUPER", "SUPPE", "SURFE", "SURRE", "SUSHI", "SUTTE", "SUTUR", "SVADA", "SVAJE", "SVAMP", "SVANE", "SVANS", "SVARE", "SVEDE", "SVEDT", "SVEJF", "SVEND", "SVIGE", "SVIGT", "SVIME", "SVIND", "SVINE", "SVING", "SVIRE", "SVIRP", "SVOVL", "SVÆLG", "SVÆRD", "SVÆRM", "SVÆVE", "SWING", "SYDEN", "SYDOM", "SYDPÅ", "SYDUD", "SYGNE", "SYNDE", "SYNES", "SYNGE", "SYNKE", "SYNSK", "SYNÅL", "SYREN", "SYRER", "SYRNE", "SYSLE", "SYTØJ", "SYVER", "SYVTI", "SÆKKE", "SÆLGE", "SÆSON", "SÆTER", "SÆTTE", "SØBAD", "SØDME", "SØGER", "SØJLE", "SØLET", "SØLLE", "SØMIL", "SØMME", "SØREN", "SØRGE", "SØRME", "SØSYG", "SØULK", "SØVEJ", "SÅDAN", "SÅGAR", "SÅPAS", "SÅSOM", "SÅSÆD", "SÅVEL", "TABEL", "TABER", "TAGGE", "TAJGA", "TAKKE", "TAKSI", "TAKST", "TALAR", "TALER", "TALON", "TALTE", "TAMPE", "TANDE", "TANGA", "TANGE", "TANGO", "TANTE", "TAPAS", "TAPET", "TAPIR", "TAPPE", "TARIF", "TAROK", "TAROT", "TASKE", "TASTE", "TATAR", "TATER", "TAVET", "TAVLE", "TAXIE", "TAZET", "TEGNE", "TEINT", "TEIST", "TEJNE", "TEJST", "TEKOP", "TEKST", "TELTE", "TEMPI", "TEMPO", "TENOR", "TEORI", "TERME", "TERPE", "TERRE", "TERTS", "TESKE", "TESTE", "THETA", "THYBO", "TIARA", "TIERE", "TIEST", "TIGER", "TIGGE", "TIKKE", "TILDE", "TILGÅ", "TILSE", "TILSÅ", "TILTE", "TIMER", "TIMES", "TIMID", "TINDE", "TINGE", "TIPPE", "TIRRE", "TISKE", "TISSE", "TITAL", "TITEL", "TITTE", "TIØRE", "TIÅRS", "TJALD", "TJALK", "TJANS", "TJAVS", "TJENE", "TJØRN", "TOAST", "TOBAK", "TOBIS", "TODDY", "TOFTE", "TOLDE", "TOLKE", "TOMAT", "TOMLE", "TOMME", "TONAL", "TONER", "TONIC", "TONSE", "TOPAS", "TOPPE", "TOPTI", "TORSK", "TORSO", "TOTEM", "TOTNE", "TOTTE", "TOTUR", "TOUCH", "TOUGH", "TOÅRS", "TRAGT", "TRAMP", "TRANE", "TRAVL", "TRAWL", "TREER", "TREMA", "TRETI", "TREVL", "TRIAS", "TRICK", "TRIND", "TRINE", "TRIOL", "TRIST", "TRIVI", "TRIØR", "TROFÆ", "TROKÆ", "TROLD", "TROPE", "TRUCK", "TRUMF", "TRUST", "TRÆDE", "TRÆET", "TRÆLS", "TRÆNE", "TRÆSK", "TRÆVL", "TRØJE", "TRØST", "TRÅDE", "TRÅDT", "TUDSE", "TUGTE", "TUKAN", "TULRE", "TUMLE", "TUMOR", "TUMPE", "TUNER", "TURBO", "TURDE", "TUSKE", "TUSSE", "TUTOR", "TUTTE", "TUTTI", "TVEBO", "TVEGE", "TVIVL", "TVÆRE", "TVÆRS", "TVÆRT", "TWEED", "TWEEN", "TWEET", "TWILL", "TWIST", "TYFON", "TYFUS", "TYGGE", "TYKKE", "TYKNE", "TYLVT", "TYNDE", "TYRAN", "TYSSE", "TYVER", "TYVTE", "TÆMME", "TÆNDE", "TÆNKE", "TÆRTE", "TÆSKE", "TÆTNE", "TÆTTE", "TÆVEN", "TØFFE", "TØFLE", "TØJRE", "TØJTE", "TØLTE", "TØMRE", "TØNDE", "TØRIS", "TØRKE", "TØRNE", "TØRST", "TØSET", "TØSNE", "TÅGET", "TÅLED", "TÅRNE", "UANET", "UBLID", "UDBUD", "UDDØD", "UDELT", "UDFRI", "UDHUS", "UDKOG", "UDLYD", "UDLÆG", "UDLØB", "UDLÅN", "UDRIV", "UDRØJ", "UDRÅB", "UDSAT", "UDSPY", "UDSTÅ", "UDSYN", "UDSÆD", "UDTAG", "UDTOG", "UDTUR", "UDVEJ", "UDØBT", "UDØRK", "UDØSE", "UDØVE", "UEGAL", "UENIG", "UFIKS", "UFRED", "UFØDT", "UFØRE", "UGIFT", "UGRÆS", "UHELD", "UHØRT", "UJÆVN", "UKLAR", "UKLOG", "ULAND", "ULAVE", "ULDEN", "ULDET", "ULIGE", "ULYST", "ULÆRD", "ULÆST", "ULØST", "UMAMI", "UMILD", "UNDGÅ", "UNDRE", "UNDSE", "UNDSÅ", "UNGMØ", "UNION", "UNODE", "UNÅDE", "URBAN", "UREDE", "UREDT", "UREEL", "URREM", "URTID", "URUND", "URÆMI", "URØRT", "USAGT", "USAND", "USKET", "USKIK", "USKØN", "USSEL", "USUND", "UTAKT", "UTALT", "UTIDE", "UTING", "UTOPI", "UTRYG", "UTUGT", "UVANE", "UVANT", "UVEJR", "UVORN", "UÆGTE", "UØVET", "VABEL", "VABLE", "VADER", "VAGER", "VAKLE", "VAKTE", "VALEN", "VALGT", "VALID", "VALLE", "VALME", "VALSK", "VALTE", "VALØR", "VANDE", "VANDT", "VANGE", "VANKE", "VANRY", "VANTE", "VARAN", "VARDE", "VARIA", "VARIG", "VARPE", "VARTE", "VARYL", "VASAL", "VASKE", "VATER", "VATRE", "VEDET", "VEDGÅ", "VEGAR", "VEGET", "VEJER", "VEJLE", "VEJRE", "VELAN", "VELAR", "VELIN", "VELÆR", "VEMOD", "VENDE", "VENØS", "VERFE", "VERST", "VERVE", "VESIR", "VETOE", "VIDDE", "VIDEO", "VIDJE", "VIDST", "VIGTE", "VIGØR", "VIKAR", "VIKKE", "VIKLE", "VIKSE", "VILDT", "VILJE", "VILLA", "VILLE", "VIMRE", "VIMSE", "VINCA", "VINGE", "VINKE", "VINYL", "VINØS", "VIOLA", "VIRAK", "VIRAL", "VIRIL", "VIRRE", "VIRUS", "VISER", "VISIR", "VISIT", "VISKE", "VISNE", "VISUM", "VITAL", "VLIES", "VODKA", "VOGTE", "VOILA", "VOKSE", "VOLDE", "VOLTE", "VOLUT", "VORDE", "VORES", "VORTE", "VOTUM", "VOVET", "VOVSE", "VRAGE", "VREDE", "VRIDE", "VRIST", "VRÆLE", "VULST", "VUNDE", "VUPTI", "VÆBNE", "VÆDDE", "VÆDRE", "VÆGRE", "VÆGTE", "VÆGUR", "VÆLGE", "VÆLIG", "VÆLSK", "VÆLTE", "VÆNGE", "VÆNNE", "VÆRDI", "VÆREN", "VÆRFT", "VÆRKE", "VÆRNE", "VÆRRE", "VÆRST", "VÆSEL", "VÆSEN", "VÆTTE", "VØLVE", "VÅBEN", "VÅDTE", "VÅGEN", "VÅGNE", "VÅSET", "WHIST", "XENON", "YACHT", "YATZY", "YDMYG", "YNDER", "YNDIG", "YNGEL", "YNGLE", "YNGRE", "YNGST", "YPPIG", "YUCCA", "ZAPPE", "ZEBRA", "ZELOT", "ZENIT", "ZLOTY", "ZOBEL", "ZOOME", "ZWECK", "ÆGIDE", "ÆKLES", "ÆLDRE", "ÆLDST", "ÆNDER", "ÆNDRE", "ÆRBAR", "ÆRGRE", "ÆRLIG", "ÆRØBO", "ÆRØSK", "ÆSTET", "ÆVRED", "ØBOER", "ØDSEL", "ØDSLE", "ØLBAS", "ØLLET", "ØLVOM", "ØMHED", "ØRIGE", "ØRKEN", "ØRRED", "ØSKEN", "ØSTAT", "ØSTOM", "ØSTPÅ", "ØSTRE", "ØSTUD", "ØVDAG", "ØVRIG", "ÅBNER", "ÅDSEL", "ÅNDET", "ÅNDIG", "ÅRBOG", "ÅRLIG", "ÅRSAG", "ÅSTED" ]
