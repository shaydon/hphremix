module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Font as Font
import Element.Region as Region
import Html exposing (Html, div)
import Html.Parser as HtmlParser exposing (Attribute, Node(..))
import Html.Parser.Util as HtmlParser
import Http
import ListArrow exposing (..)
import Parser exposing ((|.), (|=), Parser)
import Set


listTopRatedUrl =
    "https://cors-anywhere.herokuapp.com/https://edinburghfestival.list.co.uk/top-rated/"


halfPriceHutUrl =
    "https://cors-anywhere.herokuapp.com/https://tickets.edfringe.com/box-office/virgin-money-half-price-hut"


type Requested a
    = Loading
    | Resolved (Result String a)


type alias Model =
    { topRated : Requested (Dict String Rating)
    , halfPrice : Requested (List Show)
    }


type alias Show =
    { name : String
    , image : String
    , link : String
    , categories : String
    , genres : String
    , venue : Venue
    , time : String
    }


type alias Rating =
    { rank : String
    , reviews : List Review
    }


type alias Review =
    { score : String
    , publication : String
    , link : String
    }


type alias Venue =
    { name : String
    , link : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { topRated = Loading, halfPrice = Loading }
    , Cmd.batch
        [ Http.get
            { url = listTopRatedUrl
            , expect = Http.expectString GotTopRated
            }
        , Http.get
            { url = halfPriceHutUrl
            , expect = Http.expectString GotHalfPrice
            }
        ]
    )


type Msg
    = GotTopRated (Result Http.Error String)
    | GotHalfPrice (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        model_ =
            case msg of
                GotTopRated result ->
                    { model
                        | topRated =
                            processResponse result
                                |> Result.map getTopRatedDict
                                |> Resolved
                    }

                GotHalfPrice result ->
                    { model
                        | halfPrice =
                            processResponse result
                                |> Result.map getHalfPriceList
                                |> Resolved
                    }
    in
    ( model_, Cmd.none )


processResponse : Result Http.Error String -> Result String (List HtmlParser.Node)
processResponse result =
    Result.mapError (always "LOAD FAILURE") result
        |> Result.andThen
            (Result.mapError (always "PARSE ERROR") << runHtmlDocParser)


getTopRatedDict : List Node -> Dict String Rating
getTopRatedDict xs =
    xs
        |> ListArrow.map
            (deep isTopRatedList
                |> and getChildren
                |> convert paired
                |> and (getTopRatedShowRating |> split getTopRatedShowDetails)
                |> and (arr (\( x, ( y, z ) ) -> ( y, { rank = x, reviews = z } )))
            )
        |> Dict.fromList


paired : List a -> List ( a, a )
paired xs =
    pairedHelper [] xs |> List.reverse


pairedHelper : List ( a, a ) -> List a -> List ( a, a )
pairedHelper acc xs =
    case xs of
        x1 :: x2 :: xs_ ->
            pairedHelper (( x1, x2 ) :: acc) xs_

        _ ->
            acc


isTopRatedList : HtmlArrow Node
isTopRatedList =
    hasAttr ( "class", "everyReview currentYear" )


getTopRatedShowRating : HtmlArrow String
getTopRatedShowRating =
    getChildren |> and getText


getTopRatedShowDetails : HtmlArrow ( String, List Review )
getTopRatedShowDetails =
    getChildren
        |> and
            (getTopRatedShowName
                |> fanout
                    (listA getTopRatedShowReview)
            )


getTopRatedShowName : HtmlArrow String
getTopRatedShowName =
    getChildren |> and (hasName "a") |> slash getText


getTopRatedShowReview : HtmlArrow Review
getTopRatedShowReview =
    getChildren
        |> and (hasName "span")
        |> and
            ((getChildren |> and (hasName "img") |> and (getAttrValue "alt"))
                |> fanout
                    (getChildren
                        |> and (hasName "a")
                        |> and
                            (getAttrValue "href"
                                |> fanout
                                    (getChildren
                                        |> and
                                            (getText
                                                |> orElse
                                                    (hasName "strong"
                                                        |> slash getText
                                                    )
                                            )
                                    )
                            )
                    )
            )
        |> and
            (arr
                (\( score, ( link, publication ) ) ->
                    { score = score
                    , link = link
                    , publication = publication
                    }
                )
            )


getHalfPriceList : List Node -> List Show
getHalfPriceList =
    ListArrow.map
        (deep isHalfPriceList
            |> and getGrandchildren
            |> slash
                (getHalfPriceShowImage
                    |> fanout
                        getHalfPriceShowDetails
                )
            |> and
                (arr
                    (\( image, { name, link, categories, genres, venue, time } ) ->
                        { image = image
                        , name = name
                        , link = link
                        , categories = categories
                        , genres = genres
                        , venue = venue
                        , time = time
                        }
                    )
                )
        )


isHalfPriceList : HtmlArrow Node
isHalfPriceList =
    hasAttr ( "id", "search-results" )


getHalfPriceShowImage : HtmlArrow String
getHalfPriceShowImage =
    getChildren
        |> and
            (hasAttr ( "class", "col-xs-4 col-sm-3 col-md-3 event-meta" ))
        |> slash (hasName "a")
        |> slash (hasName "img")
        |> and (getAttrValue "src")


getHalfPriceShowDetails :
    HtmlArrow
        { name : String
        , link : String
        , categories : String
        , genres : String
        , venue : Venue
        , time : String
        }
getHalfPriceShowDetails =
    getChildren
        |> and
            (hasAttr
                ( "class", "col-xs-8 col-sm-9 col-md-9 event-details" )
            )
        |> and
            (getHalfPriceShowNameAndLink
                |> fanout
                    getHalfPriceShowCategoriesAndGenres
                |> fanout getHalfPriceShowVenueAndTime
            )
        |> and
            (arr
                (\( ( ( name, link ), ( categories, genres ) ), ( venue, time ) ) ->
                    { name = name
                    , link = link
                    , categories = categories
                    , genres = genres
                    , venue = venue
                    , time = time
                    }
                )
            )


getHalfPriceShowNameAndLink : HtmlArrow ( String, String )
getHalfPriceShowNameAndLink =
    getChildren
        |> and (hasName "h3")
        |> slash (hasName "a")
        |> and
            ((getChildren |> and getText)
                |> fanout
                    (getAttrValue "href")
            )


getHalfPriceShowCategoriesAndGenres : HtmlArrow ( String, String )
getHalfPriceShowCategoriesAndGenres =
    getChildren
        |> and (hasName "h4")
        |> and
            ((getChildren
                |> and getText
                |> collapse (String.trim << String.concat)
             )
                |> fanout
                    (getChildren |> and (hasName "span") |> slash getText)
            )


getHalfPriceShowVenueAndTime : HtmlArrow ( Venue, String )
getHalfPriceShowVenueAndTime =
    getChildren
        |> and (hasName "ul")
        |> and
            ((getChildren
                |> and (hasAttr ( "title", "Venue" ))
                |> slash (hasName "a")
                |> and
                    ((getChildren |> and getText)
                        |> fanout
                            (getAttrValue "href")
                    )
             )
                |> fanout
                    (getChildren
                        |> and (hasAttr ( "title", "Time" ))
                        |> slash getText
                        |> collapse (String.trim << String.concat)
                    )
            )
        |> and (arr (\( ( name, link ), time ) -> ( { name = name, link = link }, time )))


hasName : String -> HtmlArrow Node
hasName name =
    isA
        (\node ->
            case node of
                HtmlParser.Element tag _ _ ->
                    tag == name

                _ ->
                    False
        )


hasAttr : Attribute -> HtmlArrow Node
hasAttr attr =
    isA
        (\node ->
            case node of
                HtmlParser.Element _ attrs _ ->
                    List.member attr attrs

                _ ->
                    False
        )


view : Model -> Html Msg
view { topRated, halfPrice } =
    let
        scaled =
            round << modular 32 1.25

        green =
            rgb 0 200 0
    in
    layout [] <|
        column [ width fill, padding 10, spacing 20, Font.size (scaled 1) ]
            (case ( topRated, halfPrice ) of
                ( Loading, _ ) ->
                    [ text "Loading..." ]

                ( _, Loading ) ->
                    [ text "Loading..." ]

                ( Resolved (Ok topRated_), Resolved (Ok halfPrice_) ) ->
                    List.map
                        (\show ->
                            let
                                rating =
                                    Dict.get show.name topRated_
                                        |> Maybe.withDefault
                                            { rank = "Unrated"
                                            , reviews = []
                                            }
                            in
                            row [ width fill, spacing 5 ]
                                [ column [ width (px 250), alignTop ]
                                    [ image [ centerX ] { src = show.image, description = show.name } ]
                                , column [ width fill, spacing 20, alignTop ]
                                    [ row [ width fill ]
                                        [ textColumn [ width fill, spacing 5 ]
                                            [ paragraph []
                                                [ newTabLink [ Font.size (scaled 3) ]
                                                    { url = "https://tickets.edfringe.com" ++ show.link
                                                    , label = text show.name
                                                    }
                                                ]
                                            , paragraph []
                                                [ el [ Font.size (scaled 2) ] (text show.categories)
                                                , el [ Font.size (scaled 1) ] (text (" " ++ show.genres))
                                                ]
                                            ]
                                        ]
                                    , row [ width fill ]
                                        [ textColumn [ width fill, spacing 5 ]
                                            [ paragraph []
                                                [ newTabLink [ Font.size (scaled 2) ]
                                                    { url = "https://tickets.edfringe.com" ++ show.venue.link
                                                    , label = text show.venue.name
                                                    }
                                                ]
                                            , paragraph []
                                                [ el [ Font.size (scaled 1) ] (text show.time) ]
                                            ]
                                        ]
                                    , row [ width fill ]
                                        [ textColumn [ width fill, spacing 5 ]
                                            [ paragraph []
                                                [ el [ Font.size (scaled 2) ] (text rating.rank) ]
                                            , textColumn [ width fill, spacing 5, Font.color green ]
                                                (List.map
                                                    (\review ->
                                                        paragraph []
                                                            [ newTabLink [ Font.size (scaled 2) ]
                                                                { url =
                                                                    if String.startsWith "/" review.link then
                                                                        "https://edinburghfestival.list.co.uk" ++ review.link

                                                                    else
                                                                        review.link
                                                                , label = text (review.score ++ " " ++ review.publication)
                                                                }
                                                            ]
                                                    )
                                                    rating.reviews
                                                )
                                            ]
                                        ]
                                    ]
                                ]
                        )
                        halfPrice_

                _ ->
                    [ text "ERROR" ]
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


runHtmlDocParser : String -> Result (List Parser.DeadEnd) (List HtmlParser.Node)
runHtmlDocParser text_ =
    Parser.run
        (Parser.succeed Basics.identity
            |. docTypeParser
            |= restParser
        )
        text_
        |> Result.andThen HtmlParser.run


docTypeParser : Parser ()
docTypeParser =
    Parser.symbol "<!DOCTYPE html>"


restParser : Parser String
restParser =
    Parser.getChompedString <|
        Parser.chompWhile (always True)


isText : HtmlArrow Node
isText =
    isA
        (\node ->
            case node of
                Text _ ->
                    True

                _ ->
                    False
        )


getText : HtmlArrow String
getText node =
    case node of
        Text x ->
            [ x ]

        _ ->
            []


getChildren : HtmlArrow Node
getChildren node =
    case node of
        Element _ _ xs ->
            xs

        _ ->
            []


getGrandchildren : HtmlArrow Node
getGrandchildren =
    getChildren |> and getChildren


getAttrValue : String -> HtmlArrow String
getAttrValue name node =
    case node of
        Element _ attrs _ ->
            [ Maybe.withDefault "" (assocGet name attrs) ]

        _ ->
            [ "" ]


assocGet : a -> List ( a, b ) -> Maybe b
assocGet x pairs =
    case List.filter (\( x_, _ ) -> x == x_) pairs of
        ( _, y_ ) :: _ ->
            Just y_

        _ ->
            Nothing


slash : HtmlArrow a -> HtmlArrow Node -> HtmlArrow a
slash g f =
    -- Haskell's (/>) flipped
    f |> and getChildren |> and g


doubleSlash : HtmlArrow a -> HtmlArrow Node -> HtmlArrow a
doubleSlash g f =
    -- Haskell's (/>>) flipped
    f |> and getChildren |> and (deep g)


deep : HtmlArrow a -> HtmlArrow a
deep f =
    -- Lamda to add required laziness!
    \x -> (f |> orElse (getChildren |> and (deep f))) x


type alias HtmlArrow a =
    ListArrow Node a
