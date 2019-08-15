module ListArrow exposing
    ( ListArrow
    , and
    , arr
    , arrL
    , collapse
    , compose
    , constL
    , convert
    , fanout
    , first
    , identity
    , ifA
    , isA
    , listA
    , map
    , or
    , orElse
    , second
    , split
    , zeroArrow
    )


type alias ListArrow a b =
    a -> List b


map : ListArrow a b -> List a -> List b
map f xs =
    (constL xs |> and f) ()



-- Category instance


identity : ListArrow a a
identity =
    \x -> [ x ]


compose : ListArrow b c -> ListArrow a b -> ListArrow a c
compose g f =
    List.concatMap g << f


and : ListArrow b c -> ListArrow a b -> ListArrow a c
and =
    -- Haskell's (<<<), or (>>>) flipped
    compose



-- Arrow instance


arr : (a -> b) -> ListArrow a b
arr f =
    \x -> [ f x ]


first : ListArrow a b -> ListArrow ( a, c ) ( b, c )
first f =
    \( x1, x2 ) -> List.map (\y1 -> ( y1, x2 )) (f x1)


second : ListArrow a b -> ListArrow ( c, a ) ( c, b )
second f =
    \( x1, x2 ) -> List.map (\y2 -> ( x1, y2 )) (f x2)


split : ListArrow c d -> ListArrow a b -> ListArrow ( a, c ) ( b, d )
split g f =
    -- Haskell's (***) flipped
    \( x1, x2 ) -> List.concatMap (\y1 -> List.map (\y2 -> ( y1, y2 )) (g x2)) (f x1)


fanout : ListArrow a c -> ListArrow a b -> ListArrow a ( b, c )
fanout g f =
    -- Haskell's (&&&) flipped
    \x -> List.concatMap (\y1 -> List.map (\y2 -> ( y1, y2 )) (g x)) (f x)



-- ArrowZero instance


zeroArrow : ListArrow a b
zeroArrow =
    always []



-- ArrowPlus instance


or : ListArrow a b -> ListArrow a b -> ListArrow a b
or g f =
    -- Haskell's (<+>) flipped
    \x -> f x ++ g x



-- ArrowIf instance (partial)


ifA : ListArrow a b -> ListArrow a c -> ListArrow a c -> ListArrow a c
ifA test then_ else_ =
    \x ->
        (case test x of
            [] ->
                else_

            _ ->
                then_
        )
            x


orElse : ListArrow a b -> ListArrow a b -> ListArrow a b
orElse g f =
    -- Flipped from the Haskell version
    \x ->
        case f x of
            [] ->
                g x

            xs ->
                xs



-- ArrowList instance (partial)


arrL : (a -> List b) -> ListArrow a b
arrL =
    Basics.identity


constL : List b -> ListArrow a b
constL =
    always


isA : (a -> Bool) -> ListArrow a a
isA test =
    \x ->
        if test x then
            [ x ]

        else
            []


convert : (List b -> List c) -> ListArrow a b -> ListArrow a c
convert g f =
    -- Haskell's (>>.) flipped
    g << f


collapse : (List b -> c) -> ListArrow a b -> ListArrow a c
collapse g f =
    -- Haskell's (>.) flipped
    f |> convert (\x -> [ g x ])


listA : ListArrow a b -> ListArrow a (List b)
listA f =
    f |> convert (\x -> [ x ])
