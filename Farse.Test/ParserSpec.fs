module Farse.Test

open NUnit.Framework
open FsUnitTyped

open System.Text
open Farse.Parser

let crystal = Rune('\ud83d', '\udd2e')

[<SetUp>]
let Setup () = ()

[<Test>]
let RunesRoundTrips () =
    Runes.string (Runes.from "") |> shouldEqual ""

    Runes.string (Runes.from "abc")
    |> shouldEqual "abc"

    Runes.string (Runes.from "‚ ‚¢‚¤")
    |> shouldEqual "‚ ‚¢‚¤"

[<Test>]
let ResultParser () =
    result () "" |> shouldEqual [ (), "" ]
    result 42 "abc" |> shouldEqual [ 42, "abc" ]

[<Test>]
let ZeroParser () =
    zero "" |> shouldEqual []
    zero "abc" |> shouldEqual []

[<Test>]
let ItemParser () =
    item "" |> shouldEqual []
    item "abc" |> shouldEqual [ Rune 'a', "bc" ]

    item (crystal |> string)
    |> shouldEqual [ crystal, "" ]

[<Test>]
let BindParseC () =
    let p =
        parser {
            let! x = item
            let! y = item
            return (sprintf "%O%O" y x)
        }

    p "" |> shouldEqual []
    p "a" |> shouldEqual []
    p "ab" |> shouldEqual [ "ba", "" ]
    p "abc" |> shouldEqual [ "ba", "c" ]

[<Test>]
let PlusParseC () =
    let p = result (Rune 'x') ++ item
    p "" |> shouldEqual [ Rune 'x', "" ]

    p "a"
    |> shouldEqual [ Rune 'x', "a"
                     Rune 'a', "" ]

    p "ab"
    |> shouldEqual [ Rune 'x', "ab"
                     Rune 'a', "b" ]

[<Test>]
let SeqParseC () =
    let p = seq (result 42) item
    p "" |> shouldEqual []
    p "a" |> shouldEqual [ (42, Rune 'a'), "" ]

    let q = seq item item
    q "a" |> shouldEqual []
    q "ab" |> shouldEqual [ (Rune 'a', Rune 'b'), "" ]

    q "abc"
    |> shouldEqual [ (Rune 'a', Rune 'b'), "c" ]

[<Test>]
let SatParser () =
    let p = sat (fun x -> x = Rune 'x' || x = Rune 'y')

    p "" |> shouldEqual []
    p "a" |> shouldEqual []
    p "ax" |> shouldEqual []
    p "x" |> shouldEqual [ Rune 'x', "" ]
    p "yxz" |> shouldEqual [ Rune 'y', "xz" ]

[<Test>]
let RuneHelperParser () =
    let p =
        parser {
            let! x = lower
            let! y = lower
            return sprintf "%O%O" x y
        }

    p "" |> shouldEqual []
    p "a" |> shouldEqual []
    p "ab" |> shouldEqual [ "ab", "" ]
    p "abcd" |> shouldEqual [ "ab", "cd" ]
    p "aBcd" |> shouldEqual []

[<Test>]
let WordParser () =
    word "" |> shouldEqual [ "", "" ]
    word "4th" |> shouldEqual [ "", "4th" ]

    word "Yes!"
    |> shouldEqual [ "Yes", "!"
                     "Ye", "s!"
                     "Y", "es!"
                     "", "Yes!" ]

[<Test>]
let RunesParser () =
    runes "" "" |> shouldEqual [ "", "" ]
    runes "" "abc" |> shouldEqual [ "", "abc" ]
    runes "ab" "" |> shouldEqual []
    runes "ab" "a" |> shouldEqual []
    runes "ab" "xab" |> shouldEqual []
    runes "ab" "ab" |> shouldEqual [ "ab", "" ]
    runes "ab" "abc" |> shouldEqual [ "ab", "c" ]

[<Test>]
let ManyParseC () =
    many digit "" |> shouldEqual [ [], "" ]
    many digit "abc" |> shouldEqual [ [], "abc" ]

    many digit "1"
    |> shouldEqual [ [ Rune '1' ], ""
                     [], "1" ]

    many digit "123a"
    |> shouldEqual [ [ Rune '1'; Rune '2'; Rune '3' ], "a"
                     [ Rune '1'; Rune '2' ], "3a"
                     [ Rune '1' ], "23a"
                     [], "123a" ]

[<Test>]
let IdentParser () =
    ident "" |> shouldEqual []
    ident "4ab" |> shouldEqual []
    ident "a" |> shouldEqual [ "a", "" ]
    ident "ab" |> shouldEqual [ "ab", ""; "a", "b" ]

    ident "a2b"
    |> shouldEqual [ "a2b", ""
                     "a2", "b"
                     "a", "2b" ]

[<Test>]
let Many1ParseC () =
    many1 digit "" |> shouldEqual []
    many1 digit "abc" |> shouldEqual []

    many1 digit "1"
    |> shouldEqual [ [ Rune '1' ], "" ]

    many1 digit "123a"
    |> shouldEqual [ [ Rune '1'; Rune '2'; Rune '3' ], "a"
                     [ Rune '1'; Rune '2' ], "3a"
                     [ Rune '1' ], "23a" ]

[<Test>]
let IntegerParserByMany1 () =
    let nat =
        parser {
            let! xs = many1 digit
            return xs |> Runes.string |> int
        }

    nat "" |> shouldEqual []
    nat "1" |> shouldEqual [ 1, "" ]

    nat "123a"
    |> shouldEqual [ 123, "a"
                     12, "3a"
                     1, "23a" ]

    let integer =
        parser {
            let! f =
                parser {
                    let! _ = rune (Rune '-')
                    return (-) 0
                }
                ++ result id

            let! n = nat
            return f n
        }

    integer "4" |> shouldEqual [ 4, "" ]
    integer "-4" |> shouldEqual [ -4, "" ]

    let ints =
        parser {
            let! _ = rune (Rune '[')
            let! n = integer

            let! ns =
                many
                <| parser {
                    let! _ = rune (Rune ',')
                    let! x = integer
                    return x
                }

            let! _ = rune (Rune ']')

            return n :: ns
        }

    ints "[1,-42,17]"
    |> shouldEqual [ [ 1; -42; 17 ], "" ]

    let ints' =
        bracket (rune (Rune '[')) (sepby1 integer (rune (Rune ','))) (rune (Rune ']'))

    ints' "[1,-42,17]"
    |> shouldEqual [ [ 1; -42; 17 ], "" ]

    (*
        expr   ::= expr addop factor | factor
        addop  ::= + | -
        factor ::= nat | ( expr )
    *)

    let expr: Parser<int> =
        let rec expr' input =

            parser {
                let! x = factor

                let! fys =
                    many
                    <| parser {
                        let! f = addop
                        let! y = factor
                        return f, y
                    }

                return List.fold (fun s (f, y) -> f s y) x fys
            }
            <| input

        and factor: Parser<int> =
            nat
            ++ bracket (rune (Rune '(')) expr' (rune (Rune(')')))

        and addop: Parser<int -> int -> int> =
            parser {
                let! _ = rune (Rune '+')
                return (+)
            }
            ++ parser {
                let! _ = rune (Rune '-')
                return (-)
            }

        expr'


    expr "1+2-(3+4)"
    |> shouldEqual [ 1 + 2 - (3 + 4), ""
                     1 + 2, "-(3+4)"
                     1, "+2-(3+4)" ]


    let exprByChain: Parser<int> =
        let rec expr' input = chainl1 factor addop input

        and factor: Parser<int> =
            nat
            ++ bracket (rune (Rune '(')) expr' (rune (Rune(')')))

        and addop: Parser<int -> int -> int> =
            ops [ rune (Rune '+'), (+)
                  rune (Rune '-'), (-) ]

        expr'


    exprByChain "1+2-(3+4)"
    |> shouldEqual [ 1 + 2 - (3 + 4), ""
                     1 + 2, "-(3+4)"
                     1, "+2-(3+4)" ]


[<Test>]
let NatParser () =
    nat "" |> shouldEqual []
    nat "1" |> shouldEqual [ 1, "" ]

    nat "123a"
    |> shouldEqual [ 123, "a"
                     12, "3a"
                     1, "23a" ]

[<Test>]
let ExponentialGrammerParser () =
    let expr: Parser<int> =
        let rec expr' input = chainl1 term addop input
        and term input = chainr1 factor expop input

        and factor =
            nat
            ++ bracket (rune (Rune ' ')) expr' (rune (Rune ' '))

        and addop =
            ops [ rune (Rune '+'), (+)
                  rune (Rune '-'), (-) ]

        and expop = ops [ rune (Rune '^'), pown ]

        expr'

    expr "1-3+2^3^2" |> shouldContain (510, "")

[<Test>]
let LambdaExprGrammer () =
    lambdaExpr "" |> shouldEqual []

    lambdaExpr "x" |> shouldContain (Var("x"), "")

    lambdaExpr "myVariable"
    |> shouldContain (Var("myVariable"), "")

    lambdaExpr @"\x -> x"
    |> shouldContain (Lam("x", Var("x")), "")

    lambdaExpr @"\x -> x x"
    |> shouldContain (Lam("x", App(Var("x"), Var("x"))), "")

    lambdaExpr "let x = y in x"
    |> shouldContain (Let("x", Var("y"), Var("x")), "")

    lambdaExpr @"(\x -> \y -> x)"
    |> shouldContain (Lam("x", Lam("y", Var("x"))), "")
