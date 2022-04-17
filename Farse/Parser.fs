/// モナディックパーザ
module Farse.Parser

open System.Text

module Runes =
    let from (s: string) = s.EnumerateRunes() |> Seq.toList

    let string (runes: seq<Rune>) =
        runes |> Seq.map string |> String.concat ""

/// 解析結果である、ある種の木（型 'a ）および
/// 解析によって入力文字列が消費された後の値の対のリスト
/// なお、解析に失敗した場合は空のリストとする
/// 解析結果としてあり得るものはすべてリストに列挙する
type ParseResult<'a> = ('a * string) list

/// 入力文字列を与えられると、型 'a の解析結果を返す関数
type Parser<'a> = string -> ParseResult<'a>

/// 入力文字列を一切消費せずに成功するパーザ
let result (v: 'a) : Parser<'a> = fun input -> [ v, input ]

/// 入力文字列に関わらず常に失敗するパーザ
let zero: Parser<'a> = fun input -> []

/// 非空であれば最初の文字を消費し、解析結果として返すパーザ
/// ただし、入力文字列が空であれば失敗する
let item: Parser<Rune> =
    fun input ->
        match Runes.from input with
        | [] -> []
        | x :: xs -> [ x, Runes.string xs ]

/// 順次解析するモナディックパーザコンビネータ
let bind (p: Parser<'a>) (f: 'a -> Parser<'b>) : Parser<'b> =
    fun input ->
        let pResults = p input

        [ for (pResult, inputAfterP) in pResults do
              let q = f pResult
              yield! q inputAfterP ]

/// 2つの解析結果をマージするパーザコンビネータ
let plus (p: Parser<'a>) (q: Parser<'a>) : Parser<'a> =
    fun input -> List.append (p input) (q input)

let (++) p q = plus p q

/// パーザ用コンピュテーション式
type ParserBuilder() =
    member x.Bind(comp, func) = bind comp func
    member x.Return(value) = result value
    member x.ReturnFrom(value) = value
    member x.Combine(expr1, expr2) = plus expr1 expr2
    member x.Zero() = zero

let parser = ParserBuilder()

/// 2つのパーザを順次実行し、その解析結果を対にして返すアプリカティブパーザコンビネータ
let seq (p: Parser<'a>) (q: Parser<'b>) : Parser<'a * 'b> =
    parser {
        let! v = p
        let! w = q
        return (v, w)
    }

/// 文字に対する述語を受け取り、
/// 入力文字列の先頭の文字がその述語を満たすとき解析に成功するパーザ
/// その際、先頭の文字を消費して解析結果とする
/// 述語を満たさない場合は失敗する
let sat (pred: Rune -> bool) : Parser<Rune> =
    parser {
        let! x = item

        if pred x then
            return x
        else
            return! zero
    }

/// 入力文字列の先頭が指定の文字に一致するとき、その文字を消費して解析に成功する
/// さもなければ解析に失敗する
let rune (x: Rune) : Parser<Rune> = sat ((=) x)

let digit: Parser<Rune> = sat Rune.IsDigit

let lower: Parser<Rune> = sat Rune.IsLower

let upper: Parser<Rune> = sat Rune.IsUpper

let letter: Parser<Rune> = sat Rune.IsLetter

let alphanum: Parser<Rune> = sat Rune.IsLetterOrDigit

/// 文字列（letterの連続）を解析する
let word: Parser<string> =
    let rec word' input =
        let p =
            parser {
                let! x = letter
                let! xs = word'
                return (sprintf "%O%s" x xs)
            }

        (p ++ result "") input

    word'

let runes (s: string) : Parser<string> =
    let rec runes' rs =
        match rs with
        | [] -> result []
        | x :: xs ->
            parser {
                let! _ = rune x
                let! _ = runes' xs
                return x :: xs
            }

    parser {
        let! rs = runes' (Runes.from s)
        return Runes.string rs
    }

let many (p: Parser<'a>) : Parser<'a list> =
    let rec many' input =
        let q =
            parser {
                let! x = p
                let! xs = many'
                return x :: xs
            }

        (q ++ result []) input

    many'

let ident: Parser<string> =
    parser {
        let! x = lower
        let! xs = many alphanum
        return (Runes.string (x :: xs))
    }

let many1 (p: Parser<'a>) : Parser<'a list> =
    parser {
        let! x = p
        let! xs = many p
        return x :: xs
    }

let sepby1 (p: Parser<'a>) (sep: Parser<'sep>) : Parser<'a list> =
    parser {
        let! x = p

        let! xs =
            many
            <| parser {
                let! _ = sep
                let! y = p
                return y
            }

        return x :: xs
    }

let bracket (start: Parser<'start>) (p: Parser<'a>) (finish: Parser<'finish>) : Parser<'a> =
    parser {
        let! _ = start
        let! x = p
        let! _ = finish
        return x
    }

let chainl1 (p: Parser<'a>) (op: Parser<'a -> 'a -> 'a>) : Parser<'a> =
    let rec rest x =
        parser {
            let! f = op
            let! y = p
            return! rest (f x y)
        }
        ++ result x

    parser {
        let! x = p
        return! rest x
    }

let ops (xs: (Parser<'a> * 'b) list) : Parser<'b> =
    let ps =
        [ for (p, op) in xs ->
              parser {
                  let! _ = p
                  return op
              } ]

    List.foldBack (++) ps zero

let nat: Parser<int> =
    let p =
        parser {
            let! x = digit
            return Rune.GetNumericValue x |> int
        }

    let op m n = 10 * m + n

    chainl1 p (result op)

let chainr1 (p: Parser<'a>) (op: Parser<'a -> 'a -> 'a>) : Parser<'a> =
    let rec chainr1' p op input =
        parser {
            let! x = p

            let q =
                parser {
                    let! f = op
                    let! y = chainr1' p op
                    return f x y
                }

            return! q ++ result x
        }
        <| input

    chainr1' p op

let chainl (p: Parser<'a>) (op: Parser<'a -> 'a -> 'a>) (v: 'a) : Parser<'a> = chainl1 p op ++ result v
let chainr (p: Parser<'a>) (op: Parser<'a -> 'a -> 'a>) (v: 'a) : Parser<'a> = chainr1 p op ++ result v

let first (p: Parser<'a>) : Parser<'a> =
    fun input ->
        match p input with
        | [] -> []
        | x :: xs -> [ x ]

let (+++) (p: Parser<'a>) (q: Parser<'a>) : Parser<'a> = first (p ++ q)

let spaces: Parser<unit> =
    parser {
        let! _ = many1 (sat Rune.IsWhiteSpace)
        return ()
    }

let comment: Parser<unit> =
    parser {
        let! _ = runes "--"
        let! _ = many (sat (fun r -> r <> Rune '\n'))
        return ()
    }

let junk: Parser<unit> =
    parser {
        let! _ = many (spaces +++ comment)
        return ()
    }

let parse (p: Parser<'a>) : Parser<'a> =
    parser {
        do! junk
        return! p
    }

let token (p: Parser<'a>) : Parser<'a> =
    parser {
        let! v = p
        do! junk
        return v
    }

let natural: Parser<int> = token nat

let symbol (s: string) : Parser<string> = token (runes s)

let identifier (xs: string list) : Parser<string> =
    token
    <| parser {
        let! x = ident

        if not (xs |> List.contains x) then
            return x
        else
            return! zero
    }

/// λ式
type Expr =
    /// 関数適用
    | App of Expr * Expr
    /// λ抽象化
    | Lam of string * Expr
    /// 局所定義
    | Let of string * Expr * Expr
    /// 変数
    | Var of string

let lambdaExpr: Parser<Expr> =
    let rec expr input =
        chainl1 atom (result (fun e1 e2 -> App(e1, e2))) input

    and atom input = (lam +++ local +++ var +++ paren) input

    and lam input =
        parser {
            let! _ = symbol @"\"
            let! x = variable
            let! _ = symbol "->"
            let! e = expr
            return Lam(x, e)
        }
        <| input

    and local input =
        parser {
            let! _ = symbol "let"
            let! x = variable
            let! _ = symbol "="
            let! e1 = expr
            let! _ = symbol "in"
            let! e2 = expr
            return Let(x, e1, e2)
        }
        <| input

    and var input =
        parser {
            let! x = variable
            return Var x
        }
        <| input

    and paren input =
        bracket (symbol "(") expr (symbol ")") input

    and variable input = identifier [ "let"; "in" ] input

    expr
