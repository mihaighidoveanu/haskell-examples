MiniML
======

Se consideră următoarea gramatică reprezentând un fragment al limbajului OCaml

```bnf
<expr>
  ::= <int>
    | "'" <char> "'"
    | "true" | "false"
    | "[" {<expr> ";"}* "]"
    | <var>
    | "if" <expr> "then" <expr> "else" <expr>
    | "fun" <var> "->" <expr>
    | <expr> <expr>
    | "let" <var> "=" <expr> "in" <expr>
    | <expr> <bop> <expr>
    | <uop> <expr>
    | "(" <expr> ")"
    | "(" ")"

<bop>
  ::= "+" | "-" | "*" | "/" | ...
    | "==" | "!=" | "<" | ">" | "<=" | ...
    | "&&" | "||"
    | "::"
    | ":=" | ";"

<uop>
  ::= "-"
    | "not"
    | "List.hd" | "List.tl"
    | "ref" | "!"
    | "print_int" | "print_char"
    | "read_int"


<int> ::= numar intreg
<var> ::= identificator
<char> ::= caracter ASCII (extins)
```

Exemplu
-------

Cu această gramatică putem scrie programe gen:
```ml
  let fact = ref (fun x -> x) in
  fact := (fun n ->
    if n <= 1 then 1 else n * !fact (n - 1)
  ) ;
  print_int (!fact 5)
```

```ml
  let repeat = fun loc -> fun body ->
    let r = ref (fun x -> ()) in
    r := (fun x -> if body loc then () else !r ());
    !r
  in
    let n = read_int () in
    let inp = ref [] in
    repeat (ref n) (fun i ->
      inp := read_int () :: !inp ;
      i := !i - 1 ;
      !i == 0
    ) () ;
    repeat inp (fun l ->
      let x = List.hd !l in
      print_int x ;
      print_char ',' ;
      l := List.tl !l ;
      !l == []
    ) ()
```

Exerciții
---------

1. Limbajul MiniML este un fragment al limbajului OCaml. Folosiți documentația
limbajului OCaml pentru a înțelege semantica fiecărei construcții.  Scrieți
programe de test pentru e exersa fiecare element sintactic și verificați-le în
interpretorul OCaml.

1. Definiți un tip algebric de date care reprezintă termenii definiți de
gramatica de mai sus.

1. Scrieți un interpretor monadic (eventual extinzându-l pe cel dat de la curs)
pentru limbajul MiniML.  Dată fiind un termen corespunzând unei expresii,
interpretorul calculează valoarea corespunzătoare acelei expresii, sau o eroare\
explicănd de ce expresia nu a putut fi evaluată.
De asemeni interpretorul poate citi și afișa întregi în timpul execuției.

1. (Bonus) Scrieți un analizor sintactic (parser) aplicativ / monadic pentru
limbajul MiniML care primește la intrare un șir de caractere reprezentând o
expresie conform gramaticii, și calculează un termen Haskell conform definiției
folosite mai sus.  Folosiți acest analizor pentru a face un interpretor MiniML
cu o interfața asemănătoare interpretorului OCaml:
   * primește o expresie terminată cu `;;`
   * o evaluează și afișează valoarea finală sau o eroare (puteți ignora tipul)
   * așteaptă o nouă expresie
   * iese cu `#quit`

