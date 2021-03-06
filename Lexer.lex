{
 open Lexing;

 exception LexicalError of string * (int * int) (* (message, (line, column)) *)

 val currentLine = ref 1
 val lineStartPos = ref [0]
		    
 fun getPos lexbuf = getLineCol (getLexemeStart lexbuf)
				(!currentLine)
				(!lineStartPos)

 and getLineCol pos line (p1::ps) =
     if pos>=p1 then (line, pos-p1)
     else getLineCol pos (line-1) ps
   | getLineCol pos line [] = raise LexicalError ("",(0,0))

 fun lexerError lexbuf s = 
     raise LexicalError (s, getPos lexbuf)

 fun keyword (s, pos) =
     case s of
      "if"           => Parser.IF pos
    | "else"         => Parser.ELSE pos
    | "int"          => Parser.INT pos
    | "char"	     => Parser.CHAR pos
    | "return"       => Parser.RETURN pos
    | "while"	     => Parser.WHILE pos
    | _              => Parser.ID (s, pos)

}

    rule Token = parse
     [` ` `\t` `\r`]+    { Token lexbuf } (* whitespace *)
   | "/*" ([^`*`] | `*`[^`/`])* "*/"
			 { Token lexbuf } (* comment *)
   | [`\n` `\012`]       { currentLine := !currentLine+1;
                           lineStartPos :=  getLexemeStart lexbuf
			                    :: !lineStartPos;
                           Token lexbuf } (* newlines *)
   | [`0`-`9`]+          { case Int.fromString (getLexeme lexbuf) of
                            NONE   => lexerError lexbuf "Bad integer"
                          | SOME i => Parser.NUM (i, getPos lexbuf) } (* int *)
   | [`a`-`z` `A`-`Z`] [`a`-`z` `A`-`Z` `0`-`9` `_`]*
                         { keyword (getLexeme lexbuf,getPos lexbuf) }
   | `"` ([^`\` `"` `'`] | `\` ([^`a`] | `a`))+ `"`
  			{ case String.fromCString(String.substring(getLexeme lexbuf,
                               1,size(getLexeme lexbuf)-2)) of
			    NONE => lexerError lexbuf "Bad String"
			  | SOME s => Parser.CSTRING (s, getPos lexbuf) } (* string *)
   | `'` ([^`\` `'` `"`] | `\` ([^`a`] | `a`) ) `'`
  			{ case Char.fromString(String.substring(getLexeme lexbuf,
                               1, size(getLexeme lexbuf)-2)) of
  			    NONE   => lexerError lexbuf "Bad char"
  			  | SOME c => Parser.CCHAR (c, getPos lexbuf) } (* char *)
   | `+`                { Parser.PLUS (getPos lexbuf) }
   | `-`                { Parser.MINUS (getPos lexbuf) }
   | `<`                { Parser.LESS (getPos lexbuf) }
   | `=`                { Parser.ASSIGN (getPos lexbuf) }
   | "=="		{ Parser.EQUAL (getPos lexbuf) }
   | `(`                { Parser.LPAR (getPos lexbuf) }
   | `)`                { Parser.RPAR (getPos lexbuf) }
   | `{`		{ Parser.LBRACKET (getPos lexbuf) }
   | `}`		{ Parser.RBRACKET (getPos lexbuf) }
   | `[`		{ Parser.LSBRACKET (getPos lexbuf) }
   | `]`		{ Parser.RSBRACKET (getPos lexbuf) }
   | `*`		{ Parser.REF (getPos lexbuf) }
   | `,`                { Parser.COMMA (getPos lexbuf) }
   | `;`                { Parser.SEMICOLON (getPos lexbuf) }
   | eof                { Parser.EOF (getPos lexbuf) }
   | _                  { lexerError lexbuf "Illegal symbol in input" }

;
