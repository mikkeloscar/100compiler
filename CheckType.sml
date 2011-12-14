load "Lexing";
load "Nonstdio";
load "Parser";
load "Lexer";
load "Type";

fun createLexerStream ( is : BasicIO.instream ) =
      Lexing.createLexer ( fn buff => fn n => Nonstdio.buff_input is buff 0 n);

fun check f =
  let
    val lexbuf = createLexerStream (BasicIO.open_in f)
  in
    let 
    val pgm = Parser.Prog Lexer.Token lexbuf
    in
      Type.checkProg pgm
    end
end
