{
open LevelParser
}

let ignore   = '\010' | '\013' | "\013\010" | " " | "\009" | "\012"

let int = [ '0'-'9' ]+
let float =  [ '0'-'9' ]+ "." [ '0'-'9' ]+


let printable = ['\032' '\033' '\035'-'\038' '\040'-'\255'] (* on ignore le '"' et ''' *)
let atom =  printable | "\\\\" | "\\\'" | "\\n" | "\\t" | "\\b" | "\\r" 
let string = atom | "'" | "\\\"" 


rule main = parse
  | ignore+      { main lexbuf }
  | '"'          { string "" lexbuf }
  | "level"      { LEVEL }
  | "player"     { PLAYER }
  | "pos"        { POS }
  | "view_angle" { VIEW_ANGLE }
  | "hp"       	 { HP }
  | "plot"       { PLOT }
  | "enemies"    { ENEMIES }
  | "entity"      { ENTITY }
  | "map"        { MAP }
  | "ceiling"    { CEILING }
  | "floor"      { FLOOR }
  | "fov"        { FOV }
  | "="          { EQ }
  | "x"          { X }
  | "y"          { Y }
  | ';'			     { SEMICOLON }
  | '{'			     { LBRACKET }
  | '}'			     { RBRACKET }
  | float as f   { FLOAT (float_of_string f) }
  | int as n	   { INT (int_of_string n) }
  | eof					 { EOF }
  | _            { failwith "unexpected character." }

and string buffer = parse
  | '"'         { STRING ( buffer )  }
  | atom as c   { string (buffer ^ c ) lexbuf }
  | eof         { failwith "string not closed" }