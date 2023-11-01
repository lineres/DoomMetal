%{
open Ast

let plot_tile_of_int n =     
    match n with
    | 0 -> NOTHING
    | 1 -> WALL
    | 2 -> RED_WALL
    | 3 -> TRANSPARENT_WALL
	| 4 -> DOOR
	| 5 -> LEVEL_END
    | _ -> failwith "unidenfied floor tile";;

let floor_tile_of_int n =     
    match n with
    | 0 -> NORMAL
    | 1 -> ICE
    | _ -> failwith "unidenfied floor tile";;

%}

%token LEVEL EQ PLAYER POS VIEW_ANGLE HP PLOT SEMICOLON LBRACKET RBRACKET ENEMIES ENTITY X Y EOF
%token MAP CEILING FLOOR FOV
%token <int> INT
%token <float> FLOAT
%token <string> STRING

%start level
%type<Ast.level> level
%type<Ast.player> player
%type<Ast.tile list> plotCells

%start strings
%type<(string * string) list> strings

%%

strings:
	| 							{ [] }
	| STRING EQ STRING strings { ($1, $3) :: $4 }

level:
	| 	LEVEL LBRACKET
			PLAYER LBRACKET player RBRACKET
			ENEMIES LBRACKET enemies RBRACKET
			MAP LBRACKET map RBRACKET
		RBRACKET
				{ 
		{ 
			player = $5; 
			enemies = $9;
			map = $13
		}
	 }

player:
	| FOV EQ FLOAT 
	  entity
			{
				{
					fov = $3;
					entity = $4
				}
			}

enemies:
	|  										{ [] }
	| entity enemies { $1 :: $2 }

entity:
	| ENTITY LBRACKET 
		position
	    VIEW_ANGLE EQ FLOAT
	    HP EQ INT
	  RBRACKET
				{
					{
						pos = $3;
						view_angle = $6;
						hp = $9;
						maxHp = $9;
						weapon = { 
							lastHit = 0.;
							delay = 1.0
						};
						velocity = {x=0.;y=0.};
						acceleration = {x=0.;y=0.};
					}
				}


position:
	| POS LBRACKET X EQ FLOAT Y EQ FLOAT RBRACKET		{ {x=$5; y=$8} }


map:
	| 	CEILING EQ INT
		PLOT LBRACKET plotLines RBRACKET 
		FLOOR LBRACKET floorLines RBRACKET 
		{
			let plot = Array.of_list (List.map (Array.of_list) $6) in
			let floor = Array.of_list (List.map (Array.of_list) $10) in
			let height = Array.length plot in
			let width = Array.length plot.(0) in
			if Array.length floor <> height || (Array.length floor.(0) <> width) then
				failwith "plot and floor should have same sizes";
			{
				ceiling = (if $3 = 0 then true else false);
				plot = plot;
				floor = floor;
				height = height;
				width = width;
			}
		}

plotLines:
	| {[]}
	| LBRACKET plotCells RBRACKET plotLines { $2 :: $4 }

plotCells:
	| INT {[plot_tile_of_int $1]}
	| INT SEMICOLON plotCells { plot_tile_of_int $1 :: $3}

floorLines:
	| {[]}
	| LBRACKET floorCells RBRACKET floorLines { $2 :: $4 }

floorCells:
	| INT {[floor_tile_of_int $1]}
	| INT SEMICOLON floorCells { floor_tile_of_int $1 :: $3}