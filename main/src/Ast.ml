type tile = 
  | NOTHING
  | WALL
  | RED_WALL
  | TRANSPARENT_WALL
  | DOOR
  | LEVEL_END

type floor_tile = 
  | NORMAL
  | ICE

let friction_of_floor_tile = function
  | NORMAL -> 0.8
  | ICE -> 0.1

type position = { mutable x : float; mutable y : float }

type weapon = {
  mutable lastHit: float;
  mutable delay : float;
}

type entity = {
  pos : position;
  weapon: weapon;
  mutable view_angle : float;
  mutable hp : int;
  maxHp : int;
  velocity : position;
  acceleration : position;
}

type player = {
  mutable fov : float;
  entity : entity
}

type enemy = entity

type map = {
  ceiling : bool;
  plot : tile array array;
  floor : floor_tile array array;
  height : int;
  width : int;
}

type level = { 
  player : player; 
  mutable enemies : enemy list;
  map : map
}

type level_state = MAIN_MENU | PLAYING | LEVEL_FINISHED | DIED

type parameters =  {
  debug : bool;
  drawer2D : bool
}

type windows_info = {
    parameters : parameters;
    window : Sdlwindow.t;
    render : Sdltype.renderer;
    height : int ;
    width : int ;
    drawer3D_height : int;
    drawer3D_width : int;
    drawer2D_height : int;
    drawer2D_width : int;
}

type ray = {
  rayTouched : bool;
  distance: float; 
  touched_pos : position;
  intersection : position;
  angle : float;
  angle_vec : position;
  angle_min : float;
  angle_max : float;
  angle_step : float
}

type enemy_render_info = {
  enemy : enemy; 
  diff_angle : float; 
  in_fov : bool; 
  playerEnemyDistance : float; 
  rayDistance : float; 
}

type game = {
  mutable state : level_state;
  mutable level : level option;
  windows_info : windows_info;
  textures : (string * Sdltexture.t) list;
  texts : (string * Sdltexture.t) list;
  mutable selected_level : int;
  nb_levels : int;
}

let white = 255,255,255;;
let black = 0,0,0;;
let red = 255,0,0;;
let orange = 255,165,0;;
let blue = 0,0,255;;
let grey = 105,105,105;;
let green = 0,255,0;;
let violet = 255,0,255;;