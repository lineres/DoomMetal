open Ast

let quit() =
    Sdl.quit ();
    Sdlttf.quit ();
    exit 0

let degToRad f = 
    f *.( Float.pi /. 180.)


let angleAsVec view_angle =
    let radians = degToRad view_angle in
    let vec_x = sin radians *. 99. in
    let vec_y = cos radians *. 99. in
    let vec_length = sqrt (vec_x *. vec_x +. vec_y *. vec_y) in
    {
      x = (vec_x /. vec_length); 
      y = (vec_y /. vec_length)
    };;


let floatToInt x = 
    let sub = int_of_float(x) in 
    if (x -. float_of_int(sub) >= 0.5 ) then sub+1 else sub 
;;
let toucheUnMur x y (level : level)=
    match level.map.plot.(floatToInt y).(floatToInt x) with 
    | NOTHING -> false 
    | _ -> true
;;

let arrondir (x : float) (valeur : float ) (arrondit : float) =  (* arrondit pour savoir si on touche quelqu'un -1 -0.6    *)
    x +. 0.35 > arrondit && x -. arrondit < valeur
;;


let friction_of_floor_tile = function
    | NORMAL -> 0.1
    | ICE -> 0.03

let parameters = 
    let args : parameters = {
        debug = false;
        drawer2D = false;
    } in
    Array.fold_left
    (fun args arg -> 
        match arg with
        | "--debug" ->
            {args with debug=true}
        | "--2D" -> {args with drawer2D=true}
        | _ -> args
        )
    args
    Sys.argv;;
    
let make_default_windows_info () : windows_info = (
    let width, height = (500, 500) in
    Sdl.init [`VIDEO];
    at_exit print_newline;
    let window, render =
      Sdlrender.create_window_and_renderer
        ~width ~height
        ~flags:[
            Sdlwindow.Input_Grabbed;
            Sdlwindow.Input_Focus;
            Sdlwindow.Mouse_Focus;
            Sdlwindow.FullScreen_Desktop;
      ]
    in
    ignore (window);

    Sdlrender.set_draw_color render ~rgb:(120,120,120) ~a:255;
    Sdlrender.clear render;
    Sdlmouse.show_cursor ~toggle:false;
    let width, height = Sdlwindow.get_size window in
    let drawer3D_height, drawer3D_width = height, width in
    let drawer2D_height, drawer2D_width = 
        let tmp = min height width in tmp, tmp in

    {
        parameters = parameters;
        window = window;
        render = render;
        height = height;
        width = width ;
        drawer3D_height = drawer3D_height;
        drawer3D_width = drawer3D_width;
        drawer2D_height = drawer2D_height;
        drawer2D_width = drawer2D_width;
    }
)