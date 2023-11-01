open Ast


let texture_to_plot_tile textures = function
| WALL -> List.assoc "white_bricks" textures
| RED_WALL -> List.assoc "red_bricks" textures
| DOOR -> List.assoc "door" textures
| LEVEL_END -> List.assoc "level_end" textures
| NOTHING -> failwith "no texture for NOTHING"
| TRANSPARENT_WALL ->  failwith "no texture for TRANSPARENT_WALL"

let float_approx_eq f1 f2 approx = 
    f1 < (f2+.approx) && f1 > (f2-.approx)

let drawRay game ray =
    let windows_info = game.windows_info in 
    let level = Option.get game.level in 
    let textures = game.textures in
    if not (ray.rayTouched) then (failwith "ray not touched") else
    
    let width  = windows_info.drawer3D_width in
    let height = windows_info.drawer3D_height in
    


    let distance = (* fisheye fix *)
        let ca = (level.player.entity.view_angle -. ray.angle) 
                    *. (Float.pi /. 180.0) in
        let ca = 
            if ca < 0. then ca +. 2. *. Float.pi 
            else if ca > (2. *. Float.pi) then ca -. 2. *. Float.pi 
            else ca in
        ray.distance *. cos(ca) in
    let win_step = int_of_float (float_of_int width /. ((ray.angle_max -. ray.angle_min))) in 
    let rect_height = (int_of_float ( float_of_int(height) /. distance)) in
    let rec_start_X = int_of_float ((ray.angle_max -. ray.angle) *. (float_of_int win_step)) in
    let rec_start_Y = height/2 - rect_height/2 in

    let intersect_x = 
        let decimal = Float.round(ray.intersection.x *. 100.) /. 100. in
        fst (Float.modf decimal) in
    let intersect_y = 
        let decimal = Float.round(ray.intersection.y *. 100.) /. 100. in
        fst (Float.modf decimal) in
    let in_texture_intersection = intersect_x +. intersect_y in 

    let tx = 
        let tmp = int_of_float (in_texture_intersection *. 64.) in
        if (ray.angle_vec.x < 0. && float_approx_eq intersect_x 0. 0.01 ) ||
            (ray.angle_vec.y > 0. && intersect_x <> 0.) then
            63-tmp
        else
            tmp
    in

    let wall = level.map.plot.(int_of_float ray.touched_pos.y).(int_of_float ray.touched_pos.x) in
    let texture = texture_to_plot_tile textures wall in

    Sdlrender.copyEx 
        windows_info.render 
        ~texture:texture 
        ~src_rect:(Sdlrect.make ~pos:(tx,0) ~dims:(1,64))
        ~dst_rect:(Sdlrect.make ~pos:(rec_start_X, rec_start_Y) 
                    ~dims:(win_step, rect_height))
        ();
    ();;


let renderHud game = 
    let w = game.windows_info.drawer3D_width in
    let h = game.windows_info.drawer3D_height in
    let position_hud_x = 0 in 
    let position_hud_y = h - h / 5 in  
    let dst_rect = Sdlrect.make 
        ~pos:(position_hud_x,position_hud_y)
        ~dims:(w,h/5) in

    Sdlrender.copyEx
        game.windows_info.render 
        ~texture:(List.assoc "hud" game.textures) 
        ~src_rect:(Sdlrect.make ~pos:(0,0) ~dims:(600,100))
        ~dst_rect: dst_rect
        ~angle:0.
        ();;

let renderPV game level =
    let w = game.windows_info.drawer3D_width in
    let h = game.windows_info.drawer3D_height in
    let position_hud_x = w/5 + 50 in 
    let position_hud_y = h - h/7 - h/25 in  
    let hp = level.player.entity.hp * 100 / level.player.entity.maxHp in 
    let hp= if hp mod 10 <> 0 then 
        hp + (10 - (hp mod 10)) else hp in
    let dst_rect = Sdlrect.make 
        ~pos:(position_hud_x,position_hud_y)
        ~dims:(w/7,h/7) in
    Sdlrender.copyEx 
        game.windows_info.render 
        ~texture:(List.assoc (string_of_int hp) game.texts) 
        ~src_rect:(Sdlrect.make ~pos:(0,0) ~dims:(600,100))
        ~dst_rect: dst_rect
        ~angle:0.
        ();;

let renderArme game = 
    let w = game.windows_info.drawer3D_width in
    let h = game.windows_info.drawer3D_height in
    let position_hud_x = w / 2 - (w / 5 / 2) in 
    let position_hud_y = h - h /5 - h/4 in      
    let dst_rect = Sdlrect.make 
        ~pos:(position_hud_x,position_hud_y)
        ~dims:(w/5, h/4) in
    Sdlrender.copyEx 
        game.windows_info.render 
        ~texture:(List.assoc "arme" game.textures) 
        ~src_rect:(Sdlrect.make ~pos:(0,0) ~dims:(600,100))
        ~dst_rect: dst_rect
        ~angle:0.
        ();;

let renderShotgunBlast game = 
    let w = game.windows_info.drawer3D_width in
    let h = game.windows_info.drawer3D_height in
    let position_hud_x = w / 2 - (w / 5 / 2) in 
    let position_hud_y = h - h/5 - h/5 in      
    let dst_rect = Sdlrect.make 
        ~pos:(position_hud_x,position_hud_y-120)
        ~dims:(w/5,h/5) in
    Sdlrender.copyEx 
        game.windows_info.render 
        ~texture:(List.assoc "shotgun_blast" game.textures) 
        ~src_rect:(Sdlrect.make ~pos:(0,0) ~dims:(600,100))
        ~dst_rect: dst_rect
        ~angle:0.
        ();
;;

let renderReload game part = 
    let w = game.windows_info.drawer3D_width in
    let h = game.windows_info.drawer3D_height in
    let position_hud_x = w / 2 - (w/5/2) in 
    let position_hud_y = h - h/5 - h/4 in     
    let dst_rect = Sdlrect.make 
        ~pos:(position_hud_x,position_hud_y)
        ~dims:(w/5, h/4) in
    Sdlrender.copyEx 
        game.windows_info.render 
        ~texture:(List.assoc ("reload" ^ part) game.textures) 
        ~src_rect:(Sdlrect.make ~pos:(0,0) ~dims:(600,100))
        ~dst_rect: dst_rect
        ~angle:0.
        ();;


let renderEnemy game enemy =
    let windows_info = game.windows_info in 
    let level = Option.get game.level in 
    let textures = game.textures in
    if enemy.in_fov then (

        let text_height = 
            int_of_float (float_of_int windows_info.drawer3D_height *. (1. /. enemy.playerEnemyDistance)) in
        let sy = windows_info.drawer3D_height/2 - text_height/2 in

        let text_width = 
            int_of_float (float_of_int windows_info.drawer3D_width *. (0.5 /. enemy.playerEnemyDistance)) in

        let win_step = 
            int_of_float (float_of_int windows_info.drawer3D_width /. ((level.player.fov *. 2.))) in
        let sx = (windows_info.drawer3D_width/2 + win_step*(int_of_float enemy.diff_angle)) - text_width/2 in
        
        let offsetY = int_of_float (50. /. enemy.playerEnemyDistance) in

        let text = List.assoc "enemy" textures in
        Sdlrender.copyEx 
            windows_info.render 
            ~texture:text
            ~src_rect:(Sdlrect.make ~pos:(0,0) ~dims:(128,128))
            ~dst_rect:(Sdlrect.make ~pos:(sx,sy+offsetY) ~dims:(text_width, text_height)
            ) ()
    )


let render game rays = 
    let windows_info = game.windows_info in 
    let level = Option.get game.level in 
    let textures = game.textures in
    (* Rendering the sky *)
    if level.map.ceiling then (
        let text = List.assoc "sky" textures in
        Sdlrender.copyEx 
            windows_info.render 
            ~texture:text
            ~src_rect:(Sdlrect.make ~pos:(0,0) ~dims:(640-1,480-1))
            ~dst_rect:(Sdlrect.make ~pos:(0,0) 
                ~dims:(windows_info.drawer3D_width, windows_info.drawer3D_height/2))
            ();
    );

    (* Rendering the rays *)
    let rec render_rays_and_enemies rays enemies = 
        match rays, enemies with
        | [], [] -> ()
        | ray :: r1, [] -> drawRay game ray; render_rays_and_enemies r1 []
        | [], t :: r2 -> renderEnemy game t; render_rays_and_enemies [] r2
        | ray :: r1, enemy :: r2 -> 
            if ray.distance +. 0.5 < enemy.playerEnemyDistance then (
                renderEnemy game enemy;
                render_rays_and_enemies rays r2
            ) else (
                drawRay game ray;
                render_rays_and_enemies r1 enemies
            )
    in 

    let enemies = List.map (Entity.computeInfo level level.player) level.enemies in
    let enemies = List.filter (fun e -> e.in_fov) enemies in
    let enemies = List.fast_sort ( fun e1 e2 ->
                if e1.playerEnemyDistance > e2.playerEnemyDistance then -1 else 1) enemies in
    render_rays_and_enemies rays enemies;
    
    renderHud game;
    renderPV game level; 

    (* debut animation de l'arme *)
    let lastHit = level.player.entity.weapon.lastHit in
    let delay = level.player.entity.weapon.delay in
    let now = Unix.gettimeofday () in 
    if now > lastHit +. (delay *. 0.6) then (
        renderArme game
    ) else if now > lastHit +. (delay *. 0.4) then (
        renderReload game "3"
    ) else if now > lastHit +. (delay *. 0.2) then (
        renderReload game "2"
    ) else (
        renderReload game "1";
        if now < lastHit +. (delay *. 0.2) then 
            renderShotgunBlast game; 
    );
    (* fin animation *)
    
    ;;

