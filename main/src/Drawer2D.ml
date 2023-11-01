open Ast



let color_of_wall = function
    | WALL -> white
    | RED_WALL -> red
    | TRANSPARENT_WALL -> green
    | DOOR -> green
    | LEVEL_END -> violet
    | _ -> black;;


let render windows_info level rays = 
    let plot = level.map.plot in
    let block_width = (windows_info.drawer2D_width / level.map.width) in
    let block_height = (windows_info.drawer2D_height / level.map.height) in

    let drawRay ray = 
        
        if not ray.rayTouched then () else
        let pos = level.player.entity.pos in
        let posX = int_of_float (float_of_int block_width *. pos.x) in
        let posY = int_of_float (float_of_int block_height *. pos.y) in    
        
        Sdlrender.set_draw_color windows_info.render ~rgb:blue ~a:255 ;
        let rect = Sdlrect.make 
                ~pos:((int_of_float ray.touched_pos.x)*block_width, 
                    (int_of_float ray.touched_pos.y)*block_height)
                ~dims:(block_width, block_height) in
        Sdlrender.fill_rect windows_info.render rect;

        Sdlrender.set_draw_color windows_info.render ~rgb:grey ~a:255 ;
        Sdlrender.draw_line2 
            windows_info.render
            ~p1:(
                posX + int_of_float (ray.angle_vec.x *. 999.), 
                posY + int_of_float (ray.angle_vec.y *. 999.))
            ~p2:(posX, posY);
    in

    let rec drawPlot y x : unit =
        let tile = plot.(y).(x) in
        let color = color_of_wall tile in  

        Sdlrender.set_draw_color windows_info.render ~rgb:color ~a:255 ;
        let rect = Sdlrect.make 
                ~pos:(x*block_width, y*block_height)
                ~dims:(block_width, block_height) in
        Sdlrender.fill_rect windows_info.render rect;
        let (y,x) = if x >= level.map.width-1 then (y+1,0) else (y,x+1) in
        if y >= level.map.height then () else drawPlot y x
    in

    let drawEntity entity color : unit =
        let sz = 10 in
        let x = int_of_float (entity.pos.x *. float_of_int block_width) - sz/2 in
        let y = int_of_float (entity.pos.y *. float_of_int block_height) - sz/2 in
        Sdlrender.set_draw_color windows_info.render ~rgb:color ~a:255 ;
        let rect = Sdlrect.make 
                ~pos:(x, y)
                ~dims:(sz, sz) in
        Sdlrender.fill_rect windows_info.render rect;

        let entity_posX = int_of_float (float_of_int block_width *. entity.pos.x) in
        let entity_posY = int_of_float (float_of_int block_height *. entity.pos.y) in    
        let angle_vec = Common.angleAsVec entity.view_angle in
        Sdlrender.set_draw_color windows_info.render ~rgb:blue ~a:255 ;
        Sdlrender.draw_line2 
            windows_info.render
            ~p1:(
                entity_posX + int_of_float (angle_vec.x *. 100.), 
                entity_posY + int_of_float (angle_vec.y *. 100.))
            ~p2:(entity_posX, entity_posY)
    in

    drawPlot 0 0 ;
    List.iter (fun ray -> drawRay ray) rays;
    drawEntity level.player.entity white;
    List.iter (fun enemy -> drawEntity enemy orange) level.enemies;
();;