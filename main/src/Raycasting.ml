open Ast

let raycast_on_angle level rayDir
 =
    let plot = level.map.plot in
    let player_pos = level.player.entity.pos in

    let pos = {x=player_pos.x; y=player_pos.y} in

    let map = {x=float_of_int (int_of_float pos.x); y= float_of_int (int_of_float pos.y)} in
    let sideDist = {x=0.; y=0.} in
    let deltaDist = {
        x= if rayDir.x = 0. then 999. else Float.abs (1. /. rayDir.x);
        y= if rayDir.y = 0. then 999. else Float.abs (1. /. rayDir.y);
    } in
    let step = {x=0.; y=0.} in

    if rayDir.x < 0. then (
        step.x <- (-1.);
        sideDist.x <- (pos.x -. map.x) *. deltaDist.x;
    ) else (
        step.x <- 1.;
        sideDist.x <- (map.x +. 1. -. pos.x) *. deltaDist.x;
    );
    if rayDir.y < 0. then (
        step.y <- (-1.);
        sideDist.y <- (pos.y -. map.y) *. deltaDist.y;
    ) else (
        step.y <- 1.;
        sideDist.y <- (map.y +. 1. -. pos.y) *. deltaDist.y;
    );
    
    let rec aux hit side = (
        if hit then hit, side else
        let side = (
            if sideDist.x < sideDist.y then (
            sideDist.x <- sideDist.x +. deltaDist.x;
                map.x <- map.x +. step.x;
                0
            ) else (
                sideDist.y <- sideDist.y +. deltaDist.y;
                map.y <- map.y +. step.y;
                1
            )
        ) in
        let tile = plot.(int_of_float map.y).(int_of_float map.x) in
        let hit = tile <> NOTHING && tile <> TRANSPARENT_WALL in

        aux hit side;
    ) in
    let (hit, side) = aux false 0 in

    (* distance jusqu'a le mur *)
    let perpWallDist = (
        if side == 0 then (sideDist.x -. deltaDist.x)
        else (sideDist.y -. deltaDist.y);
    ) in
    
    (* coordonnées exactles du points de colision *)
    let intersection = {
        x=(player_pos.x +. (rayDir.x *. perpWallDist));
        y=(player_pos.y +. (rayDir.y *. perpWallDist))
    } in

    (
        hit,
        perpWallDist, 
        map,
        intersection
    );;


let aux_raycast level angle angle_min angle_max angle_step =

    let rayDir = Common.angleAsVec angle in

    let (rayTouched,distance,touched_pos,intersection) 
        = raycast_on_angle level rayDir in

    {
        rayTouched = rayTouched;
        distance = distance; 
        touched_pos = touched_pos;
        intersection = intersection;
        angle = angle;
        angle_vec = rayDir;
        angle_min = angle_min;
        angle_max = angle_max;
        angle_step = angle_step
    };;



let rec raycast_rec level (angle_min:float) (angle_max:float) (step:float) (cur_angle:float) rays =
    let ray = aux_raycast level cur_angle angle_min angle_max step in
    let rays = ray :: rays in
    if cur_angle = angle_max then rays else
    let cur_angle = Float.min angle_max (cur_angle+.step) in
    raycast_rec level angle_min angle_max step cur_angle rays


let raycast level = 
    let angle = level.player.entity.view_angle in
    let fov = level.player.fov in
    raycast_rec level (angle-. fov) (angle+. fov) 0.4 (angle-. fov) []
    ;;




