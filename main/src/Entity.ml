open Ast

let update_pos game entity collissionAction =

    let level = Option.get game.level in
    let vector_scalar_mult v s =
        {x=v.x *. s; y=v.y *. s} in
    
    let vector_add v1 v2 =
        {x= v1.x +. v2.x; y= v1.y +. v2.y} in

    let player_x = int_of_float entity.pos.x in
    let player_y = int_of_float entity.pos.y in
    let time_step = 0.005 in

    let player_angle = Common.degToRad entity.view_angle in

    let friction = Common.friction_of_floor_tile level.map.floor.(player_y).(player_x) in
    let move_vec = {x=entity.acceleration.x;y=entity.acceleration.y} in
    
    let cos_angle = cos player_angle in
    let sin_angle = sin player_angle in
    
    let accel_vec = {
        x=(move_vec.x *. cos_angle) +. (move_vec.y *. sin_angle);
        y=(move_vec.y *. cos_angle) -. (move_vec.x *. sin_angle)
    } in
    let new_vel = vector_add entity.velocity accel_vec in
    
    let friction_vec = vector_scalar_mult entity.velocity friction in
    entity.velocity.x <- new_vel.x -. friction_vec.x;
    entity.velocity.y <- new_vel.y -. friction_vec.y;

    let new_pos = vector_scalar_mult entity.velocity (time_step) in
    let new_pos = {
        x=Float.round (new_pos.x *. 100.) /. 100.;
        y=Float.round (new_pos.y *. 100.) /. 100.;
    } in

    let new_pos_colision =vector_add entity.pos (vector_scalar_mult new_pos 1.4) in
    let new_pos_int_x = int_of_float new_pos_colision.x in
    let new_pos_int_y = int_of_float new_pos_colision.y in
    if collissionAction && level.map.plot.(new_pos_int_y).(new_pos_int_x) <> NOTHING then (
        Level.touchedBlockAction game new_pos_int_x new_pos_int_y;
    );
    if level.map.plot.(new_pos_int_y).(new_pos_int_x) <> NOTHING then (
        if level.map.plot.(new_pos_int_y).(player_x) = NOTHING then (
            new_pos.x <- 0.;
        ) else if level.map.plot.(player_y).(new_pos_int_x) = NOTHING then (
            new_pos.y <- 0.;
        ) else (
            new_pos.x <- 0.;
            new_pos.y <- 0.;
        )
    );
    entity.pos.x <- entity.pos.x +. new_pos.x;
    entity.pos.y <- entity.pos.y +. new_pos.y;
    
    ();;

let computeInfo level (player:player) enemy =
    let player_entity = player.entity in
    let angle v1 v2 = (
        let dot = (v1.x *. v2.x) +. (v1.y *. v2.y) in
        let n1 = sqrt ((v1.x *. v1.x) +. (v1.y *. v1.y)) in
        let n2 = sqrt ((v2.x *. v2.x) +. (v2.y *. v2.y)) in
        let cos_theta = dot /. (n1 *. n2) in
        let sin_theta = (v1.x *. v2.y -. v1.y *. v2.x) /. (n1 *. n2) in
        (atan2 sin_theta cos_theta) *. 180.0 /. Float.pi ) in

    let dx = (enemy.pos.x -. player_entity.pos.x) in
    let dy = (enemy.pos.y -. player_entity.pos.y) in
    let player_angle_vec = Common.angleAsVec player_entity.view_angle in

    let diff_angle = angle player_angle_vec {x=dx;y=dy} in
    let in_fov = (Float.neg player.fov) < diff_angle && diff_angle <= player.fov in
    let playerEnemyDistance = (
        let x = Float.abs (enemy.pos.x -. player_entity.pos.x) in
        let y = Float.abs (enemy.pos.y -. player_entity.pos.y) in
        sqrt(x *. x +. y *. y)
    ) in 
    let (_,rayDistance,_,_) = (
        let enemy_angle = mod_float (level.player.entity.view_angle -. diff_angle) 360. in
        let enemy_angleVec = Common.angleAsVec enemy_angle in
        Raycasting.raycast_on_angle level enemy_angleVec 
    ) in
    {
        diff_angle=diff_angle;
        in_fov=in_fov;
        playerEnemyDistance=playerEnemyDistance;
        enemy=enemy;
        rayDistance=(rayDistance +. 0.3);
    }

let update_enemy game enemy = 
    let level = Option.get game.level in
    let info = computeInfo level level.player enemy in
    if info.playerEnemyDistance < 10.0 && info.playerEnemyDistance < info.rayDistance then (
        enemy.view_angle <- mod_float (level.player.entity.view_angle -. info.diff_angle +. 180.) 360.;
        if info.playerEnemyDistance <= 1.5 then (
            (* faire reculer l'enemi du joueur si trop pret *)
            enemy.acceleration.x <- 0.0;
            enemy.acceleration.y <- -1.0;
        ) else if info.playerEnemyDistance >= 3. then (  
            (* faire avancer l'enemi vers le joueur si il est trop loin *)
            enemy.acceleration.x <- 0.0;
            enemy.acceleration.y <- 1.0;
        ) else (
            enemy.acceleration.x <- 0.0;
            enemy.acceleration.y <- 0.0;
        );
        
        let hit_prob = Float.max 4. (10. -. 2. *. (Float.max (abs_float level.player.entity.velocity.x) 
                            (abs_float level.player.entity.velocity.y))) in
        if Random.int 100 < (int_of_float hit_prob) then
            level.player.entity.hp <- level.player.entity.hp - 1;
    ) else (
        enemy.acceleration.y <- 0.;
        enemy.acceleration.x <- 0.;
    );
    update_pos game enemy false;;