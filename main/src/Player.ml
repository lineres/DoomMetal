open Ast

let shoot level = 
  let enemies_info = List.map (Entity.computeInfo level level.player) level.enemies in
  let enemies_info = List.filter (fun info -> info.in_fov && info.playerEnemyDistance < 10.
                    && -.5. < info.diff_angle && info.diff_angle < 5. 
                    && info.playerEnemyDistance < info.rayDistance
                    ) enemies_info in
  List.iter (fun info ->
      info.enemy.hp <- info.enemy.hp -1; 
    ) enemies_info;
  level.enemies <- List.filter (fun enn -> enn.hp > 0) level.enemies;;

