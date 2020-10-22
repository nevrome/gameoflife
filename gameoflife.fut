import "lib/github.com/athas/matte/colour"
import "lib/github.com/diku-dk/lys/lys"

let replicate 't (n: i64) (x: t): [n]t =
  map (\_ -> x) (0..<n)

let plot (width: i64) (height: i64) (world:[][]bool): [height][width]argb.colour =
  let f j i =
    let is_alive = world[i,j]
    in if is_alive then argb.green else argb.black
  in tabulate_2d height width f

let conways_rules (a: u8) (b: bool): bool = 
  if a < 2 
  then false 
  else if b && ((a == 2) || (a == 3)) 
       then true 
       else if !b && a == 3 
            then true 
            else if a > 3 
                 then false 
                 else false

let shift_array_left 't (x: i64) (y: i64) (as: [][]t): [][]t =
  concat_to x as[1:x,0:y] [as[0,0:y]]

let shift_array_right 't (x: i64) (y: i64) (as: [][]t): [][]t =
  concat_to x [as[(x-1),0:y]] as[0:(x-1),0:y]

let shift_array_top 't (x: i64) (y: i64) (as: [][]t): [][]t =
  shift_array_left y x (transpose as) |> transpose

let shift_array_bottom 't (x: i64) (y: i64) (as: [][]t): [][]t =
  shift_array_right y x (transpose as) |> transpose

let shift_array_top_left 't (x: i64) (y: i64) (as: [][]t): [][]t =
  shift_array_left x y (shift_array_top x y as)

let shift_array_top_right 't (x: i64) (y: i64) (as: [][]t): [][]t =
  shift_array_right x y (shift_array_top x y as)

let shift_array_bottom_left 't (x: i64) (y: i64) (as: [][]t): [][]t =
  shift_array_left x y (shift_array_bottom x y as)

let shift_array_bottom_right 't (x: i64) (y: i64) (as: [][]t): [][]t =
  shift_array_right x y (shift_array_bottom x y as)

let sum_array_4 (as: [][]u8) (bs: [][]u8) (cs: [][]u8) (ds: [][]u8): [][]u8 =
  map4 (\a1 b1 c1 d1 -> map4 (\a2 b2 c2 d2 -> (a2 + b2 + c2 + d2)) a1 b1 c1 d1) as bs cs ds

let bool2u8 (x: bool): u8 = if x then 1 else 0

let bool2u8_array (as: [][]bool): [][]u8 =
  map (\a1 -> map (\a2 -> bool2u8 a2) a1) as

let apply_conways_rules (width: i64) (height: i64) (world: [][]bool): [][]bool =
  let world_u8 = bool2u8_array world
  -- 4 degrees of freedom shifts
  let shift_left = shift_array_left width height world_u8
  let shift_right = shift_array_right width height world_u8
  let shift_top = shift_array_top width height world_u8
  let shift_bottom = shift_array_bottom width height world_u8
  -- 8 degrees of freedom shifts 
  let shift_top_left = shift_array_top_left width height world_u8
  let shift_top_right = shift_array_top_right width height world_u8
  let shift_bottom_left = shift_array_bottom_left width height world_u8
  let shift_bottom_right = shift_array_bottom_right width height world_u8
  -- calculate sums per cell
  let number_of_true_normal = sum_array_4 shift_left shift_right shift_top shift_bottom
  let number_of_true_diagonal = sum_array_4 shift_top_left shift_top_right shift_bottom_left shift_bottom_right
  let number_true_total = map2 (\a1 b1 -> map2 (\a2 b2 -> (a2 + b2)) a1 b1) number_of_true_normal number_of_true_diagonal
  -- finally apply conway rules per cell
  let new_world = map2 (\a1 b1 -> map2 conways_rules a1 b1) number_true_total[0:width,0:height] world[0:width,0:height]
  in new_world

let starting_world_generator (h: i64) (w: i64): [][]bool =
  replicate h (replicate w false) with [9,10] = true with [8,10] = true with [7,10] = true with [9,9] = true with [8,8] = true

type text_content = (i32, i32)
module lys: lys with text_content = text_content = {

  type~ state = {t:f32, h:i64, w:i64, paused:bool, world:[][]bool}

  let init _ h w: state = {t=0, h, w, paused = false, world = starting_world_generator w h}

  let step (s: state) (td: f32) =
    if s.paused 
    then s
    else s with t = s.t + td
           with world = apply_conways_rules s.w s.h s.world
      
  let keydown (key: i32) (s: state) =
    if key == SDLK_SPACE then s with paused = !s.paused
    else s

  let event (e: event) (s: state) =
    match e
    case #step td -> step s td
    case #keydown {key} -> keydown key s
    case _ -> s

  let resize h w (s: state) = s with h = h with w = w with world = starting_world_generator w h

  let render (s: state) = plot s.w s.h s.world
  
  type text_content = text_content
  let text_format () = "FPS: %d\nt: %d"
  let text_colour = const argb.white
  let text_content (fps: f32) (s: state): text_content = (t32 fps, t32 s.t)
  let grab_mouse = false
  
}

