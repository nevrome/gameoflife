import "lib/github.com/athas/matte/colour"
import "lib/github.com/diku-dk/lys/lys"

let replicate 't (n: i64) (x: t): [n]t =
  map (\_ -> x) (0..<n)

let plot (width: i64) (height: i64) (world:[][]u8): [height][width]argb.colour =
  let f j i =
    let is_alive = world[i,j] > 0
    in if is_alive then argb.green else argb.black
  in tabulate_2d height width f

let bti (x: bool): i8 = if x then 1 else 0

let conways_rules (a: u8) (b: u8): u8 = 
  if a < 2 
  then 0 
  else if (b > 0) && ((a == 2) || (a == 3)) 
       then 1 
       else if b == 0 && a == 3 
            then 1 
            else if a > 3 
                 then 0 
                 else 0

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

let apply_conways_rules (width: i64) (height: i64) (world: [][]u8): [][]u8 =
  -- 4 degrees of freedom shifts
  let shift_left = shift_array_left width height world
  let shift_right = shift_array_right width height world
  let shift_top = shift_array_top width height world
  let shift_bottom = shift_array_bottom width height world
  -- 8 degrees of freedom shifts 
  let shift_top_left = shift_array_top_left width height world
  let shift_top_right = shift_array_top_right width height world
  let shift_bottom_left = shift_array_bottom_left width height world
  let shift_bottom_right = shift_array_bottom_right width height world
  -- calculate sums per cell
  let number_of_true_normal = sum_array_4 shift_left shift_right shift_top shift_bottom
  let number_of_true_diagonal = sum_array_4 shift_top_left shift_top_right shift_bottom_left shift_bottom_right
  let number_true_total = map2 (\a1 b1 -> map2 (\a2 b2 -> (a2 + b2)) a1 b1) number_of_true_normal number_of_true_diagonal
  -- conway
  let new_world = map2 (\a1 b1 -> map2 conways_rules a1 b1) number_true_total[0:width,0:height] world[0:width,0:height]
  in new_world

let starting_world_generator (h: i64) (w: i64): [][]u8 =
  replicate h (replicate w 0) with [9,10] = 1 with [8,10] = 1 with [7,10] = 1 with [9,9] = 1 with [8,8] = 1

type text_content = (i32, i32)
module lys: lys with text_content = text_content = {

  type~ state = {t:f32, h:i64, w:i64, world:[][]u8}

  let init _ h w: state = {t=0, h, w, world = starting_world_generator w h}

  let step (s: state) (td: f32) =
    s with t = s.t + td
      with world = apply_conways_rules s.w s.h s.world

  let event (e: event) (s: state) =
    match e
    case #step td -> step s td
    case _ -> s

  -- see https://github.com/athas/abelian-sandpile/blob/master/sandpile.fut for a solution for the resizing problem
  let resize h w (s: state) = s with h = h with w = w

  let render (s: state) = plot s.w s.h s.world
  
  type text_content = text_content
  let text_format () = "FPS: %d\nt: %d"
  let text_colour = const argb.white
  let text_content (fps: f32) (s: state): text_content = (t32 fps, t32 s.t)
  let grab_mouse = false
  
}

