import "lib/github.com/athas/matte/colour"
import "lib/github.com/diku-dk/lys/lys"

let replicate 't (n: i64) (x: t): [n]t =
  map (\_ -> x) (0..<n)

let plot (width: i64) (height: i64) (world:[][]bool): [height][width]argb.colour =
  let f j i =
    let is_alive = world[i,j]
    in if is_alive then argb.green else argb.black
  in tabulate_2d height width f

let bti (x: bool): i8 = if x then 1 else 0

let conwayslogic (width: i64) (height: i64) (world: [][]bool): [][]bool =
  -- 4x shifts
  let shift_left = concat_to width world[1:width,0:height] [world[0,0:height]]
  let shift_right = concat_to width [world[(width-1),0:height]] world[0:(width-1),0:height]
  let t_world = transpose world
  let t_shift_top = concat_to height t_world[1:height,0:width] [t_world[0,0:width]]
  let t_shift_bottom = concat_to height [t_world[(height-1),0:width]] t_world[0:(height-1),0:width]
  let shift_top = transpose t_shift_top
  let shift_bottom = transpose t_shift_bottom
  -- 8x shifts 
  let shift_top_left = concat_to width shift_top[1:width,0:height] [shift_top[0,0:height]]
  let shift_top_right = concat_to width [shift_top[(width-1),0:height]] shift_top[0:(width-1),0:height]
  let shift_bottom_left = concat_to width shift_bottom[1:width,0:height] [shift_bottom[0,0:height]]
  let shift_bottom_right = concat_to width [shift_bottom[(width-1),0:height]] shift_bottom[0:(width-1),0:height]
  -- calculate sums per cell
  let number_of_true_normal = map4 (\a1 b1 c1 d1 -> map4 (\a2 b2 c2 d2 -> (bti a2 + bti b2 + bti c2 + bti d2)) a1 b1 c1 d1) shift_left shift_right shift_top shift_bottom
  let number_of_true_diagonal = map4 (\a1 b1 c1 d1 -> map4 (\a2 b2 c2 d2 -> (bti a2 + bti b2 + bti c2 + bti d2)) a1 b1 c1 d1) shift_top_left shift_top_right shift_bottom_left shift_bottom_right
  let number_true_total = map2 (\a1 b1 -> map2 (\a2 b2 -> (a2 + b2)) a1 b1) number_of_true_normal number_of_true_diagonal
  -- conway
  let new_world = map2 (\a1 b1 -> map2 (\a2 b2 -> if a2 < 2 then false else (if b2 && ((a2 == 2) || (a2 == 3)) then true else (if !b2 && a2 == 3 then true else (if a2 > 3 then false else false)))) a1 b1) number_true_total[0:width,0:height] world[0:width,0:height]
  in new_world

let starting_world_generator (h: i64) (w: i64): [][]bool =
  replicate h (replicate w false) with [9,10] = true with [8,10] = true with [7,10] = true with [9,9] = true with [8,8] = true

type text_content = (i32, i32)
module lys: lys with text_content = text_content = {

  type~ state = {t:f32, h:i64, w:i64, world:[][]bool}

  let init _ h w: state = {t=0, h, w, world = starting_world_generator w h}

  let step (s: state) (td: f32) =
    s with t = s.t + td
      with world = conwayslogic s.w s.h s.world

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

