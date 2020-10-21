import "lib/github.com/athas/matte/colour"
import "lib/github.com/diku-dk/lys/lys"

let replicate 't (n: i64) (x: t): [n]t =
  map (\_ -> x) (0..<n)

let plot (width: i64) (height: i64) (world:[][]bool): [height][width]argb.colour =
  let f j i =
    let is_alive = world[i,j]
    in if is_alive then argb.green else argb.blue
  in tabulate_2d height width f

let conwayslogic (width: i64) (height: i64) (world: [][]bool): [][]bool =
  let shift_left = concat_to width world[1:width,0:height] [world[0,0:height]]
  let shift_right = concat_to width [world[(width-1),0:height]] world[0:(width-1),0:height]
  let new_world = map3 (\a1 b1 c1 -> map3 (\a2 b2 c2 -> (a2 || b2 || c2)) a1 b1 c1) world[0:width,0:height] shift_left shift_right
  in new_world

let starting_world_generator (h: i64) (w: i64): [][]bool =
  replicate h (replicate w false) with [10,10] = true with [9,10] = true with [10,9] = true

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
  let text_colour = const argb.black
  let text_content (fps: f32) (s: state): text_content = (t32 fps, t32 s.t)
  let grab_mouse = false
  
}

