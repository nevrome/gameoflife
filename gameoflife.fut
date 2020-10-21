import "lib/github.com/athas/matte/colour"

let replicate 't (n: i64) (x: t): [n]t =
  map (\_ -> x) (0..<n)

let plot (t: f32) (width: i64) (height: i64) (world:[][]bool): [height][width]argb.colour =
  let f j i =
    let is_alive = world[i,j]
    in if is_alive then argb.green else argb.blue
  in tabulate_2d height width f

let conwayslogic (height: i64) (width: i64) (world: [][]bool): [][]bool =
  let new_world = map2 (\a b -> map2 (\c d -> d) a b) world[0:width,0:height] world[0:width,0:height]
  in new_world

let starting_world_generator (h: i64) (w: i64): [][]bool =
  replicate h (replicate w false) with [10,10] = true with [9,10] = true with [10,9] = true

import "lib/github.com/diku-dk/lys/lys"

module lys: lys with text_content = i32 = {

  type text_content = i32
  let text_format () = "FPS: %d"
  let text_colour _ = argb.black
  let text_content fps _ = t32 fps
  let grab_mouse = false

  type~ state = {t:f32, h:i64, w:i64, world:[][]bool}

  let init _ h w: state = {t=0, h, w, world = starting_world_generator w h}

  let event (e: event) (s: state) =
    match e
    case #step td ->
      s with t = s.t + td
        with world = conwayslogic s.h s.w s.world
    case _ -> s

  let resize h w (s: state) = s with h = h with w = w

  let render (s: state) = plot s.t s.w s.h s.world
}

-- idea: store whole grid in state -> world is true
