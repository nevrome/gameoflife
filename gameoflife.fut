import "lib/github.com/athas/matte/colour"

let replicate 't (n: i64) (x: t): [n]t =
  map (\_ -> x) (0..<n)

let main (t: f32) (width: i64) (height: i64) (world:[][]bool): [height][width]argb.colour =
  let f j i =
    let is_alive = world[i,j]
    in if is_alive then argb.green else argb.blue
  in tabulate_2d height width f

import "lib/github.com/diku-dk/lys/lys"

module lys: lys with text_content = i32 = {

  type text_content = i32
  let text_format () = "FPS: %d"
  let text_colour _ = argb.black
  let text_content fps _ = t32 fps
  let grab_mouse = false

  type~ state = {t:f32, h:i64, w:i64, world:[][]bool}

  -- let world_start = replicate 2000i64 (replicate 2000i64 true)

  let init _ h w: state = {t=0, h, w, world = replicate w (replicate h true)}

  let event (e: event) (s: state) =
    match e
    case #step td -> 
      s with t = s.t + td
        with world = if s.t < 5
                     then replicate s.w (replicate s.h true)
                     else replicate s.w (replicate s.h false)
    case _ -> s

  let resize h w (s: state) = s with h = h with w = w

  let render (s: state) = main s.t s.w s.h s.world
}

-- idea: store whole grid in state -> world is true
