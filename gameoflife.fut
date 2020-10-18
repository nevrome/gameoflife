import "lib/github.com/athas/matte/colour"

let main (t: f32) (width: i64) (height: i64) (alive:[][]i64): [height][width]argb.colour =
  let f j i =
    let is_alive = reduce (||) false (map (\x -> i == x[0] && j == x[1]) alive)
    in if is_alive then argb.green else argb.blue
  in tabulate_2d height width f

import "lib/github.com/diku-dk/lys/lys"

module lys: lys with text_content = i32 = {

  type text_content = i32
  let text_format () = "FPS: %d"
  let text_colour _ = argb.black
  let text_content fps _ = t32 fps
  let grab_mouse = false

  type~ state = {t:f32, h:i64, w:i64, alive:[][]i64}

  let init _ h w: state = {t=0, h, w, alive=[[100,100], [101,101], [100,101], [101,100]]}

  let event (e: event) (s: state) =
    match e
    case #step td -> s with t = s.t + td
    case _ -> s

  let resize h w (s: state) = s with h = h with w = w

  let render (s: state) = main s.t s.w s.h s.alive
}

