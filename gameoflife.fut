import "lib/github.com/athas/matte/colour"
import "lib/github.com/diku-dk/lys/lys"

-- #### general helper functions ####

let replicate 't (n: i64) (x: t): [n]t =
  map (\_ -> x) (0..<n)

let bool2u8 (x: bool): u8 = if x then 1 else 0

let bool2u8_array (as: [][]bool): [][]u8 =
  map (\a1 -> map (\a2 -> bool2u8 a2) a1) as

-- #### world array operations ####

let starting_world_generator (h: i64) (w: i64): [][]bool =
  replicate h (replicate w false) with [9,10] = true with [8,10] = true with [7,10] = true with [9,9] = true with [8,8] = true

let mouse_world_generator (h: i64) (w: i64) (x: i64) (y: i64): [][]bool =
  replicate h (replicate w false) with [y,x] = true with [y+1,x+1] = true with [y+1,x] = true with [y,x+1] = true

let shift_array_left 't (x: i64) (y: i64) (as: [][]t) (d: i64): [][]t =
  concat_to x as[d:x,0:y] as[0:d,0:y]

let shift_array_right 't (x: i64) (y: i64) (as: [][]t) (d: i64): [][]t =
  concat_to x as[(x-d):x,0:y] as[0:(x-d),0:y]

let shift_array_top 't (x: i64) (y: i64) (as: [][]t) (d: i64): [][]t =
  shift_array_left y x (transpose as) d |> transpose

let shift_array_bottom 't (x: i64) (y: i64) (as: [][]t) (d: i64): [][]t =
  shift_array_right y x (transpose as) d |> transpose

let shift_array_top_left 't (x: i64) (y: i64) (as: [][]t) (d: i64): [][]t =
  shift_array_left x y (shift_array_top x y as d) d

let shift_array_top_right 't (x: i64) (y: i64) (as: [][]t) (d: i64): [][]t =
  shift_array_right x y (shift_array_top x y as d) d

let shift_array_bottom_left 't (x: i64) (y: i64) (as: [][]t) (d: i64): [][]t =
  shift_array_left x y (shift_array_bottom x y as d) d

let shift_array_bottom_right 't (x: i64) (y: i64) (as: [][]t) (d: i64): [][]t =
  shift_array_right x y (shift_array_bottom x y as d) d

let sum_array_4 (as: [][]u8) (bs: [][]u8) (cs: [][]u8) (ds: [][]u8): [][]u8 =
  map4 (\a1 b1 c1 d1 -> map4 (\a2 b2 c2 d2 -> (a2 + b2 + c2 + d2)) a1 b1 c1 d1) as bs cs ds

-- #### game logic ####

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

let change_world (width: i64) (height: i64) (mouse_activated: bool) (mouse: (i64, i64)) (world: [][]bool): [][]bool =
  -- apply mouse input
  let mouse_world = mouse_world_generator width height mouse.0 mouse.1
  let edited_world = 
    if mouse_activated 
    then map2 (\a1 b1 -> map2 (\a2 b2 -> a2 || b2) a1 b1) mouse_world[0:width,0:height] world[0:width,0:height]
    else world
  -- create u8 world for summing
  let world_u8 = bool2u8_array edited_world
  -- 4 degrees of freedom shifts
  let shift_left = shift_array_left width height world_u8 1
  let shift_right = shift_array_right width height world_u8 1
  let shift_top = shift_array_top width height world_u8 1
  let shift_bottom = shift_array_bottom width height world_u8 1
  -- 8 degrees of freedom shifts 
  let shift_top_left = shift_array_top_left width height world_u8 1
  let shift_top_right = shift_array_top_right width height world_u8 1
  let shift_bottom_left = shift_array_bottom_left width height world_u8 1
  let shift_bottom_right = shift_array_bottom_right width height world_u8 1
  -- calculate sums per cell
  let number_of_true_normal = sum_array_4 shift_left shift_right shift_top shift_bottom
  let number_of_true_diagonal = sum_array_4 shift_top_left shift_top_right shift_bottom_left shift_bottom_right
  let number_true_total = map2 (\a1 b1 -> map2 (\a2 b2 -> (a2 + b2)) a1 b1) number_of_true_normal number_of_true_diagonal
  -- finally apply conway rules per cell
  let new_world = map2 (\a1 b1 -> map2 conways_rules a1 b1) number_true_total[0:width,0:height] edited_world[0:width,0:height]
  in new_world

-- #### user interface with lys ####
-- most of this code is adapted from https://github.com/diku-dk/lys and https://github.com/athas/abelian-sandpile

let screen_point_to_world_point ((centre_x, centre_y): (f32,f32)) (s: f32)
                                ((sw,sh): (i64,i64)) ((ww,wh): (i64,i64))
                                ((x,y): (i32,i32)) =
  let x' = i32.f32 ((centre_x + s * (f32.i32 (x-(i32.i64 ww)/2) / f32.i64 sw)) * f32.i64 ww)
  let y' = i32.f32 ((centre_y + s * (f32.i32 (y-(i32.i64 wh)/2) / f32.i64 sh)) * f32.i64 wh)
  in (x', y')

let screen_point_to_world_point_64 ((centre_x, centre_y): (f32,f32)) (s: f32)
                                ((sw,sh): (i64,i64)) ((ww,wh): (i64,i64))
                                ((x,y): (i64,i64)) =
  let x' = i64.f32 ((centre_x + s * (f32.i64 (x-ww/2) / f32.i64 sw)) * f32.i64 ww)
  let y' = i64.f32 ((centre_y + s * (f32.i64 (y-wh/2) / f32.i64 sh)) * f32.i64 wh)
  in (x', y')

module zoom_wrapper (M: lys) : lys with text_content = M.text_content = {
  type~ state = { inner: M.state
                , centre: (f32, f32)
                , scale: f32
                , width: i64
                , height: i64
                }

  type text_content = M.text_content

  let init seed h w : state = { inner = M.init seed h w
                              , centre = (0.5, 0.5)
                              , scale = 1
                              , width = w
                              , height = h }

  let zoom dy (s : state) =
    s with scale = f32.min 1 (s.scale * (0.99**(f32.i32 dy * 10)))

  let event (e: event) (s: state) =
    match e
    case #mouse {buttons, x, y} ->
      let (x, y) = screen_point_to_world_point s.centre s.scale
                   (s.width, s.height) (s.width, s.height) (x,y)
      in s with inner = M.event (#mouse {buttons, x, y}) s.inner
    case #wheel {dx=_, dy} ->
      zoom dy s with inner = M.event e s.inner
    case e ->
      s with inner = M.event e s.inner

  let resize h w (s: state) = s with inner = M.resize h w s.inner
                                with width = w
                                with height = h

  let render (s: state) =
    let screen = M.render s.inner
    let pixel y x =
      let (x',y') = screen_point_to_world_point_64 s.centre s.scale
                    (s.width, s.height) (s.width, s.height) (x,y)
      in screen[y', x']
    in tabulate_2d s.height s.width pixel

  let grab_mouse = M.grab_mouse
  let text_format = M.text_format
  let text_content fps (s: state) = M.text_content fps s.inner
  let text_colour (s: state) = M.text_colour s.inner
}

let plot (width: i64) (height: i64) (world:[][]bool): [height][width]argb.colour =
  let f j i =
    let is_alive = world[i,j]
    in if is_alive then argb.black else argb.white
  in tabulate_2d height width f

type text_content = (i32, i32, i32)
module lys: lys with text_content = text_content = zoom_wrapper {

  type~ state = {
    t:i32,
    h:i64, 
    w:i64, 
    paused:bool,
    mouse_activated:bool,
    mouse: (i64, i64),
    world:[][]bool,
    numer_of_steps:i64,
    speed:i32
  }

  let init _ h w: state = {
    t=0, 
    h,
    w,
    paused = false,
    mouse_activated = false,
    mouse = (0,0),
    world = starting_world_generator w h,
    numer_of_steps=0,
    speed=5
  }

  let step (s: state) =
    s with t = s.t + 1
      with numer_of_steps = s.numer_of_steps + 1
      with world = change_world s.w s.h s.mouse_activated s.mouse s.world
      
  let keydown (key: i32) (s: state) =
    if key == SDLK_SPACE then s with paused = !s.paused
    else if key == SDLK_LEFT then s with world = shift_array_right s.w s.h s.world 10
    else if key == SDLK_RIGHT then s with world = shift_array_left s.w s.h s.world 10
    else if key == SDLK_UP then s with world = shift_array_bottom s.w s.h s.world 10
    else if key == SDLK_DOWN then s with world = shift_array_top s.w s.h s.world 10
    else if key == SDLK_PLUS then if s.speed == 10 
                                  then s with speed = 1
                                  else s with speed = s.speed + 1
    else if key == SDLK_MINUS then if s.speed == 1 
                                   then s with speed = 10
                                   else s with speed = s.speed - 1                              
    else s

  let event (e: event) (s: state) =
    match e
    case #step -> -- case #step td -> 
      if s.paused
      then s
      else if s.t %% (11 - s.speed) == 0
           then step s
           else s with t = s.t + 1
    case #keydown {key} -> keydown key s
    case #mouse {buttons, x, y} -> 
      if (buttons != 0) && (i64.i32 y < s.h) && (i64.i32 x < s.w)
      then s with mouse_activated = true
             with mouse =(i64.i32 y, i64.i32 x)
      else s with mouse_activated = false
    case _ -> s

  let resize h w (s: state) = s with h = h with w = w with world = starting_world_generator w h

  let render (s: state) = plot s.w s.h s.world
  
  type text_content = text_content
  let text_format () = "FPS: %d\nt: %d\nspeed: %d"
  let text_colour = const argb.black
  let text_content (fps: f32) (s: state): text_content = (
    t32 fps, 
    i32.i64 s.numer_of_steps, 
    s.speed
  )
  let grab_mouse = false
  
}

