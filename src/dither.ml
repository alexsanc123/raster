open Core

(* This should look familiar by now! *)

let get_ratio_error_0ormax image specific_color =
  let max_val = Image.max_val image in
  let current_val = specific_color // max_val in
  let new_val = if Float.O.(current_val > 0.5) then max_val else 0 in
  let error = Int.to_float (specific_color - new_val) in
  current_val, error, new_val
;;

let apply_error
  ~image
  ~(x : int)
  ~(y : int)
  ~errors:(red_error, green_error, blue_error)
  ~(error_coef : int)
  =
  match Image.get image ~x ~y with
  | exception _ -> ()
  | _ ->
    let pixel = Image.get image ~x ~y in
    let r, g, b = Pixel.red pixel, Pixel.green pixel, Pixel.blue pixel in
    let new_red = r + Float.to_int (red_error *. (error_coef // 16)) in
    let new_green = g + Float.to_int (green_error *. (error_coef // 16)) in
    let new_blue = b + Float.to_int (blue_error *. (error_coef // 16)) in
    Image.set image ~x ~y (new_red, new_green, new_blue)
;;

let transform image ~(color : bool) =
  let new_image = if color then image else Grayscale.transform image in
  Image.foldi
    new_image
    ~init:new_image
    ~f:(fun ~x ~y current_image (r, g, b) ->
    let _max_val = Image.max_val image in
    let _, red_error, red_pix = get_ratio_error_0ormax image r in
    let _, green_error, green_pix = get_ratio_error_0ormax image g in
    let _, blue_error, blue_pix = get_ratio_error_0ormax image b in
    let errors = red_error, green_error, blue_error in
    Image.set current_image ~x ~y (red_pix, green_pix, blue_pix);
    apply_error (* distrbutes 7/16 of the error to the east pixel *)
      ~image:new_image
      ~x:(x + 1)
      ~y
      ~errors
      ~error_coef:7;
    apply_error (* distrbutes 3/16 of the error to the southwest pixel *)
      ~image:new_image
      ~x:(x - 1)
      ~y:(y + 1)
      ~errors
      ~error_coef:3;
    apply_error (* distrbutes 5/16 of the error to the south pixel *)
      ~image:new_image
      ~x
      ~errors
      ~y:(y + 1)
      ~error_coef:5;
    apply_error (* distrbutes 1/16 of the error to the southeast pixel *)
      ~image:new_image
      ~x:(x + 1)
      ~y:(y + 1)
      ~errors
      ~error_coef:1;
    new_image)
;;

let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and color =
        flag
          "color"
          (required Command.Param.bool)
          ~doc:
            "Bool to use when choosing to grey or color\n\
            \   dither the image."
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~color in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
