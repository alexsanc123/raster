open Core

(* This should look familiar by now! *)

let apply_error
  ~image
  ~(error : float)
  ~(x : int)
  ~(y : int)
  ~(max_val : int)
  ~(error_coef : int)
  =
  match Image.get image ~x ~y with
  | exception _ -> ()
  | _ ->
    let current_pixel = Image.get image ~x ~y in
    let current_val = Pixel.red current_pixel in
    let added_error = error *. (error_coef // 16 *. Int.to_float max_val) in
    let new_value = current_val + Float.to_int added_error in
    Image.set image ~x ~y (new_value, new_value, new_value)
;;

let transform image =
  let greyscale = Grayscale.transform image in
  Image.foldi greyscale ~init:greyscale ~f:(fun ~x ~y current_image pixel ->
    let max_val = Image.max_val image in
    let current_value = Pixel.red pixel // max_val in
    let new_value = if Float.O.(current_value > 0.5) then 1.0 else 0.0 in
    let error = current_value -. new_value in
    if Float.O.(new_value <> 0.0)
    then Image.set current_image ~x ~y (max_val, max_val, max_val)
    else Image.set current_image ~x ~y (0, 0, 0);
    apply_error (* distrbutes 7/16 of the error to the east pixel *)
      ~image:current_image
      ~x:(x + 1)
      ~y
      ~error
      ~max_val
      ~error_coef:7;
    apply_error (* distrbutes 3/16 of the error to the southwest pixel *)
      ~image:current_image
      ~x:(x - 1)
      ~y:(y + 1)
      ~error
      ~max_val
      ~error_coef:3;
    apply_error (* distrbutes 5/16 of the error to the south pixel *)
      ~image:current_image
      ~x
      ~y:(y + 1)
      ~error
      ~max_val
      ~error_coef:5;
    apply_error (* distrbutes 1/16 of the error to the southeast pixel *)
      ~image:current_image
      ~x:(x + 1)
      ~y:(y + 1)
      ~error
      ~max_val
      ~error_coef:1;
    current_image)
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
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
