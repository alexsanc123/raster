open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)

let transform image ~radius =
  let copy_image = Image.copy image in
  Image.mapi copy_image ~f:(fun ~x ~y _pixel ->
    let x_start = if x - radius < 0 then 0 else x - radius in
    let x_end =
      if x + radius > Image.width image
      then Image.width image
      else x + radius
    in
    let y_start = if y - radius < 0 then 0 else y - radius in
    let y_end =
      if y + radius > Image.height image
      then Image.height image
      else y + radius
    in
    let square = Image.slice copy_image ~x_start ~y_start ~x_end ~y_end in
    Image.mean_pixel square)
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
