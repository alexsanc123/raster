open Core

let transform image ~threshold =
  let max_val = Image.max_val image in
  let threshold_from_max = Int.to_float max_val *. threshold in
  Image.map image ~f:(fun (r, g, b) ->
    let new_r =
      if r > Float.to_int threshold_from_max then abs (max_val - r) else r
    in
    let new_g =
      if g > Float.to_int threshold_from_max then abs (max_val - g) else g
    in
    let new_b =
      if b > Float.to_int threshold_from_max then abs (max_val - b) else b
    in
    new_r, new_g, new_b)
;;

let command =
  Command.basic
    ~summary:"Invert an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and threshold =
        flag
          "threshold"
          (required Command.Param.float)
          ~doc:
            "N the threshold to use when inverting (lower = more inverted)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~threshold in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_invert.ppm")]
;;
