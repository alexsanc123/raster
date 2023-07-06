open Core

(* This should look familiar by now! *)
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
    let () =
      match Image.get current_image ~x:(x + 1) ~y with
      | exception _ -> ()
      | _ ->
        let current_pixel = Image.get current_image ~x:(x + 1) ~y in
        let current_val = Pixel.red current_pixel in
        let seven =
          current_val
          + Float.to_int (error *. (7 // 16) *. Int.to_float max_val)
        in
        Image.set current_image ~x:(x + 1) ~y (seven, seven, seven)
    in
    let () =
      match Image.get current_image ~x:(x - 1) ~y:(y + 1) with
      | exception _ -> ()
      | _ ->
        let current_pixel = Image.get current_image ~x:(x - 1) ~y:(y + 1) in
        let current_val = Pixel.red current_pixel in
        let three =
          current_val
          + Float.to_int (error *. (3 // 16) *. Int.to_float max_val)
        in
        Image.set current_image ~x:(x - 1) ~y:(y + 1) (three, three, three)
    in
    let () =
      match Image.get current_image ~x ~y:(y + 1) with
      | exception _ -> ()
      | _ ->
        let current_pixel = Image.get current_image ~x ~y:(y + 1) in
        let current_val = Pixel.red current_pixel in
        let five =
          current_val
          + Float.to_int (error *. (5 // 16) *. Int.to_float max_val)
        in
        Image.set current_image ~x ~y:(y + 1) (five, five, five)
    in
    let () =
      match Image.get current_image ~x:(x + 1) ~y:(y + 1) with
      | exception _ -> ()
      | _ ->
        let current_pixel = Image.get current_image ~x:(x + 1) ~y:(y + 1) in
        let current_val = Pixel.red current_pixel in
        let one =
          current_val
          + Float.to_int (error *. (1 // 16) *. Int.to_float max_val)
        in
        Image.set current_image ~x:(x + 1) ~y:(y + 1) (one, one, one)
    in
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
