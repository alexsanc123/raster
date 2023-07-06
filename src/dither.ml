open Core

(* This should look familiar by now! *)
let transform image =
  let greyscale = Grayscale.transform image in
  Image.foldi greyscale ~init:greyscale ~f:(fun ~x ~y current_image pixel ->
    let max_val = Image.max_val image in
    let current_value = Pixel.red pixel // max_val in
    let new_value =
      if Float.compare current_value 0.5 > 0 then max_val else 0
    in
    let error = Float.to_int current_value - new_value in
    let seven = Float.to_int current_value + (error * 7 / 16) in
    let three = Float.to_int current_value + (error * 3 / 16) in
    let five = Float.to_int current_value + (error * 5 / 16) in
    let one = Float.to_int current_value + (error * 1 / 16) in
    Image.set current_image ~x ~y (new_value, new_value, new_value);
    match Image.set current_image ~x:(x + 1) ~y (seven, seven, seven) with
    | exception _ -> current_image
    | _ ->
      Image.set current_image ~x:(x + 1) ~y (seven, seven, seven);
      (match
         Image.set current_image ~x:(x - 1) ~y:(y + 1) (three, three, three)
       with
       | exception _ -> current_image
       | _ ->
         Image.set current_image ~x:(x - 1) ~y:(y + 1) (three, three, three);
         (match Image.set current_image ~x ~y:(y + 1) (five, five, five) with
          | exception _ -> current_image
          | _ ->
            Image.set current_image ~x ~y:(y + 1) (five, five, five);
            (match
               Image.set current_image ~x:(x + 1) ~y:(y + 1) (one, one, one)
             with
             | exception _ -> current_image
             | _ ->
               Image.set current_image ~x ~y:(y + 1) (five, five, five);
               current_image))))
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
