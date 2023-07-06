open Core

let get_last_twobits color_value =
  let lsb = color_value % 2 in
  let quotient = color_value / 2 in
  let second_lsb = quotient % 2 in
  match Int.to_string_hum second_lsb ^ Int.to_string_hum lsb with
  | "01" -> 01 * 64
  | "10" -> 10 * 64
  | "00" -> 00 * 64
  | "11" -> 11 * 64
  | _ -> 0
;;

let transform image =
  Image.map image ~f:(fun (r, g, b) ->
    let new_red = get_last_twobits r in
    let new_green = get_last_twobits g in
    let new_blue = get_last_twobits b in
    new_red, new_green, new_blue)
;;

let command =
  Command.basic
    ~summary:"Reveal a hidden image"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_hidden.ppm")]
;;
