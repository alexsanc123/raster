open Core

let transform image =
  Image.map image ~f:(fun (r, g, b) ->
    let new_red = r % 4 * 64 in
    let new_green = g % 4 * 64 in
    let new_blue = b % 4 * 64 in
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
