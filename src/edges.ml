open Core

    let horizontal_gradient image = let x_kernel = [-1; 0; 1; -2; 0; 2; -1;
   0; 1] in let half_kernel = 1 in Image.foldi image ~init:image ~f:(fun ~x
   ~y current_image (r, g, b) -> )

   ;; 

    let vertical_gradient pixel = let y_kernel = [-1; -2; -1; 0; 0; 0; 1;
   2; 1] in let half_kernel = 1 in

   ;;

   let convolve image ~x ~y ~kernel = ;;

   let transform =

   ;; 

   let command = Command.basic ~summary:"Detect the edges of an image"
   [%map_open.Command let filename = flag "filename" (required
   Command.Param.string) ~doc:"IMAGE_FILE the PPM image file" in fun () ->
   let image = Image.load_ppm ~filename |> transform in Image.save_ppm image
   ~filename: (String.chop_suffix_exn filename ~suffix:".ppm" ^
   "_edges.ppm")] ;; 

   