let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <raster_file>\n" Sys.argv.(0);
    exit 1
  end;
  let path = Sys.argv.(1) in
  Gdal.init ();
  match Gdal.Dataset.with_dataset path (fun ds ->
    let width = Gdal.Dataset.raster_x_size ds in
    let height = Gdal.Dataset.raster_y_size ds in
    let bands = Gdal.Dataset.raster_count ds in
    Printf.printf "File: %s\n" path;
    Printf.printf "Size: %d x %d, %d band(s)\n" width height bands;
    Printf.printf "Projection: %s\n" (Gdal.Dataset.projection ds);
    (match Gdal.Dataset.get_geo_transform ds with
    | Ok gt ->
      Printf.printf "GeoTransform:\n";
      Printf.printf "  Origin:     (%.6f, %.6f)\n" gt.origin_x gt.origin_y;
      Printf.printf "  Pixel size: (%.6f, %.6f)\n" gt.pixel_width gt.pixel_height;
      Printf.printf "  Rotation:   (%.6f, %.6f)\n" gt.row_rotation gt.col_rotation
    | Error msg ->
      Printf.printf "GeoTransform: %s\n" msg);
    match Gdal.Dataset.get_band ds 1 with
    | Error msg ->
      Printf.eprintf "Error getting band 1: %s\n" msg
    | Ok band ->
      (match Gdal.RasterBand.data_type band with
      | Ok dt ->
        Printf.printf "Band 1 data type: %s\n"
          (Gdal.string_of_data_type dt)
      | Error msg -> Printf.eprintf "Error: %s\n" msg);
      (match Gdal.RasterBand.block_size band with
      | Ok (bx, by) ->
        Printf.printf "Band 1 block size: %d x %d\n" bx by
      | Error msg -> Printf.eprintf "Error: %s\n" msg);
      (match Gdal.RasterBand.no_data_value band with
      | Ok (Some v) -> Printf.printf "Band 1 nodata: %f\n" v
      | Ok None -> Printf.printf "Band 1 nodata: not set\n"
      | Error msg -> Printf.eprintf "Error: %s\n" msg);
      let read_x = min 10 width in
      let read_y = min 10 height in
      Printf.printf "\nFirst %dx%d pixel values (as float64):\n" read_x read_y;
      match
        Gdal.RasterBand.read_region Gdal.BA_float64 band ~x_off:0 ~y_off:0
          ~x_size:read_x ~y_size:read_y ~buf_x:read_x ~buf_y:read_y
      with
      | Error msg ->
        Printf.eprintf "Error reading pixels: %s\n" msg
      | Ok arr ->
        for row = 0 to read_y - 1 do
          for col = 0 to read_x - 1 do
            Printf.printf "%8.2f " (Bigarray.Array2.get arr row col)
          done;
          print_newline ()
        done
  ) with
  | Ok () -> ()
  | Error msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1
