(* Print a summary of every layer in a vector dataset: CRS, extent, fields,
   and the ring/vertex structure of the first few features.

   Any OGR-readable source works, including GDAL's virtual filesystem:
     read_vector.exe boundary.geojson
     read_vector.exe /vsizip/roi.zip
     read_vector.exe /vsicurl/https://example.org/admin.gpkg *)

let max_features = 5

let describe_srs = function
  | None -> "(none)"
  | Some srs ->
    let name = Option.value (Gdal.SpatialReference.name srs) ~default:"?" in
    let kind =
      if Gdal.SpatialReference.is_projected srs then "projected"
      else if Gdal.SpatialReference.is_geographic srs then "geographic"
      else "other"
    in
    (match Gdal.SpatialReference.authority_code srs with
     | Some code -> Printf.sprintf "%s (%s, EPSG:%s)" name kind code
     | None -> Printf.sprintf "%s (%s)" name kind)

let print_feature i feat =
  let attrs =
    Gdal.Vector.Feature.attributes feat
    |> List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v)
    |> String.concat " "
  in
  Printf.printf "  feature %d: %s\n" i attrs;
  match Gdal.Vector.Feature.geometry feat with
  | None -> Printf.printf "    (no geometry)\n"
  | Some g ->
    let rings = Gdal.Vector.Geometry.rings g in
    let e = Gdal.Vector.Geometry.envelope g in
    Printf.printf "    %s, %d ring(s), %d vertices\n"
      (Gdal.Vector.string_of_geometry_type (Gdal.Vector.Geometry.geometry_type g))
      (List.length rings)
      (List.fold_left (fun n r -> n + Array.length r) 0 rings);
    Printf.printf "    envelope: (%.6f, %.6f) - (%.6f, %.6f)\n" e.min_x e.min_y
      e.max_x e.max_y

let print_layer ds i =
  match Gdal.Vector.Layer.get ds i with
  | Error msg -> Printf.eprintf "Layer %d: %s\n" i msg
  | Ok layer ->
    Printf.printf "\nLayer %d: %s\n" i (Gdal.Vector.Layer.name layer);
    Printf.printf "  Geometry: %s\n"
      (Gdal.Vector.string_of_geometry_type
         (Gdal.Vector.Layer.geometry_type layer));
    Printf.printf "  Features: %d\n" (Gdal.Vector.Layer.feature_count layer);
    Printf.printf "  CRS:      %s\n"
      (describe_srs (Gdal.Vector.Layer.spatial_reference layer));
    (match Gdal.Vector.Layer.extent layer with
     | Ok e ->
       Printf.printf "  Extent:   (%.6f, %.6f) - (%.6f, %.6f)\n" e.min_x e.min_y
         e.max_x e.max_y
     | Error msg -> Printf.printf "  Extent:   %s\n" msg);
    Printf.printf "  Fields:   %s\n"
      (String.concat ", " (Gdal.Vector.Layer.fields layer));
    let shown =
      Gdal.Vector.Layer.fold layer ~init:0 ~f:(fun n feat ->
        if n < max_features then print_feature n feat;
        n + 1)
    in
    if shown > max_features then
      Printf.printf "  ... %d more feature(s)\n" (shown - max_features)

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <vector_file>\n" Sys.argv.(0);
    exit 1
  end;
  let path = Sys.argv.(1) in
  Gdal.init ();
  match
    Gdal.Vector.with_dataset path (fun ds ->
      Printf.printf "File: %s\n" path;
      let n = Gdal.Vector.Layer.count ds in
      Printf.printf "Layers: %d\n" n;
      for i = 0 to n - 1 do
        print_layer ds i
      done)
  with
  | Ok () -> ()
  | Error msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1
