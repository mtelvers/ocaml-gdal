open Bigarray

(* ---- Helpers ---- *)

let tmpdir = Filename.get_temp_dir_name ()

let tmp name = Filename.concat tmpdir ("gdal_test_" ^ name)

let unwrap msg = function
  | Ok v -> v
  | Error s -> Alcotest.failf "%s: %s" msg s

let expect_error = function
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error but got Ok"

let float_eps = Alcotest.float 1e-6

let contains sub s =
  let len_sub = String.length sub in
  let len_s = String.length s in
  let rec check i =
    if i > len_s - len_sub then false
    else if String.sub s i len_sub = sub then true
    else check (i + 1)
  in
  check 0

let semi_major = 20037508.34

let mercator_x lon = lon *. semi_major /. 180.0

let mercator_y lat =
  let r = lat *. Float.pi /. 180.0 in
  log (tan (Float.pi /. 4.0 +. r /. 2.0)) *. semi_major /. Float.pi

let with_4326_to_3857 f =
  let src = unwrap "src" (Gdal.SpatialReference.of_epsg 4326) in
  let dst = unwrap "dst" (Gdal.SpatialReference.of_epsg 3857) in
  let ct = unwrap "ct" (Gdal.CoordinateTransformation.create src dst) in
  Fun.protect
    ~finally:(fun () ->
      Gdal.CoordinateTransformation.destroy ct;
      Gdal.SpatialReference.destroy src;
      Gdal.SpatialReference.destroy dst)
    (fun () -> f ct)

let () = Gdal.init ()

(* Create a small test GeoTIFF with known values.
   Band b, row y, col x -> (b * 64 + y + x) mod 256 *)
let test_tif = tmp "fixture.tif"
let test_width = 32
let test_height = 24
let test_bands = 2
let test_wkt =
  "GEOGCS[\"WGS 84\",DATUM[\"WGS_1984\",\
   SPHEROID[\"WGS 84\",6378137,298.257223563]],\
   PRIMEM[\"Greenwich\",0],\
   UNIT[\"degree\",0.0174532925199433]]"
let test_gt =
  Gdal.{ origin_x = 10.0; pixel_width = 0.5; row_rotation = 0.0;
         origin_y = 50.0; col_rotation = 0.0; pixel_height = -0.25 }

let pixel_value b y x = (b * 64 + y + x) mod 256

let create_fixture () =
  let driver = unwrap "driver" (Gdal.Driver.by_name "GTiff") in
  let ds = unwrap "create" (Gdal.Driver.create driver ~filename:test_tif
      ~width:test_width ~height:test_height ~bands:test_bands Gdal.Byte) in
  for b = 1 to test_bands do
    let band = unwrap "get_band" (Gdal.Dataset.get_band ds b) in
    let arr = Array2.create int8_unsigned c_layout test_height test_width in
    for y = 0 to test_height - 1 do
      for x = 0 to test_width - 1 do
        Array2.set arr y x (pixel_value b y x)
      done
    done;
    unwrap "write" (Gdal.RasterBand.write_region Gdal.BA_byte band
        ~x_off:0 ~y_off:0 ~x_size:test_width ~y_size:test_height arr)
  done;
  unwrap "set_proj" (Gdal.Dataset.set_projection ds test_wkt);
  unwrap "set_gt" (Gdal.Dataset.set_geo_transform ds test_gt);
  Gdal.Dataset.close ds

let () = create_fixture ()

(* ---- Enum tests ---- *)

let test_data_type_roundtrip () =
  let types = Gdal.[ Byte; UInt16; Int16; UInt32; Int32;
                      Float32; Float64; Int8; Unknown ] in
  List.iter (fun dt ->
    let n = Gdal.data_type_to_int dt in
    let dt' = Gdal.data_type_of_int n in
    Alcotest.(check string) "roundtrip"
      (Gdal.string_of_data_type dt) (Gdal.string_of_data_type dt')
  ) types

let test_string_of_data_type () =
  Alcotest.(check string) "Byte" "Byte" (Gdal.string_of_data_type Gdal.Byte);
  Alcotest.(check string) "Float64" "Float64" (Gdal.string_of_data_type Gdal.Float64);
  Alcotest.(check string) "Unknown" "Unknown" (Gdal.string_of_data_type Gdal.Unknown)

let test_unknown_data_type () =
  Alcotest.(check string) "out of range"
    "Unknown" (Gdal.string_of_data_type (Gdal.data_type_of_int 999))

let enum_tests = [
  Alcotest.test_case "data_type roundtrip" `Quick test_data_type_roundtrip;
  Alcotest.test_case "string_of_data_type" `Quick test_string_of_data_type;
  Alcotest.test_case "unknown data_type" `Quick test_unknown_data_type;
]

(* ---- Top-level tests ---- *)

let test_init () =
  Gdal.init ()

let test_set_config_option () =
  Gdal.set_config_option "GDAL_CACHEMAX" "64"

let test_flag_constants () =
  Alcotest.(check int) "readonly" 0x00 Gdal.gdal_of_readonly;
  Alcotest.(check int) "update" 0x01 Gdal.gdal_of_update;
  Alcotest.(check int) "raster" 0x02 Gdal.gdal_of_raster;
  Alcotest.(check int) "vector" 0x04 Gdal.gdal_of_vector;
  Alcotest.(check int) "verbose" 0x40 Gdal.gdal_of_verbose_error

let toplevel_tests = [
  Alcotest.test_case "init" `Quick test_init;
  Alcotest.test_case "set_config_option" `Quick test_set_config_option;
  Alcotest.test_case "flag constants" `Quick test_flag_constants;
]

(* ---- Driver tests ---- *)

let test_driver_by_name () =
  let _ = unwrap "GTiff" (Gdal.Driver.by_name "GTiff") in
  let _ = unwrap "MEM" (Gdal.Driver.by_name "MEM") in
  ()

let test_driver_by_name_error () =
  expect_error (Gdal.Driver.by_name "NoSuchDriver")

let test_driver_create () =
  let f = tmp "driver_create.tif" in
  let driver = unwrap "driver" (Gdal.Driver.by_name "GTiff") in
  let ds = unwrap "create" (Gdal.Driver.create driver ~filename:f
      ~width:4 ~height:4 ~bands:1 Gdal.Byte) in
  Alcotest.(check int) "x" 4 (Gdal.Dataset.raster_x_size ds);
  Alcotest.(check int) "y" 4 (Gdal.Dataset.raster_y_size ds);
  Alcotest.(check int) "bands" 1 (Gdal.Dataset.raster_count ds);
  Gdal.Dataset.close ds

let test_driver_description () =
  let driver = unwrap "driver" (Gdal.Driver.by_name "GTiff") in
  let desc = Gdal.Driver.description driver in
  Alcotest.(check string) "GTiff desc" "GeoTIFF" desc;
  let mem = unwrap "mem" (Gdal.Driver.by_name "MEM") in
  let desc2 = Gdal.Driver.description mem in
  Alcotest.(check bool) "MEM non-empty" true (String.length desc2 > 0)

let test_driver_create_copy () =
  let src = unwrap "open" (Gdal.Dataset.open_ test_tif) in
  let f = tmp "driver_copy.tif" in
  let driver = unwrap "driver" (Gdal.Driver.by_name "GTiff") in
  let dst = unwrap "copy" (Gdal.Driver.create_copy driver ~filename:f ~src ()) in
  Alcotest.(check int) "x" test_width (Gdal.Dataset.raster_x_size dst);
  Alcotest.(check int) "y" test_height (Gdal.Dataset.raster_y_size dst);
  Gdal.Dataset.close dst;
  Gdal.Dataset.close src

let driver_tests = [
  Alcotest.test_case "by_name success" `Quick test_driver_by_name;
  Alcotest.test_case "by_name error" `Quick test_driver_by_name_error;
  Alcotest.test_case "description" `Quick test_driver_description;
  Alcotest.test_case "create" `Quick test_driver_create;
  Alcotest.test_case "create_copy" `Quick test_driver_create_copy;
]

(* ---- Dataset tests ---- *)

let test_open_close () =
  let ds = unwrap "open" (Gdal.Dataset.open_ test_tif) in
  Gdal.Dataset.close ds;
  (* double close is safe *)
  Gdal.Dataset.close ds

let test_open_error () =
  expect_error (Gdal.Dataset.open_ "/nonexistent/file.tif")

let test_open_ex () =
  let ds = unwrap "open_ex" (Gdal.Dataset.open_ex test_tif) in
  Alcotest.(check int) "x" test_width (Gdal.Dataset.raster_x_size ds);
  Gdal.Dataset.close ds

let test_open_ex_flags () =
  let flags = Gdal.gdal_of_readonly lor Gdal.gdal_of_raster in
  let ds = unwrap "open_ex" (Gdal.Dataset.open_ex ~flags test_tif) in
  Alcotest.(check int) "bands" test_bands (Gdal.Dataset.raster_count ds);
  Gdal.Dataset.close ds

let test_with_dataset () =
  let w = unwrap "with" (Gdal.Dataset.with_dataset test_tif (fun ds ->
    Gdal.Dataset.raster_x_size ds)) in
  Alcotest.(check int) "width" test_width w

let test_dimensions () =
  let ds = unwrap "open" (Gdal.Dataset.open_ test_tif) in
  Alcotest.(check int) "x" test_width (Gdal.Dataset.raster_x_size ds);
  Alcotest.(check int) "y" test_height (Gdal.Dataset.raster_y_size ds);
  Alcotest.(check int) "bands" test_bands (Gdal.Dataset.raster_count ds);
  Gdal.Dataset.close ds

let test_geo_transform () =
  let ds = unwrap "open" (Gdal.Dataset.open_ test_tif) in
  let gt = unwrap "get_gt" (Gdal.Dataset.get_geo_transform ds) in
  Alcotest.check float_eps "origin_x" test_gt.origin_x gt.origin_x;
  Alcotest.check float_eps "pixel_width" test_gt.pixel_width gt.pixel_width;
  Alcotest.check float_eps "origin_y" test_gt.origin_y gt.origin_y;
  Alcotest.check float_eps "pixel_height" test_gt.pixel_height gt.pixel_height;
  Alcotest.check float_eps "row_rotation" test_gt.row_rotation gt.row_rotation;
  Alcotest.check float_eps "col_rotation" test_gt.col_rotation gt.col_rotation;
  Gdal.Dataset.close ds

let test_set_geo_transform () =
  let f = tmp "set_gt.tif" in
  let driver = unwrap "driver" (Gdal.Driver.by_name "GTiff") in
  let ds = unwrap "create" (Gdal.Driver.create driver ~filename:f
      ~width:4 ~height:4 ~bands:1 Gdal.Byte) in
  let gt = Gdal.{ origin_x = 1.0; pixel_width = 2.0; row_rotation = 0.0;
                   origin_y = 3.0; col_rotation = 0.0; pixel_height = -4.0 } in
  unwrap "set" (Gdal.Dataset.set_geo_transform ds gt);
  let gt' = unwrap "get" (Gdal.Dataset.get_geo_transform ds) in
  Alcotest.check float_eps "origin_x" 1.0 gt'.origin_x;
  Alcotest.check float_eps "pixel_height" (-4.0) gt'.pixel_height;
  Gdal.Dataset.close ds

let test_projection () =
  let ds = unwrap "open" (Gdal.Dataset.open_ test_tif) in
  let proj = Gdal.Dataset.projection ds in
  Alcotest.(check bool) "has WGS" true (String.length proj > 0);
  Alcotest.(check bool) "contains WGS 84" true (contains "WGS 84" proj);
  Gdal.Dataset.close ds

let test_set_projection () =
  let f = tmp "set_proj.tif" in
  let driver = unwrap "driver" (Gdal.Driver.by_name "GTiff") in
  let ds = unwrap "create" (Gdal.Driver.create driver ~filename:f
      ~width:4 ~height:4 ~bands:1 Gdal.Byte) in
  unwrap "set" (Gdal.Dataset.set_projection ds test_wkt);
  let proj = Gdal.Dataset.projection ds in
  Alcotest.(check bool) "non-empty" true (String.length proj > 0);
  Gdal.Dataset.close ds

let test_get_band () =
  let ds = unwrap "open" (Gdal.Dataset.open_ test_tif) in
  let _band = unwrap "band 1" (Gdal.Dataset.get_band ds 1) in
  let _band = unwrap "band 2" (Gdal.Dataset.get_band ds 2) in
  Gdal.Dataset.close ds

let test_get_band_error () =
  let ds = unwrap "open" (Gdal.Dataset.open_ test_tif) in
  expect_error (Gdal.Dataset.get_band ds 99);
  Gdal.Dataset.close ds

let test_is_null () =
  let ds = unwrap "open" (Gdal.Dataset.open_ test_tif) in
  Alcotest.(check bool) "not null" false (Gdal.Dataset.is_null ds);
  Gdal.Dataset.close ds

let test_get_metadata_item () =
  let ds = unwrap "open" (Gdal.Dataset.open_ test_tif) in
  (* AREA_OR_POINT is commonly set by GTiff *)
  let _v = Gdal.Dataset.get_metadata_item ds ~key:"AREA_OR_POINT" ~domain:"" in
  (* nonexistent key *)
  let v = Gdal.Dataset.get_metadata_item ds ~key:"NO_SUCH_KEY" ~domain:"" in
  Alcotest.(check (option string)) "missing key" None v;
  Gdal.Dataset.close ds

let test_warp () =
  let ds = unwrap "open" (Gdal.Dataset.open_ ~access:Gdal.ReadOnly test_tif) in
  let f = tmp "warped.tif" in
  let out = unwrap "warp"
    (Gdal.Dataset.warp ds ~dst_filename:f ["-t_srs"; "EPSG:3857"]) in
  Alcotest.(check bool) "not null" false (Gdal.Dataset.is_null out);
  let proj = Gdal.Dataset.projection out in
  Alcotest.(check bool) "reprojected" true (String.length proj > 0);
  Gdal.Dataset.close out;
  Gdal.Dataset.close ds

(* Simulate SCL-like classification data: a single-band Byte raster
   with discrete class values 1..11, then warp and verify non-zero output *)
let test_warp_scl () =
  let f_src = tmp "scl_src.tif" in
  let driver = unwrap "driver" (Gdal.Driver.by_name "GTiff") in
  let w = 64 and h = 64 in
  let ds = unwrap "create" (Gdal.Driver.create driver ~filename:f_src
      ~width:w ~height:h ~bands:1 Gdal.Byte) in
  (* Set WGS 84 projection and geo_transform *)
  unwrap "set_proj" (Gdal.Dataset.set_projection ds test_wkt);
  let gt = Gdal.{ origin_x = -1.0; pixel_width = 0.05; row_rotation = 0.0;
                   origin_y = 52.0; col_rotation = 0.0; pixel_height = -0.05 } in
  unwrap "set_gt" (Gdal.Dataset.set_geo_transform ds gt);
  let band = unwrap "band" (Gdal.Dataset.get_band ds 1) in
  let arr = Array2.create int8_unsigned c_layout h w in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      (* SCL values 1..11 in a pattern *)
      Array2.set arr y x (1 + ((y + x) mod 11))
    done
  done;
  unwrap "write" (Gdal.RasterBand.write_region Gdal.BA_byte band
      ~x_off:0 ~y_off:0 ~x_size:w ~y_size:h arr);
  Gdal.Dataset.close ds;
  (* Re-open as ReadOnly and warp to EPSG:3857 with nearest neighbour *)
  let src = unwrap "open" (Gdal.Dataset.open_ ~access:Gdal.ReadOnly f_src) in
  let f_dst = tmp "scl_warped.tif" in
  let warped = unwrap "warp"
    (Gdal.Dataset.warp src ~dst_filename:f_dst
       ["-t_srs"; "EPSG:3857"; "-r"; "near"]) in
  Alcotest.(check bool) "not null" false (Gdal.Dataset.is_null warped);
  let out_w = Gdal.Dataset.raster_x_size warped in
  let out_h = Gdal.Dataset.raster_y_size warped in
  Alcotest.(check bool) "has width" true (out_w > 0);
  Alcotest.(check bool) "has height" true (out_h > 0);
  let out_band = unwrap "out_band" (Gdal.Dataset.get_band warped 1) in
  let out_arr = unwrap "read" (Gdal.RasterBand.read Gdal.BA_byte out_band) in
  (* Count non-zero pixels — should be a significant portion *)
  let nonzero = ref 0 in
  for y = 0 to Array2.dim1 out_arr - 1 do
    for x = 0 to Array2.dim2 out_arr - 1 do
      if Array2.get out_arr y x <> 0 then incr nonzero
    done
  done;
  let total = Array2.dim1 out_arr * Array2.dim2 out_arr in
  Alcotest.(check bool) "has non-zero pixels"
    true (!nonzero > total / 2);
  (* Verify values are in SCL range 1..11 (or 0 for nodata border) *)
  for y = 0 to Array2.dim1 out_arr - 1 do
    for x = 0 to Array2.dim2 out_arr - 1 do
      let v = Array2.get out_arr y x in
      if v < 0 || v > 11 then
        Alcotest.failf "pixel[%d,%d]=%d out of SCL range" y x v
    done
  done;
  Gdal.Dataset.close warped;
  Gdal.Dataset.close src

let test_dataset_description () =
  let ds = unwrap "open" (Gdal.Dataset.open_ test_tif) in
  let desc = Gdal.Dataset.description ds in
  (* Description is typically the filename *)
  Alcotest.(check bool) "contains fixture"
    true (String.length desc > 0);
  Gdal.Dataset.close ds

let test_translate () =
  let ds = unwrap "open" (Gdal.Dataset.open_ ~access:Gdal.ReadOnly test_tif) in
  let f = tmp "translated.tif" in
  let out = unwrap "translate"
    (Gdal.Dataset.translate ds ~dst_filename:f
       ["-outsize"; "16"; "12"]) in
  Alcotest.(check int) "x" 16 (Gdal.Dataset.raster_x_size out);
  Alcotest.(check int) "y" 12 (Gdal.Dataset.raster_y_size out);
  Alcotest.(check int) "bands" test_bands (Gdal.Dataset.raster_count out);
  Gdal.Dataset.close out;
  Gdal.Dataset.close ds

let test_translate_format () =
  let ds = unwrap "open" (Gdal.Dataset.open_ ~access:Gdal.ReadOnly test_tif) in
  let f = tmp "translated.png" in
  let out = unwrap "translate"
    (Gdal.Dataset.translate ds ~dst_filename:f ["-of"; "PNG"]) in
  Alcotest.(check int) "x" test_width (Gdal.Dataset.raster_x_size out);
  Gdal.Dataset.close out;
  Gdal.Dataset.close ds

let dataset_tests = [
  Alcotest.test_case "open/close" `Quick test_open_close;
  Alcotest.test_case "open error" `Quick test_open_error;
  Alcotest.test_case "open_ex" `Quick test_open_ex;
  Alcotest.test_case "open_ex flags" `Quick test_open_ex_flags;
  Alcotest.test_case "with_dataset" `Quick test_with_dataset;
  Alcotest.test_case "dimensions" `Quick test_dimensions;
  Alcotest.test_case "geo_transform" `Quick test_geo_transform;
  Alcotest.test_case "set_geo_transform" `Quick test_set_geo_transform;
  Alcotest.test_case "projection" `Quick test_projection;
  Alcotest.test_case "set_projection" `Quick test_set_projection;
  Alcotest.test_case "get_band" `Quick test_get_band;
  Alcotest.test_case "get_band error" `Quick test_get_band_error;
  Alcotest.test_case "is_null" `Quick test_is_null;
  Alcotest.test_case "get_metadata_item" `Quick test_get_metadata_item;
  Alcotest.test_case "warp" `Quick test_warp;
  Alcotest.test_case "warp SCL" `Quick test_warp_scl;
  Alcotest.test_case "description" `Quick test_dataset_description;
  Alcotest.test_case "translate" `Quick test_translate;
  Alcotest.test_case "translate format" `Quick test_translate_format;
]

(* ---- RasterBand tests ---- *)

let with_band f =
  let ds = unwrap "open" (Gdal.Dataset.open_ test_tif) in
  let band = unwrap "band" (Gdal.Dataset.get_band ds 1) in
  let result = f ds band in
  Gdal.Dataset.close ds;
  result

let test_band_data_type () = with_band (fun _ds band ->
  let dt = unwrap "dt" (Gdal.RasterBand.data_type band) in
  Alcotest.(check string) "Byte" "Byte" (Gdal.string_of_data_type dt))

let test_band_block_size () = with_band (fun _ds band ->
  let (bx, by) = unwrap "bs" (Gdal.RasterBand.block_size band) in
  Alcotest.(check bool) "bx > 0" true (bx > 0);
  Alcotest.(check bool) "by > 0" true (by > 0))

let test_band_no_data_value () = with_band (fun _ds band ->
  let v = unwrap "nd" (Gdal.RasterBand.no_data_value band) in
  Alcotest.(check (option (float 0.0))) "no nodata" None v)

let test_band_x_y_size () = with_band (fun _ds band ->
  let xs = unwrap "xs" (Gdal.RasterBand.x_size band) in
  let ys = unwrap "ys" (Gdal.RasterBand.y_size band) in
  Alcotest.(check int) "x_size" test_width xs;
  Alcotest.(check int) "y_size" test_height ys)

let test_band_read () = with_band (fun _ds band ->
  let arr = unwrap "read" (Gdal.RasterBand.read Gdal.BA_byte band) in
  Alcotest.(check int) "dim1" test_height (Array2.dim1 arr);
  Alcotest.(check int) "dim2" test_width (Array2.dim2 arr);
  for y = 0 to test_height - 1 do
    for x = 0 to test_width - 1 do
      let expected = pixel_value 1 y x in
      let actual = Array2.get arr y x in
      if actual <> expected then
        Alcotest.failf "pixel[%d,%d] expected %d got %d" y x expected actual
    done
  done)

let test_band_read_region () = with_band (fun _ds band ->
  let arr = unwrap "read_region"
    (Gdal.RasterBand.read_region Gdal.BA_float64 band
       ~x_off:5 ~y_off:3 ~x_size:8 ~y_size:6 ~buf_x:8 ~buf_y:6) in
  Alcotest.(check int) "dim1" 6 (Array2.dim1 arr);
  Alcotest.(check int) "dim2" 8 (Array2.dim2 arr);
  for y = 0 to 5 do
    for x = 0 to 7 do
      let expected = Float.of_int (pixel_value 1 (3 + y) (5 + x)) in
      let actual = Array2.get arr y x in
      if Float.abs (actual -. expected) > 0.001 then
        Alcotest.failf "pixel[%d,%d] expected %.0f got %.0f" y x expected actual
    done
  done)

let test_band_write_region () =
  let f = tmp "write_region.tif" in
  let driver = unwrap "driver" (Gdal.Driver.by_name "GTiff") in
  let ds = unwrap "create" (Gdal.Driver.create driver ~filename:f
      ~width:8 ~height:8 ~bands:1 Gdal.Float32) in
  let band = unwrap "band" (Gdal.Dataset.get_band ds 1) in
  let arr = Array2.create float32 c_layout 4 4 in
  for y = 0 to 3 do
    for x = 0 to 3 do
      Array2.set arr y x (Float.of_int (y * 10 + x))
    done
  done;
  unwrap "write" (Gdal.RasterBand.write_region Gdal.BA_float32 band
      ~x_off:2 ~y_off:2 ~x_size:4 ~y_size:4 arr);
  (* Read back and verify *)
  let out = unwrap "read"
    (Gdal.RasterBand.read_region Gdal.BA_float32 band
       ~x_off:2 ~y_off:2 ~x_size:4 ~y_size:4 ~buf_x:4 ~buf_y:4) in
  for y = 0 to 3 do
    for x = 0 to 3 do
      Alcotest.check (Alcotest.float 0.001) (Printf.sprintf "px[%d,%d]" y x)
        (Float.of_int (y * 10 + x)) (Array2.get out y x)
    done
  done;
  Gdal.Dataset.close ds

let test_band_read_byte () = with_band (fun _ds band ->
  let arr = unwrap "read_byte"
    (Gdal.RasterBand.read_byte band ~x_off:0 ~y_off:0
       ~x_size:4 ~y_size:4) in
  Alcotest.(check int) "dim1" 4 (Array2.dim1 arr);
  Alcotest.(check int) "dim2" 4 (Array2.dim2 arr);
  Alcotest.(check int) "px[0,0]" (pixel_value 1 0 0) (Array2.get arr 0 0);
  Alcotest.(check int) "px[1,2]" (pixel_value 1 1 2) (Array2.get arr 1 2))

let test_band_after_close () =
  let ds = unwrap "open" (Gdal.Dataset.open_ test_tif) in
  let band = unwrap "band" (Gdal.Dataset.get_band ds 1) in
  Gdal.Dataset.close ds;
  expect_error (Gdal.RasterBand.data_type band)

let raster_band_tests = [
  Alcotest.test_case "data_type" `Quick test_band_data_type;
  Alcotest.test_case "block_size" `Quick test_band_block_size;
  Alcotest.test_case "no_data_value" `Quick test_band_no_data_value;
  Alcotest.test_case "x_size / y_size" `Quick test_band_x_y_size;
  Alcotest.test_case "read" `Quick test_band_read;
  Alcotest.test_case "read_region" `Quick test_band_read_region;
  Alcotest.test_case "write_region" `Quick test_band_write_region;
  Alcotest.test_case "read_byte" `Quick test_band_read_byte;
  Alcotest.test_case "band after close" `Quick test_band_after_close;
]

(* ---- SpatialReference tests ---- *)

let test_srs_of_epsg () =
  let srs = unwrap "epsg" (Gdal.SpatialReference.of_epsg 4326) in
  Gdal.SpatialReference.destroy srs

let test_srs_of_epsg_error () =
  expect_error (Gdal.SpatialReference.of_epsg 99999999)

let test_srs_of_wkt () =
  let srs = unwrap "wkt" (Gdal.SpatialReference.of_wkt test_wkt) in
  Gdal.SpatialReference.destroy srs

let test_srs_of_wkt_error () =
  expect_error (Gdal.SpatialReference.of_wkt "NOT VALID WKT")

let test_srs_axis_mapping () =
  (* Default is traditional GIS order after of_epsg *)
  with_4326_to_3857 (fun ct ->
    let (xmin, _, _, _) = unwrap "bounds"
      (Gdal.CoordinateTransformation.transform_bounds ct
         ~xmin:(-10.0) ~ymin:50.0 ~xmax:2.0 ~ymax:60.0 ~density:21) in
    (* With traditional order, -10deg longitude -> negative easting *)
    Alcotest.(check bool) "traditional: xmin < 0" true (xmin < 0.0));
  (* Switch to authority compliant: EPSG:4326 uses lat/lon order *)
  let src = unwrap "src" (Gdal.SpatialReference.of_epsg 4326) in
  let dst = unwrap "dst" (Gdal.SpatialReference.of_epsg 3857) in
  Gdal.SpatialReference.set_axis_mapping_strategy src
    Gdal.oams_authority_compliant;
  Gdal.SpatialReference.set_axis_mapping_strategy dst
    Gdal.oams_authority_compliant;
  let ct2 = unwrap "ct2" (Gdal.CoordinateTransformation.create src dst) in
  let (xmin2, _, _, _) = unwrap "bounds2"
    (Gdal.CoordinateTransformation.transform_bounds ct2
       ~xmin:(-10.0) ~ymin:50.0 ~xmax:2.0 ~ymax:60.0 ~density:21) in
  (* With authority order, same numeric input is interpreted as lat/lon,
     so the result should differ from the traditional order result *)
  let expected_traditional_xmin = mercator_x (-10.0) in
  Alcotest.(check bool) "authority: different result"
    true (Float.abs (expected_traditional_xmin -. xmin2) > 1.0);
  Gdal.CoordinateTransformation.destroy ct2;
  Gdal.SpatialReference.destroy src;
  Gdal.SpatialReference.destroy dst

let test_srs_to_wkt () =
  let srs = unwrap "epsg" (Gdal.SpatialReference.of_epsg 4326) in
  let wkt = unwrap "to_wkt" (Gdal.SpatialReference.to_wkt srs) in
  Alcotest.(check bool) "non-empty" true (String.length wkt > 0);
  Alcotest.(check bool) "contains WGS" true (contains "WGS" wkt);
  Gdal.SpatialReference.destroy srs

let test_srs_roundtrip () =
  (* EPSG -> WKT -> SRS should produce a valid SRS *)
  let srs1 = unwrap "epsg" (Gdal.SpatialReference.of_epsg 4326) in
  let wkt = unwrap "to_wkt" (Gdal.SpatialReference.to_wkt srs1) in
  let srs2 = unwrap "of_wkt" (Gdal.SpatialReference.of_wkt wkt) in
  let wkt2 = unwrap "to_wkt2" (Gdal.SpatialReference.to_wkt srs2) in
  Alcotest.(check string) "roundtrip" wkt wkt2;
  Gdal.SpatialReference.destroy srs1;
  Gdal.SpatialReference.destroy srs2

let spatial_ref_tests = [
  Alcotest.test_case "of_epsg" `Quick test_srs_of_epsg;
  Alcotest.test_case "of_epsg error" `Quick test_srs_of_epsg_error;
  Alcotest.test_case "of_wkt" `Quick test_srs_of_wkt;
  Alcotest.test_case "of_wkt error" `Quick test_srs_of_wkt_error;
  Alcotest.test_case "to_wkt" `Quick test_srs_to_wkt;
  Alcotest.test_case "roundtrip" `Quick test_srs_roundtrip;
  Alcotest.test_case "axis mapping" `Quick test_srs_axis_mapping;
]

(* ---- CoordinateTransformation tests ---- *)

let test_ct_create_destroy () =
  with_4326_to_3857 (fun _ct -> ())

let test_ct_transform_bounds () =
  with_4326_to_3857 (fun ct ->
    let (xmin, ymin, xmax, ymax) = unwrap "bounds"
      (Gdal.CoordinateTransformation.transform_bounds ct
         ~xmin:(-10.0) ~ymin:50.0 ~xmax:2.0 ~ymax:60.0 ~density:21) in
    let eps = Alcotest.float 1.0 in
    Alcotest.check eps "xmin" (mercator_x (-10.0)) xmin;
    Alcotest.check eps "xmax" (mercator_x 2.0) xmax;
    Alcotest.check eps "ymin" (mercator_y 50.0) ymin;
    Alcotest.check eps "ymax" (mercator_y 60.0) ymax)

let test_ct_transform_point () =
  with_4326_to_3857 (fun ct ->
    (* London: lon=-0.1278, lat=51.5074 *)
    let lon = -0.1278 and lat = 51.5074 in
    let (x, y, z) = unwrap "pt"
      (Gdal.CoordinateTransformation.transform_point ct
         ~x:lon ~y:lat ~z:0.0) in
    let eps = Alcotest.float 1.0 in
    Alcotest.check eps "x" (mercator_x lon) x;
    Alcotest.check eps "y" (mercator_y lat) y;
    Alcotest.check (Alcotest.float 0.001) "z" 0.0 z)

let test_ct_transform_point_identity () =
  let srs = unwrap "srs" (Gdal.SpatialReference.of_epsg 4326) in
  let srs2 = unwrap "srs2" (Gdal.SpatialReference.of_epsg 4326) in
  let ct = unwrap "ct" (Gdal.CoordinateTransformation.create srs srs2) in
  let (x, y, _z) = unwrap "pt"
    (Gdal.CoordinateTransformation.transform_point ct
       ~x:12.34 ~y:56.78 ~z:0.0) in
  let eps = Alcotest.float 1e-6 in
  Alcotest.check eps "x" 12.34 x;
  Alcotest.check eps "y" 56.78 y;
  Gdal.CoordinateTransformation.destroy ct;
  Gdal.SpatialReference.destroy srs;
  Gdal.SpatialReference.destroy srs2

let coord_transform_tests = [
  Alcotest.test_case "create/destroy" `Quick test_ct_create_destroy;
  Alcotest.test_case "transform_point" `Quick test_ct_transform_point;
  Alcotest.test_case "transform_point identity" `Quick test_ct_transform_point_identity;
  Alcotest.test_case "transform_bounds" `Quick test_ct_transform_bounds;
]

(* ---- Python comparison tests ---- *)

let python_available =
  let rc = Sys.command "python3 -c 'from osgeo import gdal' 2>/dev/null" in
  rc = 0

let skip_without_python () =
  if not python_available then
    Alcotest.skip ()

let run_python script =
  let f = tmp "pyscript.py" in
  let oc = open_out f in
  output_string oc script;
  close_out oc;
  let ic = Unix.open_process_in (Printf.sprintf "python3 %s 2>/dev/null" f) in
  let buf = Buffer.create 256 in
  (try while true do Buffer.add_char buf (input_char ic) done
   with End_of_file -> ());
  let _ = Unix.close_process_in ic in
  String.trim (Buffer.contents buf)

let test_python_pixel_values () =
  skip_without_python ();
  let py_output = run_python (Printf.sprintf {|
from osgeo import gdal
ds = gdal.Open("%s")
band = ds.GetRasterBand(1)
arr = band.ReadAsArray(0, 0, %d, %d)
vals = []
for y in range(%d):
    for x in range(%d):
        vals.append(str(int(arr[y][x])))
print(",".join(vals))
|} test_tif test_width test_height test_height test_width) in
  let py_vals = String.split_on_char ',' py_output
    |> List.map (fun s -> int_of_string (String.trim s)) in
  let ds = unwrap "open" (Gdal.Dataset.open_ test_tif) in
  let band = unwrap "band" (Gdal.Dataset.get_band ds 1) in
  let arr = unwrap "read" (Gdal.RasterBand.read Gdal.BA_byte band) in
  let ocaml_vals =
    List.init (test_height * test_width) (fun i ->
      let y = i / test_width in
      let x = i mod test_width in
      Array2.get arr y x)
  in
  List.iter2 (fun py_v ocaml_v ->
    Alcotest.(check int) "pixel" py_v ocaml_v
  ) py_vals ocaml_vals;
  Gdal.Dataset.close ds

let test_python_geo_transform () =
  skip_without_python ();
  let py_output = run_python (Printf.sprintf {|
from osgeo import gdal
ds = gdal.Open("%s")
gt = ds.GetGeoTransform()
print(",".join(str(v) for v in gt))
|} test_tif) in
  let py_vals = String.split_on_char ',' py_output
    |> List.map (fun s -> float_of_string (String.trim s)) in
  let ds = unwrap "open" (Gdal.Dataset.open_ test_tif) in
  let gt = unwrap "gt" (Gdal.Dataset.get_geo_transform ds) in
  let ocaml_vals = Gdal.[ gt.origin_x; gt.pixel_width; gt.row_rotation;
                           gt.origin_y; gt.col_rotation; gt.pixel_height ] in
  List.iter2 (fun py_v ocaml_v ->
    Alcotest.check float_eps "geo_transform" py_v ocaml_v
  ) py_vals ocaml_vals;
  Gdal.Dataset.close ds

let test_python_dimensions () =
  skip_without_python ();
  let py_output = run_python (Printf.sprintf {|
from osgeo import gdal
ds = gdal.Open("%s")
print(f"{ds.RasterXSize},{ds.RasterYSize},{ds.RasterCount}")
|} test_tif) in
  let parts = String.split_on_char ',' py_output
    |> List.map (fun s -> int_of_string (String.trim s)) in
  let ds = unwrap "open" (Gdal.Dataset.open_ test_tif) in
  (match parts with
   | [px; py; pb] ->
     Alcotest.(check int) "x" px (Gdal.Dataset.raster_x_size ds);
     Alcotest.(check int) "y" py (Gdal.Dataset.raster_y_size ds);
     Alcotest.(check int) "bands" pb (Gdal.Dataset.raster_count ds)
   | _ -> Alcotest.fail "unexpected python output");
  Gdal.Dataset.close ds

let test_python_transform_bounds () =
  skip_without_python ();
  let py_output = run_python {|
from osgeo import osr
src = osr.SpatialReference()
src.ImportFromEPSG(4326)
src.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
dst = osr.SpatialReference()
dst.ImportFromEPSG(3857)
dst.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
ct = osr.CoordinateTransformation(src, dst)
xmin, ymin, xmax, ymax = ct.TransformBounds(-10.0, 50.0, 2.0, 60.0, 21)
print(f"{xmin},{ymin},{xmax},{ymax}")
|} in
  let py_vals = String.split_on_char ',' py_output
    |> List.map (fun s -> float_of_string (String.trim s)) in
  with_4326_to_3857 (fun ct ->
    let (oxmin, oymin, oxmax, oymax) = unwrap "bounds"
      (Gdal.CoordinateTransformation.transform_bounds ct
         ~xmin:(-10.0) ~ymin:50.0 ~xmax:2.0 ~ymax:60.0 ~density:21) in
    let eps = Alcotest.float 1.0 in
    match py_vals with
    | [pxmin; pymin; pxmax; pymax] ->
      Alcotest.check eps "xmin" pxmin oxmin;
      Alcotest.check eps "ymin" pymin oymin;
      Alcotest.check eps "xmax" pxmax oxmax;
      Alcotest.check eps "ymax" pymax oymax
    | _ -> Alcotest.fail "unexpected python output")

let test_python_band_metadata () =
  skip_without_python ();
  let py_output = run_python (Printf.sprintf {|
from osgeo import gdal
ds = gdal.Open("%s")
band = ds.GetRasterBand(1)
print(f"{band.DataType},{band.XSize},{band.YSize},{band.GetBlockSize()[0]},{band.GetBlockSize()[1]}")
|} test_tif) in
  let parts = String.split_on_char ',' py_output
    |> List.map (fun s -> int_of_string (String.trim s)) in
  let ds = unwrap "open" (Gdal.Dataset.open_ test_tif) in
  let band = unwrap "band" (Gdal.Dataset.get_band ds 1) in
  let dt = unwrap "dt" (Gdal.RasterBand.data_type band) in
  let (bx, by) = unwrap "bs" (Gdal.RasterBand.block_size band) in
  let xs = unwrap "xs" (Gdal.RasterBand.x_size band) in
  let ys = unwrap "ys" (Gdal.RasterBand.y_size band) in
  (match parts with
   | [pdt; pxs; pys; pbx; pby] ->
     Alcotest.(check int) "data_type" pdt (Gdal.data_type_to_int dt);
     Alcotest.(check int) "x_size" pxs xs;
     Alcotest.(check int) "y_size" pys ys;
     Alcotest.(check int) "block_x" pbx bx;
     Alcotest.(check int) "block_y" pby by
   | _ -> Alcotest.fail "unexpected python output");
  Gdal.Dataset.close ds

let test_python_transform_point () =
  skip_without_python ();
  let py_output = run_python {|
from osgeo import osr
src = osr.SpatialReference()
src.ImportFromEPSG(4326)
src.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
dst = osr.SpatialReference()
dst.ImportFromEPSG(3857)
dst.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
ct = osr.CoordinateTransformation(src, dst)
x, y, z = ct.TransformPoint(-0.1278, 51.5074, 0.0)
print(f"{x},{y},{z}")
|} in
  let py_vals = String.split_on_char ',' py_output
    |> List.map (fun s -> float_of_string (String.trim s)) in
  with_4326_to_3857 (fun ct ->
    let (ox, oy, oz) = unwrap "pt"
      (Gdal.CoordinateTransformation.transform_point ct
         ~x:(-0.1278) ~y:51.5074 ~z:0.0) in
    let eps = Alcotest.float 0.01 in
    match py_vals with
    | [px; py; pz] ->
      Alcotest.check eps "x" px ox;
      Alcotest.check eps "y" py oy;
      Alcotest.check eps "z" pz oz
    | _ -> Alcotest.fail "unexpected python output")

let python_tests = [
  Alcotest.test_case "pixel values" `Quick test_python_pixel_values;
  Alcotest.test_case "geo_transform" `Quick test_python_geo_transform;
  Alcotest.test_case "dimensions" `Quick test_python_dimensions;
  Alcotest.test_case "transform_point" `Quick test_python_transform_point;
  Alcotest.test_case "transform_bounds" `Quick test_python_transform_bounds;
  Alcotest.test_case "band metadata" `Quick test_python_band_metadata;
]

(* ---- Run ---- *)

let () =
  Alcotest.run "gdal" [
    "enums", enum_tests;
    "toplevel", toplevel_tests;
    "Driver", driver_tests;
    "Dataset", dataset_tests;
    "RasterBand", raster_band_tests;
    "SpatialReference", spatial_ref_tests;
    "CoordinateTransformation", coord_transform_tests;
    "python comparison", python_tests;
  ]
