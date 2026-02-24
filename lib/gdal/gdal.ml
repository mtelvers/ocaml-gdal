open Bigarray

let ( let* ) = Result.bind

(* ---- Enum types ---- *)

type data_type =
  | Byte
  | UInt16
  | Int16
  | UInt32
  | Int32
  | Float32
  | Float64
  | Int8
  | Unknown

let data_type_to_int = function
  | Byte -> Gdal_raw.gdt_byte
  | UInt16 -> Gdal_raw.gdt_uint16
  | Int16 -> Gdal_raw.gdt_int16
  | UInt32 -> Gdal_raw.gdt_uint32
  | Int32 -> Gdal_raw.gdt_int32
  | Float32 -> Gdal_raw.gdt_float32
  | Float64 -> Gdal_raw.gdt_float64
  | Int8 -> Gdal_raw.gdt_int8
  | Unknown -> Gdal_raw.gdt_unknown

let data_type_of_int = function
  | n when n = Gdal_raw.gdt_byte -> Byte
  | n when n = Gdal_raw.gdt_uint16 -> UInt16
  | n when n = Gdal_raw.gdt_int16 -> Int16
  | n when n = Gdal_raw.gdt_uint32 -> UInt32
  | n when n = Gdal_raw.gdt_int32 -> Int32
  | n when n = Gdal_raw.gdt_float32 -> Float32
  | n when n = Gdal_raw.gdt_float64 -> Float64
  | n when n = Gdal_raw.gdt_int8 -> Int8
  | _ -> Unknown

let string_of_data_type = function
  | Byte -> "Byte"
  | UInt16 -> "UInt16"
  | Int16 -> "Int16"
  | UInt32 -> "UInt32"
  | Int32 -> "Int32"
  | Float32 -> "Float32"
  | Float64 -> "Float64"
  | Int8 -> "Int8"
  | Unknown -> "Unknown"

type access = ReadOnly | Update

let access_to_int = function
  | ReadOnly -> Gdal_raw.ga_readonly
  | Update -> Gdal_raw.ga_update

type rw_flag = Read | Write

let rw_flag_to_int = function
  | Read -> Gdal_raw.gf_read
  | Write -> Gdal_raw.gf_write

(* ---- Abstract types ---- *)

type dataset = {
  raw : Gdal_raw.dataset_h;
  mutable closed : bool;
}

type band = {
  raw : Gdal_raw.raster_band_h;
  _parent : dataset;
}

type driver = { raw : Gdal_raw.driver_h }

(* ---- Geo-transform ---- *)

type geo_transform = {
  origin_x : float;
  pixel_width : float;
  row_rotation : float;
  origin_y : float;
  col_rotation : float;
  pixel_height : float;
}

(* ---- Error handling ---- *)

let check_cpl_err code =
  if code >= Gdal_raw.ce_failure then
    Error (Gdal_raw.cpl_get_last_error_msg ())
  else Ok ()

let check_ogr_err code =
  if code <> Gdal_raw.ogrerr_none then
    Error (Gdal_raw.cpl_get_last_error_msg ())
  else Ok ()

let check_null ptr what =
  if Ctypes.is_null ptr then
    Error (Printf.sprintf "%s: %s" what (Gdal_raw.cpl_get_last_error_msg ()))
  else Ok ptr

(* ---- Initialization ---- *)

let init () = Gdal_raw.all_register ()

(* ---- Configuration ---- *)

let set_config_option key value =
  Gdal_raw.cpl_set_config_option key value

(* ---- GDALOpenEx flag constants ---- *)

let gdal_of_readonly = Gdal_raw.gdal_of_readonly
let gdal_of_update = Gdal_raw.gdal_of_update
let gdal_of_raster = Gdal_raw.gdal_of_raster
let gdal_of_vector = Gdal_raw.gdal_of_vector
let gdal_of_verbose_error = Gdal_raw.gdal_of_verbose_error

(* ---- Axis mapping strategy constants ---- *)

let oams_traditional_gis_order = Gdal_raw.oams_traditional_gis_order
let oams_authority_compliant = Gdal_raw.oams_authority_compliant

(* ---- Helpers ---- *)

let cstring_of_string s =
  let len = String.length s in
  let p = Ctypes.allocate_n Ctypes.char ~count:(len + 1) in
  for i = 0 to len - 1 do
    Ctypes.(p +@ i <-@ s.[i])
  done;
  Ctypes.(p +@ len <-@ '\000');
  p

let with_csl strs f =
  let char_ptrs = List.map cstring_of_string strs in
  let n = List.length char_ptrs in
  let arr = Ctypes.CArray.make Ctypes.(ptr char) (n + 1) in
  List.iteri (fun i p -> Ctypes.CArray.set arr i p) char_ptrs;
  Ctypes.CArray.set arr n Ctypes.(from_voidp char null);
  let result = f (Ctypes.CArray.start arr) in
  ignore (char_ptrs, arr);
  result

(* ---- Raster I/O via Bigarray ---- *)

type ('a, 'b) ba_kind_witness =
  | BA_int8 : (int, int8_signed_elt) ba_kind_witness
  | BA_byte : (int, int8_unsigned_elt) ba_kind_witness
  | BA_uint16 : (int, int16_unsigned_elt) ba_kind_witness
  | BA_int16 : (int, int16_signed_elt) ba_kind_witness
  | BA_int32 : (int32, int32_elt) ba_kind_witness
  | BA_int64 : (int64, int64_elt) ba_kind_witness
  | BA_float32 : (float, float32_elt) ba_kind_witness
  | BA_float64 : (float, float64_elt) ba_kind_witness

let ba_kind : type a b. (a, b) ba_kind_witness -> (a, b) Bigarray.kind = function
  | BA_int8 -> Int8_signed
  | BA_byte -> Int8_unsigned
  | BA_uint16 -> Int16_unsigned
  | BA_int16 -> Int16_signed
  | BA_int32 -> Int32
  | BA_int64 -> Int64
  | BA_float32 -> Float32
  | BA_float64 -> Float64

let gdal_type : type a b. (a, b) ba_kind_witness -> int = function
  | BA_int8 -> Gdal_raw.gdt_int8
  | BA_byte -> Gdal_raw.gdt_byte
  | BA_uint16 -> Gdal_raw.gdt_uint16
  | BA_int16 -> Gdal_raw.gdt_int16
  | BA_int32 -> Gdal_raw.gdt_int32
  | BA_int64 -> Gdal_raw.gdt_int64
  | BA_float32 -> Gdal_raw.gdt_float32
  | BA_float64 -> Gdal_raw.gdt_float64

let pixel_size : type a b. (a, b) ba_kind_witness -> int = function
  | BA_int8 -> 1
  | BA_byte -> 1
  | BA_uint16 -> 2
  | BA_int16 -> 2
  | BA_int32 -> 4
  | BA_int64 -> 8
  | BA_float32 -> 4
  | BA_float64 -> 8

(* ---- Dataset internals ---- *)

let finalise_dataset (ds : dataset) =
  if not ds.closed then begin
    Gdal_raw.close ds.raw;
    ds.closed <- true
  end

let wrap_dataset raw =
  let ds = { raw; closed = false } in
  Gc.finalise finalise_dataset ds;
  Ok ds

(* ---- Driver module ---- *)

module Driver = struct
  let by_name name =
    let* raw = check_null (Gdal_raw.get_driver_by_name name)
      ("Driver not found: " ^ name) in
    Ok ({ raw } : driver)

  let create (driver : driver) ~filename ~width ~height ~bands ?(options = []) dtype =
    if options = [] then
      let* raw = check_null
        (Gdal_raw.create driver.raw filename width height bands
           (data_type_to_int dtype) None)
        ("Failed to create " ^ filename) in
      wrap_dataset raw
    else
      with_csl options (fun opts_arr ->
        let raw = Gdal_raw.create driver.raw filename width height bands
          (data_type_to_int dtype) (Some opts_arr) in
        if Ctypes.is_null raw then
          Error ("Failed to create " ^ filename)
        else
          wrap_dataset raw)

  let description (driver : driver) =
    Gdal_raw.get_driver_long_name driver.raw

  let create_copy (driver : driver) ~filename ~(src : dataset) ?(strict = false) () =
    let* raw = check_null
      (Gdal_raw.create_copy driver.raw filename src.raw
         (if strict then 1 else 0) None Ctypes.null Ctypes.null)
      ("Failed to create copy " ^ filename) in
    wrap_dataset raw
end

(* ---- Dataset module ---- *)

module Dataset = struct
  let open_ ?(access = ReadOnly) path =
    let* raw = check_null (Gdal_raw.open_ path (access_to_int access))
      ("Failed to open " ^ path) in
    wrap_dataset raw

  let open_ex ?(flags = Gdal_raw.gdal_of_readonly lor Gdal_raw.gdal_of_raster lor Gdal_raw.gdal_of_verbose_error) ?(thread_safe = false) path =
    let flags = if thread_safe then flags lor Gdal_raw.gdal_of_thread_safe else flags in
    let* raw = check_null (Gdal_raw.open_ex path flags None None None)
      ("Failed to open " ^ path) in
    wrap_dataset raw

  let close (ds : dataset) =
    if not ds.closed then begin
      Gdal_raw.close ds.raw;
      ds.closed <- true
    end

  let with_dataset ?access path f =
    let* ds = open_ ?access path in
    Ok (Fun.protect
          ~finally:(fun () -> close ds)
          (fun () -> f ds))

  let raster_x_size (ds : dataset) = Gdal_raw.get_raster_x_size ds.raw
  let raster_y_size (ds : dataset) = Gdal_raw.get_raster_y_size ds.raw
  let raster_count (ds : dataset) = Gdal_raw.get_raster_count ds.raw

  let get_geo_transform (ds : dataset) =
    let buf = Ctypes.CArray.make Ctypes.double 6 in
    let* () = check_cpl_err
      (Gdal_raw.get_geo_transform ds.raw (Ctypes.CArray.start buf)) in
    let g = Ctypes.CArray.get buf in
    Ok {
      origin_x = g 0;
      pixel_width = g 1;
      row_rotation = g 2;
      origin_y = g 3;
      col_rotation = g 4;
      pixel_height = g 5;
    }

  let set_geo_transform (ds : dataset) gt =
    let buf = Ctypes.CArray.make Ctypes.double 6 in
    let s = Ctypes.CArray.set buf in
    s 0 gt.origin_x;
    s 1 gt.pixel_width;
    s 2 gt.row_rotation;
    s 3 gt.origin_y;
    s 4 gt.col_rotation;
    s 5 gt.pixel_height;
    check_cpl_err (Gdal_raw.set_geo_transform ds.raw (Ctypes.CArray.start buf))

  let projection (ds : dataset) = Gdal_raw.get_projection_ref ds.raw

  let set_projection (ds : dataset) proj =
    check_cpl_err (Gdal_raw.set_projection ds.raw proj)

  let get_band (ds : dataset) n =
    let* raw = check_null (Gdal_raw.get_raster_band ds.raw n)
      (Printf.sprintf "Band %d not found" n) in
    Ok ({ raw; _parent = ds } : band)

  let is_null (ds : dataset) = Ctypes.is_null ds.raw

  let description (ds : dataset) =
    Gdal_raw.get_description (Ctypes.to_voidp ds.raw)

  let get_metadata_item (ds : dataset) ~key ~domain =
    Gdal_raw.get_metadata_item ds.raw key domain

  let run_with_opts options ~make_opts ~free_opts f =
    with_csl options (fun opts_arr ->
      let app_opts = make_opts opts_arr Ctypes.null in
      Fun.protect
        ~finally:(fun () -> free_opts app_opts)
        (fun () ->
          let err = Ctypes.CArray.make Ctypes.int 1 in
          Ctypes.CArray.set err 0 0;
          let result = f app_opts (Ctypes.CArray.start err) in
          if Ctypes.CArray.get err 0 <> 0 || Ctypes.is_null result then
            Error (Printf.sprintf "Operation failed: %s"
                     (Gdal_raw.cpl_get_last_error_msg ()))
          else
            wrap_dataset result))

  let warp (ds : dataset) ~dst_filename options =
    let src_arr = Ctypes.CArray.make Gdal_raw.dataset_h 1 in
    Ctypes.CArray.set src_arr 0 ds.raw;
    run_with_opts options
      ~make_opts:Gdal_raw.gdal_warp_app_options_new
      ~free_opts:Gdal_raw.gdal_warp_app_options_free
      (fun wopts err ->
        Gdal_raw.gdal_warp dst_filename Ctypes.null 1
          (Ctypes.CArray.start src_arr) wopts err)

  let translate (ds : dataset) ~dst_filename options =
    run_with_opts options
      ~make_opts:Gdal_raw.gdal_translate_options_new
      ~free_opts:Gdal_raw.gdal_translate_options_free
      (fun topts err ->
        Gdal_raw.gdal_translate dst_filename ds.raw topts err)
end

(* ---- RasterBand module ---- *)

let check_band (band : band) =
  if band._parent.closed then Error "Dataset has been closed"
  else Ok ()

module RasterBand = struct
  let data_type (band : band) =
    let* () = check_band band in
    Ok (data_type_of_int (Gdal_raw.get_raster_data_type band.raw))

  let block_size (band : band) =
    let* () = check_band band in
    let x = Ctypes.CArray.make Ctypes.int 1 in
    let y = Ctypes.CArray.make Ctypes.int 1 in
    Gdal_raw.get_block_size band.raw (Ctypes.CArray.start x)
      (Ctypes.CArray.start y);
    Ok (Ctypes.CArray.get x 0, Ctypes.CArray.get y 0)

  let no_data_value (band : band) =
    let* () = check_band band in
    let success = Ctypes.CArray.make Ctypes.int 1 in
    let v = Gdal_raw.get_raster_no_data_value band.raw (Ctypes.CArray.start success) in
    Ok (if Ctypes.CArray.get success 0 <> 0 then Some v else None)

  let x_size (band : band) =
    let* () = check_band band in
    Ok (Gdal_raw.get_raster_band_x_size band.raw)

  let y_size (band : band) =
    let* () = check_band band in
    Ok (Gdal_raw.get_raster_band_y_size band.raw)

  let read_region (type a b) (w : (a, b) ba_kind_witness) (band : band) ~x_off ~y_off
      ~x_size ~y_size ~buf_x ~buf_y : ((a, b, c_layout) Array2.t, string) result =
    let* () = check_band band in
    let arr = Array2.create (ba_kind w) c_layout buf_y buf_x in
    let data_ptr = Ctypes.bigarray_start Ctypes.array2 arr in
    let ps = pixel_size w in
    let void_ptr = Ctypes.to_voidp data_ptr in
    let* () = check_cpl_err
      (Gdal_raw.raster_io band.raw (rw_flag_to_int Read) x_off y_off x_size
         y_size void_ptr buf_x buf_y (gdal_type w) ps (ps * buf_x)) in
    Ok arr

  let read w (band : band) =
    let* () = check_band band in
    let ds = band._parent in
    let xs = Dataset.raster_x_size ds in
    let ys = Dataset.raster_y_size ds in
    read_region w band ~x_off:0 ~y_off:0 ~x_size:xs ~y_size:ys ~buf_x:xs
      ~buf_y:ys

  let write_region (type a b) (w : (a, b) ba_kind_witness) (band : band) ~x_off
      ~y_off ~x_size ~y_size (arr : (a, b, c_layout) Array2.t) :
      (unit, string) result =
    let* () = check_band band in
    let buf_y = Array2.dim1 arr in
    let buf_x = Array2.dim2 arr in
    let data_ptr = Ctypes.bigarray_start Ctypes.array2 arr in
    let ps = pixel_size w in
    let void_ptr = Ctypes.to_voidp data_ptr in
    check_cpl_err
      (Gdal_raw.raster_io band.raw (rw_flag_to_int Write) x_off y_off x_size
         y_size void_ptr buf_x buf_y (gdal_type w) ps (ps * buf_x))

  let read_byte (band : band) ~x_off ~y_off ~x_size ~y_size =
    read_region BA_byte band ~x_off ~y_off ~x_size ~y_size ~buf_x:x_size
      ~buf_y:y_size
end

(* ---- SpatialReference module ---- *)

module SpatialReference = struct
  type t = {
    raw : Gdal_raw.spatial_reference_h;
    mutable closed : bool;
  }

  let finalise_srs t =
    if not t.closed then begin
      Gdal_raw.osr_destroy_spatial_reference t.raw;
      t.closed <- true
    end

  let make_srs import_fn =
    let* raw = check_null (Gdal_raw.osr_new_spatial_reference None)
      "Failed to create SpatialReference" in
    match import_fn raw with
    | Error msg ->
      Gdal_raw.osr_destroy_spatial_reference raw;
      Error msg
    | Ok () ->
      Gdal_raw.osr_set_axis_mapping_strategy raw
        Gdal_raw.oams_traditional_gis_order;
      let t = { raw; closed = false } in
      Gc.finalise finalise_srs t;
      Ok t

  let of_epsg code =
    make_srs (fun raw ->
      check_ogr_err (Gdal_raw.osr_import_from_epsg raw code))

  let of_wkt wkt =
    make_srs (fun raw ->
      let cstr = cstring_of_string wkt in
      let wkt_ptr = Ctypes.allocate Ctypes.(ptr char) cstr in
      let err = Gdal_raw.osr_import_from_wkt raw wkt_ptr in
      ignore (cstr, wkt_ptr);
      check_ogr_err err)

  let to_wkt (t : t) =
    let out = Ctypes.allocate Ctypes.(ptr char) (Ctypes.from_voidp Ctypes.char Ctypes.null) in
    let* () = check_ogr_err (Gdal_raw.osr_export_to_wkt t.raw out) in
    let p = Ctypes.(!@ out) in
    if Ctypes.is_null p then Error "OSRExportToWkt returned null"
    else
      let s = Ctypes.coerce Ctypes.(ptr char) Ctypes.string p in
      Gdal_raw.cpl_free (Ctypes.to_voidp p);
      Ok s

  let set_axis_mapping_strategy (t : t) strategy =
    Gdal_raw.osr_set_axis_mapping_strategy t.raw strategy

  let destroy t = finalise_srs t
end

(* ---- CoordinateTransformation module ---- *)

module CoordinateTransformation = struct
  type t = {
    raw : Gdal_raw.coordinate_transformation_h;
    mutable closed : bool;
  }

  let finalise_ct t =
    if not t.closed then begin
      Gdal_raw.oct_destroy_coordinate_transformation t.raw;
      t.closed <- true
    end

  let create (src : SpatialReference.t) (dst : SpatialReference.t) =
    let* raw = check_null (Gdal_raw.oct_new_coordinate_transformation src.raw dst.raw)
      "Failed to create CoordinateTransformation" in
    let t = { raw; closed = false } in
    Gc.finalise finalise_ct t;
    Ok t

  let transform_point (ct : t) ~x ~y ~z =
    let xa = Ctypes.CArray.make Ctypes.double 1 in
    let ya = Ctypes.CArray.make Ctypes.double 1 in
    let za = Ctypes.CArray.make Ctypes.double 1 in
    Ctypes.CArray.set xa 0 x;
    Ctypes.CArray.set ya 0 y;
    Ctypes.CArray.set za 0 z;
    let ok =
      Gdal_raw.oct_transform ct.raw 1
        (Ctypes.CArray.start xa) (Ctypes.CArray.start ya)
        (Ctypes.CArray.start za)
    in
    if ok = 0 then
      Error
        (Printf.sprintf "TransformPoint failed: %s"
           (Gdal_raw.cpl_get_last_error_msg ()))
    else
      Ok (Ctypes.CArray.get xa 0, Ctypes.CArray.get ya 0, Ctypes.CArray.get za 0)

  let transform_bounds (ct : t) ~xmin ~ymin ~xmax ~ymax ~density =
    let out_xmin = Ctypes.CArray.make Ctypes.double 1 in
    let out_ymin = Ctypes.CArray.make Ctypes.double 1 in
    let out_xmax = Ctypes.CArray.make Ctypes.double 1 in
    let out_ymax = Ctypes.CArray.make Ctypes.double 1 in
    let ok =
      Gdal_raw.oct_transform_bounds ct.raw xmin ymin xmax ymax
        (Ctypes.CArray.start out_xmin) (Ctypes.CArray.start out_ymin)
        (Ctypes.CArray.start out_xmax) (Ctypes.CArray.start out_ymax)
        density
    in
    if ok = 0 then
      Error
        (Printf.sprintf "TransformBounds failed: %s"
           (Gdal_raw.cpl_get_last_error_msg ()))
    else
      Ok
        ( Ctypes.CArray.get out_xmin 0,
          Ctypes.CArray.get out_ymin 0,
          Ctypes.CArray.get out_xmax 0,
          Ctypes.CArray.get out_ymax 0 )

  let destroy ct = finalise_ct ct
end

(** {1 File operations} *)

let copy_file ~src ~dst =
  let rc = Gdal_raw.cpl_copy_file dst src in
  if rc = 0 then Ok ()
  else Error (Printf.sprintf "CPLCopyFile failed: %s -> %s" src dst)
