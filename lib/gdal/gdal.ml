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
  | CInt16
  | CInt32
  | CFloat32
  | CFloat64
  | UInt64
  | Int64
  | Int8
  | Float16
  | Unknown

let gdal_version_num =
  lazy (int_of_string (Gdal_raw.version_info "VERSION_NUM"))

let data_type_to_int = function
  | Byte -> Gdal_raw.gdt_byte
  | UInt16 -> Gdal_raw.gdt_uint16
  | Int16 -> Gdal_raw.gdt_int16
  | UInt32 -> Gdal_raw.gdt_uint32
  | Int32 -> Gdal_raw.gdt_int32
  | Float32 -> Gdal_raw.gdt_float32
  | Float64 -> Gdal_raw.gdt_float64
  | CInt16 -> Gdal_raw.gdt_cint16
  | CInt32 -> Gdal_raw.gdt_cint32
  | CFloat32 -> Gdal_raw.gdt_cfloat32
  | CFloat64 -> Gdal_raw.gdt_cfloat64
  | UInt64 -> Gdal_raw.gdt_uint64
  | Int64 -> Gdal_raw.gdt_int64
  | Int8 -> Gdal_raw.gdt_int8
  | Float16 ->
    if Lazy.force gdal_version_num < 3110000 then
      failwith "Float16 requires GDAL >= 3.11"
    else Gdal_raw.gdt_float16
  | Unknown -> Gdal_raw.gdt_unknown

let data_type_of_int = function
  | n when n = Gdal_raw.gdt_byte -> Byte
  | n when n = Gdal_raw.gdt_uint16 -> UInt16
  | n when n = Gdal_raw.gdt_int16 -> Int16
  | n when n = Gdal_raw.gdt_uint32 -> UInt32
  | n when n = Gdal_raw.gdt_int32 -> Int32
  | n when n = Gdal_raw.gdt_float32 -> Float32
  | n when n = Gdal_raw.gdt_float64 -> Float64
  | n when n = Gdal_raw.gdt_cint16 -> CInt16
  | n when n = Gdal_raw.gdt_cint32 -> CInt32
  | n when n = Gdal_raw.gdt_cfloat32 -> CFloat32
  | n when n = Gdal_raw.gdt_cfloat64 -> CFloat64
  | n when n = Gdal_raw.gdt_uint64 -> UInt64
  | n when n = Gdal_raw.gdt_int64 -> Int64
  | n when n = Gdal_raw.gdt_int8 -> Int8
  | n when n = Gdal_raw.gdt_float16 -> Float16
  | _ -> Unknown

let string_of_data_type = function
  | Byte -> "Byte"
  | UInt16 -> "UInt16"
  | Int16 -> "Int16"
  | UInt32 -> "UInt32"
  | Int32 -> "Int32"
  | Float32 -> "Float32"
  | Float64 -> "Float64"
  | CInt16 -> "CInt16"
  | CInt32 -> "CInt32"
  | CFloat32 -> "CFloat32"
  | CFloat64 -> "CFloat64"
  | UInt64 -> "UInt64"
  | Int64 -> "Int64"
  | Int8 -> "Int8"
  | Float16 -> "Float16"
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

(* GDAL leaves the error buffer empty for failures it does not consider
   worth a message — a missing layer or driver name, say — so only append it
   when there is something to append, rather than ending every such error
   with a bare ": ". *)
let check_null ptr what =
  if Ctypes.is_null ptr then
    match Gdal_raw.cpl_get_last_error_msg () with
    | "" -> Error what
    | msg -> Error (Printf.sprintf "%s: %s" what msg)
  else Ok ptr

(* ---- Initialization ---- *)

let init () = Gdal_raw.all_register ()

let usable_physical_ram () = Gdal_raw.usable_physical_ram ()

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

  (* Adopt an already-created handle that we are responsible for freeing.
     Not exported: callers outside this file can only obtain a [t] through
     the importers below, which never hand out a borrowed handle. *)
  let of_raw_owned raw =
    Gdal_raw.osr_set_axis_mapping_strategy raw
      Gdal_raw.oams_traditional_gis_order;
    let t = { raw; closed = false } in
    Gc.finalise finalise_srs t;
    t

  let make_srs import_fn =
    let* raw = check_null (Gdal_raw.osr_new_spatial_reference None)
      "Failed to create SpatialReference" in
    match import_fn raw with
    | Error msg ->
      Gdal_raw.osr_destroy_spatial_reference raw;
      Error msg
    | Ok () -> Ok (of_raw_owned raw)

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

  let name (t : t) = Gdal_raw.osr_get_name t.raw

  let authority_code ?target (t : t) =
    Gdal_raw.osr_get_authority_code t.raw target

  let is_projected (t : t) = Gdal_raw.osr_is_projected t.raw <> 0
  let is_geographic (t : t) = Gdal_raw.osr_is_geographic t.raw <> 0

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

  (* Transform a whole vertex array in one FFI call. OCTTransform is
     variable-cost per point (datum shifts, grid lookups), so batching also
     lets GDAL amortise its internal setup across the ring. *)
  let transform_points (ct : t) (pts : (float * float) array) =
    let n = Array.length pts in
    if n = 0 then Ok [||]
    else begin
      let xs = Ctypes.CArray.make Ctypes.double n in
      let ys = Ctypes.CArray.make Ctypes.double n in
      let zs = Ctypes.CArray.make Ctypes.double n in
      Array.iteri
        (fun i (x, y) ->
          Ctypes.CArray.set xs i x;
          Ctypes.CArray.set ys i y;
          Ctypes.CArray.set zs i 0.)
        pts;
      let ok =
        Gdal_raw.oct_transform ct.raw n (Ctypes.CArray.start xs)
          (Ctypes.CArray.start ys) (Ctypes.CArray.start zs)
      in
      if ok = 0 then
        Error
          (Printf.sprintf "Transform of %d point(s) failed: %s" n
             (Gdal_raw.cpl_get_last_error_msg ()))
      else
        Ok
          (Array.init n (fun i ->
               (Ctypes.CArray.get xs i, Ctypes.CArray.get ys i)))
    end

  let destroy ct = finalise_ct ct
end

(* ---- Vector (OGR) module ---- *)

module Vector = struct
  type geometry_type =
    | Point
    | LineString
    | Polygon
    | MultiPoint
    | MultiLineString
    | MultiPolygon
    | GeometryCollection
    | LinearRing
    | CircularString
    | CompoundCurve
    | CurvePolygon
    | MultiCurve
    | MultiSurface
    | PolyhedralSurface
    | Tin
    | Triangle
    | NoGeometry
    | UnknownGeometry of int

  let geometry_type_of_int n =
    (* Strip the Z/M and 2.5D bits so a PolygonZ reads as Polygon. *)
    match Gdal_raw.ogr_gt_flatten n with
    | f when f = Gdal_raw.wkb_point -> Point
    | f when f = Gdal_raw.wkb_line_string -> LineString
    | f when f = Gdal_raw.wkb_polygon -> Polygon
    | f when f = Gdal_raw.wkb_multi_point -> MultiPoint
    | f when f = Gdal_raw.wkb_multi_line_string -> MultiLineString
    | f when f = Gdal_raw.wkb_multi_polygon -> MultiPolygon
    | f when f = Gdal_raw.wkb_geometry_collection -> GeometryCollection
    | f when f = Gdal_raw.wkb_linear_ring -> LinearRing
    | f when f = Gdal_raw.wkb_circular_string -> CircularString
    | f when f = Gdal_raw.wkb_compound_curve -> CompoundCurve
    | f when f = Gdal_raw.wkb_curve_polygon -> CurvePolygon
    | f when f = Gdal_raw.wkb_multi_curve -> MultiCurve
    | f when f = Gdal_raw.wkb_multi_surface -> MultiSurface
    | f when f = Gdal_raw.wkb_polyhedral_surface -> PolyhedralSurface
    | f when f = Gdal_raw.wkb_tin -> Tin
    | f when f = Gdal_raw.wkb_triangle -> Triangle
    | f when f = Gdal_raw.wkb_none -> NoGeometry
    | f -> UnknownGeometry f

  let string_of_geometry_type = function
    | Point -> "Point"
    | LineString -> "LineString"
    | Polygon -> "Polygon"
    | MultiPoint -> "MultiPoint"
    | MultiLineString -> "MultiLineString"
    | MultiPolygon -> "MultiPolygon"
    | GeometryCollection -> "GeometryCollection"
    | LinearRing -> "LinearRing"
    | CircularString -> "CircularString"
    | CompoundCurve -> "CompoundCurve"
    | CurvePolygon -> "CurvePolygon"
    | MultiCurve -> "MultiCurve"
    | MultiSurface -> "MultiSurface"
    | PolyhedralSurface -> "PolyhedralSurface"
    | Tin -> "TIN"
    | Triangle -> "Triangle"
    | NoGeometry -> "None"
    | UnknownGeometry n -> Gdal_raw.ogr_geometry_type_to_name n

  type envelope = {
    min_x : float;
    max_x : float;
    min_y : float;
    max_y : float;
  }

  (* A layer is owned by its dataset; a feature returned by GetNextFeature is
     owned by us; a geometry from GetGeometryRef is borrowed from its feature
     and must not outlive it. [keep] is the geometry's owner, held so the GC
     cannot finalise it while the geometry is still reachable, and consulted
     by [check_alive] so a use-after-free surfaces as an exception rather
     than a segfault. *)
  type layer = { lraw : Gdal_raw.layer_h; _lparent : dataset }

  type feature = {
    fraw : Gdal_raw.feature_h;
    mutable fdestroyed : bool;
  }

  type owned_geometry = {
    oraw : Gdal_raw.geometry_h;
    mutable odestroyed : bool;
  }

  type keep = Keep_feature of feature | Keep_owned of owned_geometry

  type geometry = { graw : Gdal_raw.geometry_h; gkeep : keep }

  let keep_alive = function
    | Keep_feature f -> not f.fdestroyed
    | Keep_owned o -> not o.odestroyed

  let check_alive (g : geometry) =
    if not (keep_alive g.gkeep) then
      invalid_arg
        "Gdal.Vector: geometry used after its owning feature/geometry was \
         destroyed"

  let envelope_of_struct s =
    {
      min_x = Ctypes.getf s Gdal_raw.env_min_x;
      max_x = Ctypes.getf s Gdal_raw.env_max_x;
      min_y = Ctypes.getf s Gdal_raw.env_min_y;
      max_y = Ctypes.getf s Gdal_raw.env_max_y;
    }

  let default_open_flags =
    Gdal_raw.gdal_of_readonly lor Gdal_raw.gdal_of_vector
    lor Gdal_raw.gdal_of_verbose_error

  let open_ ?(flags = default_open_flags) path =
    let* raw = check_null (Gdal_raw.open_ex path flags None None None)
      ("Failed to open vector dataset " ^ path) in
    wrap_dataset raw

  let with_dataset ?flags path f =
    let* ds = open_ ?flags path in
    Ok (Fun.protect ~finally:(fun () -> Dataset.close ds) (fun () -> f ds))

  module Geometry = struct
    let geometry_type (g : geometry) =
      check_alive g;
      geometry_type_of_int (Gdal_raw.ogr_g_get_geometry_type g.graw)

    let sub_count (g : geometry) =
      check_alive g;
      Gdal_raw.ogr_g_get_geometry_count g.graw

    let sub (g : geometry) i =
      check_alive g;
      let raw = Gdal_raw.ogr_g_get_geometry_ref g.graw i in
      if Ctypes.is_null raw then None
      else Some { graw = raw; gkeep = g.gkeep }

    let subs g = List.init (sub_count g) (fun i -> sub g i) |> List.filter_map Fun.id

    let point_count (g : geometry) =
      check_alive g;
      Gdal_raw.ogr_g_get_point_count g.graw

    let point (g : geometry) i =
      check_alive g;
      (Gdal_raw.ogr_g_get_x g.graw i, Gdal_raw.ogr_g_get_y g.graw i)

    (* One FFI call for the whole vertex list; per-vertex OGR_G_GetX/GetY is
       roughly an order of magnitude slower on the million-vertex rings that
       administrative boundary layers contain. *)
    let points (g : geometry) =
      check_alive g;
      let n = Gdal_raw.ogr_g_get_point_count g.graw in
      if n = 0 then [||]
      else begin
        let xs = Ctypes.CArray.make Ctypes.double n in
        let ys = Ctypes.CArray.make Ctypes.double n in
        let stride = Ctypes.sizeof Ctypes.double in
        let got =
          Gdal_raw.ogr_g_get_points g.graw
            (Ctypes.to_voidp (Ctypes.CArray.start xs))
            stride
            (Ctypes.to_voidp (Ctypes.CArray.start ys))
            stride Ctypes.null 0
        in
        Array.init got (fun i ->
            (Ctypes.CArray.get xs i, Ctypes.CArray.get ys i))
      end

    (* Every vertex-bearing leaf of the geometry tree, as closed vertex
       lists. Polygon -> its exterior and interior LinearRings;
       MultiPolygon / GeometryCollection -> recursively each member's rings.
       Rings are returned flat, with no exterior/hole distinction: an
       even-odd (ray-casting) point-in-polygon test over the flat list gives
       the same answer as testing exterior-minus-holes, because a point
       inside a hole is enclosed by both the hole ring and its exterior.
       Leaves with fewer than 3 vertices bound no area and are dropped. *)
    let rings g =
      let acc = ref [] in
      let rec walk g =
        let n = sub_count g in
        if n > 0 then
          for i = 0 to n - 1 do
            match sub g i with Some s -> walk s | None -> ()
          done
        else
          let p = points g in
          if Array.length p >= 3 then acc := p :: !acc
      in
      walk g;
      List.rev !acc

    let envelope (g : geometry) =
      check_alive g;
      let e = Ctypes.make Gdal_raw.ogr_envelope in
      Gdal_raw.ogr_g_get_envelope g.graw (Ctypes.addr e);
      envelope_of_struct e

    let spatial_reference (g : geometry) =
      check_alive g;
      let raw = Gdal_raw.ogr_g_get_spatial_reference g.graw in
      if Ctypes.is_null raw then None
      else
        let c = Gdal_raw.osr_clone raw in
        if Ctypes.is_null c then None
        else Some (SpatialReference.of_raw_owned c)

    let is_valid (g : geometry) =
      check_alive g;
      Gdal_raw.ogr_g_is_valid g.graw <> 0

    let transform (g : geometry) (ct : CoordinateTransformation.t) =
      check_alive g;
      check_ogr_err (Gdal_raw.ogr_g_transform g.graw ct.CoordinateTransformation.raw)

    let transform_to (g : geometry) (srs : SpatialReference.t) =
      check_alive g;
      check_ogr_err
        (Gdal_raw.ogr_g_transform_to g.graw srs.SpatialReference.raw)

    let finalise_owned (o : owned_geometry) =
      if not o.odestroyed then begin
        Gdal_raw.ogr_g_destroy_geometry o.oraw;
        o.odestroyed <- true
      end

    (* A deep copy that owns its memory, so it can outlive the feature it
       came from. Released by a finaliser, or eagerly by [destroy]. *)
    let clone (g : geometry) =
      check_alive g;
      let* raw = check_null (Gdal_raw.ogr_g_clone g.graw) "OGR_G_Clone failed" in
      let o = { oraw = raw; odestroyed = false } in
      Gc.finalise finalise_owned o;
      Ok { graw = raw; gkeep = Keep_owned o }

    let destroy (g : geometry) =
      match g.gkeep with
      | Keep_owned o -> finalise_owned o
      | Keep_feature _ ->
        invalid_arg
          "Gdal.Vector.Geometry.destroy: geometry is borrowed from a feature; \
           destroy the feature instead"
  end

  module Feature = struct
    let finalise_feature (f : feature) =
      if not f.fdestroyed then begin
        Gdal_raw.ogr_f_destroy f.fraw;
        f.fdestroyed <- true
      end

    let wrap raw =
      let f = { fraw = raw; fdestroyed = false } in
      Gc.finalise finalise_feature f;
      f

    let destroy f = finalise_feature f

    let fid (f : feature) = Gdal_raw.ogr_f_get_fid f.fraw

    let geometry (f : feature) =
      if f.fdestroyed then None
      else
        let raw = Gdal_raw.ogr_f_get_geometry_ref f.fraw in
        if Ctypes.is_null raw then None
        else Some { graw = raw; gkeep = Keep_feature f }

    let field_count (f : feature) = Gdal_raw.ogr_f_get_field_count f.fraw

    let field_name (f : feature) i =
      let defn = Gdal_raw.ogr_f_get_defn_ref f.fraw in
      if Ctypes.is_null defn then None
      else
        let fld = Gdal_raw.ogr_fd_get_field_defn defn i in
        if Ctypes.is_null fld then None
        else Some (Gdal_raw.ogr_fld_get_name_ref fld)

    let field (f : feature) i =
      if Gdal_raw.ogr_f_is_field_set_and_not_null f.fraw i = 0 then None
      else Some (Gdal_raw.ogr_f_get_field_as_string f.fraw i)

    let field_by_name (f : feature) name =
      let i = Gdal_raw.ogr_f_get_field_index f.fraw name in
      if i < 0 then None else field f i

    let attributes (f : feature) =
      List.init (field_count f) (fun i ->
          match field_name f i with
          | None -> None
          | Some n -> Some (n, Option.value (field f i) ~default:""))
      |> List.filter_map Fun.id
  end

  module Layer = struct
    let count (ds : dataset) = Gdal_raw.dataset_get_layer_count ds.raw

    let get (ds : dataset) i =
      let* raw = check_null (Gdal_raw.dataset_get_layer ds.raw i)
        (Printf.sprintf "Layer %d not found" i) in
      Ok ({ lraw = raw; _lparent = ds } : layer)

    let by_name (ds : dataset) name =
      let* raw = check_null (Gdal_raw.dataset_get_layer_by_name ds.raw name)
        ("Layer not found: " ^ name) in
      Ok ({ lraw = raw; _lparent = ds } : layer)

    let name (l : layer) = Gdal_raw.ogr_l_get_name l.lraw

    let geometry_type (l : layer) =
      geometry_type_of_int (Gdal_raw.ogr_l_get_geom_type l.lraw)

    (* [force:false] returns -1 when the driver cannot answer without a full
       scan of the layer. *)
    let feature_count ?(force = true) (l : layer) =
      Int64.to_int
        (Gdal_raw.ogr_l_get_feature_count l.lraw (if force then 1 else 0))

    let spatial_reference (l : layer) =
      let raw = Gdal_raw.ogr_l_get_spatial_ref l.lraw in
      if Ctypes.is_null raw then None
      else
        (* The layer owns its SRS, so hand back an owned clone rather than
           a borrowed handle our finaliser would wrongly free. *)
        let c = Gdal_raw.osr_clone raw in
        if Ctypes.is_null c then None
        else Some (SpatialReference.of_raw_owned c)

    let extent ?(force = true) (l : layer) =
      let e = Ctypes.make Gdal_raw.ogr_envelope in
      let* () =
        check_ogr_err
          (Gdal_raw.ogr_l_get_extent l.lraw (Ctypes.addr e)
             (if force then 1 else 0))
      in
      Ok (envelope_of_struct e)

    let fields (l : layer) =
      let defn = Gdal_raw.ogr_l_get_layer_defn l.lraw in
      if Ctypes.is_null defn then []
      else
        List.init (Gdal_raw.ogr_fd_get_field_count defn) (fun i ->
            let fld = Gdal_raw.ogr_fd_get_field_defn defn i in
            if Ctypes.is_null fld then None
            else Some (Gdal_raw.ogr_fld_get_name_ref fld))
        |> List.filter_map Fun.id

    let set_spatial_filter_rect (l : layer) ~min_x ~min_y ~max_x ~max_y =
      Gdal_raw.ogr_l_set_spatial_filter_rect l.lraw min_x min_y max_x max_y

    let set_attribute_filter (l : layer) where =
      check_ogr_err (Gdal_raw.ogr_l_set_attribute_filter l.lraw where)

    let reset_reading (l : layer) = Gdal_raw.ogr_l_reset_reading l.lraw

    let next_feature (l : layer) =
      let raw = Gdal_raw.ogr_l_get_next_feature l.lraw in
      if Ctypes.is_null raw then None else Some (Feature.wrap raw)

    (* Each feature is destroyed as soon as [f] returns, so a geometry
       borrowed from it must not be retained past the callback — use
       [Geometry.clone] to keep one. *)
    let fold (l : layer) ~init ~f =
      reset_reading l;
      let rec loop acc =
        match next_feature l with
        | None -> acc
        | Some feat ->
          let acc =
            Fun.protect
              ~finally:(fun () -> Feature.destroy feat)
              (fun () -> f acc feat)
          in
          loop acc
      in
      loop init

    let iter (l : layer) ~f = fold l ~init:() ~f:(fun () feat -> f feat)
  end
end

(** {1 File operations} *)

let copy_file ~src ~dst =
  let rc = Gdal_raw.cpl_copy_file dst src in
  if rc = 0 then Ok ()
  else Error (Printf.sprintf "CPLCopyFile failed: %s -> %s" src dst)
