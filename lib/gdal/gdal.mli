(** OCaml bindings to GDAL's raster C API. *)

(** {1 Enumerations} *)

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

val data_type_to_int : data_type -> int
val data_type_of_int : int -> data_type
val string_of_data_type : data_type -> string

type access = ReadOnly | Update
type rw_flag = Read | Write

(** {1 Abstract types} *)

type dataset
type band
type driver

(** {1 Geo-transform} *)

type geo_transform = {
  origin_x : float;
  pixel_width : float;
  row_rotation : float;
  origin_y : float;
  col_rotation : float;
  pixel_height : float;
}

(** {1 Initialization} *)

val init : unit -> unit
(** Register all GDAL drivers. Call once before using other functions. *)

(** {1 Configuration} *)

val set_config_option : string -> string -> unit
(** [set_config_option key value] sets a GDAL configuration option. *)

(** {1 GDALOpenEx flag constants} *)

val gdal_of_readonly : int
val gdal_of_update : int
val gdal_of_raster : int
val gdal_of_vector : int
val gdal_of_verbose_error : int

(** {1 Raster I/O via Bigarray} *)

type ('a, 'b) ba_kind_witness =
  | BA_int8 : (int, Bigarray.int8_signed_elt) ba_kind_witness
  | BA_byte : (int, Bigarray.int8_unsigned_elt) ba_kind_witness
  | BA_uint16 : (int, Bigarray.int16_unsigned_elt) ba_kind_witness
  | BA_int16 : (int, Bigarray.int16_signed_elt) ba_kind_witness
  | BA_int32 : (int32, Bigarray.int32_elt) ba_kind_witness
  | BA_int64 : (int64, Bigarray.int64_elt) ba_kind_witness
  | BA_float32 : (float, Bigarray.float32_elt) ba_kind_witness
  | BA_float64 : (float, Bigarray.float64_elt) ba_kind_witness

(** {1 Driver operations} *)

module Driver : sig
  val by_name : string -> (driver, string) result
  (** Look up a driver by short name (e.g. ["GTiff"]). *)

  val create :
    driver ->
    filename:string ->
    width:int ->
    height:int ->
    bands:int ->
    ?options:string list ->
    data_type ->
    (dataset, string) result
  (** Create a new raster dataset. [options] are driver-specific creation
      options such as ["COMPRESS=DEFLATE"] or ["TILED=YES"]. *)

  val description : driver -> string
  (** The long name / description of the driver (e.g. ["GeoTIFF"]). *)

  val create_copy :
    driver ->
    filename:string ->
    src:dataset ->
    ?strict:bool ->
    unit ->
    (dataset, string) result
  (** Create a copy of an existing dataset. *)
end

(** {1 Dataset operations} *)

module Dataset : sig
  val open_ : ?access:access -> string -> (dataset, string) result
  (** Open a raster dataset. *)

  val open_ex : ?flags:int -> ?thread_safe:bool -> string -> (dataset, string) result
  (** Open a dataset with extended flags (GDALOpenEx).
      When [~thread_safe:true] is passed, GDAL 3.10+ will return a
      thread-safe wrapper with per-thread block caches (RFC 101). *)

  val close : dataset -> unit
  (** Close a dataset. Safe to call multiple times. *)

  val with_dataset :
    ?access:access -> string -> (dataset -> 'a) -> ('a, string) result
  (** [with_dataset path f] opens [path], applies [f], then closes. *)

  val raster_x_size : dataset -> int
  val raster_y_size : dataset -> int
  val raster_count : dataset -> int
  val get_geo_transform : dataset -> (geo_transform, string) result
  val set_geo_transform : dataset -> geo_transform -> (unit, string) result
  val projection : dataset -> string
  val set_projection : dataset -> string -> (unit, string) result

  val get_band : dataset -> int -> (band, string) result
  (** Get a raster band by 1-based index. *)

  val is_null : dataset -> bool
  (** Test whether the dataset handle is null. *)

  val description : dataset -> string
  (** The description (typically the filename) of the dataset. *)

  val get_metadata_item : dataset -> key:string -> domain:string -> string option
  (** Fetch a single metadata item. *)

  val warp : dataset -> dst_filename:string -> string list -> (dataset, string) result
  (** [warp ds ~dst_filename options] warps [ds] to [dst_filename] using
      the given GDALWarp option strings. *)

  val translate : dataset -> dst_filename:string -> string list -> (dataset, string) result
  (** [translate ds ~dst_filename options] converts [ds] to [dst_filename]
      using the given GDALTranslate option strings (e.g. ["-of"; "PNG"]). *)
end

(** {1 RasterBand operations} *)

module RasterBand : sig
  val data_type : band -> (data_type, string) result
  val block_size : band -> (int * int, string) result
  val no_data_value : band -> (float option, string) result

  val x_size : band -> (int, string) result
  (** Width of the band in pixels. *)

  val y_size : band -> (int, string) result
  (** Height of the band in pixels. *)

  val read :
    ('a, 'b) ba_kind_witness ->
    band ->
    (('a, 'b, Bigarray.c_layout) Bigarray.Array2.t, string) result
  (** Read the entire band into a bigarray. *)

  val read_region :
    ('a, 'b) ba_kind_witness ->
    band ->
    x_off:int ->
    y_off:int ->
    x_size:int ->
    y_size:int ->
    buf_x:int ->
    buf_y:int ->
    (('a, 'b, Bigarray.c_layout) Bigarray.Array2.t, string) result
  (** Read a sub-region of a band, optionally resampling to [buf_x * buf_y]. *)

  val write_region :
    ('a, 'b) ba_kind_witness ->
    band ->
    x_off:int ->
    y_off:int ->
    x_size:int ->
    y_size:int ->
    ('a, 'b, Bigarray.c_layout) Bigarray.Array2.t ->
    (unit, string) result
  (** Write a bigarray into a sub-region of a band. *)

  val read_byte :
    band ->
    x_off:int ->
    y_off:int ->
    x_size:int ->
    y_size:int ->
    ((int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t, string) result
  (** Convenience: read a region as unsigned bytes. *)
end

(** {1 Spatial Reference} *)

module SpatialReference : sig
  type t

  val of_epsg : int -> (t, string) result
  (** Create a spatial reference from an EPSG code.
      Axis mapping is set to traditional GIS order (lon/lat = x/y). *)

  val of_wkt : string -> (t, string) result
  (** Create a spatial reference from a WKT string.
      Axis mapping is set to traditional GIS order (lon/lat = x/y). *)

  val to_wkt : t -> (string, string) result
  (** Export the spatial reference as a WKT string. *)

  val set_axis_mapping_strategy : t -> int -> unit
  (** Override the axis mapping strategy. Use [oams_traditional_gis_order]
      (0) for lon/lat = x/y, or [oams_authority_compliant] (1) for the
      authority-defined axis order. *)

  val destroy : t -> unit
end

val oams_traditional_gis_order : int
val oams_authority_compliant : int

(** {1 Coordinate Transformation} *)

module CoordinateTransformation : sig
  type t

  val create : SpatialReference.t -> SpatialReference.t -> (t, string) result
  (** [create src dst] creates a transformation from [src] to [dst]. *)

  val transform_point :
    t ->
    x:float ->
    y:float ->
    z:float ->
    (float * float * float, string) result
  (** Transform a single point. Returns [(x, y, z)]. *)

  val transform_bounds :
    t ->
    xmin:float ->
    ymin:float ->
    xmax:float ->
    ymax:float ->
    density:int ->
    (float * float * float * float, string) result
  (** Transform a bounding box. Returns [(xmin, ymin, xmax, ymax)]. *)

  val destroy : t -> unit
end

(** {1 File operations} *)

val copy_file : src:string -> dst:string -> (unit, string) result
(** [copy_file ~src ~dst] copies a file using GDAL's virtual filesystem.
    Works with VSI paths (e.g. [/vsicurl/https://...]). *)
