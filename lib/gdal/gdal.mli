(** OCaml bindings to GDAL's raster and vector (OGR) C APIs. *)

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

val usable_physical_ram : unit -> int64
(** Usable physical RAM in bytes, or [0L] if it cannot be determined. Wraps
    GDAL's [CPLGetUsablePhysicalRAM], which is implemented per-platform
    (Linux [/proc], macOS/BSD [sysctl], Windows [GlobalMemoryStatusEx]). *)

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

  val name : t -> string option
  (** The CRS name, e.g. ["WGS 84"] or ["Pulkovo 1942(58) / Stereo70"]. *)

  val authority_code : ?target:string -> t -> string option
  (** [authority_code srs] is the authority code of the CRS as a string,
      e.g. [Some "4326"]. [target] selects a sub-node such as ["GEOGCS"];
      omit it to query the root node. *)

  val is_projected : t -> bool
  val is_geographic : t -> bool

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

  val transform_points :
    t -> (float * float) array -> ((float * float) array, string) result
  (** [transform_points ct pts] transforms a whole [(x, y)] array in a single
      call. Prefer this over repeated {!transform_point} for vertex lists:
      one FFI crossing, and GDAL amortises its per-call setup over the
      batch. *)

  val destroy : t -> unit
end

(** {1 Vector (OGR) operations}

    A dataset opened with {!Vector.open_} exposes layers of features, each
    feature carrying attribute fields and a geometry.

    {2 Ownership}

    A [layer] is owned by its dataset. A [feature] from {!Vector.Layer.iter},
    {!Vector.Layer.fold} or {!Vector.Layer.next_feature} is owned by the
    caller and released by a finaliser (or eagerly by
    {!Vector.Feature.destroy}; [iter] and [fold] destroy each feature as soon
    as the callback returns). A [geometry] from {!Vector.Feature.geometry} is
    {e borrowed} from its feature and must not be used after that feature is
    destroyed — doing so raises [Invalid_argument] rather than crashing. Use
    {!Vector.Geometry.clone} to obtain a copy that can outlive the feature. *)

module Vector : sig
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
        (** The flattened (2D) OGR geometry type: the Z/M and 2.5D bits are
            stripped, so a PolygonZ reads as [Polygon]. *)

  val string_of_geometry_type : geometry_type -> string

  type envelope = {
    min_x : float;
    max_x : float;
    min_y : float;
    max_y : float;
  }

  type layer
  type feature
  type geometry

  val open_ : ?flags:int -> string -> (dataset, string) result
  (** Open a vector dataset. The default [flags] are read-only vector access
      with verbose errors. GDAL's virtual filesystem applies, so
      ["/vsizip/roi.zip"] opens a zipped shapefile and
      ["/vsicurl/https://..."] a remote one. *)

  val with_dataset : ?flags:int -> string -> (dataset -> 'a) -> ('a, string) result
  (** [with_dataset path f] opens [path] as a vector dataset, applies [f],
      then closes it. *)

  module Geometry : sig
    val geometry_type : geometry -> geometry_type
    val sub_count : geometry -> int
    (** Number of member geometries: the rings of a polygon, the polygons of
        a multipolygon, [0] for a simple geometry. *)

    val sub : geometry -> int -> geometry option
    val subs : geometry -> geometry list
    val point_count : geometry -> int
    val point : geometry -> int -> float * float

    val points : geometry -> (float * float) array
    (** All vertices of a vertex-bearing geometry (a point, line string or
        linear ring), read in one call. Empty for container geometries such
        as polygons and multipolygons — use {!sub} or {!rings} for those. *)

    val rings : geometry -> (float * float) array list
    (** Every vertex-bearing leaf of the geometry tree, flattened: for a
        polygon its exterior and interior rings, for a multipolygon or
        collection each member's rings, recursively. Leaves with fewer than
        three vertices are dropped.

        Exterior rings and holes are {e not} distinguished, which is exactly
        what an even-odd (ray-casting) point-in-polygon test wants: a point
        inside a hole is enclosed by both the hole ring and the exterior
        ring, so it counts twice and correctly tests as outside. *)

    val envelope : geometry -> envelope
    val spatial_reference : geometry -> SpatialReference.t option
    val is_valid : geometry -> bool

    val transform :
      geometry -> CoordinateTransformation.t -> (unit, string) result
    (** Reproject the geometry in place. *)

    val transform_to : geometry -> SpatialReference.t -> (unit, string) result
    (** Reproject the geometry in place into the given CRS, using the
        geometry's own CRS as the source. Fails if the geometry has no CRS
        assigned. *)

    val clone : geometry -> (geometry, string) result
    (** A deep copy that owns its memory and so may outlive the feature the
        original was borrowed from. Released by a finaliser, or eagerly by
        {!destroy}. *)

    val destroy : geometry -> unit
    (** Release a geometry obtained from {!clone}.
        @raise Invalid_argument on a geometry borrowed from a feature. *)
  end

  module Feature : sig
    val fid : feature -> int64
    val geometry : feature -> geometry option
    (** The feature's geometry, borrowed: valid only while the feature is. *)

    val field_count : feature -> int
    val field_name : feature -> int -> string option
    val field : feature -> int -> string option
    (** The field's value as a string, or [None] if unset or null. *)

    val field_by_name : feature -> string -> string option
    val attributes : feature -> (string * string) list
    (** All set fields as [(name, value)] pairs. *)

    val destroy : feature -> unit
    (** Release the feature. Idempotent. *)
  end

  module Layer : sig
    val count : dataset -> int
    (** Number of layers in the dataset. *)

    val get : dataset -> int -> (layer, string) result
    (** Fetch a layer by 0-based index. *)

    val by_name : dataset -> string -> (layer, string) result
    val name : layer -> string
    val geometry_type : layer -> geometry_type

    val feature_count : ?force:bool -> layer -> int
    (** [feature_count ~force:false] returns [-1] when the driver cannot
        answer without scanning the whole layer. Defaults to [true]. *)

    val spatial_reference : layer -> SpatialReference.t option
    (** The layer's CRS, as an owned copy. [None] when the layer declares no
        CRS (e.g. a shapefile with no [.prj]). *)

    val extent : ?force:bool -> layer -> (envelope, string) result

    val fields : layer -> string list
    (** Attribute field names, in order. *)

    val set_spatial_filter_rect :
      layer -> min_x:float -> min_y:float -> max_x:float -> max_y:float -> unit
    (** Restrict subsequent reads to features intersecting the rectangle, in
        layer CRS coordinates. *)

    val set_attribute_filter : layer -> string option -> (unit, string) result
    (** Restrict subsequent reads with an SQL [WHERE] clause, e.g.
        [Some "NAM_0 = 'Brazil'"]. [None] clears the filter. *)

    val reset_reading : layer -> unit
    val next_feature : layer -> feature option
    (** The next feature, owned by the caller. [None] at end of layer. *)

    val fold : layer -> init:'a -> f:('a -> feature -> 'a) -> 'a
    (** Fold over every feature, from the start of the layer. Each feature is
        destroyed as soon as [f] returns, so a geometry borrowed from it must
        not be retained past the callback — {!Geometry.clone} it to keep
        one. *)

    val iter : layer -> f:(feature -> unit) -> unit
    (** Like {!fold}, discarding the accumulator. *)
  end
end

(** {1 File operations} *)

val copy_file : src:string -> dst:string -> (unit, string) result
(** [copy_file ~src ~dst] copies a file using GDAL's virtual filesystem.
    Works with VSI paths (e.g. [/vsicurl/https://...]). *)
