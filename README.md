# gdal-ocaml

OCaml bindings to [GDAL](https://gdal.org/)'s raster and vector (OGR) C APIs
via [ctypes](https://github.com/yallop/ocaml-ctypes).

## Features

- Two-layer architecture: thin ctypes FFI (`gdal.raw`) plus idiomatic OCaml
  wrappers with `result` types
- Type-safe raster I/O through Bigarray with a GADT witness for element types
- Vector (OGR) reading: layers, features, attribute fields, geometry trees,
  spatial and attribute filters, in-place reprojection
- Coordinate transformation and spatial reference support (EPSG, WKT)
- Dataset warping (`GDALWarp`) and format conversion (`GDALTranslate`)
- GC-safe: datasets are released by finalisers, bands hold a reference to their
  parent dataset, and `release_runtime_lock` is used for long-running C calls
- Safe for OCaml 5 multi-domain use

## Requirements

- OCaml >= 4.14.0
- [ctypes](https://opam.ocaml.org/packages/ctypes/) >= 0.20.0 and ctypes-foreign
- libgdal (`libgdal-dev` or equivalent) installed and visible to the linker
- **GDAL >= 3.10 recommended** for multi-domain use.  GDAL 3.10 introduced
  `GDALThreadSafeDataset` (RFC 101) with per-thread block caches, eliminating
  the global `hRBMutex` that serialises raster reads in earlier versions.  With
  GDAL < 3.10, parallel speedup from `Domain.spawn` is limited to roughly 3×
  regardless of the number of workers.

## Installation

```
opam install . --deps-only
dune build
```

## Quick start

```ocaml
let () =
  Gdal.init ();
  match Gdal.Dataset.with_dataset "input.tif" (fun ds ->
    Printf.printf "Size: %d x %d, %d band(s)\n"
      (Gdal.Dataset.raster_x_size ds)
      (Gdal.Dataset.raster_y_size ds)
      (Gdal.Dataset.raster_count ds);
    match Gdal.Dataset.get_band ds 1 with
    | Error msg -> Printf.eprintf "Error: %s\n" msg
    | Ok band ->
      let arr = Result.get_ok
        (Gdal.RasterBand.read Gdal.BA_float64 band) in
      Printf.printf "Top-left pixel: %f\n"
        (Bigarray.Array2.get arr 0 0)
  ) with
  | Ok () -> ()
  | Error msg -> Printf.eprintf "%s\n" msg
```

## API overview

Call `Gdal.init ()` once before using any other function. All fallible
operations return `(_, string) result`.

### Driver

```ocaml
Gdal.Driver.by_name : string -> (driver, string) result
Gdal.Driver.create  : driver -> filename:string -> width:int -> height:int ->
                       bands:int -> data_type -> (dataset, string) result
Gdal.Driver.create_copy : driver -> filename:string -> src:dataset ->
                           ?strict:bool -> unit -> (dataset, string) result
Gdal.Driver.description : driver -> string
```

### Dataset

```ocaml
Gdal.Dataset.open_         : ?access:access -> string -> (dataset, string) result
Gdal.Dataset.open_ex       : ?flags:int -> ?thread_safe:bool -> string -> (dataset, string) result
Gdal.Dataset.close         : dataset -> unit
Gdal.Dataset.with_dataset  : ?access:access -> string -> (dataset -> 'a) -> ('a, string) result
Gdal.Dataset.raster_x_size : dataset -> int
Gdal.Dataset.raster_y_size : dataset -> int
Gdal.Dataset.raster_count  : dataset -> int
Gdal.Dataset.get_band      : dataset -> int -> (band, string) result
Gdal.Dataset.projection    : dataset -> string
Gdal.Dataset.set_projection   : dataset -> string -> (unit, string) result
Gdal.Dataset.get_geo_transform : dataset -> (geo_transform, string) result
Gdal.Dataset.set_geo_transform : dataset -> geo_transform -> (unit, string) result
Gdal.Dataset.warp      : dataset -> dst_filename:string -> string list -> (dataset, string) result
Gdal.Dataset.translate  : dataset -> dst_filename:string -> string list -> (dataset, string) result
Gdal.Dataset.description      : dataset -> string
Gdal.Dataset.get_metadata_item : dataset -> key:string -> domain:string -> string option
Gdal.Dataset.is_null   : dataset -> bool
```

### RasterBand

Raster I/O uses a GADT witness to select the Bigarray element type at compile
time:

```ocaml
Gdal.BA_byte    (* int, int8_unsigned_elt *)
Gdal.BA_int8    (* int, int8_signed_elt   *)
Gdal.BA_uint16  (* int, int16_unsigned_elt *)
Gdal.BA_int16   (* int, int16_signed_elt   *)
Gdal.BA_int32   (* int32, int32_elt        *)
Gdal.BA_int64   (* int64, int64_elt        *)
Gdal.BA_float32 (* float, float32_elt      *)
Gdal.BA_float64 (* float, float64_elt      *)
```

```ocaml
Gdal.RasterBand.read        : ('a, 'b) ba_kind_witness -> band ->
                               (('a, 'b, c_layout) Array2.t, string) result
Gdal.RasterBand.read_region : ('a, 'b) ba_kind_witness -> band ->
                               x_off:int -> y_off:int -> x_size:int -> y_size:int ->
                               buf_x:int -> buf_y:int ->
                               (('a, 'b, c_layout) Array2.t, string) result
Gdal.RasterBand.write_region : ('a, 'b) ba_kind_witness -> band ->
                                x_off:int -> y_off:int -> x_size:int -> y_size:int ->
                                ('a, 'b, c_layout) Array2.t -> (unit, string) result
Gdal.RasterBand.read_byte   : band -> x_off:int -> y_off:int -> x_size:int -> y_size:int ->
                               ((int, int8_unsigned_elt, c_layout) Array2.t, string) result
Gdal.RasterBand.data_type     : band -> (data_type, string) result
Gdal.RasterBand.block_size    : band -> (int * int, string) result
Gdal.RasterBand.no_data_value : band -> (float option, string) result
Gdal.RasterBand.x_size        : band -> (int, string) result
Gdal.RasterBand.y_size        : band -> (int, string) result
```

### SpatialReference

```ocaml
Gdal.SpatialReference.of_epsg  : int -> (t, string) result
Gdal.SpatialReference.of_wkt   : string -> (t, string) result
Gdal.SpatialReference.to_wkt   : t -> (string, string) result
Gdal.SpatialReference.set_axis_mapping_strategy : t -> int -> unit
Gdal.SpatialReference.destroy  : t -> unit
```

Both `of_epsg` and `of_wkt` set the axis mapping to traditional GIS order
(longitude, latitude = x, y). Use `set_axis_mapping_strategy` with
`Gdal.oams_authority_compliant` to switch to authority-defined axis order.

### CoordinateTransformation

```ocaml
Gdal.CoordinateTransformation.create : SpatialReference.t -> SpatialReference.t ->
                                        (t, string) result
Gdal.CoordinateTransformation.transform_point : t -> x:float -> y:float -> z:float ->
                                                  (float * float * float, string) result
Gdal.CoordinateTransformation.transform_points : t -> (float * float) array ->
                                                   ((float * float) array, string) result
Gdal.CoordinateTransformation.transform_bounds : t -> xmin:float -> ymin:float ->
                                                   xmax:float -> ymax:float -> density:int ->
                                                   (float * float * float * float, string) result
Gdal.CoordinateTransformation.destroy : t -> unit
```

`transform_points` transforms a whole vertex array in one FFI call; prefer it
over repeated `transform_point` for rings and tracks.

### Vector (OGR)

Datasets opened through `Gdal.Vector` expose layers of features, each with
attribute fields and a geometry.

```ocaml
Gdal.Vector.open_        : ?flags:int -> string -> (dataset, string) result
Gdal.Vector.with_dataset : ?flags:int -> string -> (dataset -> 'a) -> ('a, string) result

Gdal.Vector.Layer.count      : dataset -> int
Gdal.Vector.Layer.get        : dataset -> int -> (layer, string) result
Gdal.Vector.Layer.by_name    : dataset -> string -> (layer, string) result
Gdal.Vector.Layer.name       : layer -> string
Gdal.Vector.Layer.geometry_type  : layer -> geometry_type
Gdal.Vector.Layer.feature_count  : ?force:bool -> layer -> int
Gdal.Vector.Layer.spatial_reference : layer -> SpatialReference.t option
Gdal.Vector.Layer.extent     : ?force:bool -> layer -> (envelope, string) result
Gdal.Vector.Layer.fields     : layer -> string list
Gdal.Vector.Layer.set_spatial_filter_rect :
  layer -> min_x:float -> min_y:float -> max_x:float -> max_y:float -> unit
Gdal.Vector.Layer.set_attribute_filter : layer -> string option -> (unit, string) result
Gdal.Vector.Layer.fold  : layer -> init:'a -> f:('a -> feature -> 'a) -> 'a
Gdal.Vector.Layer.iter  : layer -> f:(feature -> unit) -> unit
Gdal.Vector.Layer.next_feature : layer -> feature option

Gdal.Vector.Feature.geometry      : feature -> geometry option
Gdal.Vector.Feature.attributes    : feature -> (string * string) list
Gdal.Vector.Feature.field_by_name : feature -> string -> string option
Gdal.Vector.Feature.destroy       : feature -> unit

Gdal.Vector.Geometry.geometry_type : geometry -> geometry_type
Gdal.Vector.Geometry.sub_count : geometry -> int
Gdal.Vector.Geometry.sub       : geometry -> int -> geometry option
Gdal.Vector.Geometry.points    : geometry -> (float * float) array
Gdal.Vector.Geometry.rings     : geometry -> (float * float) array list
Gdal.Vector.Geometry.envelope  : geometry -> envelope
Gdal.Vector.Geometry.transform_to : geometry -> SpatialReference.t -> (unit, string) result
Gdal.Vector.Geometry.clone     : geometry -> (geometry, string) result
```

`Geometry.rings` flattens the geometry tree to its vertex-bearing leaves: for a
polygon its exterior ring and holes, for a multipolygon or collection each
member's rings, recursively. Holes are not distinguished from exteriors, which
is what an even-odd (ray-casting) point-in-polygon test wants — a point inside a
hole is enclosed by both the hole and the exterior ring, so it counts twice and
correctly tests as outside.

Ownership follows the C API: a layer belongs to its dataset, a feature from
`next_feature`/`fold`/`iter` belongs to the caller (`fold` and `iter` destroy
each one when the callback returns), and a geometry from `Feature.geometry` is
borrowed from its feature. Using a geometry after its feature is gone raises
`Invalid_argument` instead of crashing; `Geometry.clone` returns a copy that can
outlive the feature.

Reading a zipped shapefile and reprojecting it to WGS84:

```ocaml
let () =
  Gdal.init ();
  let wgs84 = Result.get_ok (Gdal.SpatialReference.of_epsg 4326) in
  Gdal.Vector.with_dataset "/vsizip/roi.zip" (fun ds ->
    let layer = Result.get_ok (Gdal.Vector.Layer.get ds 0) in
    Gdal.Vector.Layer.iter layer ~f:(fun feat ->
      match Gdal.Vector.Feature.geometry feat with
      | None -> ()
      | Some g ->
        ignore (Gdal.Vector.Geometry.transform_to g wgs84);
        List.iter
          (fun ring -> Printf.printf "ring of %d vertices\n" (Array.length ring))
          (Gdal.Vector.Geometry.rings g)))
  |> Result.iter_error (Printf.eprintf "%s\n")
```

## Examples

The `examples/` directory contains:

- **read_raster.ml** -- open a raster file and print its metadata and first few
  pixel values
- **read_vector.ml** -- summarise every layer of a vector file: CRS, extent,
  fields, and the ring structure of the first few features
- **gc_stress.ml** -- multi-domain stress test exercising concurrent dataset
  reads under GC pressure

Build and run:

```
dune exec examples/read_raster.exe -- path/to/file.tif
dune exec examples/read_vector.exe -- path/to/boundary.geojson
```

## Tests

The test suite uses [Alcotest](https://github.com/mirage/alcotest) with 67
tests covering enums, drivers, datasets, raster band I/O, spatial references,
coordinate transformations, warp/translate, and the vector API. When Python 3
with GDAL bindings is available, additional cross-validation tests compare OCaml
results against the Python GDAL API.

```
dune test
```

## Project structure

```
lib/raw/gdal_raw.ml   Thin ctypes FFI bindings to libgdal
lib/gdal/gdal.ml      High-level OCaml API
lib/gdal/gdal.mli     Public interface
examples/             Usage examples
test/test_gdal.ml     Alcotest test suite
```
