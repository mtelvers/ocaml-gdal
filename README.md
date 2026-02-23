# gdal-ocaml

OCaml bindings to [GDAL](https://gdal.org/)'s raster C API via
[ctypes](https://github.com/yallop/ocaml-ctypes).

## Features

- Two-layer architecture: thin ctypes FFI (`gdal.raw`) plus idiomatic OCaml
  wrappers with `result` types
- Type-safe raster I/O through Bigarray with a GADT witness for element types
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
Gdal.CoordinateTransformation.transform_bounds : t -> xmin:float -> ymin:float ->
                                                   xmax:float -> ymax:float -> density:int ->
                                                   (float * float * float * float, string) result
Gdal.CoordinateTransformation.destroy : t -> unit
```

## Examples

The `examples/` directory contains:

- **read_raster.ml** -- open a raster file and print its metadata and first few
  pixel values
- **gc_stress.ml** -- multi-domain stress test exercising concurrent dataset
  reads under GC pressure

Build and run:

```
dune exec examples/read_raster.exe -- path/to/file.tif
```

## Tests

The test suite uses [Alcotest](https://github.com/mirage/alcotest) with 56
tests covering enums, drivers, datasets, raster band I/O, spatial references,
coordinate transformations, and warp/translate. When Python 3 with GDAL bindings
is available, additional cross-validation tests compare OCaml results against
the Python GDAL API.

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
