open Ctypes
open Foreign

let libgdal = Dl.dlopen ~filename:"libgdal.so" ~flags:[ Dl.RTLD_LAZY ]

(* Opaque handle types *)
type dataset_h = unit ptr
let dataset_h : dataset_h typ = ptr void

type raster_band_h = unit ptr
let raster_band_h : raster_band_h typ = ptr void

type driver_h = unit ptr
let driver_h : driver_h typ = ptr void

(* GDALAccess enum *)
let ga_readonly = 0
let ga_update = 1

(* GDALRWFlag enum *)
let gf_read = 0
let gf_write = 1

(* GDALDataType enum *)
let gdt_unknown = 0
let gdt_byte = 1
let gdt_uint16 = 2
let gdt_int16 = 3
let gdt_uint32 = 4
let gdt_int32 = 5
let gdt_float32 = 6
let gdt_float64 = 7
let gdt_cint16 = 8
let gdt_cint32 = 9
let gdt_cfloat32 = 10
let gdt_cfloat64 = 11
let gdt_uint64 = 12
let gdt_int64 = 13
let gdt_int8 = 14
let gdt_float16 = 15

(* CPLErr enum *)
let ce_none = 0
let ce_debug = 1
let ce_warning = 2
let ce_failure = 3
let ce_fatal = 4

(* Version *)
let version_info =
  foreign ~from:libgdal "GDALVersionInfo" (string @-> returning string)

(* Registration *)
let all_register =
  foreign ~from:libgdal "GDALAllRegister" (void @-> returning void)

(* Dataset operations *)
let open_ =
  foreign ~from:libgdal "GDALOpen" (string @-> int @-> returning dataset_h)

let close =
  foreign ~from:libgdal "GDALClose" (dataset_h @-> returning void)

(* Driver operations *)
let get_driver_by_name =
  foreign ~from:libgdal "GDALGetDriverByName" (string @-> returning driver_h)

let create =
  foreign ~from:libgdal "GDALCreate"
    (driver_h @-> string @-> int @-> int @-> int @-> int
    @-> ptr_opt (ptr char) @-> returning dataset_h)

let create_copy =
  foreign ~from:libgdal "GDALCreateCopy"
    (driver_h @-> string @-> dataset_h @-> int @-> ptr_opt string @-> ptr void
    @-> ptr void @-> returning dataset_h)

(* Dimension queries *)
let get_raster_x_size =
  foreign ~from:libgdal "GDALGetRasterXSize" (dataset_h @-> returning int)

let get_raster_y_size =
  foreign ~from:libgdal "GDALGetRasterYSize" (dataset_h @-> returning int)

let get_raster_count =
  foreign ~from:libgdal "GDALGetRasterCount" (dataset_h @-> returning int)

(* Band access *)
let get_raster_band =
  foreign ~from:libgdal "GDALGetRasterBand"
    (dataset_h @-> int @-> returning raster_band_h)

(* Geo-transform *)
let get_geo_transform =
  foreign ~from:libgdal "GDALGetGeoTransform"
    (dataset_h @-> ptr double @-> returning int)

let set_geo_transform =
  foreign ~from:libgdal "GDALSetGeoTransform"
    (dataset_h @-> ptr double @-> returning int)

(* Projection *)
let get_projection_ref =
  foreign ~from:libgdal "GDALGetProjectionRef"
    (dataset_h @-> returning string)

let set_projection =
  foreign ~from:libgdal "GDALSetProjection"
    (dataset_h @-> string @-> returning int)

(* Raster band info *)
let get_raster_data_type =
  foreign ~from:libgdal "GDALGetRasterDataType"
    (raster_band_h @-> returning int)

let get_block_size =
  foreign ~from:libgdal "GDALGetBlockSize"
    (raster_band_h @-> ptr int @-> ptr int @-> returning void)

let get_raster_no_data_value =
  foreign ~from:libgdal "GDALGetRasterNoDataValue"
    (raster_band_h @-> ptr int @-> returning double)

(* Raster I/O *)
let raster_io =
  foreign ~from:libgdal ~release_runtime_lock:true "GDALRasterIO"
    (raster_band_h @-> int @-> int @-> int @-> int @-> int @-> ptr void
    @-> int @-> int @-> int @-> int @-> int @-> returning int)

(* Error handling *)
let cpl_get_last_error_msg =
  foreign ~from:libgdal "CPLGetLastErrorMsg" (void @-> returning string)

(* Configuration *)
let cpl_set_config_option =
  foreign ~from:libgdal "CPLSetConfigOption"
    (string @-> string @-> returning void)

(* Opaque handle types for OGR/OSR *)
type spatial_reference_h = unit ptr
let spatial_reference_h : spatial_reference_h typ = ptr void

type coordinate_transformation_h = unit ptr
let coordinate_transformation_h : coordinate_transformation_h typ = ptr void

type warp_options_h = unit ptr
let warp_options_h : warp_options_h typ = ptr void

(* OGRErr enum *)
let ogrerr_none = 0

(* OSRAxisMappingStrategy enum *)
let oams_traditional_gis_order = 0
let oams_authority_compliant = 1

(* GDALOpenEx flags *)
let gdal_of_readonly = 0x00
let gdal_of_update = 0x01
let gdal_of_raster = 0x02
let gdal_of_vector = 0x04
let gdal_of_verbose_error = 0x40
let gdal_of_thread_safe = 0x800

(* GDALOpenEx *)
let open_ex =
  foreign ~from:libgdal "GDALOpenEx"
    (string @-> int @-> ptr_opt string @-> ptr_opt string @-> ptr_opt string
    @-> returning dataset_h)

(* Metadata *)
let get_metadata_item =
  foreign ~from:libgdal "GDALGetMetadataItem"
    (dataset_h @-> string @-> string @-> returning string_opt)

(* Raster band dimensions *)
let get_raster_band_x_size =
  foreign ~from:libgdal "GDALGetRasterBandXSize"
    (raster_band_h @-> returning int)

let get_raster_band_y_size =
  foreign ~from:libgdal "GDALGetRasterBandYSize"
    (raster_band_h @-> returning int)

(* Spatial Reference *)
let osr_new_spatial_reference =
  foreign ~from:libgdal "OSRNewSpatialReference"
    (string_opt @-> returning spatial_reference_h)

let osr_destroy_spatial_reference =
  foreign ~from:libgdal "OSRDestroySpatialReference"
    (spatial_reference_h @-> returning void)

let osr_import_from_epsg =
  foreign ~from:libgdal "OSRImportFromEPSG"
    (spatial_reference_h @-> int @-> returning int)

let osr_set_axis_mapping_strategy =
  foreign ~from:libgdal "OSRSetAxisMappingStrategy"
    (spatial_reference_h @-> int @-> returning void)

let osr_import_from_wkt =
  foreign ~from:libgdal "OSRImportFromWkt"
    (spatial_reference_h @-> ptr (ptr char) @-> returning int)

let osr_export_to_wkt =
  foreign ~from:libgdal "OSRExportToWkt"
    (spatial_reference_h @-> ptr (ptr char) @-> returning int)

let cpl_free =
  foreign ~from:libgdal "VSIFree" (ptr void @-> returning void)

(* Coordinate Transformation *)
let oct_new_coordinate_transformation =
  foreign ~from:libgdal "OCTNewCoordinateTransformation"
    (spatial_reference_h @-> spatial_reference_h
    @-> returning coordinate_transformation_h)

let oct_destroy_coordinate_transformation =
  foreign ~from:libgdal "OCTDestroyCoordinateTransformation"
    (coordinate_transformation_h @-> returning void)

let oct_transform =
  foreign ~from:libgdal "OCTTransform"
    (coordinate_transformation_h @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> returning int)

let oct_transform_bounds =
  foreign ~from:libgdal "OCTTransformBounds"
    (coordinate_transformation_h @-> double @-> double @-> double @-> double
    @-> ptr double @-> ptr double @-> ptr double @-> ptr double @-> int
    @-> returning int)

(* Warp *)
let gdal_warp_app_options_new =
  foreign ~from:libgdal "GDALWarpAppOptionsNew"
    (ptr (ptr char) @-> ptr void @-> returning warp_options_h)

let gdal_warp_app_options_free =
  foreign ~from:libgdal "GDALWarpAppOptionsFree"
    (warp_options_h @-> returning void)

let gdal_warp =
  foreign ~from:libgdal "GDALWarp"
    (string @-> ptr void @-> int @-> ptr dataset_h @-> warp_options_h
    @-> ptr int @-> returning dataset_h)

(* Translate *)
type translate_options_h = unit ptr
let translate_options_h : translate_options_h typ = ptr void

let gdal_translate_options_new =
  foreign ~from:libgdal "GDALTranslateOptionsNew"
    (ptr (ptr char) @-> ptr void @-> returning translate_options_h)

let gdal_translate_options_free =
  foreign ~from:libgdal "GDALTranslateOptionsFree"
    (translate_options_h @-> returning void)

let gdal_translate =
  foreign ~from:libgdal "GDALTranslate"
    (string @-> dataset_h @-> translate_options_h @-> ptr int
    @-> returning dataset_h)

(* Description *)
let get_description =
  foreign ~from:libgdal "GDALGetDescription"
    (ptr void @-> returning string)

let get_driver_long_name =
  foreign ~from:libgdal "GDALGetDriverLongName"
    (driver_h @-> returning string)

(* VSI file copy *)
let cpl_copy_file =
  foreign ~from:libgdal ~release_runtime_lock:true "CPLCopyFile"
    (string @-> string @-> returning int)
