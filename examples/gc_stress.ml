(* GC stress test for GDAL ctypes bindings.

   Strategy: run multiple domains concurrently —
   - "reader" domains repeatedly open a dataset, read bands via raster_io
     (which uses release_runtime_lock:true), and check the results
   - "allocator" domains churn the heap hard to force GC collections
     while the readers hold ctypes-managed pointers

   If ctypes-managed values (CArrays, char ptrs, bigarrays) are collected
   while C code is using derived raw pointers, we'll see crashes, SIGSEGV,
   or corrupted data. *)

let n_readers = 4
let n_allocators = 4
let iterations = 500
let test_file = "/tmp/gc_stress_test.tif"

(* Create a test GeoTIFF with known pixel values *)
let create_test_file () =
  Printf.printf "  init...\n%!";
  Gdal.init ();
  Printf.printf "  get driver...\n%!";
  match Gdal.Driver.by_name "GTiff" with
  | Error msg -> failwith msg
  | Ok driver ->
    Printf.printf "  create dataset...\n%!";
    match Gdal.Driver.create driver ~filename:test_file ~width:256 ~height:256
            ~bands:3 Gdal.Byte with
    | Error msg -> failwith msg
    | Ok ds ->
      for b = 1 to 3 do
        Printf.printf "  get_band %d...\n%!" b;
        match Gdal.Dataset.get_band ds b with
        | Error msg -> failwith msg
        | Ok band ->
          Printf.printf "  creating bigarray...\n%!";
          let arr = Bigarray.Array2.create Bigarray.int8_unsigned Bigarray.c_layout 256 256 in
          for y = 0 to 255 do
            for x = 0 to 255 do
              Bigarray.Array2.set arr y x ((b * 64 + y + x) mod 256)
            done
          done;
          Printf.printf "  write_region band %d...\n%!" b;
          (match Gdal.RasterBand.write_region Gdal.BA_byte band
                   ~x_off:0 ~y_off:0 ~x_size:256 ~y_size:256 arr with
          | Error msg -> failwith msg
          | Ok () -> Printf.printf "  write_region band %d done\n%!" b)
      done;
      Printf.printf "  set_projection...\n%!";
      (match Gdal.Dataset.set_projection ds
               "GEOGCS[\"WGS 84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS 84\",6378137,298.257223563]],PRIMEM[\"Greenwich\",0],UNIT[\"degree\",0.0174532925199433]]" with
      | Error msg -> failwith msg
      | Ok () -> ());
      Printf.printf "  set_geo_transform...\n%!";
      (match Gdal.Dataset.set_geo_transform ds
               { origin_x = -180.0; pixel_width = 1.40625;
                 row_rotation = 0.0; origin_y = 90.0;
                 col_rotation = 0.0; pixel_height = -0.703125 } with
      | Error msg -> failwith msg
      | Ok () -> ());
      Printf.printf "  close...\n%!";
      Gdal.Dataset.close ds;
      Printf.printf "  done.\n%!"

(* Verify pixel values match the expected pattern *)
let check_pixels band_num arr x_off y_off x_size y_size =
  for y = 0 to y_size - 1 do
    for x = 0 to x_size - 1 do
      let expected = (band_num * 64 + (y_off + y) + (x_off + x)) mod 256 in
      let actual = Bigarray.Array2.get arr y x in
      if actual <> expected then
        Printf.eprintf
          "CORRUPTION: band=%d x_off=%d y_off=%d x=%d y=%d expected=%d got=%d\n%!"
          band_num x_off y_off x y expected actual
    done
  done

(* Reader domain: repeatedly open dataset, read random regions, verify *)
let reader_work domain_id =
  let errors = Atomic.make 0 in
  for i = 0 to iterations - 1 do
    (* Vary access pattern each iteration *)
    let band_num = 1 + (i mod 3) in
    let x_off = (i * 17 + domain_id * 37) mod 200 in
    let y_off = (i * 13 + domain_id * 41) mod 200 in
    let size = 16 + (i mod 32) in
    (match Gdal.Dataset.open_ test_file with
    | Error msg ->
      Printf.eprintf "Domain %d iter %d: open failed: %s\n%!" domain_id i msg;
      Atomic.incr errors
    | Ok ds ->
      (* Exercise multiple code paths that use ctypes-managed memory *)

      (* 1. Read a band region (exercises bigarray + raster_io with release_runtime_lock) *)
      (match Gdal.Dataset.get_band ds band_num with
      | Error msg ->
        Printf.eprintf "Domain %d: get_band failed: %s\n%!" domain_id msg;
        Atomic.incr errors
      | Ok band ->
        (match Gdal.RasterBand.read_region Gdal.BA_byte band
                 ~x_off ~y_off ~x_size:size ~y_size:size
                 ~buf_x:size ~buf_y:size with
        | Error msg ->
          Printf.eprintf "Domain %d: read_region failed: %s\n%!" domain_id msg;
          Atomic.incr errors
        | Ok arr ->
          check_pixels band_num arr x_off y_off size size);

        (* 2. Read as float64 too — different bigarray type, same raster_io path *)
        (match Gdal.RasterBand.read_region Gdal.BA_float64 band
                 ~x_off ~y_off ~x_size:size ~y_size:size
                 ~buf_x:size ~buf_y:size with
        | Error msg ->
          Printf.eprintf "Domain %d: read_region f64 failed: %s\n%!" domain_id msg;
          Atomic.incr errors
        | Ok arr ->
          for y = 0 to size - 1 do
            for x = 0 to size - 1 do
              let expected = Float.of_int ((band_num * 64 + (y_off + y) + (x_off + x)) mod 256) in
              let actual = Bigarray.Array2.get arr y x in
              if Float.abs (actual -. expected) > 0.001 then
                Printf.eprintf
                  "CORRUPTION (f64): band=%d x=%d y=%d expected=%.0f got=%.0f\n%!"
                  band_num x y expected actual
            done
          done);

        (* 3. Exercise band metadata queries (CArray for out-params) *)
        (match Gdal.RasterBand.block_size band with
        | Error msg ->
          Printf.eprintf "Domain %d: block_size failed: %s\n%!" domain_id msg;
          Atomic.incr errors
        | Ok _ -> ());

        (match Gdal.RasterBand.data_type band with
        | Error msg ->
          Printf.eprintf "Domain %d: data_type failed: %s\n%!" domain_id msg;
          Atomic.incr errors
        | Ok _ -> ()));

      (* 4. Exercise string return paths (projection) *)
      let _proj = Gdal.Dataset.projection ds in

      (* 5. Exercise geo_transform (CArray of doubles) *)
      (match Gdal.Dataset.get_geo_transform ds with
      | Error msg ->
        Printf.eprintf "Domain %d: get_geo_transform failed: %s\n%!" domain_id msg;
        Atomic.incr errors
      | Ok gt ->
        if Float.abs (gt.origin_x -. (-180.0)) > 0.001 then
          Printf.eprintf "CORRUPTION: geo_transform origin_x=%.6f\n%!" gt.origin_x);

      Gdal.Dataset.close ds);

    (* Force minor GC on this domain between iterations *)
    if i mod 10 = 0 then Gc.minor ()
  done;
  Atomic.get errors

(* Allocator domain: churn the heap to force GC collections *)
let allocator_work _domain_id =
  for _ = 0 to iterations * 100 do
    (* Allocate strings of varying sizes to pressure both minor and major heap *)
    let s = String.make (1 + Random.int 4096) 'x' in
    let _ = String.concat "" [s; s; s] in
    (* Allocate lists to create garbage *)
    let _ = List.init 100 (fun i -> String.make (i + 1) 'y') in
    (* Allocate bigarrays too — same type as what readers use *)
    let ba = Bigarray.Array2.create Bigarray.float64 Bigarray.c_layout 32 32 in
    Bigarray.Array2.fill ba 0.0;
    (* Periodically force compaction *)
    if Random.int 1000 = 0 then Gc.compact ()
  done;
  0

let () =
  Printf.printf "Creating test file...\n%!";
  (try create_test_file ()
   with exn -> Printf.eprintf "create_test_file failed: %s\n%!" (Printexc.to_string exn); exit 1);
  Printf.printf "Launching %d reader domains + %d allocator domains (%d iterations each)...\n%!"
    n_readers n_allocators iterations;
  let domains =
    List.init n_readers (fun id ->
      Domain.spawn (fun () -> reader_work id))
    @
    List.init n_allocators (fun id ->
      Domain.spawn (fun () -> allocator_work (n_readers + id)))
  in
  let total_errors =
    List.fold_left (fun acc d -> acc + Domain.join d) 0 domains
  in
  Printf.printf "Done. Total errors: %d\n%!" total_errors;
  if total_errors > 0 then exit 1
