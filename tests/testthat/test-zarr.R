td <- tempfile(fileext = ".zarr")

test_that("open/create zarr group", {
  # open zarr
  create_zarr(store = td)
  expect_true(dir.exists(td))
  expect_true(file.exists(file.path(td, ".zgroup")))

  # create group one group
  create_zarr_group(store = td, name = "sample")
  expect_true(dir.exists(file.path(td, "sample")))
  expect_true(file.exists(file.path(td, "sample", ".zgroup")))

  # create nested two groups
  create_zarr_group(store = td, name = "sample1/layer1")
  expect_true(dir.exists(file.path(td, "sample1")))
  expect_true(file.exists(file.path(td, "sample1", ".zgroup")))
  expect_true(dir.exists(file.path(td, "sample1/layer1")))
  expect_true(file.exists(file.path(td, "sample1/layer1", ".zgroup")))

  # create nested three groups
  create_zarr_group(store = td, name = "sample2/layer1/assay1")
  expect_true(dir.exists(file.path(td, "sample2")))
  expect_true(file.exists(file.path(td, "sample2", ".zgroup")))
  expect_true(dir.exists(file.path(td, "sample2/layer1")))
  expect_true(file.exists(file.path(td, "sample2/layer1", ".zgroup")))
  expect_true(dir.exists(file.path(td, "sample2/layer1/assay1")))
  expect_true(file.exists(file.path(
    td,
    "sample2/layer1/assay1",
    ".zgroup"
  )))
  
  # clean
  unlink(td, recursive = TRUE)
})

test_that("validation", {

  # create empty zarr with no valid path
  expect_error(create_zarr(store = NA))
  expect_error(create_zarr(store = NULL))
  expect_error(create_zarr())

  create_zarr(store = td)

  # create group with no valid path
  expect_error(create_zarr_group(store = td, name = NA))
  expect_error(create_zarr_group(store = td, name = 1))
  expect_error(create_zarr_group(store = td, name = NULL))
  
  # create root group, but already exists
  lapply(
    c("", "/", "//", "///"), 
    \(.){
      expect_message(create_zarr_group(store = td, name = .))
      expect_identical(
        list.files(td, all.files = TRUE),
        c(".", "..", ".zgroup")
      )
    }
  )
})