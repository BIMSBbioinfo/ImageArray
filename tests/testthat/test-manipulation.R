library(magick)
library(HDF5Array)
library(ZarrArray)

output_h5 <- tempfile(fileext = ".h5")
output_zarr <- tempfile(fileext = ".zarr")

# build image array
set.seed(1)
mat <- array(
  data = sample(0:255, 2000 * 5000 * 3, replace = TRUE),
  dim = c(2000, 5000, 3)
)
mat_image <- mat

test_that("manipulate h5 ImageArray", {
  # create image array
  mat_list <- writeImageArray(
    mat_image,
    output = output_h5,
    name = "image",
    format = "h5",
    replace = TRUE,
    verbose = FALSE
  )
  expect_equal(length(mat_list), 4)
  expect_equal(dim(mat_list), c(2000, 5000, 3))
  expect_equal(axes(mat_list), c("x", "y", "c"))

  # aperm
  mat_list_perm <- aperm(mat_list, perm = c(2, 1, 3))
  expect_equal(dim(mat_list_perm), c(5000, 2000, 3))

  # crop
  mat_list_cropped <- crop(mat_list, ind = list(1001:2000, 2001:3000, NULL))
  expect_equal(dim(mat_list_cropped), c(1000, 1000, 3))

  # negate
  mat_list_negated <- negate(mat_list)
  tmp <- realize(mat_list[[1]]) + realize(mat_list_negated[[1]])
  expect_equal(unique(as.vector(tmp)), 255)
  expect_equal(type(mat_list_negated[[1]]), "integer")

  # flip flop
  mat_list_flipflop <- flip(mat_list)
  expect_equal(
    realize(mat_list_flipflop)[,,1][1,],
    rev(realize(mat_list)[,,1][1,])
  )
  mat_list_flipflop <- flop(mat_list)
  expect_equal(
    realize(mat_list_flipflop)[,,1][,1],
    rev(realize(mat_list)[,,1][,1])
  )
})

test_that("manipulate h5 ImageArray (with magick)", {
  # modulate
  mat_list <- writeImageArray(
    mat_image,
    output = output_h5,
    name = "image",
    format = "h5",
    engine = "magick-image",
    axes = c("x", "y", "c"),
    replace = TRUE,
    verbose = FALSE
  )
  expect_equal(length(mat_list), 4)
  expect_equal(dim(mat_list), c(2000, 5000, 3))
  mat_list_modulated <- modulate(mat_list, brightness = 200)
  expect_equal(type(mat_list_modulated[[1]]), "integer")
  expect_equal(type(mat_list_modulated), "integer")
  orig <- realize(mat_list[1:10, 1:10,]) * 2
  orig[orig > 255] <- 255
  newmat <- realize(mat_list_modulated[1:10, 1:10,])
  expect_equal(orig, newmat)
})

test_that("manipulate zarr ImageArray", {
  unlink(output_zarr, recursive = TRUE)
  mat_list <- writeImageArray(
    mat_image,
    output = output_zarr,
    name = "image",
    format = "zarr",
    replace = TRUE,
    verbose = FALSE
  )
  expect_equal(length(mat_list), 4)
  expect_equal(dim(mat_list), c(2000, 5000, 3))

  # aperm
  mat_list_perm <- aperm(mat_list, perm = c(2, 1, 3))
  expect_equal(dim(mat_list_perm), c(5000, 2000, 3))

  # crop
  mat_list_cropped <- crop(mat_list, ind = list(1001:2000, 2001:3000, NULL))
  expect_equal(dim(mat_list_cropped), c(1000, 1000, 3))
  mat_list_cropped <- mat_list[1001:2000, 2001:3000,]
  expect_equal(
    mat_list_cropped,
    crop(mat_list, ind = list(1001:2000, 2001:3000, NULL))
  )
  expect_error(crop(mat_list, ind = list(2001:3000, c(10, 20), NULL)))

  # negate
  mat_list_negated <- negate(mat_list)
  tmp <- realize(mat_list[[1]]) + realize(mat_list_negated[[1]])
  expect_equal(unique(as.vector(tmp)), 255)

  # flip flop
  mat_list_flipflop <- flip(mat_list)
  expect_equal(
    realize(mat_list_flipflop)[,,1][1,],
    rev(realize(mat_list)[,,1][1,])
  )
  mat_list_flipflop <- flop(mat_list)
  expect_equal(
    realize(mat_list_flipflop)[,,1][,1],
    rev(realize(mat_list)[,,1][,1])
  )
})

test_that("manipulate zarr ImageArray (with magick)", {
  mat_list <- writeImageArray(
    mat_image,
    output = output_zarr,
    name = "image",
    format = "zarr",
    engine = "magick-image",
    axes = c("x", "y", "c"),
    replace = TRUE,
    verbose = FALSE
  )
  expect_equal(length(mat_list), 4)
  expect_equal(dim(mat_list), c(2000, 5000, 3))
  mat_list_modulated <- modulate(mat_list, brightness = 200)
  expect_equal(type(mat_list_modulated[[1]]), "integer")
  expect_equal(type(mat_list_modulated), "integer")
  orig <- realize(mat_list[1:10, 1:10,]) * 2
  orig[orig > 255] <- 255
  newmat <- realize(mat_list_modulated[1:10, 1:10,])
  expect_equal(orig, newmat)
})
