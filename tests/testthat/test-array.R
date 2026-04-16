library(EBImage)

# image file
img.file <- system.file("images", "sample.png", package = "EBImage")

test_that("check indexing", {
  
  # create ImageArray
  imgarray <- createImageArray(img.file, n.levels = 2)

  # crop
  imgarray_vis <- crop(imgarray, ind = list(100:200, 100:200))
  imgarray_vis <- as.raster(imgarray_vis)
  plot(imgarray_vis)

  # [ method works
  imgarray_vis <- imgarray[100:200,]
  expect_equal(dim(imgarray_vis), c(101, dim(imgarray)[2]))
  imgarray_vis <- imgarray[,100:200]
  expect_equal(dim(imgarray_vis), c(dim(imgarray)[1], 101))
  imgarray_vis <- imgarray[,]
  expect_equal(dim(imgarray_vis), dim(imgarray))
  
  # [ indexing error
  expect_error(imgarray[-100:200,])
  expect_error(imgarray[,-100])
  expect_error(imgarray[1000:3000,])
  expect_error(imgarray["art",])
  expect_error(imgarray[integer(0),])
  expect_error(imgarray[,numeric(0)])
})

img.file <- system.file(
  "extdata",
  "xy_12bit__plant.ome.tiff",
  package = "ImageArray"
)

test_that("check indexing (BFArray)", {
  
  # create ImageArray
  imgarray <- createImageArray(img.file, series = 1, resolution = 1:2)
  
  # crop
  imgarray_vis <- crop(imgarray, ind = list(100:200, 100:200))
  imgarray_vis <- as.raster(imgarray_vis)
  plot(imgarray_vis)
  
  # crop using names
  imgarray_vis <- crop(imgarray, ind = list(x = 100:200, y = 100:200))
  imgarray_vis <- as.raster(imgarray_vis)
  plot(imgarray_vis)
  expect_error(
    imgarray_vis <- crop(imgarray, ind = list(z = 100:200, y = 100:200))
  )
  
  # [ method works
  imgarray_vis <- imgarray[100:200,,]
  expect_equal(dim(imgarray_vis), c(101, dim(imgarray)[2], 1))
  imgarray_vis <- imgarray[,100:200,]
  expect_equal(dim(imgarray_vis), c(dim(imgarray)[1], 101, 1))
  imgarray_vis <- imgarray[,,]
  expect_equal(dim(imgarray_vis), dim(imgarray))
  
  # [ indexing error
  expect_error(imgarray[100:200,])
  expect_error(imgarray[100:200,,2])
  expect_error(imgarray[-100:200,,])
  expect_error(imgarray[,-100,])
  expect_error(imgarray[1000:3000,])
})