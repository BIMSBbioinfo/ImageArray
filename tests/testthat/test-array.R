library(EBImage)

test_that("check indexing", {
  
  # image file
  img.file <- system.file("images", "sample.png", package = "EBImage")
  
  # create ImageArray
  imgarray <- ImageArray(img.file, n.levels = 2)

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

test_that("check indexing (3D)", {
  
  # create ImageArray
  imgarray <- ImageArray(image = list(array(1:2197, dim = c(13,13,13)),
                                       array(1:1000, dim = c(10,10,10)),
                                       array(1:216, dim = c(6,6,6))),
                         axes = c("z", "x", "y"))

  # crop
  imgarray_vis <- crop(imgarray, ind = list(1:10, 2:11, 3:12))

  # [ method works
  imgarray_vis <- imgarray[1:5,,]
  expect_equal(dim(imgarray_vis), c(5, dim(imgarray)[2:3]))
  imgarray_vis <- imgarray[,5:10,]
  expect_equal(dim(imgarray_vis), c(dim(imgarray)[1], 6, dim(imgarray)[3]))
  imgarray_vis <- imgarray[,,]
  expect_equal(dim(imgarray_vis), dim(imgarray))
  
  # [ indexing error
  expect_error(imgarray[-10:20,,])
  expect_error(imgarray[,-10,])
  expect_error(imgarray[200:300,,])
  expect_error(imgarray["art",,])
  expect_error(imgarray[integer(0),,])
  expect_error(imgarray[,numeric(0),])
})

test_that("check indexing (BFArray)", {
  
  # image file
  img.file <- system.file(
    "extdata",
    "xy_12bit__plant.ome.tiff",
    package = "ImageArray"
  )
  
  # create ImageArray
  imgarray <- ImageArray(img.file, series = 1, resolution = 1:2)
  
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
  imgarray_vis <- imgarray[100:200,]
  expect_equal(dim(imgarray_vis), c(101, dim(imgarray)[2]))
  imgarray_vis <- imgarray[,100:200]
  expect_equal(dim(imgarray_vis), c(dim(imgarray)[1], 101))
  imgarray_vis <- imgarray[,]
  expect_equal(dim(imgarray_vis), dim(imgarray))
  
  # [ indexing error
  expect_error(imgarray[,100:200,])
  expect_error(imgarray[100:200,,2])
  expect_error(imgarray[-100:200,,])
  expect_error(imgarray[,-100,])
  expect_error(imgarray[1000:3000,])
})