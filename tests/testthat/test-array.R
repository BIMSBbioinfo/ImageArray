library(EBImage)

# arrays of images
img2d <- array(
  data = sample(0:255, 20 * 50, replace = TRUE),
  dim = c(20, 50)
)
img3d <- array(
  data = sample(0:255, 20 * 50 * 3, replace = TRUE),
  dim = c(20, 50, 3)
)

test_that("array downscaled with EBImage and n.levels", {
  
  # no axes, has to be c("x", "y", "c") for EBImage
  imgarray <- ImageArray(img3d, 
                         n.levels = 2)
  expect_equal(axes(imgarray), c("x", "y", "c"))
  expect_equal(dim(imgarray[[2]]), c(10, 25, 3))
  
  # no axes 2D
  imgarray <- ImageArray(img2d,
                         n.levels = 2)
  expect_equal(axes(imgarray), c("x", "y"))
  expect_equal(dim(imgarray[[2]]), c(10, 25))
  
  # custom axes 3D
  imgarray <- ImageArray(img3d, axes = c("y", "x", "c"), 
                         n.levels = 2)
  expect_equal(axes(imgarray), c("y", "x", "c"))

  # custom axes 2D
  imgarray <- ImageArray(img2d, axes = c("y", "x"), 
                         n.levels = 2)
  expect_equal(axes(imgarray), c("y", "x"))
})

test_that("array downscaled with magick and n.levels", {

  # no axes, has to be c("c", "x", "y") for magick
  expect_error(
    imgarray <- ImageArray(img3d, 
                           n.levels = 2, 
                           engine = "magick-image"),
    "Expected an \\(x, y\\) or \\(c, x, y\\) image with 1,3,4 channels."
  )

  # custom axes 2D
  imgarray <- ImageArray(img2d,
                         n.levels = 2, 
                         engine = "magick-image")
  expect_equal(axes(imgarray), c("x", "y"))
  expect_equal(dim(imgarray[[2]]), c(10, 25))
  
  # custom axes 3D
  imgarray <- ImageArray(img3d, axes = c("x", "y", "c"), 
                         n.levels = 2, 
                         engine = "magick-image")
  expect_equal(axes(imgarray), c("x", "y", "c"))
  expect_equal(dim(imgarray[[2]]), c(10, 25, 3))
  
  # custom axes 2D
  imgarray <- ImageArray(img2d, axes = c("y", "x"), 
                         n.levels = 2, 
                         engine = "magick-image")
  expect_equal(axes(imgarray), c("y", "x"))
})

test_that("array downscaled with EBImage and scales", {
  
  imgarray <- ImageArray(img3d, 
                         scales = list(c(x = 1, y = 1, c = 1),
                                       c(x = 1.5, y = 5, c = 1)))
  expect_equal(dim(imgarray[[2]]), c(13, 10, 3))
})

test_that("array downscaled with magick and scales", {
  
  imgarray <- ImageArray(img3d, 
                         scales = list(c(x = 1, y = 1, c = 1),
                                       c(x = 1.5, y = 5, c = 1)), 
                         engine = "magick-image")
  expect_equal(dim(imgarray[[2]]), c(13, 10, 3))
})