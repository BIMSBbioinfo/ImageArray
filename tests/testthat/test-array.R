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

test_that("array downscaled with EBImage", {
  
  # no axes, has to be c("x", "y", "c") for EBImage
  imgarray <- ImageArray(img3d, 
                         n.levels = 2)
  expect_equal(axes(imgarray), c("x", "y", "c"))
  
  # no axes 2D
  imgarray <- ImageArray(img2d,
                         n.levels = 2)
  expect_equal(axes(imgarray), c("x", "y"))
  
  # custom axes 3D
  imgarray <- ImageArray(img3d, axes = c("y", "x", "c"), 
                         n.levels = 2)
  expect_equal(axes(imgarray), c("y", "x", "c"))
  
  # custom axes 2D
  imgarray <- ImageArray(img2d, axes = c("y", "x"), 
                         n.levels = 2)
  expect_equal(axes(imgarray), c("y", "x"))
})

test_that("array downscaled with magick", {

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
  
  # custom axes 3D
  imgarray <- ImageArray(img3d, axes = c("x", "y", "c"), 
                         n.levels = 2, 
                         engine = "magick-image")
  expect_equal(axes(imgarray), c("x", "y", "c"))
  
  # custom axes 2D
  imgarray <- ImageArray(img2d, axes = c("y", "x"), 
                         n.levels = 2, 
                         engine = "magick-image")
  expect_equal(axes(imgarray), c("y", "x"))
})