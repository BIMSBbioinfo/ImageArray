test_that("image array class", {
  
  # correct image array construction
  imgarray <- ImageArray(meta = list(axes = c("c", "y", "x")), 
                         levels = list(array(1:75, dim = c(3,5,5))))
  imgarray <- ImageArray(meta = list(axes = c("y", "x")), 
                         levels = list(array(1:25, dim = c(5,5))))
  
  # incorrect axes names
  expect_error(
    imgarray <- ImageArray(meta = list(axes = c("b", "a", "d")), 
                           levels = list(array(1:75, dim = c(3,5,5))))
  )
  
  # incorrect dimensions
  expect_error(
    imgarray <- ImageArray(meta = list(axes = c("c", "y", "x")), 
                           levels = list(array(1:75, dim = c(3,5,5)),
                                         array(1:75, dim = c(3,6,6,2))))
  )
})
