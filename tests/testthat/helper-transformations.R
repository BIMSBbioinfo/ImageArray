# test affine
test_affine <- function(img, axes, m){
  actual_dim <- dim(img)
  xy <- match(c("x", "y"), axes)
  ap <- c(xy, setdiff(seq_along(actual_dim), xy))
  img <- aperm(img, perm = ap)
  img <- EBImage::Image(img)
  adj <- .adjust_affine_matrix(actual_dim, axes, m)
  img_affine <- EBImage::affine(EBImage::Image(img), m = adj$m, output.dim = adj$output.dim[xy])
  img_affine <- EBImage::imageData(img_affine)
  aperm(img_affine, perm = order(ap))
}

# test scale
test_scale <- function(img, axes, output.dim){
  actual_dim <- dim(img)
  xy <- match(c("x", "y"), axes)
  ap <- c(xy, setdiff(seq_along(actual_dim), xy))
  img <- aperm(img, perm = ap)
  img <- EBImage::Image(img)
  img_scale <- EBImage::resize(EBImage::Image(img), 
                                w = output.dim[1], h = output.dim[2])
  img_scale <- EBImage::imageData(img_scale)
  aperm(img_scale, perm = order(ap))
}

# test scale
test_subset <- function(img, axes, index){
  actual_dim <- dim(img)
  xy <- match(c("x", "y"), axes)
  ap <- c(xy, setdiff(seq_along(actual_dim), xy))
  img <- aperm(img, perm = ap)
  if(length(dim(img)) == 2){
    img_subset <- img[index[[1]], index[[2]]]
  } else {
    img_subset <- img[index[[1]], index[[2]], , drop = FALSE]
  }
  aperm(img_subset, perm = order(ap))
}