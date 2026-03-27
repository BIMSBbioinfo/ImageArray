#' @importFrom EBImage rotate flip flop
NULL

#' @describeIn ImageArray-methods rotate image array to 90, 180, 270 degrees
#' @export
setMethod("rotate", signature = "ImageArray", function(x, angle) {
  # validate rotation
  if (!angle %in% c(0, 90, 180, 270, 360)) {
    stop("Only rotations of 0,90,180,270,360 degrees are supported!")
  }

  # check dimensions
  .check_dim(x)
  dim_img <- dim(x[[1]])
  ax <- axes(x)

  # array perm.
  if (angle %in% c(90, 270)) {
    cur_perm <- .swap(
      seq_along(dim_img),
      which(ax == "x"),
      which(ax == "y")
    )
    x <- aperm(x, perm = cur_perm)
  }

  # flop
  if (angle %in% c(90, 180)) {
    x <- flop(x)
  }

  # flip
  if (angle %in% c(180, 270)) {
    x <- flip(x)
  }

  # return
  x
})

#' @describeIn ImageArray-methods permute image
#' @exportMethod aperm
setMethod("aperm", signature = "ImageArray", function(a, perm) {
  for (i in seq_along(a@levels)) {
    a[[i]] <- aperm(a[[i]], perm = perm)
  }
  a
})

#' @describeIn ImageArray-methods negate image
#' @exportMethod negate
setMethod("negate", signature = "ImageArray", function(object) {
  for (i in seq_along(object@levels)) {
    object[[i]] <- 255L - object[[i]]
  }
  object
})

#' @describeIn ImageArray-methods modulate image
#' @exportMethod modulate
setMethod("modulate", signature = "ImageArray", function(object, brightness) {
  if (brightness < 0) {
    stop("Brightness should be more than 0, typically more than 100")
  }
  for (i in seq_along(object@levels)) {
    tmp <- ceiling(object[[i]] * (brightness / 100))
    max <- if (type(object[[i]]) == "double") 1 else 255
    tmp[tmp > max] <- max
    if (max == 255) {
      type(tmp) <- "integer"
    }
    object[[i]] <- tmp
  }
  object
})

#' @importFrom stats setNames
#' @noRd
.flipflop <- function(object, direction = "x") {
  ax <- axes(object)

  # check dim
  .check_dim(object)

  # flip all
  for (i in seq_along(object@levels)) {
    img <- object[[i]]
    dim_img <- stats::setNames(dim(img), ax)
    cur_ind <- stats::setNames(lapply(dim_img, seq_len), ax)
    cur_ind[[direction]] <- rev(cur_ind[[direction]])
    object[[i]] <- .subset_array(object[[i]], cur_ind, drop = FALSE)
  }
  object
}

#' @describeIn ImageArray-methods vertical flipping image
#' @export
setMethod("flip", signature = "ImageArray", function(x) {
  .flipflop(x, direction = "y")
})

#' @describeIn ImageArray-methods horizontal flipping image
#' @export
setMethod("flop", signature = "ImageArray", function(x) {
  .flipflop(x, direction = "x")
})

#' @describeIn ImageArray-methods cropping image
#' @importFrom utils head tail
#' @importFrom stats setNames
#' @exportMethod crop
setMethod("crop", signature = "ImageArray", function(object, index) {
  # check_dim
  .check_dim(object)

  # get axes
  ax <- axes(object)
  dim_img <- stats::setNames(dim(object), ax)

  # check ind
  if (missing(index)) {
    index <- vector(mode = "list", length = length(ax))
  }
  index <- .check_indices(index = index, dim = dim_img, ax = ax)

  # check sequential
  check_sequential <- all(vapply(index[c("x", "y")], is.sequential, logical(1)))
  if (!check_sequential) {
    stop(
      "'index' should be a list of sequantial integer 
                   vectors (hence slice)"
    )
  }

  # crop all images
  for (i in seq_along(object@levels)) {
    img <- object[[i]]
    dim_img <- stats::setNames(dim(img), ax)[c("x", "y")]
    cur_ind <- index
    cur_ind[c("x", "y")] <-
      lapply(seq_along(index[c("x", "y")]), function(j) {
        curind <- index[c("x", "y")][[j]]
        id <- c(
          floor(utils::head(curind, 1) / (2^(i - 1))),
          ceiling(utils::tail(curind, 1) / (2^(i - 1)))
        )
        seq(max(id[1], 1), min(id[2], dim_img[j]))
      })
    object[[i]] <- .subset_array(img, cur_ind, drop = FALSE)
  }

  object
})

#' @describeIn ImageArray-methods get axes metadata of the ImageArray object
#' @exportMethod axes
setMethod("axes", "ImageArray", function(object) meta(object)[["axes"]])

#' @describeIn ImageArray-methods get metadata of the ImageArray object
#' @exportMethod meta
setMethod("meta", "ImageArray", function(object) object@meta)
