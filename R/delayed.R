####
# Classes ####
####

## Delayed affine helpers + constructor
## Assumes these packages are available/loaded:
## library(DelayedArray)
## library(S4Arrays)
## library(EBImage)

setClass(
  "DelayedAffineSeed",
  contains = "DelayedUnaryOp",
  slots = c(
    seed = "ANY",
    dim = "integer",
    dimnames = "list",
    m = "matrix",
    axes = "character",
    filter = "character",
    bg.col = "character",
    antialias = "logical"
  )
)

setClass(
  "DelayedTranslateSeed",
  contains = "DelayedUnaryOp",
  slots = c(
    seed = "ANY",
    dim = "integer",
    dimnames = "list",
    shift = "numeric",
    axes = "character"
  )
)

setClassUnion("DelayedImageSeed", 
              c("DelayedAffineSeed", "DelayedTranslateSeed"))

setMethod("dim", "DelayedImageSeed", function(x) {
  x@dim
})

setMethod("dimnames", "DelayedImageSeed", function(x) {
  x@dimnames
})

setMethod("type", "DelayedImageSeed", function(x) {
  DelayedArray::type(x@seed)
})

####
# Utils ####
####

.as_homogeneous <- function(m) {
  if (!is.matrix(m) || !identical(dim(m), c(3L, 2L))) {
    stop("'m' must be a 3 x 2 EBImage affine matrix")
  }
  
  cbind(m, c(0, 0, 1))
}

.from_homogeneous <- function(M) {
  M[, seq_len(2), drop = FALSE]
}

.translate_h <- function(dx, dy) {
  matrix(
    c(
      1,  0,  0,
      0,  1,  0,
      dx, dy, 1
    ),
    nrow = 3,
    byrow = TRUE
  )
}

.default_axes <- function(ndim) {
  c(
    "x",
    "y",
    if (ndim > 2L) paste0("d", seq.int(3L, ndim)) else character()
  )
}

.spatial_perm <- function(ndim, axes) {
  xy <- match(c("x", "y"), axes)
  c(xy, setdiff(seq_len(ndim), xy))
}

.spatial_first <- function(a, axes) {
  p <- .spatial_perm(length(dim(a)), axes)
  
  if (identical(p, seq_along(p))) {
    a
  } else {
    aperm(a, p)
  }
}

.spatial_back <- function(a, axes) {
  ndim <- length(dim(a))
  p <- .spatial_perm(ndim, axes)
  inv <- order(p)
  
  if (identical(inv, seq_len(ndim))) {
    a
  } else {
    aperm(a, inv)
  }
}

.normalize_index <- function(index, dim) {
  if (length(index) == 0L) {
    index <- vector("list", length(dim))
  }
  
  if (length(index) != length(dim)) {
    stop("'index' must have one entry per dimension")
  }
  
  Map(
    function(i, n) {
      if (is.null(i)) {
        seq_len(n)
      } else {
        as.integer(i)
      }
    },
    index,
    dim
  )
}

.is_full_index <- function(index, dim) {
  all(vapply(
    seq_along(dim),
    function(k) identical(index[[k]], seq_len(dim[k])),
    logical(1)
  ))
}

.bg_array <- function(x, dim) {
  type <- tryCatch(
    DelayedArray::type(x@seed),
    error = function(e) "double"
  )
  
  proto <- switch(
    type,
    logical = logical(prod(dim)),
    integer = integer(prod(dim)),
    double = numeric(prod(dim)),
    numeric = numeric(prod(dim)),
    complex = complex(prod(dim)),
    character = character(prod(dim)),
    raw = raw(prod(dim)),
    numeric(prod(dim))
  )
  
  array(proto, dim = dim)
}

.source_bbox_from_output_query <- function(m,
                                           out_x,
                                           out_y,
                                           source_dim_xy,
                                           filter = "bilinear") {
  M <- .as_homogeneous(m)
  Minv <- solve(M)
  
  ## EBImage-style pixel coordinates are treated as zero-based here.
  ## R index 1 corresponds to coordinate 0.
  x0 <- min(out_x) - 1
  x1 <- max(out_x) - 1
  y0 <- min(out_y) - 1
  y1 <- max(out_y) - 1
  
  output_corners <- rbind(
    c(x0, y0, 1),
    c(x1, y0, 1),
    c(x0, y1, 1),
    c(x1, y1, 1)
  )
  
  source_corners <- output_corners %*% Minv
  
  sx_min <- min(source_corners[, 1])
  sx_max <- max(source_corners[, 1])
  sy_min <- min(source_corners[, 2])
  sy_max <- max(source_corners[, 2])
  
  pad <- if (identical(filter, "bilinear")) 2L else 1L
  
  sx0 <- floor(sx_min) + 1L - pad
  sx1 <- ceiling(sx_max) + 1L + pad
  sy0 <- floor(sy_min) + 1L - pad
  sy1 <- ceiling(sy_max) + 1L + pad
  
  sx0 <- max(1L, sx0)
  sy0 <- max(1L, sy0)
  sx1 <- min(source_dim_xy[1], sx1)
  sy1 <- min(source_dim_xy[2], sy1)
  
  list(
    x = if (sx0 <= sx1) seq.int(sx0, sx1) else integer(0),
    y = if (sy0 <= sy1) seq.int(sy0, sy1) else integer(0)
  )
}

.localize_affine <- function(m,
                             source_origin_index,
                             output_origin_index) {
  M <- .as_homogeneous(m)
  
  ## Convert R indices to zero-based EBImage coordinates.
  source_origin0 <- source_origin_index - 1
  output_origin0 <- output_origin_index - 1
  
  M_local <-
    .translate_h(source_origin0[1], source_origin0[2]) %*%
    M %*%
    .translate_h(-output_origin0[1], -output_origin0[2])
  
  .from_homogeneous(M_local)
}

.collect_affine_transform_seed <- function(x) {
  full_index <- vector("list", length(dim(x@seed)))
  
  arr <- S4Arrays::extract_array(x@seed, full_index)
  arr <- .spatial_first(arr, x@axes)
  
  xy <- match(c("x", "y"), x@axes)

  out <- EBImage::affine(
    arr,
    m = x@m,
    filter = x@filter,
    output.dim = x@dim[xy],
    bg.col = x@bg.col,
    antialias = x@antialias
  )

  .spatial_back(as.array(out), x@axes)
}

.get_extent_from_box <- function(bboxmin, bboxmax) {
  list(x = c(bboxmin[1], bboxmax[1]),
       y = c(bboxmin[2], bboxmax[2]))
}

.get_bbox_from_extent <- function(ext,m){
  px <- as.matrix(expand.grid(ext[[1]], ext[[2]]))
  transformed <- sweep(px %*% m[seq_len(2),], 2L, m[3,], "+")
  bbox.min <- apply(transformed, 2L, min)
  bbox.max <- apply(transformed, 2L, max)
  list(min = bbox.min, max = bbox.max)
}

.adjust_affine_matrix <- function(dim, axes, m){
  dim <- setNames(dim, axes)
  bbox <- .get_bbox_from_extent(
    list(x = c(0,dim[["x"]]), y = c(0,dim[["y"]])),
    m = m)
  m[3, ] <- m[3,] - bbox$min
  newdim <- bbox$max - bbox$min
  dim[c("x", "y")] <- newdim
  names(dim) <- NULL
  list(m=m, output.dim=dim)
}

# extract_array ####

setMethod(
  "extract_array",
  "DelayedAffineSeed",
  function(x, index) {
    out_dim <- dim(x)
    index <- .normalize_index(index, out_dim)
    
    ans_dim <- vapply(index, length, integer(1))
    
    if (any(ans_dim == 0L)) {
      return(.bg_array(x, ans_dim))
    }
    
    if (.is_full_index(index, out_dim)) {
      return(.collect_affine_transform_seed(x))
    }
    
    xdim <- match("x", x@axes)
    ydim <- match("y", x@axes)
    
    out_x <- index[[xdim]]
    out_y <- index[[ydim]]
    
    ## Render a contiguous output window, then subset/reorder at the end.
    out_x_window <- seq.int(min(out_x), max(out_x))
    out_y_window <- seq.int(min(out_y), max(out_y))
    
    source_dim <- dim(x@seed)
    
    bbox <- .source_bbox_from_output_query(
      m = x@m,
      out_x = out_x_window,
      out_y = out_y_window,
      source_dim_xy = source_dim[c(xdim, ydim)],
      filter = x@filter
    )
    
    if (length(bbox$x) == 0L || length(bbox$y) == 0L) {
      return(.bg_array(x, ans_dim))
    }
    
    source_index <- index
    source_index[[xdim]] <- bbox$x
    source_index[[ydim]] <- bbox$y
    
    source_patch <- S4Arrays::extract_array(x@seed, source_index)
    source_patch <- .spatial_first(source_patch, x@axes)
    
    m_local <- .localize_affine(
      m = x@m,
      source_origin_index = c(min(bbox$x), min(bbox$y)),
      output_origin_index = c(min(out_x_window), min(out_y_window))
    )
    
    rendered <- EBImage::affine(
      source_patch,
      m = m_local,
      filter = x@filter,
      output.dim = c(length(out_x_window), length(out_y_window)),
      bg.col = x@bg.col,
      antialias = x@antialias
    )
    
    rendered <- .spatial_back(as.array(rendered), x@axes)
    
    local_index <- vector("list", length(out_dim))
    local_index[[xdim]] <- match(out_x, out_x_window)
    local_index[[ydim]] <- match(out_y, out_y_window)
    
    for (k in seq_along(out_dim)) {
      if (k != xdim && k != ydim) {
        local_index[[k]] <- seq_along(index[[k]])
      }
    }
    
    do.call(
      `[`,
      c(list(rendered), local_index, list(drop = FALSE))
    )
  }
)

setMethod(
  "extract_array",
  "DelayedTranslateSeed",
  function(x, index) {
    out_dim <- dim(x)
    index <- .normalize_index(index, out_dim)
    S4Arrays::extract_array(x@seed, index)
  })

# extent ####

setGeneric("extent", \(x, ...) standardGeneric("extent"))

setMethod("extent", "ImageArray", function(x){
  dims <- c("x", "y")
  xy <- match(dims, axes(x))
  setNames(extent(x[[1]])[xy], dims)
})

setMethod("extent", "DelayedArray", function(x){
  extent(x@seed)
})

setMethod("extent", "DelayedAffineSeed", function(x){
  ext <- extent(x@seed)
  xy <- match(c("x", "y"), x@axes)
  bbox <- .get_bbox_from_extent(ext[xy], x@m)
  ext[xy] <- .get_extent_from_box(bbox$min, bbox$max)
  ext
})

setMethod("extent", "DelayedTranslateSeed", function(x){
  ext <- extent(x@seed)
  xy <- match(c("x", "y"), x@axes)
  ext[xy] <- Map(\(i,j){
    i+j
  }, ext[xy],x@shift)
  ext
})

setMethod("extent", "Array", function(x){
  lapply(dim(x), \(.){
    c(0,.)
  })
})

# lazy transformations ####

.affine_transform <- function(x,
                              m,
                              axes = NULL,
                              filter = c("bilinear", "none"),
                              bg.col = "black",
                              antialias = TRUE) {
  filter <- match.arg(filter)
  
  adj <- .adjust_affine_matrix(dim(x), axes, m)
  output.dim <- as.integer(adj$output.dim)
  m <- adj$m
  dn <- vector("list", length(output.dim))
  
  seed <- new(
    "DelayedAffineSeed",
    seed = x@seed,
    dim = as.integer(output.dim),
    dimnames = dn,
    m = m,
    axes = axes,
    filter = filter,
    bg.col = bg.col,
    antialias = isTRUE(antialias)
  )
  
  DelayedArray::DelayedArray(seed)
}

setMethod("affine_transform", 
          signature = "ImageArray", 
          function(x,
                   m,
                   axes = NULL,
                   filter = c("bilinear", "none"),
                   bg.col = "black",
                   antialias = TRUE) {
            ax <- axes(x)
            for (i in seq_along(x@levels)) {
              scl <- rep(2^(i - 1), 2)
              m <- solve(diag(c(scl, 1))) %*% m %*% diag(scl) 
              x[[i]] <- affine_transform(
                x[[i]],
                m = m,
                axes = ax,
                filter = filter,
                bg.col = bg.col,
                antialias = antialias
              )
            }
            x
          })

setMethod("affine_transform", signature = "DelayedArray", .affine_transform)

.scale_transform <- function(x,
                             output.dim = NULL,
                             output.origin = c(0,0),
                             axes = NULL,
                             filter = c("bilinear", "none"),
                             bg.col = "black",
                             antialias = TRUE) {
  if (length(output.origin) != 2L || !is.numeric(output.origin)) 
    stop("'output.origin' must be a numeric vector of length 2")
  xy <- match(c("x", "y"), axes)
  ratio <- output.dim/dim(x)[xy]
  m <- matrix(c(ratio[1], 0, (1 - ratio[1]) * output.origin[1], 
                0, ratio[2], (1 - ratio[2]) * output.origin[2]), 3L, 
              2L)
  .affine_transform(x,
                    m,
                    axes = axes,
                    filter = filter,
                    bg.col = bg.col,
                    antialias = antialias)
}

setMethod("scale_transform", 
          signature = "ImageArray", 
          function(x,
                   output.dim = NULL,
                   output.origin = c(0,0),
                   axes = NULL,
                   filter = c("bilinear", "none"),
                   bg.col = "black",
                   antialias = TRUE) {
            ax <- axes(x)
            for (i in seq_along(x@levels)) {
              cur_output.dim <- output.dim / 2^(i - 1)
              x[[i]] <- scale_transform(
                x[[i]],
                output.dim = cur_output.dim,
                output.origin = output.origin,
                axes = ax,
                filter = filter,
                bg.col = bg.col,
                antialias = antialias
              )
            }
            x
          })

setMethod("scale_transform", signature = "DelayedArray", .scale_transform)

.rotate_transform <- function(x,
                              angle,
                              output.dim = NULL,
                              output.origin = c(0,0),
                              axes = NULL,
                              filter = c("bilinear", "none"),
                              bg.col = "black",
                              antialias = TRUE) {
  if (length(angle) != 1L || !is.numeric(angle)) 
    stop("'angle' must be a number")
  if (!missing(output.dim)) 
    if (length(output.dim) != 2L || !is.numeric(output.dim)) 
      stop("'output.dim' must be a numeric vector of length 2")
  if ((angle%%90) == 0) 
    filter <- "none"
  angle <- angle * pi/180
  xy <- match(c("x", "y"), axes)
  d <- dim(x)[xy]
  cos <- cos(angle)
  sin <- sin(angle)
  if (missing(output.origin)) {
    newdim <- c(d[1] * abs(cos) + d[2] * abs(sin), d[1] * abs(sin) + 
                  d[2] * abs(cos))
    offset <- c(d[1] * max(0, -cos) + d[2] * max(0, sin), d[1] * 
                 max(0, -sin) + d[2] * max(0, -cos))
    if (missing(output.dim)) 
      output.dim <- newdim
    else offset <- offset + (output.dim - newdim)/2
  }
  else {
    if (length(output.origin) != 2L || !is.numeric(output.origin)) 
      stop("'output.origin' must be a numeric vector of length 2")
    offset <- c(output.origin[1L] * (1 - cos) + output.origin[2L] * 
                 sin, output.origin[2L] * (1 - cos) - output.origin[1L] * 
                 sin)
  }
  m <- matrix(c(cos, -sin, offset[1], sin, cos, offset[2]), 
              3L, 2L)
  affine_transform(x,
                   m,
                   axes = axes,
                   filter = filter,
                   bg.col = bg.col,
                   antialias = antialias)
}

setMethod("rotate_transform", signature = "DelayedArray", .rotate_transform)

.translate_transform <- function(x,
                                 shift = c(0,0),
                                 axes = NULL) {
  if (length(shift) != 2L || !is.numeric(shift)) 
    stop("'shift' must be a numeric vector of length 2, ", 
         "associated with x,y dimenions of an image!")
  
  d1 <- dim(x)
  dn <- vector("list", length(d1))
  
  seed <- new(
    "DelayedTranslateSeed",
    seed = x@seed,
    dim = as.integer(d1),
    dimnames = dn,
    shift = shift,
    axes = axes
  )
  
  DelayedArray::DelayedArray(seed)
}

setMethod("translate_transform", 
          signature = "ImageArray", 
          function(x,
                   shift = c(0,0),
                   axes = NULL) {
            ax <- axes(x)
            for (i in seq_along(x@levels)) {
              x[[i]] <- translate_transform(
                x[[i]],
                shift = shift,
                axes = ax
              )
            }
            x
          })

setMethod("translate_transform", signature = "DelayedArray", .translate_transform)