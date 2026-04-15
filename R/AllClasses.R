setOldClass("ome_zarr")

.ImageArray <- setClass(
  Class = "ImageArray",
  slots = c(
    meta = "list",
    levels = "list"
  )
)

.BFArraySeed <- setClass(
  "BFArraySeed",
  contains = "Array",
  slots = c(
    filepath = "character",
    series = "numeric",
    resolution = "numeric",
    shape = "numeric",
    type = "character"
  )
)

.BFArray <- setClass(
  Class = "BFArray",
  contains = c("DelayedArray"),
  slots = c(seed = "BFArraySeed")
)
