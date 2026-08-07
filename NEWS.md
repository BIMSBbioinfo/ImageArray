# ImageArray 1.1.6

## New features

* `ImageArray` function now accepts Bioformats compatible image pyramid
  formats beyond ome.tiff and qptiff. See https://bio-formats.readthedocs.io/en/latest/supported-formats.html 
  for more information.
  
# ImageArray 1.1.5

## New features

* Built-in `create_zarr` and `create_zarr_group` functions are now replaced
  by the `Rarr::write_zarr_group` as of Rarr v2.1.27.
  
## Bug fixes

* Fixing internal `read_image` method for reading magick bitmap image class.
  
# ImageArray 1.1.4

## New features

* `createImageArray` function is now deprecated and replaced by `ImageArray`
  constructor. 
* `ImageArray` function now accepts arguments `axes` and `scales` that are
  default axes ordering and list of scaling vectors for each pyramid layers.

# ImageArray 1.1.3

## New features

* Novel delayed (or lazy) transformations for image pyramids are introduced. 
  Available transformations are: Translation, scaling, rotation, and affine 
  (#45).
* New `extent` function returns the physical space that the image occupies.
  
# ImageArray 1.1.2

## New features

* Images with any combination of XYZCT dimensions are can now be read.
  See OME webpage for more information:
  https://docs.openmicroscopy.org/ome-model/6.2.2/ome-tiff/specification.html#dimensionorder