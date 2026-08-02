# ImageArray 1.1.3

## New features

* Novel delayed (or lazy) transformations for image pyramids are introduced. 
  Available transformations are: Translation, scaling, rotation, and affine 
  (#45).
* New `extent` function returns the physical space that the image occupies.
* `ImageArray` function now accepts arguments `axes` and `scales` that are
  default axes ordering and list of scaling vectors for each pyramid layers.
  
# ImageArray 1.1.2

## New features

* Images with any combination of XYZCT dimensions are can now be read.
  See OME webpage for more information:
  https://docs.openmicroscopy.org/ome-model/6.2.2/ome-tiff/specification.html#dimensionorder