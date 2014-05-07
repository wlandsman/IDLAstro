                                                            June 1998

This directory includes a few procedures for image processing or or analysis.

The procedures MAX_ENTROPY and MAX_LIKELIHOOD were written by Frank Varosi
to provide simple deconvolution methods.    Additional procedures are available
in pub/contrib/varosi/vlibm/deconv for monitoring the deconvolution iterations.
Also see the LaTeX file in pub/contrib/varosi/deconv.tex

-------
-------
BOXAVE() - Boxave an image, always using at least REAL*4 arithmetic
CONVOLVE() - Convolve an image with a PSF using the product of Fourier Transforms
CORREL_IMAGES() - Correlation of two images.   Called by CORREL_OPTIMIZE
CORREL_OPTIMIZE - Compute the optimal pixel offset of one image relative
         to another by maximizing the correlation function.
CORRMAT_ANALYZE - Analyze the correlation function made by CORREL_IMAGE
CR_REJECT - General iterative cosmic ray rejection for 2 or more images
DIST_CIRCLE - Create a mask array useful for circular aperture photometry.
DIST_ELLIPSE - Create a mask array useful for elliptical aperture photometry.
FILTER_IMAGE() - Like MEDIAN or SMOOTH but handles edges & allows iteration
FREBIN() - Shrink or expand an image by an arbitrary amount while conserving flux
IMLIST - Display image pixel values around a specified center
MAX_ENTROPY - Deconvolution by Maximum Entropy, given a PSF
MAX_LIKELIHOOD - Deconvolution by maximum likelihood, given a PSF
MEDARR - Median filter across a set of images (e.g. for cosmic ray removal)
POSITIVITY() - Map an image uniquely and smoothly into all positive values
PSF_GAUSSIAN() - Create a 1-d, 2-d, or 3-d Gaussian with specified FWHM, center
RINTER() = Cubic interpolation at a set of reference points
SIGMA_FILTER() - Replace pixels deviant by more than a specified sigma from
    its neighbors.  
----------------------------------------------------------------------------

