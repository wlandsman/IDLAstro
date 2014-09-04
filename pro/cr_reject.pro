PRO cr_reject, input_cube, rd_noise_dn, dark_dn, gain, mult_noise, $
               combined_image, combined_noise, combined_npix, $
               MASK_CUBE=mask_cube, NOISE_CUBE=noise_cube, $
               NSIG=nsig, MEDIAN_LOOP=median_loop, MEAN_LOOP=mean_loop, $
               MINIMUM_LOOP=minimum_loop, INIT_MED=init_med, $
               INIT_MIN=init_min, INIT_MEAN=init_mean, EXPTIME=exptime,$
               BIAS=bias, VERBOSE=verbose, $
               TRACKING_SET=tracking_set, DILATION=dilation, DFACTOR=dfactor, $
               NOSKYADJUST=noskyadjust,NOCLEARMASK=noclearmask, $
               XMEDSKY=xmedsky, RESTORE_SKY=restore_sky, $
               SKYVALS=skyvals, NULL_VALUE=null_value, $
               INPUT_MASK=input_mask, WEIGHTING=weighting, SKYBOX=skybox
;+
; NAME:
;     CR_REJECT
;
; PURPOSE:
;     General, iterative cosmic ray rejection using two or more input images.
;
; EXPLANATION:
;     Uses a noise model input by the user, rather than
;     determining noise empirically from the images themselves.
;
;     The image returned has the combined exposure time of all the input
;     images, unless the bias flag is set, in which case the mean is
;     returned.  This image is computed by summation (or taking mean)
;     regardless of loop and initialization options (see below).
;
; CALLING SEQUENCE:
;     cr_reject, input_cube, rd_noise_dn, dark_dn, gain, mult_noise, $
;        combined_image, combined_npix, combined_noise
;
; MODIFIED ARGUMENT:
;     input_cube - Cube in which each plane is an input image.
;                  If the noise model is used (rd_noise_dn, dark_dn,
;                  gain), then input_cube must be in units of DN.
;                  If an input noise cube is supplied (rd_noise_dn
;                  <0), then the units of input_cube and noise_cube
;                  merely need to be consistent.  
;
;                  This array is used as a buffer and its contents 
;                  are not guaranteed on output (although for now, 
;                  weighting=0 with /restore_sky should give you back 
;                  your input unaltered).
;
; INPUT ARGUMENTS:
;     rd_noise_dn - Read noise per pixel.  Units are DN.
;                   If negative, then the user supplies an error cube
;                   via the keyword noise_cube.  In the latter case,
;                   mult_noise still applies, since it is basically a fudge.
;     dark_dn     - Dark rate in DN per pixel per s.  This can be a scalar,
;                   or it can be a dark image divided by the exposure
;                   time.
;     gain        - Electrons per DN.
;     mult_noise  - Coefficient for multiplicative noise term -- helps
;                   account for differing PSFs or subpixel image shifts.
;
; INPUT KEYWORDS:
;     exptime    - If the images have different exposure times, pass
;                  them in a vector.  You can leave this off for 
;                  frames with the same exposure time, but dark counts
;                  won't be treated correctly.
;     verbose    - If set, lots of output.
;     nsig       - Rejection limit in units of pixel-to-pixel noise
;                  (sigma) on each input image.  Can be specified as
;                  an array, in which case the dimension gives the
;                  maximum number of iterations to run.  (Default = 
;                  [8, 6, 4]
;     dilation   - With dfactor, provides functionality similar to the
;                  expansion of the CR with pfactor and radius in STSDAS 
;                  crrej.  Dilate gives the size of the border to be
;                  tested around each initially detected CR pixel.
;                  E.g., dilate=1 searches a 9 X 9 area centered on the
;                  original pixel.  If dfactor is set, the default is 1.
;     dfactor    - See dilation.  This parameter is equivalent to pfactor
;                  in STSDAS crrej.  The current threshold for rejection
;                  is multiplied by this factor when doing the search
;                  with the dilated mask.  If dilation is set, the default
;                  for this parameter is 0.5.
;     bias       - Set if combining biases (divides through by number
;                  of images at end, since exposure time is 0).
;     tracking_set - Subscripts of pixels to be followed through the 
;                    computation.
;     noskyadjust  - Sky not to be subtracted before rejection tests.  Default
;                  is to do the subtraction.
;     xmedsky    - Flag.  If set, the sky is computed as a 1-d array
;                  which is a column-by-column median.  This is intended
;                  for STIS slitless spectra.  If sky adjustment is
;                  disabled, this keyword has no effect.
;     input_mask - Mask cube input by the user.  Should be byte data
;                  because it's boolean.  1 means use the pixel,
;                  and 0 means reject the pixel - these rejections
;                  are in addition to those done by the CR rejection
;                  algorithm as such.
;
;     The following keywords control how the current guess at a CR-free
;     "check image" is recomputed on each iteration:
;
;     median_loop  - If set, the check image for each iteration is
;                    the pixel-by-pixel median. THE MEAN IS
;                    RETURNED in combined_image even if you set
;                    this option.  (Default is mean_loop.)
;     minimum_loop - If set, the check image for each iteration is
;                    the pixel-by-pixel minimum. THE MEAN IS
;                    RETURNED in combined_image even if you set
;                    this option.  (Default is mean_loop.)
;     mean_loop    - If set, the check image for each iteration is
;                    the pixel-by-pixel mean.  (Same as the default.)
;     noclearmask  - By default, the mask of CR flags is reset before
;                    every iteration, and a pixel that has been
;                    rejected has a chance to get back in the game
;                    if the average migrates toward its value.  If this
;                    keyword is set, then any rejected pixel stays 
;                    rejected in subsequent iterations.  Note that what 
;                    stsdas.hst_calib.wfpc.crrej does is the same
;                    as the default.  For this procedure, the default
;                    was NOT to clear the flags, until 20 Oct. 1997.
;     restore_sky  - Flag.  If set, the routine adds the sky back into
;                    input_cube before returning.  Works only if
;                    weighting=0.
;     null_value   - Value to be used for output pixels to which no
;                    input pixels contribute.  Default=0
;     weighting    - Selects weighting scheme in final image
;                    combination:
;                     0 (default) - Poissonian weighting - co-add
;                         detected DN from non-CR pixels.  (Pixel-by-
;                         pixel scaling up to total exposure time,
;                         for pixels in stack where some rejected.)
;                         Equivalent to exptime weighting of rates.
;                     1 or more - Sky and read noise weighting of rates.
;                         Computed as weighted average of DN rates,
;                         with total exp time multiplied back in
;                         afterward.
;
;                    In all cases, the image is returned as a sum in
;                    DN with the total exposure time of the image 
;                    stack, and with total sky added back in.
;
;     The following keywords allow the initial guess at a CR-free "check
;     image" to be of a different kind from the iterative guesses:
;
;     init_med  - If set, the initial check image is
;                 the pixel-by-pixel median.  (Not permitted if
;                 input_cube has fewer than 3 planes; default is minimum.)
;     init_mean - If set, the initial check image is
;                 the pixel-by-pixel mean.  (Default is minimum.)    
;     init_min  - If set, the initial check image is
;                 the pixel-by-pixel minimum.  (Same as the default.)    
;  
; OUTPUT ARGUMENTS::
;     combined_image - Mean image with CRs removed.
;     combined_npix  - Byte (or integer) image of same dimensions as
;                      combined_image, with each element containing
;                      the number of non-CR stacked pixels that
;                      went into the  result.
;     combined_noise - Noise in combined image according to noise model
;                      or supplied noise cube.
;
; OUTPUT KEYWORDS:
;     mask_cube      - CR masks for each input image.  1 means
;                      good pixel; 0 means CR pixel.
;     skyvals        - Sky value array.  For an image cube with N planes,
;                      this array is fltarr(N) if the sky is a scalar per
;                      image plane, and fltarr(XDIM, N), is the XMEDSKY
;                      is set.
;
; INPUT/OUTPUT KEYWORD:
;     noise_cube     - Estimated noise in each pixel of input_cube as
;                      returned (if rd_noise_dn ge 0), or input noise
;                      per pixel of image_cube (if rd_noise_dn lt 0).
;     skybox         - X0, X1, Y0, Y1 bounds of image section used
;                      to compute sky.  If supplied by user, this 
;                      region is used.  If not supplied, the
;                      image bounds are returned.  This parameter might
;                      be used (for instance) if the imaging area
;                      doesn't include the whole chip.
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  none
;
; METHOD: 
;     
;     COMPARISON WITH STSDAS
;
;     Cr_reject emulates the crrej routine in stsdas.hst_calib.wfpc.
;     The two routines have been verified to give identical results
;     (except for some pixels along the edge of the image) under the 
;     following conditions:
;
;          no sky adjustment
;          no dilation of CRs
;          initialization of trial image with minimum
;          taking mean for each trial image after first (no choice
;             in crrej)
;     
;     Dilation introduces a difference between crrej and this routine
;     around the very edge of the image, because the IDL mask
;     manipulation routines don't handle the edge the same way as crrej
;     does.  Away from the edge, crrej and cr_reject are the same with
;     respect to dilation.
;
;     The main difference between crrej and cr_reject is in the sky
;     computation.  Cr_reject does a DAOPHOT I sky computation on a 
;     subset of pixels grabbed from the image, whereas crrej searches
;     for a histogram mode.
;
;     REMARKS ON USAGE
;
;     The default is that the initial guess at a CR-free image is the
;     pixel-by-pixel minimum of all the input images.  The pixels
;     cut from each component image are the ones more than nsig(0) sigma
;     from this minimum image.  The next iteration is based on the
;     mean of the cleaned-up component images, and the cut is taken
;     at nsig(1) sigma.  The next iteration is also based on the mean with
;     the cut taken at nsig(2) sigma.
;
;     The user can specify an arbitrary sequence of sigma cuts, e.g.,
;     nsig=[6,2] or nsig=[10,9,8,7].  The user can also specify that
;     the initial guess is the median (/init_med) rather than the
;     minimum, or even the mean.  The iterated cleaned_up images after
;     the first guess can be computed as the mean or the median
;     (/mean_loop or /median_loop).  The minimum_loop option is also
;     specified, but this is a trivial case, and you wouldn't want
;     to use it except perhaps for testing.
;
;     The routine takes into account exposure time if you want it to, 
;     i.e., if the pieces of the CR-split aren't exactly the same.
;     For bias frames (exposure time 0), set /bias to return the mean
;     rather than the total of the cleaned-up component images.
;
;     The crrej pfactor and radius to propagate the detected CRs
;     outward from their initial locations have been implemented
;     in slightly different form using the IDL DILATE function.
;
;     It is possible to end up with output pixels to which no valid
;     input pixels contribute.  These end up with the value
;     NULL_VALUE, and the corresponding pixels of combined_npix are
;     also returned as 0.  This result can occur when the pixel is
;     very noisy across the whole image stack, i.e., if all the
;     values are, at any step of the process, far from the stack
;     average at that position even after rejecting the real
;     outliers.  Because  pixels are flagged symmetrically N sigma
;     above and below the  current combined image (see code), all
;     the pixels at a given  position can end up getting flagged.
;     (At least, that's how I think it happens.)
;
; MODIFICATION HISTORY:
;      5 Mar. 1997 - Written.  R. S. Hill
;     14 Mar. 1997 - Changed to masking approach to keep better
;                    statistics and return CR-affected pixels to user.
;                    Option to track subset of pixels added.
;                    Dilation of initially detected CRs added.
;                    Other small changes.  RSH
;     17 Mar. 1997 - Arglist and treatment of exposure times fiddled
;                    to mesh better with stis_cr.  RSH
;     25 Mar. 1997 - Fixed bug if dilation finds nothing.  RSH
;      4 Apr. 1997 - Changed name to cr_reject.  RSH
;     15 Apr. 1997 - Restyled with emacs, nothing else done.  RSH
;     18 Jun. 1997 - Input noise cube allowed.  RSH
;     19 Jun. 1997 - Multiplicative noise deleted from final error.  RSH
;     20 Jun. 1997 - Fixed error in using input noise cube.  RSH
;     12 July 1997 - Sky adjustment option.  RSH
;     27 Aug. 1997 - Dilation kernel made round, not square, and
;                    floating-point radius allowed.  RSH
;     16 Sep. 1997 - Clearmask added.  Intermediate as well as final
;                    mean is exptime weighted.  RSH
;     17 Sep. 1997 - Redundant zeroes around dilation kernel trimmed.  RSH
;      1 Oct. 1997 - Bugfix in preceding.  RSH
;     21 Oct. 1997 - Clearmask changed to noclearmask.  Bug in robust
;                    array division fixed (misplaced parens).  Sky as
;                    a function of X (option).  RSH
;     30 Jan. 1998 - Restore_sky keyword added.  RSH
;      5 Feb. 1998 - Quick help corrected and updated.  RSH
;      6 Feb. 1998 - Fixed bug in execution sequence for tracking_set 
;                    option.  RSH
;     18 Mar. 1998 - Eliminated confusing maxiter spec.  Added
;                    null_value keyword.  RSH
;     15 May  1998 - Input_mask keyword.  RSH
;     27 May  1998 - Initialization of minimum image corrected. NRC, RSH
;      9 June 1998 - Input mask cube processing corrected.  RSH
;     21 Sep. 1998 - Weighting keyword added.  RSH
;      7 Oct. 1998 - Fixed bug in input_mask processing (introduced
;                    in preceding update).  Input_mask passed to
;                    skyadj_cube.  RSH
;      5 Mar. 1999 - Force init_min for 2 planes.  RSH
;      1 Oct. 1999 - Make sure weighting=1 not given with noise cube.  RSH
;      1 Dec. 1999 - Corrections to doc; restore_sky needs weighting=0.  RSH
;     17 Mar. 2000 - SKYBOX added.  RSH
;-
on_error,0
IF n_params(0) LT 6 THEN BEGIN
    print,'CALLING SEQUENCE:  cr_reject, input_cube, rd_noise_dn, $'
    print,'   dark_dn, gain, mult_noise, combined_image, combined_noise, $'
    print,'   combined_npix'
    print,'KEYWORD PARAMETERS:  nsig, exptime, bias, verbose,'
    print,'   tracking_set, median_loop, mean_loop, minimum_loop, '
    print,'   init_med, init_mean, init_min,'
    print,'   mask_cube, noise_cube, dilation, dfactor, noclearmask, '
    print,'   noskyadjust, xmedsky, restore_sky, skyvals, null_value'
    print,'   input_mask, weighting, skybox'
    return
ENDIF

verbose = keyword_set(verbose)
xmed = keyword_set(xmedsky)

track = n_elements(tracking_set) GT 0

sz = size(input_cube)
IF sz[0] NE 3 THEN BEGIN
    print,'CR_REJECT:  Input cube must have 3 dimensions.'
    return
ENDIF

IF n_elements(input_mask) GT 0 THEN BEGIN
    szinpm = size(input_mask)
    wsz = where(szinpm[0:3] NE sz[0:3], cwsz)
    IF cwsz GT 0 THEN BEGIN
        print,'CR_REJECT:  INPUT_MASK must be same size as IMAGE_CUBE.'
        return
    ENDIF ELSE BEGIN
        IF verbose THEN print,'CR_REJECT:  Using INPUT_MASK.'
    ENDELSE
    use_input_mask = 1b
ENDIF ELSE BEGIN
    use_input_mask = 0b
ENDELSE    

xdim = sz[1]
ydim = sz[2]
nimg = sz[3]
npix = xdim*ydim

usemedian = keyword_set(median_loop)
usemean   = keyword_set(mean_loop)
usemin    = keyword_set(minimum_loop)
IF (usemean + usemedian + usemin) GT 1  THEN BEGIN
    print,'CR_REJECT:  Specify only one of MEDIAN_LOOP, MEAN_LOOP' $
      + ', or MINIMUM_LOOP'
    return
ENDIF
IF (usemean + usemedian + usemin) EQ 0  THEN BEGIN
    usemean = 1
ENDIF

inimed  = keyword_set(init_med)
inimean = keyword_set(init_mean)
inimin  = keyword_set(init_min)
IF (inimean + inimed + inimin) GT 1  THEN BEGIN
    print,'CR_REJECT:  Specify only one of INIT_MED, INIT_MEAN,' $
      + ' or INIT_MIN.'
    return
ENDIF
IF (inimean + inimed + inimin) EQ 0  THEN BEGIN
    inimin = 1
ENDIF
IF nimg LT 3 AND inimed THEN BEGIN
    inimed = 0
    inimin = 1
    IF verbose THEN BEGIN
        print,'CR_REJECT:  INIT_MED only permitted for 3 or more ' $
            + 'images.'
        print,'            Forcing INIT_MIN.'
    ENDIF
ENDIF

;
;  Accumulation mode for bad pixels.
;
IF keyword_set(noclearmask) THEN BEGIN
    clearmask = 0b
    IF verbose THEN print,'CR_REJECT:  CR flags accumulate strictly.'
ENDIF ELSE BEGIN
    clearmask = 1b
    IF verbose THEN print,'CR_REJECT:  CR flags cleared between iterations.'
ENDELSE 
;
;  Default iterations.
;
IF (n_elements(nsig) LT 1) THEN BEGIN
    nsig = [8, 6, 4]
ENDIF
sig_limit = nsig
maxiter = n_elements(nsig)
IF n_elements(null_value) LT 1 THEN null_value=0
IF verbose THEN BEGIN
    print,'CR_REJECT: Iteration spec:  '
    print,'           nsig = ',nsig
    print,'           maxiter = ',maxiter
    print,'           null_value = ',null_value
ENDIF
;
IF n_elements(exptime) NE 0 THEN BEGIN
    IF n_elements(exptime) NE nimg THEN BEGIN
        print,'CR_REJECT:  EXPTIME must have one element per input image.'
        return
    ENDIF
    zexp = 0b
    FOR i=0,nimg-1 DO zexp = zexp OR (exptime[i] LE 0.0)
    IF zexp THEN BEGIN
        save_expt = exptime
        exptime = make_array(nimg, value=1.0)
        IF verbose THEN print, $
          'CR_REJECT:  All exposure times <= 0.'
    ENDIF
ENDIF ELSE BEGIN
    zexp = 1b
    save_expt = make_array(nimg, value=0.0)
    exptime = make_array(nimg, value=1.0)
ENDELSE
etot = total(exptime)

IF n_elements(weighting) GT 0 THEN BEGIN
    wgt = weighting
    wgt = round(wgt)
    IF wgt ne 0 and wgt ne 1 THEN BEGIN
        print, 'CR_REJECT:  Weighting must be 0 or 1'
        print,'             Executing return'
        return
    ENDIF
ENDIF ELSE BEGIN
    wgt = 0
ENDELSE

IF verbose THEN BEGIN
    print,'CR_REJECT:  gain = ',gain
    IF n_elements(dark_dn) EQ 1 THEN BEGIN
        print,'           dark rate = ',dark_dn
    ENDIF ELSE BEGIN
        print,'           dark image supplied '
    ENDELSE
    print,'           read noise = ',rd_noise_dn
    print,'           multiplicative noise coefficient = ',mult_noise
    print,'           number of images = ',nimg
    print,'           exposure times: '
    print,exptime
    print,'           total exposure time = ',etot
    CASE wgt OF
        0:  print,'           flux to be co-added'
        1:  print,'           weighting of rate by sky and read noise'
    ENDCASE
ENDIF

;
;  Process dilation specs
;
IF keyword_set(dilation) OR keyword_set(dfactor) THEN BEGIN
    do_dilation = 1b
    IF n_elements(dilation) LT 1 THEN dilation = 1
    IF n_elements(dfactor) LT 1 THEN dfactor = 0.5
    IF (dilation LE 0) OR (dfactor LE 0) THEN BEGIN
        print,'CR_REJECT:  Dilation specs not valid: '
        print,'           dilation = ',dilation
        print,'           dfactor  = ',dfactor
        return
    ENDIF
    kdim = 1 + 2*floor(dilation+1.e-4)
    kernel = make_array(kdim, kdim, value=1b)
    half_kern = fix(kdim/2)
    wkz = where(shift(dist(kdim),half_kern,half_kern) $
        GT (dilation+0.0001), ckz)
    IF ckz GT 0 THEN kernel[wkz] = 0b
    IF verbose THEN BEGIN
        print,'CR_REJECT:  Dilation will be done.  Specs:'
        print,'           dilation = ',dilation
        print,'           dfactor  = ',dfactor
        print,'           kernel = '
        print,kernel
    ENDIF      
ENDIF ELSE BEGIN
    do_dilation = 0b
    IF verbose THEN print,'CR_REJECT:  Mask dilation will not be done.' 
ENDELSE


IF verbose THEN print,'CR_REJECT:  Initializing noise and mask cube.'
IF rd_noise_dn GE 0 THEN BEGIN
    IF verbose THEN print,'CR_REJECT:  Noise cube computed.'
    supplied = 0b
    noise_cube = 0.0*input_cube
    FOR i=0, nimg-1 DO BEGIN
        noise_cube[0,0,i] = sqrt((rd_noise_dn^2 $
                                  + ((input_cube[*,*,i] $
                                      + dark_dn*exptime[i])>0)/gain) > 0.0)
    ENDFOR
ENDIF ELSE BEGIN
    IF verbose THEN print,'CR_REJECT:  Noise cube supplied.'
    supplied = 1b
    IF wgt EQ 1 THEN BEGIN
        print, 'CR_REJECT:  WEIGHTING=1 incompatible with supplying ', $
            'noise cube.'
        print, '            Executing return.'
        return
    ENDIF
ENDELSE
;
;  Mask flags CR with zeroes
;
mask_cube = make_array(xdim, ydim, nimg, value=1B)
IF nimg LE 255 THEN ivalue=byte(nimg) ELSE ivalue=fix(nimg)
combined_npix = make_array(xdim, ydim, value=ivalue)

IF keyword_set(noskyadjust) THEN BEGIN
    skyvals = fltarr(nimg)
    totsky = 0
ENDIF ELSE BEGIN
    IF verbose THEN print,'CR_REJECT:  Sky adjustment being made.'
    skyadj_cube, input_cube, skyvals, totsky, $
      verbose=verbose, xmedsky=xmed, input_mask=input_mask, $
      region=skybox
ENDELSE

IF verbose THEN print,'CR_REJECT:  Scaling by exposure time.'

FOR i=0,nimg-1 DO BEGIN
    input_cube[0,0,i] = input_cube[*,*,i]/exptime[i]
    noise_cube[0,0,i] = noise_cube[*,*,i]/exptime[i]
ENDFOR

;
;  Initialization of main loop.
;
ncut_tot = lonarr(nimg)
cr_subs  = lonarr(npix)
IF inimin OR usemin THEN flagval = max(input_cube)+1
IF inimed THEN BEGIN
    IF verbose THEN print,'CR_REJECT:  Initializing with median.'
    IF use_input_mask THEN BEGIN
        medarr,input_cube,combined_image,input_mask
    ENDIF ELSE BEGIN
        medarr,input_cube,combined_image
    ENDELSE
ENDIF ELSE IF inimean THEN BEGIN
    IF verbose THEN print,'CR_REJECT:  Initializing with mean.'
    IF use_input_mask THEN BEGIN
        tm = total(input_mask,3) > 1e-6
        combined_image = total(input_cube*input_mask,3)/tm
        wz = where(temporary(tm) le 0.001, cwz)
        IF cwz GT 0 THEN $
            combined_image[temporary(wz)] = 0
    ENDIF ELSE BEGIN
        combined_image = total(input_cube,3)/nimg
    ENDELSE
ENDIF ELSE IF inimin THEN BEGIN
    IF verbose THEN print,'CR_REJECT:  Initializing with minimum.'
    IF use_input_mask THEN BEGIN
        combined_image = make_array(xdim,ydim,value=flagval)
        FOR i=0, nimg-1 DO BEGIN
            indx = where(input_mask[*,*,i] gt 0, cindx)
            IF cindx GT 0 THEN $
                combined_image[indx] = $
                    (combined_image < input_cube[*,*,i])[indx]
        ENDFOR
        wf = where(combined_image EQ flagval, cf)
        IF cf GT 0 THEN combined_image[wf] = null_value
    ENDIF ELSE BEGIN
        combined_image = input_cube[*,*,0]
        FOR i=1, nimg-1 DO BEGIN
            combined_image = (combined_image < input_cube[*,*,i])
        ENDFOR
    ENDELSE
ENDIF ELSE BEGIN
    print,'CR_REJECT:  Logic error in program initializing check image.'
    return
ENDELSE
;
; ---------------- MAIN CR REJECTION LOOP. ------------------
;
iter=0
main_loop:
iter=iter+1

IF clearmask THEN mask_cube[*]=1b

IF track THEN BEGIN
    print,'CR_REJECT:  Tracking.  Iter = ',strtrim(iter,2)
    print,'   Combined_image:  '
    print,combined_image[tracking_set]
    FOR i = 0, nimg-1 DO BEGIN
        print,'   Image ', strtrim(i,2), ':'
        print,(input_cube[*,*,i])[tracking_set]
        print,'   Noise ', strtrim(i,2), ':'
        print,(noise_cube[*,*,i])[tracking_set]
        print,'   Mask  ', strtrim(i,2), ':'
        print,(mask_cube[*,*,i])[tracking_set]
    ENDFOR
ENDIF
IF verbose THEN BEGIN
    print,'CR_REJECT:  Beginning iteration number ',strtrim(iter,2)
    print,'           Sigma limit = ',sig_limit[iter-1]
ENDIF

FOR i=0, nimg-1 DO BEGIN

    skyarray = fltarr(xdim, ydim)
    IF xmed THEN BEGIN  
        FOR jl = 0,ydim-1 DO skyarray[0,jl] = skyvals[*,i]
    ENDIF ELSE BEGIN 
        skyarray[*] = skyvals[i]
    ENDELSE 
    model_image = $
      (temporary(skyarray) + (combined_image + dark_dn)*exptime[i])>0
    
    IF supplied THEN BEGIN
        current_var = noise_cube[*,*,i]^2 $
          + ((mult_noise*temporary(model_image))/exptime[i])^2
    ENDIF ELSE BEGIN
        current_var = (rd_noise_dn^2 + model_image/gain $
                       + (mult_noise*temporary(model_image))^2) $
                       / (exptime[i]^2)
    ENDELSE 

    IF track THEN BEGIN
        print,'CR_REJECT:  Tracking.  Iter = ',strtrim(iter,2), $
          ' Image = ',strtrim(i,2)
        print,'           Current_var:  '
        print,current_var[tracking_set]
    ENDIF

    testnoise = sig_limit[iter-1] * sqrt(temporary(current_var))
 
    IF track THEN BEGIN
        print,'           Testnoise:  '
        print,testnoise[tracking_set]
    ENDIF
;
;  Absolute value used so that if you remove too much, at least you
;  won't introduce a new bias.
;
    cr_subs[0] = $
      where(abs(input_cube[*,*,i] - combined_image) $
            GT testnoise, count)
    IF count GT 0 THEN BEGIN
        mask_cube[i*npix + cr_subs[0:count-1]] $
          = replicate(0b,count)
    ENDIF
    IF verbose THEN print,'CR_REJECT:  ',strtrim(count,2), $
      ' pixels flagged in image ',strtrim(i,2)
    
;
;  Dilation of mask
;
    count2 = 0
    IF do_dilation THEN BEGIN
        tempw = where(dilate(1b-mask_cube[*,*,i], kernel),dct)
        IF dct GT 0 THEN BEGIN
            ic1 = input_cube[npix*i + tempw]
            tn1 = testnoise[tempw]
            cmi = combined_image[tempw]
            tewsub = where(abs(temporary(ic1) $
                               - temporary(cmi)) $
                           GT (dfactor*temporary(tn1)), count2)
            cr_subs[0] = (temporary(tempw))[temporary(tewsub)>0]
            IF count2 GT 0 THEN BEGIN
                mask_cube[i*npix + cr_subs[0:count2-1]] $
                  = replicate(0b,count2)
            ENDIF
        ENDIF
        IF verbose THEN print,'CR_REJECT:  Mask dilation performed.  ', $
          strtrim(count2,2), ' pixels flagged in image ',strtrim(i,2)
    ENDIF
ENDFOR

FOR i=0, nimg-1 DO BEGIN
    cr_subs[0] = where(1b-mask_cube[*,*,i],count)
;   IF verbose THEN print,'CR_REJECT:  ',strtrim(count,2), $
;     ' accumulated flags in image ',strtrim(i,2)
;    IF count GT 0 THEN BEGIN
;        input_cube(i*npix + cr_subs(0:count-1)) $
;          = combined_image(cr_subs(0:count-1))
;        noise_cube(i*npix + cr_subs(0:count-1)) $
;          = sqrt(current_var(cr_subs(0:count-1)))
;    ENDIF
ENDFOR

IF use_input_mask THEN BEGIN
    combined_npix[0,0] = total((mask_cube AND input_mask),3)
ENDIF ELSE BEGIN
    combined_npix[0,0] = total(mask_cube,3)
ENDELSE
;
;  Loop termination condition.
;
IF (iter GE maxiter) THEN GOTO,end_main_loop

IF usemedian THEN BEGIN
    IF verbose THEN print,'CR_REJECT:  Taking median.'
    IF use_input_mask THEN BEGIN
        medarr,input_cube,combined_image,mask_cube AND input_mask
    ENDIF ELSE BEGIN
        medarr,input_cube,combined_image,mask_cube
    ENDELSE
ENDIF ELSE IF usemean THEN BEGIN
    IF verbose THEN print,'CR_REJECT:  Taking mean.'
    IF use_input_mask THEN BEGIN
        maskprod = input_mask[*,*,0] AND mask_cube[*,*,0]
        combined_image = input_cube[*,*,0]*maskprod*exptime[0]
        combined_expt  = temporary(maskprod)*exptime[0]
        IF nimg GT 1 THEN BEGIN
            FOR i=1,nimg-1 DO BEGIN
                maskprod = input_mask[*,*,i] AND mask_cube[*,*,i]
                combined_image = combined_image $
                  + input_cube[*,*,i]*maskprod*exptime[i]
                combined_expt = combined_expt $
                  + temporary(maskprod)*exptime[i]
            ENDFOR
        ENDIF
        wexpt0 = where(combined_expt LE 0,cexpt0)
        combined_image = combined_image / (combined_expt>1e-6)
        IF cexpt0 GT 0 THEN combined_image[wexpt0] = 0
    ENDIF ELSE BEGIN
        combined_image = input_cube[*,*,0]*mask_cube[*,*,0]*exptime[0]
        combined_expt  = mask_cube[*,*,0]*exptime[0]
        IF nimg GT 1 THEN BEGIN
            FOR i=1,nimg-1 DO BEGIN
                combined_image = combined_image $
                  + input_cube[*,*,i]*mask_cube[*,*,i]*exptime[i]
                combined_expt = combined_expt $
                  + mask_cube[*,*,i]*exptime[i]
            ENDFOR
        ENDIF
        wexpt0 = where(combined_expt LE 0,cexpt0)
        combined_image = combined_image / (combined_expt>1e-6)
        IF cexpt0 GT 0 THEN combined_image[wexpt0] = 0
    ENDELSE
ENDIF ELSE IF usemin THEN BEGIN
    IF verbose THEN print,'CR_REJECT:  Taking minimum.'
    IF use_input_mask THEN BEGIN
        combined_image[*] = flagval
        FOR i=0, nimg-1 DO BEGIN
            indx = where((input_mask[*,*,i] $ 
                         AND mask_cube[*,*,i]) gt 0, cindx)
            IF cindx GT 0 THEN $
                combined_image[indx] = $
                    (combined_image < input_cube[*,*,i])[indx]
        ENDFOR
        wf = where(combined_image EQ flagval, cf)
        IF cf GT 0 THEN combined_image[wf] = null_value
    ENDIF ELSE BEGIN
        combined_image = input_cube[*,*,0]
        FOR i=1, nimg-1 DO BEGIN
            combined_image = (combined_image < input_cube[*,*,i])
        ENDFOR
    ENDELSE

    IF use_input_mask THEN BEGIn
        combined_image = input_cube[*,*,0]*input_mask[*,*,0]
        FOR i=1, nimg-1 DO BEGIN
            combined_image = (combined_image < input_cube[*,*,i] $
                             *input_mask[*,*,i])
        ENDFOR
    ENDIF ELSE BEGIN
        combined_image = input_cube[*,*,0]
        FOR i=1, nimg-1 DO BEGIN
            combined_image = (combined_image < input_cube[*,*,i])
        ENDFOR
    ENDELSE
ENDIF ELSE BEGIN
    print,'CR_REJECT:  Logic error in program recomputing check image.'
    return
ENDELSE

GOTO,main_loop
END_main_loop:
;
;  End of CR rejection loop.
;
IF verbose THEN BEGIN
    FOR i=0,nimg-1 DO BEGIN
        wdummy = where(1b-mask_cube[*,*,i],count) 
        ncut_tot[i] = count
    ENDFOR
    print,'CR_REJECT:  Total pixels changed:  '
    print,ncut_tot
ENDIF

IF track THEN BEGIN
    print,'CR_REJECT:  Tracking.  After loop exit.'
    print,'   Combined_image:  '
    print,combined_image[tracking_set]
;    print,'   Current_var:  '
;    print,current_var[tracking_set]
    FOR i = 0, nimg-1 DO BEGIN
        print,'   Image ', strtrim(i,2), ':'
        print,(input_cube[*,*,i])[tracking_set]
        print,'   Noise ', strtrim(i,2), ':'
        print,(noise_cube[*,*,i])[tracking_set]
        print,'   Mask  ', strtrim(i,2), ':'
        print,(mask_cube[*,*,i])[tracking_set]
    ENDFOR
ENDIF  

;
;   Compute weights according to scheme chosen
;
xrepl = make_array(dim=xdim,value=1)
yrepl = make_array(dim=ydim,value=1)

IF wgt EQ 0 THEN BEGIN
    wgts = xrepl # exptime
ENDIF ELSE BEGIN
    IF xmed THEN skytmp = skyvals>1e-6 ELSE skytmp = xrepl # (skyvals>1e-6)
    exp2tmp = xrepl # (exptime^2)
    sky_rate_var = temporary(skytmp)/gain/exp2tmp
    ron_rate_var = rd_noise_dn^2/temporary(exp2tmp)
    wgts = 1.0/(temporary(sky_rate_var) + temporary(ron_rate_var))
ENDELSE

;
;   Do the final co-addition
;    
wgt_coeff = fltarr(xdim, ydim)
FOR i=0,nimg-1 DO BEGIN
    plane_wgts = wgts[*,i] # yrepl
    input_cube[0,0,i] = input_cube[*,*,i]*plane_wgts
    noise_cube[0,0,i] = noise_cube[*,*,i]*plane_wgts
    IF use_input_mask THEN BEGIN
        mcim = (mask_cube[*,*,i] AND input_mask[*,*,i])
    ENDIF ELSE BEGIN
        mcim = mask_cube[*,*,i]
    ENDELSE
    wgt_coeff[0,0] = wgt_coeff + temporary(mcim) * temporary(plane_wgts)
ENDFOR
wh0 = where(combined_npix EQ 0,c0)
wgt_coeff = etot/(wgt_coeff > 1.0e-8)
IF c0 GT 0 THEN wgt_coeff[wh0] = 0.0

IF verbose THEN BEGIN
    IF c0 GT 0 THEN $
      print,'CR_REJECT:  ',strtrim(c0,2),' pixels rejected on all inputs.'
ENDIF

IF use_input_mask THEN BEGIN
    IF xmed THEN BEGIN
        combined_image = wgt_coeff * total(input_cube $
                                 * (mask_cube AND input_mask),3) $
                         + totsky#yrepl
    ENDIF ELSE BEGIN
        combined_image = wgt_coeff * total(input_cube $
                                 * (mask_cube AND input_mask),3) $
                         + totsky
    ENDELSE
    combined_noise =  wgt_coeff * sqrt(total((noise_cube $
                              * (mask_cube AND input_mask))^2,3))
ENDIF ELSE BEGIN
    IF xmed THEN BEGIN
        combined_image = wgt_coeff * total(input_cube*mask_cube,3) $
                                 + totsky#yrepl
    ENDIF ELSE BEGIN
        combined_image = wgt_coeff * total(input_cube*mask_cube,3) $
                                 + totsky
    ENDELSE
    combined_noise = wgt_coeff * sqrt(total((noise_cube*mask_cube)^2,3))
ENDELSE

IF keyword_set(bias) THEN BEGIN
    print,'CR_REJECT:  Bias flag set -- returning mean instead of total.'
    combined_image = combined_image/nimg
    combined_noise = combined_noise/nimg
ENDIF

IF c0 GT 0 THEN combined_image[wh0] = null_value

IF keyword_set(restore_sky) THEN BEGIN
    IF wgt EQ 0 THEN BEGIN
        IF verbose THEN print,'CR_REJECT:  Adding sky back into data cube'
        IF xmed THEN BEGIN
            FOR i=0,nimg-1 DO BEGIN
                FOR j=0, ydim-1 DO input_cube[0,j,i] = input_cube[*,j,i] $
                                                       + skyvals[*,i]
            ENDFOR
        ENDIF ELSE BEGIN
            FOR i=0,nimg-1 DO $
                input_cube[0,0,i] = input_cube[*,*,i] + skyvals[i]
        ENDELSE
    ENDIF ELSE BEGIN
        print, 'CR_REJECT:  /RESTORE_SKY ignored because weighting spec ' $
            + 'not zero.'
    ENDELSE
ENDIF

IF zexp THEN exptime = save_expt

return
END
