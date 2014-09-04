function poly_smooth, data, width, DEGREE=degree, NLEFT=nl, NRIGHT=nr,  $
                                DERIV_ORDER=order, COEFFICIENTS=filter_coef
;+
; NAME:
;       POLY_SMOOTH  
;
; PURPOSE:
;       Apply a least-squares (Savitzky-Golay) polynomial smoothing filter
; EXPLANATION:
;       Reduce noise in 1-D data (e.g. time-series, spectrum) but retain 
;       dynamic range of variations in the data by applying a least squares 
;       smoothing polynomial filter,
;
;       Also called the Savitzky-Golay smoothing filter, cf. Numerical
;       Recipes (Press et al. 1992, Sec.14.8)
;
;       The low-pass filter coefficients are computed by effectively
;       least-squares fitting a polynomial in moving window,
;       centered on each data point, so the new value will be the
;       zero-th coefficient of the polynomial. Approximate first derivates
;       of the data can be computed by using first degree coefficient of
;       each polynomial, and so on. The filter coefficients for a specified
;       polynomial degree and window width are computed independent of any
;       data, and stored in a common block. The filter is then convolved
;       with the data array to result in smoothed data with reduced noise,
;       but retaining higher order variations (better than SMOOTH).
;
;       This procedure became partially obsolete in IDL V5.4 with the 
;       introduction of the SAVGOL function, which computes the smoothing
;       coefficients.
; CALLING SEQUENCE:
;
;       spectrum = poly_smooth( data, [ width, DEGREE = , NLEFT = , NRIGHT = 
;                                       DERIV_ORDER = ,COEFF = ]
;
; INPUTS:
;       data = 1-D array, such as a spectrum or time-series.
;
;       width = total number of data points to use in filter convolution,
;               (default = 5, using 2 past and 2 future data points),
;               must be larger than DEGREE of polynomials, and a guideline is to
;               make WIDTH between 1 and 2 times the FWHM of desired features.
;
; OPTIONAL INPUT KEYWORDS:
;
;       DEGREE = degree of polynomials to use in designing the filter
;               via least squares fits, (default DEGREE = 2)
;               The higher degrees will preserve sharper features.
;
;       NLEFT = # of past data points to use in filter convolution,
;               excluding current point, overrides width parameter,
;               so that width = NLEFT + NRIGHT + 1.  (default = NRIGHT)
;
;       NRIGHT = # of future data points to use (default = NLEFT).
;
;       DERIV_ORDER = order of derivative desired (default = 0, no derivative).
;
; OPTIONAL OUTPUT KEYWORD:
;
;       COEFFICIENTS = optional output of the filter coefficients applied,
;               but they are all stored in common block for reuse, anyway.
; RESULTS:
;       Function returns the data convolved with polynomial filter coefs.
;
; EXAMPLE:
;
;       Given a wavelength - flux spectrum (w,f), apply a 31 point quadratic
;       smoothing filter and plot
;
;       IDL> cgplot, w, poly_smooth(f,31) 
; COMMON BLOCKS:
;       common poly_smooth, degc, nlc, nrc, coefs, ordermax
;
; PROCEDURE:
;       As described in Numerical Recipes, 2nd edition sec.14.8, 
;       Savitsky-Golay filter.
;       Matrix of normal eqs. is formed by starting with small terms
;       and then adding progressively larger terms (powers).
;       The filter coefficients of up to derivative ordermax are stored
;       in common, until the specifications change, then recompute coefficients.
;       Coefficients are stored in convolution order, zero lag in the middle.
;
; MODIFICATION HISTORY:
;       Written, Frank Varosi NASA/GSFC 1993.
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Use /EDGE_TRUNCATE keyword to CONVOL  W. Landsman March 2006
;-
        compile_opt idl2
        On_error,2

        if N_params() LT 1 then begin
                print,'Syntax - smoothdata = ' + $ 
                        'poly_smooth( data , width, [ DEGREE = , NLEFT = '
        print,f='(35x,A)', 'NRIGHT = , DERIV_ORDER =, COEFFICIENT = ]'
        return, -1
        endif

  common poly_smooth, degc, nlc, nrc, coefs, ordermax

        if N_elements( degree ) NE 1 then degree = 2
        if N_elements( order ) NE 1 then order = 0
        order = ( order < (degree-1) ) > 0

        if N_elements( width ) EQ 1 then begin
                width = fix( width ) > 3
                if (N_elements(nr) NE 1) AND (N_elements(nl) NE 1) then begin
                        nl = width/2
                        nr = width - nl -1
                   endif
           endif

        if N_elements( nr ) NE 1 then begin
                if N_elements( nl ) EQ 1 then  nr = nl  else  nr = 2
           endif

        if N_elements( nl ) NE 1 then begin
                if N_elements( nr ) EQ 1 then  nl = nr  else  nl = 2
           endif

        if N_elements( coefs ) LE 1 then begin
                degc = 0
                nlc = 0
                nrc = 0
                ordermax = 3
           endif

        if (degree NE degc) OR (nl NE nlc) OR (nr NE nrc) OR $
                                                (order GT ordermax) then begin
                degree = degree > 2
                ordermax = ( ordermax < 3 ) > order
                nj = degree+1
                nl = nl > 0
                nr = nr > 0
                nrl = nr + nl + 1

                if (nrl LE degree) then begin
                        message,"# of points in filter must be > degree",/INFO
                        return, data
                   endif

                ATA = fltarr( nj, nj )
                ATA[0,0] = 1
                iaj = indgen( nj ) # replicate( 1, nj )
                iaj = iaj + transpose( iaj )
                m1_iaj = (-1)^iaj

                for k = 1, nr>nl do begin
                    k_iaj = float( k )^iaj
                    CASE 1 OF
                        ( k LE nr<nl ): ATA = ATA + ( k_iaj + k_iaj*m1_iaj )
                        ( k LE nr ):    ATA = ATA + k_iaj
                        ( k LE nl ):    ATA = ATA + k_iaj * m1_iaj
                     ENDCASE
                  endfor

                LUdc, ATA, LUindex, /COL

                Bmat = fltarr( nj, degree<(ordermax+1) )
                B = fltarr( nj )

                for m = 0, (degree-1)<ordermax do begin
                        B[*] = 0
                        B[m] = 1
                        Bmat[0,m] = LUsol(ATA, LUindex, B, /COL)
                  endfor

                kvec = [0]
                if (nl GT 0) then kvec = [ rotate( -indgen( nl )-1, 2 ), kvec ]
                if (nr GT 0) then kvec = [ kvec, indgen( nr )+1 ]
                Kmat = fltarr( nrl, nj )
                Kmat[*,0] = 1
                for m = 1,degree do Kmat[0,m] = Kmat[*,m-1] * kvec

                coefs = Kmat # Bmat
                degc = degree
                nlc = nl
                nrc = nr

                if (nr GT nl) then begin
                        sc = size( coefs )
                        coefs = [  fltarr( nr-nl, sc[2] ),  coefs ]
                  endif else if (nl GT nr) then begin
                        sc = size( coefs )
                        coefs = [ coefs,  fltarr( nl-nr, sc[2] )  ]
                   endif
           endif

        filter_coef = coefs[*,order]

return, convol( data, filter_coef, /EDGE_TRUNCATE )
end
