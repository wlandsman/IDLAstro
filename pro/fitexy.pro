;+
; NAME:
;       FITEXY
; PURPOSE:
;       Best straight-line fit to data with errors in both coordinates
; EXPLANATION:
;       Linear Least-squares approximation in one-dimension (y = a + b*x),
;       when both x and y data have errors   Users might be interested in 
;       Michael Williams MPFITEXY routines which include a number of 
;       enhancements to FITEXY. 
;       ( https://github.com/williamsmj/mpfitexy )
;               
;
; CALLING EXAMPLE:
;       FITEXY, x, y, A, B, X_SIG= , Y_SIG= , [sigma_A_B, chi_sq, q, TOL=]
;
; INPUTS:
;       x = array of values for independent variable.
;       y = array of data values assumed to be linearly dependent on x.
;
; REQUIRED INPUT KEYWORDS:
;       X_SIGMA = scalar or array specifying the standard deviation of x data.
;       Y_SIGMA = scalar or array specifying the standard deviation of y data.
;
; OPTIONAL INPUT KEYWORD:
;       TOLERANCE = desired accuracy of minimum & zero location, default=1.e-3.
;
; OUTPUTS:
;       A_intercept = constant parameter result of linear fit,
;       B_slope = slope parameter, so that:
;                       ( A_intercept + B_slope * x ) approximates the y data.
; OPTIONAL OUTPUT:
;       sigma_A_B = two element array giving standard deviation of 
;                A_intercept and B_slope parameters, respectively.
;                The standard deviations are not meaningful if (i) the
;                fit is poor (see parameter q), or (ii) b is so large that
;                the data are consistent with a vertical (infinite b) line.
;                If the data are consistent with *all* values of b, then
;                sigma_A_B = [1e33,e33]  
;       chi_sq = resulting minimum Chi-Square of Linear fit, scalar
;       q - chi-sq probability, scalar (0-1) giving the probability that
;              a correct model would give a value equal or larger than the
;              observed chi squared.   A small value of q indicates a poor
;              fit, perhaps because the errors are underestimated.   As 
;              discussed by Tremaine et al. (2002, ApJ, 574, 740) an 
;              underestimate of the errors (e.g. due to an intrinsic dispersion)
;              can lead to a bias in the derived slope, and it may be worth
;              enlarging the error bars to get a reduced chi_sq ~ 1
;
; COMMON:
;       common fitexy, communicates the data for computation of chi-square.
;
; PROCEDURE CALLS:
;       CHISQ_FITEXY()            ;Included in this file
;       MINF_BRACKET, MINF_PARABOLIC, ZBRENT    ;In IDL Astronomy Library 
;       MOMENT(), CHISQR_PDF()     ;In standard IDL distribution
;
; PROCEDURE:
;       From "Numerical Recipes" column by Press and Teukolsky: 
;       in "Computer in Physics",  May, 1992 Vol.6 No.3
;       Also see the 2nd edition of the book "Numerical Recipes" by Press et al.
;
;       In order to avoid  problems with data sets where X and Y are of very 
;       different order of magnitude the data are normalized before the fitting
;       process is started.     The following normalization is used:
;       xx = (x - xm) / xs    and    sigx = x_sigma / xs    
;                             where xm = MEAN(x) and xs = STDDEV(x)
;       yy = (y - ym) / ys    and    sigy = y_sigma / ys    
;                             where ym = MEAN(y) and ys = STDDEV(y)
;
;
; MODIFICATION HISTORY:
;       Written, Frank Varosi NASA/GSFC  September 1992.
;       Now returns q rather than 1-q   W. Landsman  December 1992
;       Use CHISQR_PDF, MOMENT instead of STDEV,CHI_SQR1 W. Landsman April 1998
;       Fixed typo for initial guess of slope, this error was nearly
;             always insignificant          W. Landsman   March 2000
;       Normalize X,Y before calculation (from F. Holland) W. Landsman Nov 2006
;-
function chisq_fitexy, B_angle
;
; NAME:
;       chisq_fitexy
; PURPOSE:
;       Function minimized by fitexy  (computes chi-square of linear fit).
;       It is called by minimization procedures during execution of fitexy.
; CALLING SEQUENCE:
;               chisq = chisq_fitexy( B_angle )
; INPUTS:
;       B_angle = arc-tangent of B_slope of linear fit.
; OUTPUTS:
;       Result of function = chi_square - offs  (offs is in COMMON).
; COMMON:
;       common fitexy, communicates the data from pro fitexy.
; PROCEDURE:
;       From "Numerical Recipes" column: Computer in Physics Vol.6 No.3
; MODIFICATION HISTORY:
;       Written, Frank Varosi NASA/GSFC 1992.

  common fitexy, xx, yy, sigx, sigy, ww, Ai, offs

        B_slope = tan( B_angle )
        ww = 1/( ( (B_slope * sigx)^2 + sigy^2 ) > 1.e-30 )
        if N_elements( ww ) EQ 1 then sumw = ww * N_elements( xx ) $
                                 else sumw = total( ww )
        y_Bx = yy - B_slope * xx
        Ai = total( ww * y_Bx )/sumw

return, total( ww * (y_Bx - Ai)^2 ) - offs
end
;-------------------------------------------------------------------------------
pro fitexy, x, y, A_intercept, B_slope, sigma_A_B, chi_sq, q, TOLERANCE=Tol, $
                                        X_SIGMA=x_sigma, Y_SIGMA=y_sigma
  compile_opt idl2					
  common fitexy, xx, yy, sigx, sigy, ww, Ai, offs

  if N_params() LT 4 then begin
     print,'Syntax -  fitexy, x, y, A, B, X_SIG=sigx, Y_SIG=sigy,' 
     print,'                  [sigma_A_B, chi_sq, q, TOLERANCE = ]'
     return
  endif
  
; Normalize data before running fitexy

  xm = (MOMENT(x, SDEV = xs, /DOUBLE))[0]
  ym = (MOMENT(y, SDEV = ys, /DOUBLE))[0]
  xx = (x - xm) / xs
  yy = (y - ym) / ys
  sigx = x_sigma / xs
  sigy = y_sigma / ys
 
   
;Compute first guess for B_slope using standard 1-D Linear Least-squares fit,
; where the non-linear term involving errors in x are ignored.
; (note that Tx is a transform to reduce roundoff errors)

        ww = sigx^2 + sigy^2
        if N_elements( ww ) EQ 1 then sumw = ww * N_elements( xx ) $
                                 else sumw = total( ww )
        Sx = total( xx * ww )
        Tx = xx - Sx/sumw
        B = total( ww * yy * Tx ) / total( ww * Tx^2 )

;Find the minimum chi-sq while including the non-linear term (B * sigx)^2
; involving variance in x data (computed by function chisq_fitexy):
; using minf_bracket (=MNBRAK) and minf_parabolic (=BRENT)
        offs = 0
        ang = [ 0, atan( B ), 1.571 ]
        chi = fltarr( 3 )
        for j=0,2 do chi[j] = chisq_fitexy( ang[j] )    ;this is for later...
        if N_elements( Tol ) NE 1 then Tol=1.e-3
        a0 = ang[0]
        a1 = ang[1]
        minf_bracket, a0,a1,a2, c0,c1,c2, FUNC="chisq_fitexy"
        minf_parabolic, a0,a1,a2, Bang, chi_sq, FUNC="chisq_fitexy", TOL=Tol

        if N_params() EQ 7 then q = 1 - chisqr_pdf( chi_sq, N_elements(x) - 2 )
        A_intercept = Ai        ;computed in function chisq_fitexy
        ang = [a0,a1,a2,ang]
        chi = [c0,c1,c2,chi]

;Now compute the variances of estimated parameters,
; by finding roots of ( (chi_sq + 1) - chisq_fitexy ).
;Note: ww, Ai are computed in function chisq_fitexy.

        offs = chi_sq + 1
        wc = where( chi GT offs, nc )

        if (nc GT 0) then begin

                angw = [ang[wc]]
                d1 = abs( angw - Bang ) MOD !PI
                d2 = !PI - d1
                wa = where( angw LT Bang, na )

                if (na GT 0) then begin
                        d = d1[wa]
                        d1[wa] = d2[wa]
                        d2[wa] = d
                   endif

                Bmax = zbrent( Bang,Bang+max(d1),F="chisq_fitexy",T=Tol ) -Bang
                Amax = Ai - A_intercept
                Bmin = zbrent( Bang,Bang-min(d2),F="chisq_fitexy",T=Tol ) -Bang
                Amin = Ai - A_intercept

                if N_elements( ww ) EQ 1 then r2 = 2/( ww * N_elements( x ) ) $
                                         else r2 = 2/total( ww )

                sigma_A_B = [ Amin^2 + Amax^2 + r2 , Bmin^2 + Bmax^2 ]
                sig_A_B = sqrt( sigma_A_B/2 ) / ([1,cos(Bang)^2])

          endif 

;Finally, transform parameters back to orignal units.


        B_slope = tan( Bang ) *ys /xs
        A_intercept = A_intercept*ys - tan(Bang) * ys / xs *xm + ym
        if Nc GT 0 then sigma_A_B = [SQRT( (sig_A_B[0] * ys)^2 +  $
                    (sig_A_B[1] * ys / xs * xm)^2 ), sig_A_B[1] * ys / xs] $
                else sigma_A_B = [1.e33,1.e33]    

return
end
