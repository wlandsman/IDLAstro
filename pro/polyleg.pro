function polyleg,x,coeff
;+
; NAME:
;       POLYLEG
;
; PURPOSE:
;       Evaluate a Legendre polynomial with specified coefficients.
; EXPLANATION:
;       Meant to be used analogously to the POLY function in the IDL User's
;       Library distribution.
;
; CALLING SEQUENCE:
;       Result = POLYLEG( X, C )        
;
; INPUTS:
;       X - input variable, scalar or vector    
;       C - vector of Legendre polynomial coefficients. 
; OUTPUTS:
;       POLYLEG returns a result equal to:
;               C[0] + C[1]*P_1(x) + C[2]*P_2(x) + ...
;
;       where P_j(x) is the jth Legendre polynomial.   The output will have
;       the same dimensions as the input X variable.
;
; EXAMPLE:
;       If x = [0.5, 1.0] and C = [2.4, 1.3, 2.5] then
;       print, polyleg(x, c)    ====> [2.7375, 6.20]
;
;       The result can be checked using the first 3 Legendre polynomial terms
;       C[0] + C[1]*x + C[2]*(0.5*(3*x^2-1))
; METHOD:
;       Uses the recurrence relation of Legendre polynomials
;               (n+1)*P_n+1(x) = (2n+1)*x*P_n(x) - n*P_n-1(x)
;       evaluated with the Clenshaw recurrence formula, see Numerical Recipes
;       by Press et al. (1992), Section 5.5
;
; REVISION HISTORY:
;       Written W. Landsman   Hughes STX Co.        April, 1995    
;       Fixed for double precision  W. Landsman     May, 1997
;       Converted to IDL V5.0   W. Landsman   September 1997
;-
 On_error,2
 
 if N_params() LT 2 then begin
        print,'Syntax - result = POLYLEG( X, Coeff)'
        return, -1
 endif

 N= N_elements(coeff) -1
 M = N_elements(x)

 case N of 
 0: return, replicate( coeff, M)
 1: return, x* coeff[1] + coeff[0]
 else:
 endcase

; If X is double then compute in double; otherwise compute in real

 if size(x,/TNAME) EQ 'DOUBLE'  then begin      
        y = dblarr( M, N+2)
        jj = dindgen(N) + 2.0d
 endif else begin
        y = fltarr( M, N+2 )
        jj = findgen(N) + 2.
 endelse

 beta1 =  -jj / (jj+1)
 for j = N,1,-1 do begin

        alpha = (2*j + 1.)*x/float(j + 1.) 
        y[0,j-1] = alpha*y[*,j] + beta1[j-1]*y[*,j+1] + coeff[j]
 endfor

 return, -0.5*y[*,1] + x*y[*,0] + coeff[0]
 end
