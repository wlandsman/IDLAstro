pro make_2d,x,y,xx,yy
;+
; NAME:
;       MAKE_2D
; PURPOSE:
;       Change from 1-d indexing to 2-d indexing
; EXPLANATION:
;       Convert an N element X vector, and an M element Y vector, into
;       N x M arrays giving all possible combination of X and Y pairs.
;       Useful for obtaining the X and Y positions of each element of
;       a regular grid.
;
; CALLING SEQUENCE:
;       MAKE_2D, X, Y, [ XX, YY ]
;
; INPUTS:
;       X - N element vector of X positions
;       Y - M element vector of Y positions
;
; OUTPUTS:
;       XX - N x M element array giving the X position at each pixel
;       YY - N x M element array giving the Y position of each pixel
;               If only 2 parameters are supplied then X and Y will be
;               updated to contain the output arrays
;
; EXAMPLE:
;       To obtain the X and Y position of each element of a 30 x 15 array
;
;       IDL> x = indgen(30)  &  y = indgen(15)     
;       IDL> make_2d, x, y 
; REVISION HISTORY:
;       Written,    Wayne Landsman    ST Systems Co.    May, 1988
;       Added /NOZERO keyword       W. Landsman         Mar, 1991
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Improved speed          P. Broos      July 2000
;-          
 On_error,2
 if N_params() LT 2 then begin
     print,'Syntax - make_2d, x, y, [xx, yy]'
     print,'         x,y - Input X,Y vectors'
     print,'         xx,yy - Output arrays specifying X and Y indices'
     return
 endif

 ny = N_elements(y)
 nx = N_elements(x)

 xx = rebin(reform(x, nx,  1,/OVERWRITE), nx, ny, /SAMPLE)
 yy = rebin(reform(y,  1, ny,/OVERWRITE), nx, ny, /SAMPLE)

 if N_params() LT 3 then begin  ;Update X and Y vectors
     x = temporary(xx)
     y = temporary(yy)
 endif     

 return
 end
