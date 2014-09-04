FUNCTION MEDSMOOTH,ARRAY,WINDOW
;+
; NAME:
;       MEDSMOOTH
;
; PURPOSE:
;       Median smoothing of a vector, including points near its ends.
;
; CALLING SEQUENCE:
;       SMOOTHED = MEDSMOOTH( VECTOR, WINDOW_WIDTH )
;
; INPUTS:
;       VECTOR  = The (1-d numeric) vector to be smoothed
;       WINDOW = Odd integer giving the full width of the window over which 
;               the median is determined for each point.     (If WINDOW is
;               specified as an even number, then the effect is the same as
;               using WINDOW+1)   
;
; OUTPUT:
;       Function returns the smoothed vector
;
; PROCEDURE:
;       Each point is replaced by the median of the nearest WINDOW of points.
;       The width of the window shrinks towards the ends of the vector, so that
;       only the first and last points are not filtered. These points are 
;       replaced by forecasting from smoothed interior points.
;
; EXAMPLE:
;       Create a vector with isolated high points near its ends
;       IDL> a = randomn(seed,40) & a[1] = 10  & a[38] = 10
;       Now do median smoothing with a 7 point window 
;       IDL> b = medsmooth(a,7)
;       Note that, unlike MEDIAN(), that MEDSMOOTH will remove the isolated
;       high points near the ends.
; REVISION HISTORY:
;       Written, H. Freudenreich, STX, 12/89
;       H.Freudenreich, 8/90: took care of end-points by shrinking window.
;       Speed up using vector median when possible  W. Landsman February 2002
;-

 LEND = N_ELEMENTS(ARRAY)-1
 IF (LEND+1) LT WINDOW THEN BEGIN
   message,/CON, $
         'ERROR - Size of smoothing window must be smaller than array size'
   RETURN,ARRAY
 ENDIF

 OFFSET = FIX(WINDOW/2)

 smoothed = median(array, window )

; Fix the ends:
 NUMLOOP = (WINDOW-1)/2 - 1
 IF NUMLOOP GT 0 THEN BEGIN
   FOR J=1,NUMLOOP DO BEGIN 

     LEN = 2*J+1
     SMOOTHED[J] = MEDIAN(ARRAY[0:LEN-1])
     SMOOTHED[LEND-J] =  MEDIAN(ARRAY[LEND-LEN+1:LEND]) 

   ENDFOR
ENDIF

; Now replace the very last and first points:
 Y0 = 3.*ARRAY[0]-2.*ARRAY[1]         ; Predicted value of point -1
 SMOOTHED[0] = MEDIAN([Y0,ARRAY[0],ARRAY[1]])
 Y0 = 3.*ARRAY[LEND]-2.*ARRAY[LEND-1] ; Predicted value of point LEND+1
 SMOOTHED[LEND] = MEDIAN([Y0,ARRAY[LEND],ARRAY[LEND-1]])
               
 RETURN,SMOOTHED
 END
