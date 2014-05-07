pro tics,radec_min,radec_max,numx,ticsize,incr,RA=ra
;+
; NAME:
;       TICS
; PURPOSE:
;       Compute a nice increment between tic marks for astronomical images.
; EXPLANATION:       
;       For use in labelling a displayed image with right ascension
;       or declination axes.  An approximate distance between tic 
;       marks is input, and a new value is computed such that the 
;       distance between tic marks is in simple increments of the 
;       tic label values.
;
; CALLING SEQUENCE:
;       tics, radec_min, radec_max, numx, ticsize, incr, [ /RA ]
;
; INPUTS:
;       radec_min - minimum axis value (degrees)
;       radec_max - maximum axis value (degrees)
;       numx  - number of pixels in x direction
;
; INPUT/OUTPUT  
;       ticsize - distance between tic marks (pixels)
;
; OUTPUTS:
;       incr    - incremental value for tic labels (in minutes of 
;               time for R.A., minutes of arc for dec.)
;
; REVISON HISTORY:
;       written by B. Pfarr, 4/14/87
;       Added some more tick precision (i.e. 1 & 2 seconds in case:) EWD May92
;       Added sub arcsecond tick precision   W. Landsman   May 2000
;       Plate scale off by 1 pixel  W. Landsman July 2004
;-
  On_error,2

  numtics = numx/ticsize                   ;initial number of tics

;     Convert total distance to arc minutes for dec. or to
;     minutes of time for r.a.

  if keyword_set(RA) then mul = 4.0 else mul = 60.
  mins = abs(radec_min-radec_max)*mul       ;total distance in minutes
  rapix = (numx-1)/mins                        ;pixels per minute
  incr = mins/numtics                      ;minutes per tic

;                                        determine increment
  case 1 of 
    incr GE 120.0  : incr = 480.0       ; 4 hours
    incr GE  60.0  : incr = 120.0       ; 2 hours
    incr GE  30.0  : incr =  60.0       ; 1 hour
    incr GE  15.0  : incr =  30.0       ; 30 minutes 
    incr GE  10.0  : incr =  15.0       ; 15 minutes
    incr GE   5.0  : incr =  10.0       ; 10 minutes
    incr GE   2.0  : incr =   5.0       ;  5 minutes
    incr GE   1.0  : incr =   2.0       ;  2 minutes
    incr GE   0.5  : incr =   1.0       ;  1 minute
    incr GE   0.25 : incr =   0.5       ; 30 seconds
    incr GE   10/60.0d  : incr =   0.25      ; 15 seconds
    incr GE   5/60.0d   : incr =   10/60.0d  ; 10 seconds
    incr GE   2/60.0d   : incr =   5/60.0d   ;  5 seconds
    incr GE   1/60.0d   : incr =   2/60.0d   ;  2 seconds
    incr GE   0.5/60.0d : incr =   1./60.0d  ;  1 seconds
    incr GE   0.2/60.0d : incr = 0.5/60.0d   ;  0.5 seconds
    incr GE   0.1/60.0d  : incr = 0.2/60.0d    ;  0.2 seconds
    incr GE   0.05/60.0d : incr = 0.1/60.0d    ;  0.1 seconds
    incr GE   0.02/60.0d : incr = 0.05/60.0d   ;  0.05 seconds
    incr GE   0.01/60.0d : incr = 0.02/60.0d   ;  0.02 seconds
    incr GE   0          : incr = 0.01/60.0d   ;  0.01 seconds
  endcase

   ticsize = rapix*incr                 ;determine ticsize
   if ( radec_min GT radec_max ) then incr = -incr 

   return 
  end
