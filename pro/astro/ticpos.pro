pro ticpos,deglen,pixlen,ticsize,incr,units      ;Compute tic positions
;+
; NAME:
;       TICPOS
; PURPOSE:
;       Specify distance between tic marks for astronomical coordinate overlays
; EXPLANATION:
;       User inputs number an approximate distance
;       between tic marks, and the axis length in degrees.  TICPOS will return 
;       a distance between tic marks such that the separation is a round
;       multiple in arc seconds, arc minutes, or degrees
;
; CALLING SEQUENCE:
;       TICPOS, deglen, pixlen, ticsize, incr, units
;
; INPUTS:
;       deglen - length of axis in DEGREES
;       pixlen - length of axis in plotting units (pixels)
;       ticsize - distance between tic marks (pixels).  This value will be
;               adjusted by TICPOS such that the distance corresponds to
;               a round multiple in the astronomical coordinate.
;
; OUTPUTS:
;       ticsize - distance between tic marks (pixels), positive scalar 
;       incr    - incremental value for tic marks in round units given 
;               by the UNITS parameter
;       units - string giving units of ticsize, either 'ARC SECONDS',
;               'ARC MINUTES', or 'DEGREES'
;
; EXAMPLE:
;       Suppose a 512 x 512 image array corresponds to 0.2 x 0.2 degrees on
;       the sky.   A tic mark is desired in round angular units, approximately 
;       every 75 pixels.
;
;       IDL> ticsize = 75
;       IDL> TICPOS,0.2,512,ticsize,incr,units   
;
;       ==> ticsize = 85.333, incr = 2. units = 'Arc Minutes'
;
;       i.e. a good tic mark spacing is every 2 arc minutes, corresponding
;       to 85.333 pixels.
;
; REVISON HISTORY:
;       written by W. Landsman            November, 1988
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Don't use all capital letters  W. Landsman May 2003
;       Fix case where incr crosses degree/minute or minute/degree boundary
;               A. Mortier/W.Landsman April 2005
;-
  On_error,2

  minpix = deglen*60./pixlen            ;Arc minute per pixel
  incr = minpix*ticsize                 ;Arc minutes between tics

  if (incr LT 0 ) then sgn = -1 else sgn = 1
  incr = abs(incr)
  if ( incr GE 30 )  then units = 'Degrees' else $
  if ( incr LE 0.5 ) then units = 'Arc Seconds'  $
                     else units = 'Arc Minutes'
;                                        determine increment
  case 1 of 

    incr GE 120.0  : incr =  4.         ;degrees
    incr GE  60.0  : incr =  2.         ;degrees
    incr GE  30.0  : incr =  1.         ;degrees
    incr GT  15.0  : incr = 30.         ;minutes 
    incr GE  10.0  : incr = 15.         ;minutes  
    incr GE   5.0  : incr = 10.         ;minutes
    incr GE   2.0  : incr =  5.         ;minutes
    incr GE   1.0  : incr =  2.         ;minutes
    incr GT   0.5  : incr =  1.         ;minutes
    incr GE   0.25 : incr = 30.         ;seconds
    incr GE   0.16 : incr = 15.         ;seconds
    incr GE   0.08 : incr = 10.         ;seconds
    incr GE   0.04 : incr =  5.         ;seconds
    incr GE   0.02 : incr = 2.           ;seconds
    incr LT   0.02 : incr = 1.           ;seconds
  
  endcase                                         

  if ( units EQ 'Arc Seconds' ) then minpix = minpix*60. else $
  if ( units EQ 'Degrees' )  then minpix = minpix/60.

  ticsize= incr/abs(minpix)                ;determine ticsize
  incr = incr*sgn
 
  return 
  end
