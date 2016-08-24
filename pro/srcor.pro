PRO srcor,x1in,y1in,x2in,y2in,dcr,ind1,ind2,option=option,magnitude=magnitude,$
   spherical=spherical,silent=silent,count = count
;+
; NAME:
;       SRCOR
; PURPOSE:
;       Correlate the source positions found on two lists.
;
; EXPLANATION:
;       Source matching is done by finding sources within a specified radius.
;       If you have position errors available and wish to match by significance
;       level, then try match_xy.pro in the TARA library 
;      (http://www.astro.psu.edu/xray/docs/TARA/)
;
; CALLING SEQUENCE:
;       srcor,x1in,ylin,x2in,y2in,dcr,ind1,ind2,
;                         [MAGNITUDE=,SPHERICAL=,COUNT=,/SILENT]
; INPUTS:
;       x1in,y1in - First set of x and y coordinates.  The program
;                   marches through this list element by element,
;                   looking in list 2 for the closest match.  So, the program
;                   will run faster if this is the shorter of the two lists.
;                   Unless you use the option or magnitude keyword, there is
;                   nothing to guarantee unique matches.  
;       x2in,y2in - Second set of x and y coordinates.  This list is
;                   searched in its entirety every time one element of list 1
;                   is processed.
;       dcr - Critical radius outside which correlations are rejected;
;             but see 'option' below.
; OPTIONAL KEYWORD INPUT:
;       option - Changes behavior of program and description of output
;                lists slightly, as follows: 
;       OPTION=0 or left out
;             Same as older versions of SRCOR.  The closest match from list2
;             is found for each element of list 1, but if the distance is
;             greater than DCR, the match is thrown out.  Thus the index
;             of that element within list 1 will not appear in the IND1 output
;             array.
;       OPTION=1
;             Forces the output mapping to be one-to-one.  OPTION=0 results,
;             in general, in a many-to-one mapping from list 1 to list 2.
;             Under OPTION=1, a further processing step is performed to
;             keep only the minimum-distance match, whenever an entry from
;             list 1 appears more than once in the initial mapping.
;       OPTION=2
;             Same as OPTION=1, except the critical distance parameter DCR
;             is ignored.  I.e., the closest object is retrieved from list 2
;             for each object in list 1 WITHOUT a critical-radius criterion,
;             then the clean-up of duplicates is done as under OPTION=1.
;       magnitude
;             An array of stellar magnitudes corresponding to x1in and y1in.  
;             If this is supplied, then the brightest star from list 1
;             within the selected distance of the star in list 2 is taken.
;             The option keyword is ignored in this case.
;       spherical
;             If SPHERICAL=1, it is assumed that the input arrays are in
;             celestial coordinates (RA and Dec), with x1in and x2in in
;             decimal hours and y1in and y2in in decimal degrees.  If
;             SPHERICAL=2 then it is assumed that the input arrays are in
;             longitude and latitude with x1in,x2in,y1in,y2in in decimal
;             degrees.  In both cases, the critial radius dcr is in
;             *arcseconds*.  Calculations of spherical distances are made
;             with the gcirc program.
; OUTPUTS:
;       ind1 - index of matched stars in first list, set to -1 if no matches
;              found
;       ind2 - index of matched stars in second list
; OPTIONAL OUTPUT KEYWORD:
;       Count - integer giving number of matches returned
; PROCEDURES USED:
;       GCIRC, REMOVE
; REVISON HISTORY:
;       Adapted from UIT procedure  J.Wm.Parker, SwRI 29 July 1997
;       Improve speed for spherical searches, added /SILENT keyword  
;                               W. Landsman  Mar 2009
;       Avoid error when no matches found with /SPHERICAL  O. Trottier June 2009
;       Added output Count keyword     W.L   June 2009
;       Adjust right ascension for cosine angle W.L. December 2009
;       Return as soon as no matches found W.L.  December 2009
;       Use some V6.0 notation  W.L.   February 2011
;       Fix problem when /Spherical and Option =2 set, and sources separated
;          by more han 180 degrees.   W.L.  March 2011
;       
;-
;
 ON_Error,2   ; Return if error (incl. non-info message)
 compile_opt idl2
;;;
;   If not enough parameters, then print out the syntax.
;
IF N_params() lt 7 THEN BEGIN
  print,'SRCOR calling sequence: '
  print,'srcor,x1in,y1in,x2in,y2in,dcr,ind1,ind2 [,option={0, 1, or 2}] $'
  print,'      [,magnitude=mag_list_1, COUNT=count, spherical={1 or 2}, /SILENT]'
  RETURN
ENDIF
 count = 0

;;;
;   Keywords.
;
IF ~keyword_set(option) THEN option=0
IF (option lt 0) || (option gt 2) THEN MESSAGE,'Invalid option code.'

SphereFlag = keyword_set(Spherical)

;;;
;   Store the input variables into internal arrays that we can manipulate and
; modify.
;
x1 = x1in
y1 = y1in
x2 = x2in
y2 = y2in 

;;;
;   If the Spherical keyword is set, then convert the input values (degrees
; and maybe hours) into radians, so GCIRC doesn't have to make this calculation
; each time it is called in the FOR loop.  Also convert the critical radius
; (which is in arcsec, so convert by 3600.) to radians
;
if SphereFlag then begin
   dcr2 = dcr
   XScale = Spherical EQ 1 ? 15.0 : 1.0
   d2r  = !DPI/180.0d0
   x1 = x1 * (XScale * d2r)
   y1 = y1 * d2r
   x2 = x2 * (XScale * d2r)
   y2 = y2 * d2r
   cosy2 = sin(y2)
   dcr2 = dcr2 * (d2r / 3600.)
   radcr2 = dcr2/cos(y2)        ;Adjust RA for declination
endif else dcr2=dcr^2


;;;
;   Set up some other variables.
;
 n1 = N_elements(x1)  
 n2 = N_elements(x2) 
 if ~keyword_set(silent) then begin 
      message,/info,'Option code = '+strtrim(option,2)
      message,/info,strtrim(n1,2)+' sources in list 1'
       message,/info,strtrim(n2,2)+' sources in list 2'
  endif

;;;
;   The main loop.  Step through each index of list 1, look for matches in 2.
;
  nmch = 0L
 ind1 = lonarr(n1)-1 & ind2 = ind1
   
   if SphereFlag then begin         
      if option EQ 2 then begin      ;Closest source, no critical distance
;For speed we find the maximum value of cos(d) where d is the arc distance
;This avoids having to calculate the arc cosine.    Test modified Mar 2011       
         cosy2 = cos(y2)
          siny2 = sin(y2)
     FOR i=0L,n1-1 DO BEGIN
         d2  =  siny2*sin(y1[i]) + cosy2*cos(y1[i])*cos(x1[i]-x2)
         dmch = max(d2,m)                 ;Uncommented 29-May-2009 	 
	 ind1[nmch] = i
         ind2[nmch] = m
         nmch++
      ENDFOR
      
      endif else begin               ;Closest source within critical distance
        
;For speed we first find sources within a square of the size of the critical
;distance.    Exact distances are then computed for sources within the square.      
     FOR i=0L,n1-1 DO BEGIN
           xx = x1[i] & yy = y1[i]

        g = where(( x2 GE (xx-radcr2)) and (x2 LE (xx+radcr2)) and $
	(y2 GE (yy-dcr2)) and  (y2 LE (yy + dcr2)), Ng)

        if Ng GT 0 then begin 
          gcirc,0,x2[g],y2[g],xx,yy,d2
          dmch = min(d2,mg)
          if dmch LE dcr2 then begin 
	      ind1[nmch] = i
	      ind2[nmch] = g[mg]
	      nmch++
       endif
       endif 
       ENDFOR
       endelse
    endif else begin 
    FOR i=0L,n1-1 DO BEGIN

       d2=(x1[i]-x2)^2+(y1[i]-y2)^2
       dmch=min(d2,m)
         IF (option eq 2) || (dmch le dcr2) THEN BEGIN
      ind1[nmch] = i
      ind2[nmch] = m
      nmch++
   ENDIF 
   ENDFOR  
   endelse

if ~keyword_set(silent) then message,/info,strtrim(nmch,2)+' matches found.'

count = nmch
if nmch GT 0 then begin 
   ind1 = ind1[0:nmch-1]
   ind2 = ind2[0:nmch-1]
endif else begin 
   ind1 = -1 & ind2 = -1
   return
endelse   
;;;
;   Modify the matches depending on input options.
;
use_mag = (n_elements(magnitude) ge 1)
IF (option eq 0) && (~use_mag) THEN RETURN
if ~keyword_set(silent) then begin
IF use_mag THEN BEGIN
   message,/info,'Cleaning up output list using magnitudes.'
ENDIF ELSE BEGIN
   
   IF option eq 1 then message,/info,'Cleaning up output list (option = 1).'
   IF option eq 2 then message,/info,'Cleaning up output list (option = 2).'
ENDELSE
endif

FOR i=0L,max(ind2) DO BEGIN
   csave = n_elements(ind2)
   ww = where(ind2 eq i,count) ; All but one of the list in WW must
                               ; eventually be removed.
   IF count gt 1 THEN BEGIN
      IF use_mag THEN BEGIN
         dummy = min(magnitude[ind1[ww]],m)
      ENDIF ELSE BEGIN
         xx=x2[i] & yy=y2[i]
         if SphereFlag then gcirc,0,xx,yy,x1[ind1[ww]],y1[ind1[ww]],d2 else $
                            d2=(xx-x1[ind1[ww]])^2+(yy-y1[ind1[ww]])^2
         IF n_elements(d2) ne count THEN MESSAGE,'Logic error 1'
         dummy = min(d2,m)
      ENDELSE
      remove,m,ww              ; Delete the minimum element
                               ; from the deletion list itself.

      remove,ww,ind1,ind2      ; Now delete the deletion list from
                               ; the original index arrays.
      IF n_elements(ind2) ne (csave-count+1) THEN MESSAGE,'Logic error 2'
      IF n_elements(ind1) ne (csave-count+1) THEN MESSAGE,'Logic error 3'
      IF n_elements(ind2) ne n_elements(ind1) THEN MESSAGE,'Logic error 4'
   ENDIF
ENDFOR

 count = N_elements(ind1)
 if ~keyword_set(silent) then $
  message,/info,strtrim(n_elements(ind1),2)+' final matches found'

;
RETURN
end
