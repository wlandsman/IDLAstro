pro ticlabels, minval, numtics, incr, ticlabs, RA=ra, DELTA=delta, FONT=font
;+
; NAME:
;	TICLABELS
; PURPOSE:
;	Create tic labels for labeling astronomical images.
; EXPLANATION: 
;	Used to display images with right ascension or declination
;	axes.  This routine creates labels for already determined tic
;	marks (every other tic mark by default)
;
; CALLING SEQUENCE:
;	TICLABELS, minval, numtics, incr, ticlabs, [ RA = ,DELTA = ]
;
; INPUTS:
;	minval  - minimum value for labels (degrees)
;	numtics - number of tic marks
;	incr    - increment in minutes for labels
;
; OUTPUTS:
;	ticlabs - array of charater string labels
;
; OPTIONAL INPUT KEYWORDS:
;	/RA - if this keyword is set then the grid axis is assumed to be
;		a Right Ascension.   Otherwise a declination axis is assumed
;	DELTA - Scalar specifying spacing of labels.   The default is 
;		DELTA = 2 which means that a label is made for every other tic
;		mark.  Set DELTA=1 to create a label for every tic mark.
;       FONT - scalar font graphics keyword (-1,0 or 1) for text
;
; PROCEDURES USED:
;	RADEC
;
; RESTRICTIONS:
;	Invalid for wide field (> 2 degree) images since it assumes that a 
;	fixed interval in Y (or X) corresponds to a fixed interval in Dec 
;	(or RA)
;
; REVISON HISTORY:
;	written by B. Pfarr, 4/15/87
;	Added DELTA keywrd for compatibility with IMCONTOUR W. Landsman Nov 1991
;	Added nicer hms and dms symbols when using native PS fonts Deutsch 11/92
;	Added Patch for bug in IDL <2.4.0 as explained in NOTES E. Deutsch 11/92
;	Fix when crossing 0 dec or 24h RA
;	Fix DELTA keyword so that it behaves according to the documentation
;			W. Landsman  Hughes STX,  Nov 95  
;       Allow sub arcsecond formatting  W. Landsman   May 2000
;       Better formatting for non-unity DELTA values  W. Landsman July 2004
;       Allow FONT keyword to be passed.  T. Robishaw Apr. 2006
;       Write 0h rather than 24h  W. L. August 2008
;       Fix problem when tic values is exactly 0 degrees   Mar 2012
;       Only modulo 24 when /RA is set  WL.  October 2012
;-
 On_error,2
 compile_opt idl2
;                               convert min to hours, minutes, secs
  if N_params() LT 4 then begin

     print,'Syntax - ticlabels, minval, numtics, incr, ticlabs, ' + $
                     '[ /RA  ,DELTA = ]'
     return

  endif

  if N_elements(FONT) eq 0 then font = !p.font

  ticlabs = replicate(' ',numtics )

  if minval LT 0 then begin 
	neg = -1 & sgn = '-'
  endif else begin
	neg = 1  & sgn = '' 
  endelse
   firstval = minval
  if ~keyword_set( DELTA ) then delta = 2

   
  if keyword_set( RA ) then begin                  ;Define RA tic symbols

       radec, firstval, 0, minh, minm, mins, dum1, dum2, dum3 
       sd = '!Ah!N' & sm = '!Am!N'  & ss = '!As!N' 

       if (!d.name eq 'PS') and (font eq 0) then begin    ;Postscript fonts?
         sd ='!Uh!N' & sm='!Um!N' & ss='!Us!N' 
         endif

 endif else begin 

       radec, 0, firstval, dum1, dum2, dum3, minh, minm, mins
       minm = abs(minm)
       mins = abs(mins)
       sd = "!Ao!N" & sm = "'" &  ss = "''"

       if (!d.name eq 'PS') and (font eq 0) then begin

         RtEF = '!X'
         sd = '!9' + string(176b) + RtEF 
         sm = '!9' + string(162b) + RtEF
         ss = '!9' + string(178b) + RtEF
       endif
 
 endelse

     inc1 = incr*60.0d
      inc = incr*60.0d*delta           ;increment in arc seconds
      if abs(inc1) GE 1.0 then begin 
             mins = round(mins)
            sfmt = '(i2.2)' 
      endif else $  
         if abs(inc1) GT 0.1 then  sfmt = '(f4.1)' else sfmt = '(f5.2)'
      if abs(inc) GE 1.0 then  inc = round(inc)
      

     while (mins GE 60) do begin
            mins = mins - 60
            minm++
     endwhile
     
     if (minm ge 60) then begin
             minm = minm - 60
             minh = minh + neg
     endif
 

 if (abs(mins) GT 1) || (abs(incr) LT 1.0/DELTA)  then begin      ;Seconds

    ticlabs[0] = sgn + string( abs(minh), '(i2.2)') + sd + ' ' + $
            string(minm,'(i2.2)') + sm + ' ' + string( mins, sfmt) + ss  
  
      for i = delta,numtics-1, delta do begin
     
         mins = mins + neg*inc
         if ( ( mins GE 60) || (mins LE 0) ) then begin

            while ( mins GE 60 ) do begin
                mins = mins - 60
                minm++
            endwhile

            while ( mins LT 0 ) do begin
                mins = mins + 60
                minm--
             endwhile
    
         if (minm ge 60) then begin
             minm = minm - 60
             minh = minh + neg
             ticlabs[i]= sgn + string(abs(minh),'(i2.2)') + sd + ' ' + $ 
                         string(minm,'(i2.2)') + sm
 
        endif else if (minm LE 0) then begin
	     
	     if minh EQ 0 then begin             ;Cross zero Dec or RA?
		if keyword_set(RA) then begin
			minh = 23
			minm = minm + 60
		endif else begin
			 minm = -minm
	                 neg = -neg
		         if neg EQ 1 then sgn = '' else sgn = '-'
		endelse
	     endif else begin
	             minm = minm + 60
	             minh = minh - neg
             endelse

	     ticlabs[i]= sgn + string(abs(minh),'(i2.2)') + sd + ' ' + $ 
              string((minm),'(i2)') + sm + ' ' +string(mins,sfmt) + ss


        endif else ticlabs[i] = string( minm, '(i2.2)' ) + sm + ' '+ $
                         string( mins, sfmt) + ss

         endif else ticlabs[i] = string( mins, sfmt ) + ss

      endfor

 endif else $
    if (abs(minm) gt 1) || (abs(incr) LT 60.0/DELTA) then begin ;MINUTES

      inc = fix(incr*DELTA)
      ticlabs[0] = sgn + string(abs(minh),'(i2.2)')+ sd+ ' ' + $
		string(minm,'(i2.2)') + sm
      for i = delta,numtics-1, delta do begin
         minm = minm + neg*inc

         if (minm ge 60) then begin
             minm = minm - 60
             minh = minh + neg
	     if keyword_set(RA) then begin
	        while minh LT 0 do minh = minh + 24
	        while minh GE 24 do minh = minh - 24
             endif  
             ticlabs[i]= sgn + string(abs(minh),'(i2.2)') + sd + ' ' + $
			string(minm,'(i2.2)') +sm

         endif else if (minm LT 0) then begin

	     if minh EQ 0 then begin             ;Cross zero Dec or RA?
		if keyword_set(RA) then begin
			minh = 23
			minm = minm + 60
		endif else begin
			 minm = -minm
	                 neg = -neg
		         if neg EQ 1 then sgn = '' else sgn = '-'
		endelse
	     endif else begin
	             minm = minm + 60
	             minh = minh - neg
             endelse
	     ticlabs[i]= sgn + string(abs(minh),'(i2.2)') + sd + ' ' + $
			string((minm),'(i2.2)') + sm
      endif else ticlabs[i] = string(minm,'(i2.2)')
      endfor
 endif else begin                        ;Hours/Degrees

      inc = fix(DELTA*incr/60.0)
      ticlabs[0] = strtrim(minh,2) + sd 
      for i = delta,numtics-1, delta do begin
          minh = minh + inc
	  if keyword_set(RA) then begin
	  
		while minh LT 0 do minh = minh + 24
		while minh GE 24 do minh = minh - 24
          endif
          ticlabs[i] = strtrim( minh,2) + sd
      endfor      

 endelse

 return    
 end
