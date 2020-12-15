pro imlist, image, xc, yc, DX=dx, DY = DY, WIDTH=width, TEXTOUT = textout, $
                           DESCRIP = descr,OFFSET = offset
;+
; NAME:
;      IMLIST        
; PURPOSE:
;      Display pixel values on an image surrounding a specified X,Y center.
; EXPLANATION:
;      IMLIST is similar to TVLIST but the center pixel is supplied directly by
;      the user, rather than being read off of the image display
;
; CALLING SEQUENCE:
;      IMLIST, Image, Xc, Yc, [ TEXTOUT = , DX = , DY = ,WIDTH = ,DESCRIP = ]
;
; INPUTS:
;      Image - Two-dimensional array containing the image 
;      Xc  -   X pixel value at which to center the display, integer scalar 
;      Yc -    Y pixel value at which to center the display, integer scalar 
;
; OPTIONAL INPUTS KEYWORDS:
;      TEXTOUT - Scalar number (1-7) or string which determines output device.
;               (see TEXTOPEN) The following dev/file is opened for output.
;
;               textout=1       TERMINAL using /more option
;               textout=2       TERMINAL without /more option
;               textout=3       <program>.prt
;               textout=4       laser.tmp
;               textout=5       user must open file
;               textout=7       same as 3 but text is appended to <program>.prt
;                               if file already exists
;               textout = filename (default extension of .prt)
;
;       DX     -Integer scalar giving the number of pixels inthe  X direction 
;               to be displayed.  If omitted then DX = 18 for byte images, and 
;               DX = 14 for integer images.  IMLIST will display REAL data 
;               with more significant figures if more room is available to 
;               print.  
;
;       DY    - Same as DX, but in Y direction.  If omitted, then DY = DX 
;       WIDTH - Integer scalar giving the character width of the output device.
;               Default is 80 characters.
;       DESCRIP =  Scalar string which will be written as a description over
;               the output pixel values.   If DESCRIP is not supplied, and the
;               output device specified by TEXTOUT is not a terminal, then the
;               user will be prompted for a description.
;       OFFSET - 2 element numeric vector giving an offset to apply to the 
;               display of the X,Y coordinates of the image (e.g. if the 
;               supplied image array is a subarray of a larger image).
; OUTPUTS:
;       None.
;
; PROCEDURE:
;       Corresponding region of image is then displayed at
;       the terminal.   If necessary, IMLIST will divide all pixel values
;       in a REAL*4 image by a (displayed) factor of 10 to make a pretty format.
;
; SYSTEM VARIABLES:
;       If the keyword TEXTOUT is not supplied, then the non-standard system
;       variable !TEXTOUT will be read.    (The procedure ASTROLIB is used
;       to add the non-standard system variable if not already present.)
;
; RESTRICTIONS:
;       IMLIST may not be able to correctly format all pixel values if the
;       dynamic range of the values near the center pixel is very large
;
; EXAMPLE:
;       Display the pixel values of an image array IM in the vicinity of 254,111
;
;       IDL> imlist, IM, 254, 111
;
; PROCEDURES USED
;       TEXTOPEN, F_FORMAT(), TEXTCLOSE
; REVISION HISTORY:
;       Written,    W. Landsman             June, 1991
;       Added DESCRIP keyword    W. Landsman      December, 1991
;       Treat LONG image as integer when possible, call TEXTOPEN with /STDOUT
;       keyword, W. Landsman   April, 1996
;       Use SYSTIME() instead of !STIME  August 1997
;       Recognize new integer types, added OFFSET keyword  W. Landsman Jan. 2000
;       Replace DATATYPE() with size(/TNAME)  W. Landsman Nov. 2001
;       Handle NAN values in output display W. Landsman June 2004
;       Use V6.0 notation  W. Landsman April 2011
;       Remove unnecessary checks if system variable defined W. Landsman May 2016
;-
 On_error,2                                   ;Return to caller
 compile_opt idl2

 if N_params() LT 3 then begin
    print,'Syntax - IMLIST, Image, Xc, Yc, [TEXTOUT= ,DX=, DY=, WIDTH= ,DESC= ]'
    print,'        Image - Any IDL numeric 2-d array'
    print,'        Xc, Yc - X,Y of center pixel of region to display'
    return
 endif

  defsysv,'!TEXTUNIT',exist=i
  if i EQ 0 then astrolib

 if N_elements( TEXTOUT ) EQ 0 then textout = !TEXTOUT      ;Use default
 if N_elements( OFFSET) NE 2 then offset = [0,0]

 if size( TEXTOUT,/TNAME ) NE 'STRING' then begin
        textout = textout > 2                  ;Don't use /MORE
        hardcopy =  (textout GE 3) && (textout NE 5) 
 endif else hardcopy = 1


 textopen, 'IMLIST', TEXTOUT = textout, /STDOUT            ;Open output device

 sz = size(image)
 if (sz[0] LT 2) || (sz[sz[0]+2] NE sz[1]*sz[2]) then $
                message,'Image array (first parameter) not 2-dimensional'

 type = sz[ sz[0] + 1 ]      ;Byte or Integer or Float image?

 if hardcopy then begin   ;Direct output to a disk file
        printf,!TEXTUNIT,'IMLIST: ' + strmid(systime(),4,20)
        if ~keyword_set( DESCR ) then begin
           descr = ''
           read,'Enter a brief description to be written to disk: ',descr
        endif
        printf,!TEXTUNIT,descr
        printf,!TEXTUNIT,' '
 endif                 

 xdim = sz[1] - 1 
 ydim = sz[2] - 1 

; Make sure supplied center pixel is actually within image

 if (xc LT 0) || (xc GT xdim) then $
        message,'ERROR - X pixel center must be between 0 and '+strtrim(xdim,2)
 if (yc LT 0) || (yc GT ydim) then $
        message,'ERROR - Y pixel center must be between 0 and '+strtrim(ydim,2)

 xim = round(xc)
 yim = round(yc)
 if ~keyword_set( WIDTH ) then width = 80

 case type of
 1: fmtsz = 4
 2: fmtsz = 6
12: fmtsz = 6
else: fmtsz = 5
endcase

 if ~keyword_set(DX) then dx = fix((width - 5)/fmtsz)
 if ~keyword_set(DY) then dy = dx

; Don't try to print outside the image
  xmax = (xim + dx/2) < xdim
  xmin = (xim - dx/2) > 0 
  ymax = (yim + dy/2) < ydim
  ymin = (yim - dy/2) > 0 

 dx = xmax - xmin + 1 & dy = ymax - ymin + 1
 if fmtsz EQ 5 then  fmtsz = ( width-4 ) / dx
 sfmt = strtrim( fmtsz,2 )
 cdx = string(dx,'(i2)')
 flt_to_int = 0               ;Convert floating point to integer?


; For Integer and Byte datatypes we already know the best output format
; For other datatypes the function F_FORMAT is used to get the best format
; If all values of a LONG image can be expressed with 5 characters 
; (-9999 < IM < 99999) then treat as an integer image.
REDO:
 case 1 of                                    ;Get proper print format

   type EQ 1:  fmt = '(i4,' + cdx + 'i' + sfmt + ')'           ;byte

   (type EQ 2):   fmt = '(i4,' + cdx + 'i' + sfmt + ')'        ;Integer
   (type EQ 12):  fmt = '(i4,1x,' + cdx + 'i' + sfmt + ')'     ;Unsigned Integer

   (type EQ 4) || (type EQ 3) || (type EQ 5) || (type GE 13):  begin      ;Long, Real or Double

      temp = image[ xmin:xmax,ymin:ymax ]
      minval = min( temp, MAX = maxval, /nan)
      if (type EQ 3) || (type GE 13) then  begin

                if (maxval LT 999.) && (minval GT -99.) then begin
                type = 1 & sfmt = '4'
                 goto, REDO
                endif
                if (maxval LT 9999.) && (minval GT -999.) then begin
                type = 12 & sfmt = '5'
                goto, REDO
                endif
                if (maxval LT 99999.) && (minval GT -9999.) then begin
                type = 2  & sfmt = '6'
                goto, REDO
                endif
      endif

      realfmt = F_FORMAT( minval, maxval, factor, fmtsz )
      if strmid(realfmt,0,1) EQ 'I' then flt_to_int = 1
      fmt = '(i4,1x,' + cdx + realfmt + ')'
      if factor NE 1 then $                                   
       printf,!TEXTUNIT,form='(/,A,E7.1,/)',' IMLIST: Scale Factor ',factor

      end

    else: message,'ERROR - Unrecognized data type'
 endcase 

; Compute and print x-indices above array

 index = indgen(dx) + xmin + offset[0]

 if type NE 1 then $
     printf,!TEXTUNIT,form='(A,'+ cdx + 'i' + sfmt + ')',' col ',index  $
 else printf,!TEXTUNIT,form='(A,'+ cdx + 'i' + sfmt + ')',' col',index

 printf,!TEXTUNIT,'$(A)',' row'
 for i = ymax,ymin,-1 do begin  ;list pixel values

        row = image[i*sz[1]+xmin:i*sz[1]+xmax]      ;from supplied image array
        if type EQ 1 then row = fix(row)
        if (type EQ 4) || (type EQ 3) || (type EQ 5) || (type GE 13) then $
                   row = row/factor
        if flt_to_int then row = round( row )
        printf, !TEXTUNIT, FORM = fmt, i + offset[1], row

 endfor
 
 textclose, TEXTOUT=textout
 
 return
 end
