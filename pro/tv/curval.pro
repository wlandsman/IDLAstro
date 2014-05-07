pro curval, hd, im, OFFSET = offset, ZOOM = zoom, Filename=Filename, ALT = alt
;+
; NAME:
;       CURVAL
; PURPOSE:   
;       Cursor controlled display of image intensities and astronomical coords
; EXPLANATION
;       CURVAL displays different information depending whether the user 
;       supplied an image array, and/or a FITS header array
;
;       Note that in the usual truecolor mode, the byte intensity returned by 
;       CURVAL does not correspond to the byte scaled image value but rather 
;       returns the maximum value in each color gun.
; CALLING SEQUENCE(S):
;       curval          ;Display x,y and byte intensity (inten)
;       
;       curval, im   ;Display x,y,inten, and also pixel value (from image array)
;       
;       curval, hdr, [ im, OFFSET= , ZOOM=, FILENAME=, ALT=]        
;
; OPTIONAL INPUTS:
;       Hdr  = FITS Header array
;       Im  = Array containing values that are displayed.  Any type.
;
; OPTIONAL KEYWORD INPUTS:
;      ALT - single character 'A' through 'Z' or ' ' specifying an alternate
;            astrometry system present in the FITS header.    The default is
;            to use the primary astrometry or ALT = ' '.   If /ALT is set,
;            then this is equivalent to ALT = 'A'.   See Section 3.3 of
;            Greisen & Calabretta (2002, A&A, 395, 1061) for information about
;            alternate astrometry keywords.
;      OFFSET - 2 element vector giving the location of the image pixel (0,0) 
;               on the window display.   OFFSET can be positive (e.g if the 
;               image is centered in a larger window) or negative (e.g. if the
;               only the central region of an image much larger than the window
;               is being displayed. 
;               Default value is [0,0], or no offset.
;       ZOOM - Scalar specifying the magnification of the window with respect
;               to the image variable.    Use, for example, if image has been
;               REBINed before display.
;       FILENAME  = name of file to where CURVAL data can be saved.
;               Data will only be saved if left or center mouse button
;               are pressed.
;
; OUTPUTS:
;       None.
;
; SIDE EFFECTS:
;       X and Y values, etc., of the pixel under the cursor are constantly
;       displayed.  
;       Pressing left or center mouse button prints a line of output, and 
;       starts a new line.
;       Pressing right mouse button exits the procedure.
;       If the keyword FILENAME is defined, the date and time, and a heading 
;       will be printed in the file before the data.
;
; PROCEDURES CALLED:
;       ADSTRING(), EXTAST, GSSSXYAD, RADEC, SXPAR(), UNZOOM_XY, XY2AD
; REVISION HISTORY:
;       Written,  K. Rhode,  STX  May 1990
;       Added keyword FILENAME  D. Alexander  June 1991
;       Don't write to Journal file   W. Landsman    March 1993
;       Use astrometry structure  W. Landsman      Feb 1994
;       Modified for Mac IDL          I.   Freedman     April 1994
;       Allow for zoomed or offset image  W. Landsman      Mar 1996
;       Proper rounding of zoomed pixel values   W. Landsman/R. Hurt  Dec. 1997
;       Remove unneeded calls to obsolete !ERR   W. Landsman   December 2000
;       Replace remaining !ERR calls with !MOUSE.BUTTON W. Landsman Jan 2001
;       Allow for non-celestial (e.g. Galactic) coordinates W. Landsman Apr 2003
;       Work if RA/Dec reversed in CTYPE keyword  W. Landsman Feb. 2004
;       Always call UNZOOM_XY for MOUSSE compatibility W. Landsman Sep. 2004
;       Added ALT keyword  W. Landsman October 2004 
;       Always test if offset/zoom supplied  W. Landsman  Feb 2008 
;-
 On_error,2    ;if an error occurs, return to caller
 compile_opt idl2


 f_header = 0b           ;True if a FITS header supplied
 f_image =  0b           ;True if an image array supplied
 f_astrom = 0b           ;True if FITS header contains astrometry
 f_bscale = 0b           ;True if FITS header contains BSCALE factors
 f_imhd   = 0b           ;True if image array is in HD (1 parameter)
 npar = N_params()
 fileflag=0             ;True once left or middle mouse button pressed

 if !D.WINDOW EQ -1 then begin
        message,'ERROR - No image window active',/INF
        return
 endif


if (!D.FLAGS and 256) EQ 256 then wshow,!D.WINDOW  ;Bring active window to foreground

; Print formats and header for different astrometry,image, BSCALE combinations

 cr = string(13b)
 line0 = '  X     Y     Byte Inten'
 line1 = '  X     Y     Byte Inten   Value'
 line5 = '  X     Y   ByteInten   Value   Flux'

 f0 = "($,a,i4,2x,i4,6x,i4)"
 f1 = "($,a,i4,2x,i4,6x,i4,5x,a)"
 f2 = "($,a,i4,2x,i4,6x,i4,7x,a,1x,a)"
 f3 = "($,a,i4,2x,i4,2x,i4,7x,a,2x,a,1x,a,3x,e9.2)"
 f4 = "($,a,i4,2x,i4,2x,i4,7x,a,1x,a,a)"
 f5 = "($,a,i4,2x,i4,2x,i4,3x,a,5x,e9.2)"

 g0 = "(a,i4,2x,i4,6x,i4)"
 g1 = "(a,i4,2x,i4,6x,i4,5x,a)"
 g2 = "(a,i4,2x,i4,6x,i4,7x,a,1x,a)"
 g3 = "(a,i4,2x,i4,2x,i4,7x,a,2x,a,1x,a,3x,e9.2)"
 g4 = "(a,i4,2x,i4,2x,i4,7x,a,2x,a,1x,a)"
 g5 = "(a,i4,2x,i4,2x,i4,3x,a,5x,e9.2)"

if (npar gt 0) then begin
  type = size(hd)
  if (npar eq 1) and (type[0] eq 2) then begin
    f_image = 1b  & f_imhd = 1b 
    imtype = type
  endif else if (type[2] ne 7) or (type[0] ne 1) then begin
    print,'Syntax options: CURVAL        ;Display byte values'
    print,'                CURVAL, IM    ;where IM is a 2-D image,'
    print,'                CURVAL, Hdr   ;where Hdr is a FITS header,'
    print,'            or  CURVAL, Hdr,IM'
    return
  endif else if (type[2] eq 7) and (type[0] eq 1) then f_header = 1b
  if (npar eq 2) then begin
    f_image = 1b & f_header = 1b
    imtype = size(im)
    if (imtype[0] lt 2) or $
     (imtype[imtype[0]+2] ne imtype[1]*imtype[2]) then $
       message,'Image array (second parameter) is not two dimensional.'
  endif
endif    

; Get information from the header

 if f_header then begin     

  EXTAST, hd, astr, noparams, alt=alt                 ;Extract astrometry structure
  if (noparams ge 0) then f_astrom = 1b

  if f_image then begin
  bscale = sxpar(hd,'BSCALE')
  if (bscale ne 0) then begin
    bzero = sxpar(hd,'BZERO')
    bunit = sxpar(hd,'BUNIT', Count = N_Bunit)
    if N_Bunit GE 1 then $ 
    if f_astrom then line3 = line3 + '('+bunit+ ')' else $
                     line5 = line5 + '('+bunit+')'
    f_bscale = 1b
  endif
  endif
 endif

; Determine if an offset or zoom supplied
 unzoom = f_image  or f_header or keyword_set(offset) or keyword_set(zoom)

 if f_astrom GT 0 then begin
  coord = strmid(astr.ctype,0,4)
  coord = repchr(coord,'-',' ')
  if (coord[0] EQ 'DEC ') or (coord[0] EQ 'ELAT') or $
     (coord[0] EQ 'GLAT') then coord = rotate(coord,2)

  line2 = '  X     Y     Byte Inten        '  + coord[0] + '       ' +coord[1]
  line3 = '  X     Y   ByteInten    Value       ' + coord[0]  + '         ' + $
             coord[1] + '           Flux' 
  line4 = '  X     Y   ByteInten     Value      '  + coord[0] + '          ' + $
             coord[1]

  sexig = strupcase(strmid(coord[0],0,4))  EQ 'RA  ' 
 endif

 print,'Press left or center mouse button for new output line,'
 print,'... right mouse button to exit.'  

; different print statements, depending on the parameters

 case 1 of

(f_image eq 0b) and (f_astrom eq 0b):  begin   
   curtype = 0 & print, line0  & end      ;No image or header info

(f_image) and (f_astrom eq 0b) and (f_bscale eq 0b): begin
   curtype = 1  & print,line1 & end       ;Only image array supplied

(f_image eq 0b) and (f_astrom) and (f_bscale eq 0b): begin 
   curtype = 2  & print,line2 & end       ;Astrometry but no image array

(f_image) and (f_astrom) and (f_bscale): begin
   curtype =3   & print,line3 & end       ;Image array + astrometry + BSCALE

(f_image) and (f_astrom) and (f_bscale eq 0b): begin
   curtype = 4  & print,line4 & end       ;Image array +astrometry

(f_image) and (f_astrom eq 0b) and (f_bscale): begin
   curtype = 5  & print,line5 & end       ;Image array + BSCALE

endcase
 if f_image then begin
      dtype = imtype[imtype[0]+1]
      if (dtype LT 4) or (dtype GE 12) then dfmt = '(I8)' else  dfmt = '(G8.3)'
 endif

 LOOP: sv_err = !MOUSE.BUTTON
 !MOUSE.BUTTON = 0
 cursor,x,y,2,/DEVICE,/CHANGE                                 
 cr_err = !MOUSE.BUTTON

 if cr_err EQ 4 then begin
    print,' '
    if fileflag then free_lun,lun
    return

 endif


  x = x>0 & y = y>0
  inten = fix(tvrd(x,y,1,1))   ; read the byte intensity 

 if unzoom then unzoom_xy,x,y,offset=offset,zoom=zoom

 if f_astrom then begin

        case strmid(astr.ctype[0],5,3) of 
        'GSS': gsssxyad, astr, x, y, a, d
        else:  xy2ad, x, y, astr, a, d            ; convert to ra and dec
        endcase

        if sexig then begin 
            str = adstring(a,d,2)
            a = strmid(str,1,13)
            d  = strmid(str,14,13)
        endif else begin
            a = string(a,'(f10.2)') + '   '
            d = string(d,'(f10.2)') + '   '
        endelse
 endif

 x = round(x)  & y = round(y)

 if f_image then begin
      if (x LT 0) or (x GE imtype[1]) or $
         (y LT 0) or (y GE imtype[2]) then value = 0 else $
      if f_imhd then value = hd[x,y] else value = im[x,y]
      svalue = string(value,f=dfmt)
 endif

 if f_bscale  then flux = bscale*value + bzero  
 case curtype of
        0:  print,form=f0,cr,x,y,inten  
        1:  print,form=f1,cr,x,y,inten,svalue 
        2:  print,form=f2,cr,x,y,inten,a,d        
        3:  print,form=f3,cr,x,y,inten,svalue,a,d,flux
        4:  print,form=f4,cr,x,y,inten,svalue,a,d
        5:  print,form=f5,cr,x,y,inten,svalue,flux
 endcase

; Were left or center buttons been pressed?

 if (cr_err GE 1) and (cr_err LE 3) and (cr_err NE sv_err) then begin  
    print,form="($,a)",string(10b)   ; print a form feed
    if keyword_set(filename) and (not fileflag) then begin      ; open file & print table header to file
        get_lun,lun
        openw,lun,filename
        printf,lun,'CURVAL:   ',systime()      ;print time and date to file
        case 1 of               ;different print statements for file, depending on parameters

        (f_image eq 0b) and (f_astrom eq 0b) : begin
           printf, lun, line0  & end                    ;No image or header info

        (f_image) and (f_astrom eq 0b) and (f_bscale eq 0b) : begin
           printf, lun, line1 & end                     ;Only image array supplied

        (f_image eq 0b) and (f_astrom) and (f_bscale eq 0b) : begin
           printf, lun, line2 & end                     ;Astrometry but no image array

        (f_image) and (f_astrom) and (f_bscale) : begin
           printf, lun, line3 & end                     ;Image array + astrometry + BSCALE

        (f_image) and (f_astrom) and (f_bscale eq 0b) : begin
           printf, lun, line4 & end                     ;Image array + astrometry

        (f_image) and (f_astrom eq 0b) and (f_bscale) : begin
           printf, lun, line5 & end                     ;Image array + BSCALE
        endcase
        fileflag=1
    endif
    if keyword_set(filename) then begin
        case curtype of 
           0: printf, lun, form=g0,'', x, y, inten
           1: printf, lun, form=g1,'', x, y, inten, svalue 
           2: printf, lun, form=g2,'', x, y, inten, a, d
           3: printf, lun, form=g3,'', x, y, inten, svalue, a, d, flux
           4: printf, lun, form=g4,'', x, y, inten, svalue, a, d
           5: printf, lun, form=g5,'', x, y, inten, svalue, flux
        endcase
    endif
 endif

 goto,LOOP

 end
