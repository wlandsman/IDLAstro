pro wfpc2_read,filename,chip1,header1,chip2,header2, $
               chip3,header3,chip4,header4,num_chip=num_chip, $
               trim=trim, path = path,batwing=batwing
;+
; NAME:
;    WFPC2_READ
;
; PURPOSE:
;   Read WFPC2 images in either FITS or STSDAS format into IDL variables.
;
; EXPLANATION:
;   This a versatile procedure for reading Wide Field Planetary Camera 2 
;   (WFPC2) images.   One can read either multi-extension FITS or  STSDAS or
;   STSDAS converted to FITS format, and specific 
;   chip or chips.    One can also read all four chips into a "batwing" mosaic--
;   so-called because the PC chip (chip 1) has a plate scale of 0.045", while
;   the other three WF chips have a plate scale of 0.1"
; 
; CALLING SEQUENCE:
;    WFPC2_READ,filename,chip1,hdr1,chip2,hdr2,chip3,hdr3,chip4,hdr4
;                   or
;    WFPC2_READ,filename,chip,hdr, NUM_CHIP = [1,2,3,4], [/TRIM, PATH = ]
;                   or
;    WFPC2_READ,filename,image,hdr,/BATWING
;
; INPUTS:
;    filename - Name of FITS or STSDAS file with a stack of images from
;            the four WF/PC-2 chips, followed by a FITS ASCII
;            table with header parameters for each chip.    If the file
;            name extension ends in 'h' then it is assumed to be an
;            STSDAS file.   If no extension is supplied, and the file is
;            is not found, then WFPC2_READ first tries appending a '.fits'
;            extension, and then tries appending a '.c0h' extension.  
;
;            The file may als be gzip compressed (with a .gz extension) 
; INPUT KEYWORD PARAMETERS:
;    NUM_CHIP - Integer scalar or vector, subset of 1, 2, 3, 4, specifying 
;               particular chip numbers to read.    Outputs will be in same 
;               order as specification of subset.   (See Example 2.)
;    /TRIM   - If set, trim off areas with no image and re-orient so that
;              all  the chips have a common orientation suitable for insertion 
;               into "bat-wing" mosaic (no image distortion removal, however).
;    PATH   -   scalar string specifying a !PATH-like list of directories
;               in which to search for the file.   Default is to look only
;               in the current directory.
;    /BATWING -  Return a 1600 x 1600 array containing all four chips in a
;               "bat wing" mosaic formation.     This image is mainly for 
;               display  purposes, since the PC chip is compressed to match the plate 
;               scale of the WF chips.    In addition, a small astrometry error
;               is introduced since chips do not have the same rotation, nor    
;               are they aligned at the integer pixel level.
; OUTPUTS:
;    chipN    - 800 X 800 image from chip N.   If /TRIM is set then the output
;               size is somewhat smaller (e.g. 756 x 757)
;    headerN  - Individual FITS header for chip N with correct astrometry.
;
; PROCEDURES USED:
;     For FITS I/O: FITS_CLOSE, FITS_OPEN, FITS_READ
;     For STSDAS I/O: EXTGRP, FTGET(), SXOPEN, SXREAD()
;     Other procedures:  CHECK_FITS, FDECOMP, FIND_WITH_DEF(), FREBIN, HEXTRACT, 
;           HROTATE, SXADDHIST, SXADDPAR, SXPAR()
; EXAMPLE: 
;    (1) Read all four chips of the FITS file u2ou0201t_c0f.fits
; 
;    IDL> wfpc2_read,'u2ou0201t_c0f',c1,h1,c2,h2,c3,h3,c4,h4
;
;     (2) Note that supplying the .fits extension is optional.   Now read only
;     chips 1 (the PC chip) and 3.   Trim off portions of the arrays where
;     there is no image.   
;
;    IDL> wfpc2_read,'u2ou0201t_c0f',c1,h1,c3,h3,num=[1,3],/trim
;
;      (3) Note that with the /TRIM option the output chip sizes are no longer
;          800 x 800 but odd sizes such as 770 by 753.    Now read all 4 chips
;          into a 1600 x 1600 "batwing" mosaic
;
;    IDL> wfpc2_read,'u2ou0201t_c0f',im,h,/batwing
;
; MODIFICATION HISTORY:
;     Written by W. Landsman, Raytheon STX, for IDL V5.0     June 1998
;     Based on code by Robert Hill, Raytheon STX
;     Better astrometry of PC image in "batwing" configuration, W. Landsman
;                August 1999
;     Use vector call to SXADDHIST  W. Landsman   March 2003
;     Don't use EXECUTE() for V6.1 or later W. Landsman Dec 2006
;     Assume since V6.1  W. Landsman  June 2009
;     Ability to read multi-extension format FITS  W. Landsman May 2010
;     Correct header in MEF form when only reading PC chip.  W.L. July 2010
;-
 compile_opt idl2
 if N_params() LT 2 then begin
    print,'Syntax:'
    print,' WFPC2_READ,filename,chip1,hdr1,chip2,hdr2,chip3,hdr3,chip4,hdr4'
    print,'           or'
    print,' WFPC2_READ, filename,chip,hdr, NUM_CHIP =[1,2,3,4], [/TRIM, PATH=]'
    print,'           or'
    print,' WFPC2_READ,filename,image,hdr,/BATWING, PATH=]'  
    return
 endif

; EXTPARS gives the region of each chip containing valid data.    ROTPARS 
; gives the IDL ROTATE parameter to apply to each chip to give them a common 
; orientation.   X1 and Y1 give the starting pixel of each chip in the 
; "batwing" mosaic.

 extpars = [[44, 799, 52, 799], [ 0, 773, 46, 799], $
           [ 0, 769,  0, 752], [44, 799,  0, 756] ]
 rotpars = [0, 1, 2, 3]
 x1 = [800,26,30,800]      
 y1 = [800,800,47,43]

 if N_elements(num_chip) EQ 0 then $
	if (N_params() LE 3) and ~keyword_set(BATWING) then num_chip = 1 
 if N_elements(num_chip) GT 0 then num_c = num_chip $
    else num_c = [1,2,3,4]
 if keyword_set(batwing) then begin 
      chip1 = fltarr(1600,1600)
      num_c = [1,2,3,4]
      trim = 1
 endif
 Nout = N_elements(num_c)

; If the specified file is not found, try adding a '.fits' and then a '.c0h'
; extension
 
 if N_elements(PATH) EQ 0 then path = ''
 a = FIND_WITH_DEF(filename, path, '.fits,.c0h')
 FDECOMP, a[0], disk, dir, fname, ext

 if strlowcase(strmid(ext,2,1) EQ 'h') then begin      ;SDAS format
      SXOPEN, 1, a[0], htab
      for i = 0, Nout-1 do begin
        j = num_c[i] - 1
        thischp = SXREAD(1, j, par)
        thishdr = htab 
        EXTGRP, thishdr, par         ;Insert group parameters into FITS header
        if keyword_set(TRIM) then begin 
           HROTATE, thischp, thishdr, rotpars[j]
           HEXTRACT, thischp, thishdr, extpars[0,j], extpars[1,j], $
               extpars[2,j], extpars[3,j], /SILENT
        endif
        ii = strtrim(i+1,2)
        jj = strtrim(j+1,2)
    sxaddhist, ['----------------------------------------------------------', $
                '      WFPC2_READ:  ' + systime(),  $
                '      Header parameters for chip ' + jj + $
                 ' replaced from group parameters'],  thishdr
        if keyword_set(batwing) then begin
           if i EQ 0 then $
                    chip1[x1[0],y1[0]] = FREBIN(thischp,345.7,342,/total) else $
                    chip1[x1[i],y1[i]] =  thischp
           if i EQ 3 then begin
                    crpix = sxpar(thishdr,'CRPIX*')
                    sxaddpar, thishdr, 'CRPIX1', crpix[0] + x1[3]
                    sxaddpar, thishdr, 'CRPIX2', crpix[1] + y1[3]
                    header1 = thishdr
                    CHECK_FITS,chip1,header1,/update,/silent,/FITS
            endif 
        endif else begin  
	   (scope_varfetch('chip' + ii)) = temporary(thischp)
	   (scope_varfetch('header' + ii)) = thishdr
	endelse
        endfor
 
 endif else begin
 
 FITS_OPEN, a[0], fcb

; Is a converted GEIS file or the newer multi-extension format (MEF)?
 if (fcb.nextend EQ 4) && (fcb.naxis[0,0] EQ 0) then begin 
       if Nout EQ 1 then begin
             FITS_READ, fcb, chip1, header1, exten_no=num_chip
	endif else begin 
	      d = fltarr(800,800,4,/nozero)
	      if keyword_set(batwing) then $
	           fits_read, fcb, chip_pc, header1, exten=1 else $
		   fits_read, fcb, chip1, header1, exten=1
              fits_read,fcb, chip2,header2
              fits_read,fcb, chip3,header3
              fits_read,fcb, chip4,header4
              if keyword_set(batwing) then begin 
	      chip1[x1[0],y1[0]] = FREBIN(chip_pc,345.7,342.0, /total)
	      chip1[x1[1],y1[1]] = chip2
	      chip1[x1[2],y1[2]] = chip3
	      chip1[x1[3],y1[3]] = chip4
	      crpix = sxpar(header3,'CRPIX*')
              sxaddpar, header1, 'CRPIX1', crpix[0] + x1[3]
              sxaddpar, header1, 'CRPIX2', crpix[1] + y1[3]
              CHECK_FITS, chip1, header1, /update, /silent, /FITS
              endif else if keyword_set(trim) then begin 
              HROTATE, chip1, header1, rotpars[0]
              HEXTRACT, chip1, header1, extpars[0,0], extpars[1,0], $
          extpars[2,0], extpars[3,0],/SILENT
              HROTATE, chip2, header2, rotpars[1]
              HEXTRACT, chip2, header2, extpars[0,1], extpars[1,1], $
          extpars[2,1], extpars[3,1],/SILENT
              HROTATE, chip3, header3, rotpars[2]
              HEXTRACT, chip3, header3, extpars[0,2], extpars[1,2], $
          extpars[2,2], extpars[3,2],/SILENT
              HROTATE, chip4, header4, rotpars[3]
              HEXTRACT, chip4, header4, extpars[0,3], extpars[1,3], $
          extpars[2,3], extpars[3,3],/SILENT

	  endif
	  endelse    	     
          return
  endif 
 if Nout EQ 1 then begin
	ns = fcb.axis[0,0]
	nl = fcb.axis[1,0]
	i1 = (ns*nl)*(num_chip-1)
	i2 = i1 + ns*nl-1
	FITS_READ, fcb, chip1, h, first=i1, last=i2
	chip1 = reform(chip1,ns,nl)
 endif else FITS_READ,fcb,d,h

; Now read the first FITS extension which contains the ASCII table, giving
; separate astrometry parameters for each of the four chips.

 FITS_READ, fcb, dtab, htab, /no_pdu
 tf = sxpar(htab,'TFIELDS')
 name = sxpar(htab,'TTYPE*')
 fmt = sxpar(htab,'TFORM*')
 comment = strarr(tf)
 for j=0,tf-1 do comment[j] = sxpar(htab,name[j])
 
 if fcb.axis[1,0] LT max(num_c) then begin
	message, /inf,'Image ' + filename + ' contains only PC image'
	num_c = fcb.axis[1,0]
 endif
 FITS_CLOSE, fcb

 ftinfo,htab,ft_str
 for i = 0, Nout-1 do begin
    cn = num_c[i]
    cn_0 = cn - 1
    cn_str = strtrim(num_c[i],2)
    cn_arg = strtrim(i+1,2)
    hdr = 'header' + cn_arg
    chp = 'chip' + cn_arg
    thishdr = h
    sxaddhist, ['----------------------------------------------------------', $
                '      WFPC2_READ:  ' + systime(), $
                '      Header parameters for chip ' + cn_str $
              + ' replaced from table.'], thishdr   
 for j=0,tf-1 do begin
         value = ftget(ft_str,dtab,j+1,cn_0)
        sxaddpar, thishdr,name[j],value[0],comment[j],format=fmt[j]
    endfor
     if nout GT 1 then begin

	thischp = d[*,*,cn_0] 
        CHECK_FITS, thischp,  thishdr, /fits, /update, /silent
          
    if keyword_set(trim) then begin
        HROTATE, thischp, thishdr, rotpars[cn_0]
        HEXTRACT, thischp, thishdr, extpars[0,cn_0], extpars[1,cn_0], $
          extpars[2,cn_0], extpars[3,cn_0],/SILENT
    endif
       
      if keyword_set(batwing) then begin

           if i EQ 0 then $
                    chip1[x1[0],y1[0]] = FREBIN(thischp,345.7,342.0, /total) else $
                    chip1[x1[i],y1[i]] =  thischp
           if i EQ 3 then begin 
                    crpix = sxpar(thishdr,'CRPIX*')
                    sxaddpar, thishdr, 'CRPIX1', crpix[0] + x1[3]
                    sxaddpar, thishdr, 'CRPIX2', crpix[1] + y1[3]
                    header1 = thishdr
                    CHECK_FITS, chip1, header1, /update, /silent, /FITS
           endif
    endif else begin
    
        (SCOPE_VARFETCH(chp)) = temporary(thischp) 
        (SCOPE_VARFETCH(hdr)) = temporary(thishdr)
    endelse
   endif else begin
       header1 = thishdr
       CHECK_FITS, chip1, header1, /fits,/update,/silent  
       if keyword_set(TRIM) then begin
         HROTATE, chip1, header1, rotpars[cn_0]
         HEXTRACT, chip1, header1, extpars[0,cn_0], extpars[1,cn_0], $
              extpars[2,cn_0], extpars[3,cn_0],/silent
       endif
   endelse

endfor  

 endelse 
return
end
