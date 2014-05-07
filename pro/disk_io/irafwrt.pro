pro irafwrt, image, hd, filename, PIXDIR = pixdir
;+
; NAME:
;     IRAFWRT
; PURPOSE:
;     Write IDL data in IRAF (OIF) format (.imh and .pix files).
; EXPLANATION:
;     Does the reverse of IRAFRD.    IRAFWRT writes the "old" IRAF format
;     used prior to v2.11.   However, this "old" format is still readable by
;     the current version of IRAF.
;
; CALLING SEQUENCE: 
;    IRAFWRT, image, hdr, filename, [ PIXDIR = ]
;
; INPUTS:
;     image - array containing data
;     hdr   - The  corresponding FITS header.   Use MKHDR to create a minimal
;             FITS header if one does not already exist.
;     filename - Scalar string giving the name of the file to be written 
;             Should not include the extension name, which will be supplied 
;             by IRAFWRT.
; OUTPUTS:
;     None
;
; OPTIONAL KEYWORD INPUT:
;      PIXDIR - scalar string specifying the directory into which to write
;              the IRAF pixel (.pix) file.   The default is to write the pixel
;              file to the same directory as the header (.imh) file
;
; SIDE EFFECTS:
;      Image array and  FITS header are written to IRAF pixel file 
;               'filename'.pix and header file 'filename'.imh
;
; EXAMPLE:
;       Write an empty 50 x 50 array of all zeros to an IRAF file named 'EMPTY'
;
;       IDL> im = intarr( 50, 50)         ;Create empty array
;       IDL> mkhdr, hdr, im               ;Create a minimal FITS header
;       IDL> irafwrt, im, hdr, 'empty'    ;Write to a IRAF file named 'empty'
;
; PROCEDURE:
;       IRAFWRT gets information about the data - image dimensions, size, 
;       datatype, maximum and minimum pixel values - and writes it into
;       the binary part of the header. The ASCII part of the header
;       is directly copied after deleting records with certain keywords 
;       A pixel file is created, with a header in the first 1024 bytes
;
; RESTRICTIONS:
;       (1) The files are not created by IRAFWRT are not identical to those 
;               created by the IRAF routine rfits.    However, the files 
;               created by IRAFWRT appear to be compatible with all the IRAF 
;               routines tested so far.
;       (2)  IRAFWRT has been tested on a limited number of data types
;       (3)  IRAFWRT has only been tested on Unix and VMS systems.
;
; PROCEDURES CALLED:
;       FDECOMP, IS_IEEE_BIG(), ISARRAY(), REPCHR(), STRN(), SXDELPAR, SXPAR()
; MODIFICATION HISTORY:
;       Written K. Venkatakrishna, STX February 1992
;       VMS compatibility    W. Landsman      April 1992
;       Work with headers without DATE-OBS or ORIGIN           August 1992
;       Preserve HISTORY records with other FITS records       March 1995    
;       Fix case where a minimal FITS header supplied          August 1995
;       Work under Alpha/OSF and Linux                         Dec.   1995
;       Make sureheader has 80 char lines, use IS_IEEE_BIG()   May    1997
;       Don't apply strlowcase to .pix name   W. Landsman      April 1999
;       Work with double precision            W. Landsman      May 1999
;       Minimize use of obsolete !ERR         W. Landsman      Feb. 2000
;       Assume since V5.5, remove VMS support W. Landsman       Sep. 2006
;-     
 On_error,2

 if N_params() LT 3 then begin
        print,'Syntax - IRAFWRT, image, header, filename, [PIXDIR = ]'
        return
  endif
;
; Get the dimensions, vector of dimensions and the data type

  imsize  =  size(image) 
  naxis = imsize[0]
  imdim = imsize[1:naxis]
  type = imsize[naxis+1]
  im_max = max(image,min=im_min)  ; find the minimum and maximum pixel values

  case type of
  1: datatype = 1
  2: datatype = 3
  3: datatype = 4
  4: datatype = 6
  5: datatype = 7
  else: message,'ERROR - Input data type is currently unsupported'
  endcase

  fname = filename

  big_endian = is_ieee_big()

 header = fname+'.imh'
 openw, lun1, header, /GET_LUN

 object = sxpar( hd, 'OBJECT',Count = N_object)
 if ( N_object EQ 0 ) or ( object EQ '' ) then object = ' '
 origin = sxpar( hd, 'ORIGIN', Count = N_origin)
 if ( N_origin EQ 0 ) or ( origin EQ '') then origin = ' '
 date_obs = sxpar( hd, 'DATE-OBS', Count = N_date ) 
 if ( N_date EQ 0 ) or ( date_obs EQ '')  then date_obs = ' '

 hist_rec = where(strpos(hd,'HISTORY') EQ 0, Nhist)        ; Get history records
 if Nhist GT 0 then history = hd[hist_rec] else $
                    history = ' '

;Copy header to new variable and leave original variable unmodified
 xhdr = hd                   

 delete_rec = ['SIMPLE', 'BITPIX', 'NAXIS ', 'NAXIS1', 'NAXIS2', 'DATATYPE', $
            'OBJECT', 'ORIGIN', 'BSCALE', 'BZERO', 'GROUPS', $
            'IRAFNAME', 'END']

 sxdelpar, xhdr, delete_rec

 nmax = N_elements(xhdr)
 bhdr = replicate(32b, 80, nmax)         ;Make sure it is 80 bytes
 for i = 0l,nmax-1 do bhdr[0,i] = byte(xhdr[i])

 if isarray(xhdr) then $
         hdrlen = (nmax*162 + 2056)/4 $
    else hdrlen = 514

 hdr = bytarr(hdrlen*4)             ; Create header array

 inp = [ fix(hdrlen), fix(datatype), fix(naxis)]

 buf = bytarr(1024)
 hdr[12] = byte(inp,0,2)            ; write header length, data type
 hdr[16] = byte(inp,2,2)            ; and number of dimensions into
 hdr[20] = byte(inp,4,2)            ; header
 buf[20] = byte(inp,4,2)
;
; find current time in seconds wrt Jan-01-80 00:00:00
;
 time_creat = systime(2)-315550800.
 if big_endian then byteorder, hdr, /LSWAP   

 min = strn(im_min,format = '(E13.6)')
 max = strn(im_max,format = '(E13.6)') 
 max_rec_pos = where(strpos(xhdr,'IRAF-MAX = ') EQ 0)
 min_rec_pos = where(strpos(xhdr,'IRAF-MIN = ') EQ 0)
 if (max_rec_pos[0] GE 0) then begin
            max_rec = xhdr[max_rec_pos[0]]   ; write maximum
            min_rec = xhdr[min_rec_pos[0]]   ; and minimum pixel
            strput,max_rec,max,18        ; values
            strput,min_rec,min,18
            xhdr[max_rec_pos[0]] = max_rec
            xhdr[min_rec_pos[0]] = min_rec
 end
;
; write the ascii part of the header
;
  if hdrlen GT 514 then $
        for i = 0, nmax-1 do begin
            hdr[ 2052 + 162L*i + lindgen(80)*2]  =  bhdr[*,i]
            hdr[2052+162L*i+160] = 10B
        endfor

  if big_endian then byteorder,hdr,/SSWAP
  if not big_endian then offset = 0 else offset = 1
  hdr[ 732 + indgen(strlen(object))*2+offset] = byte(object)
  hdr[indgen(5)*2 + offset] = byte('imhdr')
  hdr[24] = byte(imdim,0,4*naxis)
  buf[24] = byte(imdim,0,4*naxis)
  hdr[52] = byte(imdim,0,4*naxis)
  hdr[120] = byte(im_max,0,4)
  hdr[124] = byte(im_min[0],0,4)
  cd,current = dir

     host = getenv('HOST')
    dir  =  dir + path_sep()
 
  if keyword_set(pixdir) then dir = pixdir
  pixname = host+'!' + dir + fname + '.pix'
  len1 = strlen(pixname)
  len2 = strlen(header)
  hdr[ 412 + offset + indgen(len1[0])*2] = byte(pixname)   ; write pixel file location    
  hdr[ 572 + offset + indgen(len2[0])*2] = byte(header)    ; into header
; Get the history records
;
 ind = 893
 hdr[ind+indgen(strlen(origin[0]))*2] = byte(origin[0])
 ind = ind+2*strlen(origin[0])
 hdr[ind] = 10B
 ind = ind+2
 hdr[ind+indgen(strlen(date_obs[0]))*2] = byte(date_obs[0])
 ind = ind+2*strlen(date_obs[0])
 hdr[ind] = 10B
 ind = ind+2

; write the history comment strings (as many as possible) in binary form
; into the available 1160 bytes 

 for i = 0, N_elements(history)-1 do begin
            hist = strtrim(strmid(history[i],8,72))
            if ( strlen(hist) EQ 0 ) then goto, SKIP    
            if (ind + 2*strlen(hist) GT 2052 ) then goto, HIST_END    
            hdr[ ind + indgen( strlen(hist) )*2 ] = byte(hist)
            ind = ind+2*strlen(hist)
            hdr[ind] = 10B
            ind = ind+2 
            SKIP:   
 end
 HIST_END:   
 hdr[88 + 2*offset] = byte(513,0,2)
 hdr[108] = byte(long(time_creat),0,4)       ; write time of image creation
 buf[108] = byte(long(time_creat),0,4)       ; time of last modification
 hdr[112] = byte(long(time_creat),0,4)       ; and time minimum and maximum 
 hdr[116] = byte(long(time_creat),0,4)       ; pixel values were computed
 
         hdr[32 + indgen(5)*4 + 3*offset] = 1
         buf[32 + indgen(5)*4 + 3*offset] = 1
         if big_endian then begin
            hdr[63 + indgen(5)*4] = 1
            buf[63 + indgen(5)*4] = 1
         endif
         hdr[63 + indgen(5)*4 - 3*offset] = 128
         buf[63 + indgen(5)*4 - 3*offset] = 128

  writeu,lun1,hdr
  free_lun,lun1

; Write the data into the .pix file   

 buf[ offset + indgen(5)*2] = byte('impix')
 if not big_endian then buf[12] = [65b, 58b] else $
                              buf[14] = [58b, 65b]
 hdrname = repchr(pixname,'pix','imh')
 buf[ 412 + offset+ indgen(len1[0])*2 ] = byte(hdrname)
 buf[ 572 + offset + indgen(len2[0])*2] = byte(header)
 node = strpos( pixname, '!')
 pixfile = strmid( pixname, node+1,strlen(pixname)-node+1 )

 openw,lun2, pixfile, /GET_LUN

 writeu, lun2, buf
 writeu, lun2, image

 free_lun, lun2

 return
 end
