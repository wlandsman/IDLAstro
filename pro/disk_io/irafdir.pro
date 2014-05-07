pro irafdir,directory,TEXTOUT=textout
;+
; NAME:
;	IRAFDIR
; PURPOSE:
;	Provide a brief description of the IRAF images on a directory
; CALLING SEQUENCE:
;	IRAFDIR, [ directory, TEXTOUT = ]
;
; OPTIONAL INPUT PARAMETERS:
;	DIRECTORY - Scalar string giving file name, disk or directory to 
;		be searched 
;
; OPTIONAL INPUT KEYWORD:
;	TEXTOUT - specifies output device (see TEXTOPEN)
;		textout=1	TERMINAL using /more option
;		textout=2	TERMINAL without /more option
;		textout=3	<program>.prt
;		textout=4	laser.tmp
;		textout=5      user must open file
;		textout=7       Append to existing <program>.prt file
;		textout = 'filename' (default extension of .prt)
;
; OUTPUT PARAMETERS:
;	None
;
; PROCEDURE:
;	FINDFILE is used to find all '.imh' files in the directory. 
;	The object name and image size (NAXIS1, NAXIS2) are extracted
;	from the header. Each header is also searched for the parameters
;	DATE-OBS (or TDATEOBS), TELESCOP (or OBSERVAT), EXPTIME.
;  
; RESTRICTIONS:
;	(1) Some fields may be truncated since IRAFDIR uses a fixed format 
;		output
;	(2) No more than 2 dimension sizes are displayed 
; SYSTEM VARIABLES:
;	If 'textout' keyword is not specified to select an output device,
;	!TEXTOUT will be the default.    This non-standard system variable
;	can be added using the procedure ASTROLIB.
;
; PROCEDURE CALLS:
;	EXPAND_TILDE(), FDECOMP, REMCHAR, TEXTOPEN, TEXTCLOSE
; MODIFICATION HISTORY:
;	Written, K. Venkatakrishna, ST Systems Corp, August 1991
;	Work for IRAF V2.11 format   W. Landsman   November 1997
;	Assume since V5.5 use file_search W. Landsman   Sep 2006
;-

 On_error,2                          ;Return to caller
 
 ext='*.imh'

 defsysv,'!TEXTUNIT',exist=i
 if i EQ 0 THEN astrolib
 if keyword_set(directory) then begin 
	dir = strlowcase(directory)
	if strpos(dir,'~') GE 0 then dir = expand_tilde(dir)
 endif

 if N_ELEMENTS(dir) eq 0 then cd,current = dir

 dir = dir + path_sep()

 fil = file_search( dir + ext, COUNT=nfiles)
 if nfiles EQ 0 then begin
    message,'No IRAF (*.imh) files found ',/CON
    return
 endif

; Set output device according to keyword TEXTOUT or system variable !TEXTOUT

 if not keyword_set(textout) then textout=!textout
 textopen,'irafdir',TEXTOUT=textout

;  Print the title header
 printf,!textunit,format='(a,/)','IRAF file directory  '+strmid(systime(),4,20)
 printf,!textunit,$
'       NAME         SIZE     OBJECT      DATE-OF-OBS     TELESCOP      EXP TIME'

 get_lun,lun1
 fmt = '(a15,1x,i5,1x,i5,2x,a10,4x,a8,7x,a8,5x,a8)'
 dir2 = 'dummy'
 for i=0,nfiles-1 do begin                          ;Loop over each .imh file
   file1 = fil[i]                                       
   fdecomp,file1,disk,dir2,fname,qual             ;Decompose into disk+filename
   openr,lun1,file1,/stream                       ;open the file
   irafver = bytarr(5)
   readu,lun1,irafver
   newformat = string(irafver) EQ 'imhv2'
   point_lun,lun1,0
   tmp = assoc(lun1,bytarr(32))
   hdr = tmp[0]
 
   exptim ='    ?   '                ;Set default values
   telescop = '   ?      '
   date = '   ?      '

 if not newformat then begin
   hdr2 = hdr                                         ;Read the first 572 bytes
   byteorder,hdr,/sswap                               ; Perform byte swaps
   byteorder,hdr,/lswap 
   hdrlen = fix(hdr,12)                               ;Extract header length,
   ndim = fix(hdr,20)                                 ; number of dimensions,
   naxis1 = long(hdr2,24)                             ; dimension vector
   naxis2 = long(hdr2,28)
   if hdrlen EQ 0 then begin
		close,lun1
		goto, PRINTER
   endif
   tmp1 = assoc(lun1,bytarr(hdrlen*4l,/NOZERO))               
   hdr = tmp1[0]                                     ;Read the entire header
   close,lun1
   byteorder,hdr,/sswap  ;
   nfits = (hdrlen*4l-2054)/162                     ; find the number of records
   linelen = 162
   index = 2052l + indgen(80)*2
  
 endif else begin

   hdrlen = fix(hdr,8)                               ;Extract header length,
   ndim = fix(hdr,20)                                 ; number of dimensions,
   naxis1 = long(hdr,22)                             ; dimension vector
   naxis2 = long(hdr,26)
   tmp1 = assoc(lun1,bytarr(hdrlen*2l,/NOZERO))               
   hdr = tmp1[0]                                     ;Read the entire header
   close,lun1
   nfits = (hdrlen*2l-2049)/81                     ; find the number of records
   linelen = 81
   index = 2046l + indgen(80)
  endelse

; Form the string 'hd', 
; hd will be a FITS style header, that contains all the basic information 

 if nfits EQ 0 then goto, PRINTER
 hd = strarr(nfits)                              ; to break the header into
 for j = 0l,nfits-1 do hd[j] = string(hdr[linelen*j + index] )   
 

   keyword = strtrim( strmid(hd,0,8),2 )
   value = strtrim( strmid(hd,10,20),2 )
 l = where(keyword EQ 'TELESCOP',nfound)           ;Search for OBSERVAT keyword
 if nfound EQ 0 then l = where(keyword EQ 'OBSERVAT', nfound)
 if nfound GT 0 then begin
      telescop = value[l[0]]
      remchar,telescop,"'"
 endif 

 l = where(keyword EQ 'EXPTIME',nfound)           ;Search for EXPTIME keyword
 if nfound GT 0 then begin
    exptim = float(value[l[0]])
    if exptim EQ 0. then exptim = '   ?      ' else $
                 exptim = string(exptim,format= '(f7.1)')     
 endif 

 l = where(keyword EQ 'DATE-OBS' ,nfound)       ;Search for DATE-OBS keyword 
 if nfound EQ 0 then l = where(keyword EQ 'TDATEOBS', nfound)
 if nfound GT 0 then begin
   date=value[l[0]]
   remchar,date,"'"
 endif 

;Extract object name
PRINTER:
 if newformat then object = string( hdr[638 + indgen(8)]) else $
		   object = string( hdr[732 + indgen(8)*2]) 

 if dir2 NE dir then begin			;Has directory changed?   
       if ( dir2 EQ '' ) then cd,current=dir else dir = dir2
       printf,!textunit,format='(/a/)',disk+dir   ;Print new directory
       dir = dir2                                  ;Save new directory
 endif                   
;                                                  original header 

 printf,!textunit,FORMAT=fmt,fname,naxis1,naxis2,object,date,telescop,exptim
 if textout EQ 1 then if !ERR EQ 1 then return
 endfor

 textclose, TEXTOUT=textout
 free_lun, lun1

 return
 end

