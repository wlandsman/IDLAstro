pro fitsdir ,directory, TEXTOUT = textout, Keywords = keywords, $ 
     nosize = nosize, alt1_keywords=alt1_keywords, alt2_keywords=alt2_keywords,$
     alt3_keywords = alt3_keywords, NoTelescope = NoTelescope,exten=exten
;+
; NAME:
;     FITSDIR 
; PURPOSE:
;     Display selected FITS keywords from the headers of FITS files.   
; EXPLANATION:
;
;     The values of either user-specified or default FITS keywords are 
;     displayed in either the primary header and/or the first extension header.
;     Unless the /NOSIZE keyword is set, the data size is also displayed.
;     The default keywords are as follows (with keywords in 2nd row used if
;     those in the first row not found, and the 3rd row if neither the keywords
;     in the first or second rows found:)
;
;     DATE-OBS     TELESCOP   OBJECT    EXPTIME       
;     TDATEOBS     TELNAME    TARGNAME  INTEG        ;First Alternative
;     DATE         OBSERVAT             EXPOSURE     ;Second Alternative
;                  INSTRUME             EXPTIM       ;Third Alternative
;
;      FITSDIR will also recognize gzip compressed files (must have a .gz 
;      or FTZ extension).
; CALLING SEQUENCE:
;     FITSDIR , [ directory, TEXTOUT =, EXTEN=, KEYWORDS=, /NOSIZE, /NoTELESCOPE
;                            ALT1_KEYWORDS= ,ALT2_KEYWORDS = ,ALT3_KEYWORDS =  
;
; OPTIONAL INPUT PARAMETERS:
;     DIRECTORY - Scalar string giving file name, disk or directory to be 
;             searched.   Wildcard file names are allowed.    Examples of 
;             valid names include 'iraf/*.fits' (Unix) or 'd:\myfiles\f*.fits',
;             (Windows).  
;            
; OPTIONAL KEYWORD INPUT PARAMETER
;      KEYWORDS - FITS keywords to display, as either a vector of strings or as
;                 a comma delimited scalar string, e.g.'testname,dewar,filter'
;                 If not supplied, then the default keywords are 'DATE-OBS',
;                 'TELESCOP','OBJECT','EXPTIME'
;      ALT1_KEYWORDS - A list (either a vector of strings or a comma delimited
;                 strings of alternative keywords to use if the default 
;                 KEYWORDS cannot be found.   By default, 'TDATEOBS', is the 
;                 alternative to DATE-OBS, 'TELNAME' for 'TELESCOP','TARGNAME'
;                 for 'OBJECT', and 'INTEG' for EXPTIME
;      ALT2_KEYWORDS - A list (either a vector of strings or a comma delimited
;                 strings of alternative keywords to use if neither KEYWORDS 
;                 nor ALT1_KEYWORDS can be found.    
;      ALT3_KEYWORDS - A list (either a vector of strings or a comma delimited
;                 strings of alternative keywords to use if neither KEYWORDS 
;                 nor ALT1_KEYWORDS nor ALT2_KEYWORDS can be found.    
;      /NOSIZE - if set then information about the image size is not displayed  
;      TEXTOUT - Controls output device as described in TEXTOPEN procedure
;               textout=1       TERMINAL using /more option
;               textout=2       TERMINAL without /more option
;               textout=3       <program>.prt
;               textout=4       laser.tmp
;               textout=5       user must open file
;               textout=7       Append to existing <program>.prt file
;               textout = filename (default extension of .prt)
;       EXTEN - Specifies an extension number (/EXTEN works for first extension)
;               which is checked for the  desired keywords.      FITSDIR searches
;               both the extension header and the primary header when an extension
;               number is specified.
;       /NOTELESCOPE - If set, then if the default keywords are used, then the
;                TELESCOPE (or TELNAME, OBSERVAT, INSTRUME) keywords are omitted
;                to give more room for display other keywords.   The /NOTELESCOP
;                 keyword has no effect if the default keywords are not used.
; OUTPUT PARAMETERS:
;       None.
;
; EXAMPLES:  
;  (1) Print info on all'*.fits' files in the current  directory using default
;          keywords.   Include information from the first extension    
;       IDL> fitsdir,/exten
;
;  (2) Write a driver program to display selected keywords in HST/ACS drizzled
;       (*drz) images
;         pro acsdir
;        keywords = 'date-obs,targname,detector,filter1,filter2,exptime'
;        fitsdir,'*drz.fits',key=keywords,/exten
;        return & end
;
;   (3)  Write info on all *.fits files in the Unix directory /usr2/smith, to a 
;       file 'smith.txt' using the default keywords, but don't display the value
;        of the TELESCOPE keyword
;
;       IDL> fitsdir ,'/usr2/smith/*.fits',t='smith.txt', /NoTel 
;
; PROCEDURE:
;       FILE_SEARCH() is used to find the specified FITS files.   The 
;       header of each file is read, and the selected keywords are extracted.
;       The formatting is adjusted so that no value is truncated on display.        
;
; SYSTEM VARIABLES:
;       TEXTOPEN (called by FITSDIR) will automatically define the following 
;       non-standard system variables if they are not previously defined:
;
;       DEFSYSV,'!TEXTOUT',1
;       DEFSYSV,'!TEXTUNIT',0
;
; PROCEDURES USED:
;       FDECOMP, FXMOVE(), MRD_HREAD, REMCHAR, SPEC_DIR(), TEXTOPEN, TEXTCLOSE
; MODIFICATION HISTORY:
;       Written, W. Landsman,  HSTX    February, 1993
;       Search alternate keyword names    W.Landsman    October 1998
;       Avoid integer truncation for NAXISi >32767  W. Landsman  July 2000
;       Don't leave open unit    W. Landsman  July 2000 
;       Added EXTEN keyword, work with compressed files, additional alternate
;       keywords W. Landsman     December 2000
;       Don't assume floating pt. exposure time W. Landsman   September 2001
;       Major rewrite, KEYWORD & ALT*_KEYWORDS keywords, no truncation, 
;             /NOSIZE keyword     W. Landsman,  SSAI   August 2002
;       Assume V5.3 or later W. Landsman November 2002
;       Fix case where no keywords supplied  W. Landsman January 2003
;       NAXIS* values must be integers W. Landsman SSAI  June 2003
;       Trim spaces off of input KEYWORD values W. Landsman March 2004
;       Treat .FTZ extension as gzip compressed  W. Landsman September 2004
;       Assume since V5.5, file_search() available W. Landsman Aug 2006
;       Don't assume all images compressed or uncompressed W. L. Apr 2010
;       Use V6.0 notation W.L. Feb 2011
;       Don't let a corrupted file cause an abort    W.L. Feb 2014
;       Let textopen.pro define !TEXTUNIT            W.L. Sep 2016
;-
; On_error,2

 compile_opt idl2     
 
 if N_elements(directory) EQ 0 then directory = '*.fits'
 if N_elements(exten) EQ 0 then exten = 0 

 FDECOMP, directory, disk, dir, filename, ext
 if filename EQ '' then begin 
      directory = disk + dir + '*.fits'
      filename = '*'
      ext = 'fits'
 endif else if !VERSION.OS_FAMILY EQ 'unix' then begin
        if (strpos(filename,'*') LT 0) && (ext EQ '') then begin  
        directory = disk + dir + filename + '/*.fits'
        filename = '*'
        ext = 'fits'
        endif
 endif

 if N_elements(keywords) EQ 0 then begin
     keywords = ['date-obs','telescop','object','exptime'] 
     if N_elements(alt1_keywords) EQ 0 then $
          alt1_keywords = ['tdateobs','telname','targname','integ']
     if N_elements(alt2_keywords) EQ 0 then $
          alt2_keywords = ['date','observat','','exposure']
     if N_elements(alt3_keywords) EQ 0 then $
          alt3_keywords = ['','instrume','','exptim' ]
     if keyword_set(NoTelescope) then begin
        ii = [0,2,3]
        keywords = keywords[ii] & alt1_keywords = alt1_keywords[ii]
        alt2_keywords = alt2_keywords[ii] & alt3_keywords = alt3_keywords[ii]
      endif
 endif
 if N_elements(keywords) EQ 1 then $
   keys = strtrim(strupcase(strsplit(keywords,',',/EXTRACT)),2) else $
   keys = strupcase(keywords)
   Nkey = N_elements(keys)

  case N_elements(alt1_keywords) of
  0: alt1_set = bytarr(Nkey)
  1: alt1_keys = strtrim(strupcase(strsplit(alt1_keywords[0],',',/EXTRACT)),2) 
  else: alt1_keys = strupcase(alt1_keywords) 
  endcase
  if N_elements(alt1_set) EQ 0 then alt1_set = strlen(strtrim(alt1_keys,2)) GT 0

  case N_elements(alt2_keywords) of
  0: alt2_set = bytarr(Nkey)
  1: alt2_keys = strtrim(strupcase(strsplit(alt2_keywords,',',/EXTRACT)),2) 
  else: alt2_keys = strupcase(alt2_keywords) 
  endcase
 if N_elements(alt2_set) EQ 0 then alt2_set = strlen(strtrim(alt2_keys,2)) GT 0

  case N_elements(alt3_keywords) of
  0: alt3_set = bytarr(Nkey)
  1: alt3_keys = strtrim(strupcase(strsplit(alt3_keywords,',',/EXTRACT)),2) 
   else: alt3_keys = strupcase(alt3_keywords) 
  endcase
  if N_elements(alt3_set) EQ 0 then alt3_set = strlen(strtrim(alt3_keys,2)) GT 0
  
   keylen = strlen(keys)
 
  direct = spec_dir(directory)
  files = file_search(directory,COUNT = n,/full) 

 if n EQ 0 then begin                                      ;Any files found?
       message,'No files found on '+ direct, /CON
       return
 endif 

  good = where( strlen(files) GT 0, Ngood)
  if Ngood EQ 0 then message,'No FITS files found on '+ directory $
                 else files = files[good]

 dir = 'dummy'
 num = 0

 get_lun,unit

 fdecomp, files, disk, dir2, fname, qual     ;Decompose into disk+filename
 fname = strtrim(fname,2)
 keyvalue = strarr(n,nkey)
 bignaxis = strarr(n)
 namelen = max(strlen(fname))

 for i = 0,n-1 do begin                           ;Loop over each FITS file
     compress = (qual[i] EQ 'gz') || (strupcase(qual[i]) EQ 'FTZ') 
     openr, unit, files[i], error = error, compress = compress 
    if error LT 0 then goto, BADHD
    mrd_hread, unit, h, status, /silent, ERRMSG = errmsg
   if status LT 0 then goto, BADHD

   if exten GT 0 then begin 
         close,unit
            openr, unit, files[i], error = error, compress = compress   
         stat = fxmove(unit, exten, /silent)
         mrd_hread, unit, h1, extstatus, /silent, ERRMSG = errmsg
         if extstatus EQ 0 then h = [h1,h]    ;Merge primary & extension header
    endif 

   keyword = strtrim( strmid(h,0,8),2 )       ;First 8 chars is FITS keyword
   lvalue = strtrim(strmid(h,10,20),2 ) 
   value = strtrim( strmid(h,10,68),2 )        ;Chars 10-30 is FITS value
 
 if ~keyword_set(nosize) then begin
 l= where(keyword EQ 'NAXIS',Nfound)            ;Must have NAXIS keyword
    if Nfound GT 0 then naxis  = long( lvalue[ l[0] ] ) else goto, BADHD

 if naxis EQ 0 then naxisi = '0' else begin

 l = where( keyword EQ 'NAXIS1', Nfound)         ;Must have NAXIS1 keyword
    if Nfound gt 0 then naxis1  = long( lvalue[l[0] ] ) else goto, BADHD 
    naxisi = strtrim( naxis1,2 )
 endelse

 if NAXIS GE 2 then begin
 l = where(keyword EQ 'NAXIS2', Nfound)          ;Must have NAXIS2 keyword
    if Nfound gt 0 then naxis2  = long(lvalue[l[0]]) else goto, BADHD
    naxisi = naxisi + ' ' + strtrim( naxis2, 2 )
 endif

 if NAXIS GE 3 then begin
 l = where( keyword EQ 'NAXIS3', Nfound )          ;Must have NAXIS3 keyword
    if Nfound GT 0 then naxis3  = long( lvalue[l[0]] ) else goto, BADHD
    naxisi = naxisi + ' ' + strtrim( naxis3, 2 )
 endif
 bignaxis[i] = strtrim(naxisi)
 endif

 for k=0,nkey-1 do begin
     l = where(keyword EQ keys[k], Nfound)
     if Nfound EQ 0 then  if alt1_set[k] then $
        l = where(keyword EQ alt1_keys[k], Nfound)
     if Nfound EQ 0 then  if alt2_set[k] then $
        l = where(keyword EQ alt2_keys[k], Nfound)
     if Nfound EQ 0 then  if alt3_set[k] then $
        l = where(keyword EQ alt3_keys[k], Nfound)
     if nfound GT 0 then begin
            kvalue = value[l[0]]
            if strpos(kvalue,"'") GE 0 then begin
               temp = gettok(kvalue,"'")
               keyvalue[i,k] = strtrim(gettok(kvalue,"'"),2)
            endif else keyvalue[i,k] = strtrim(gettok(kvalue,'/'),2) 
     endif 
        
    endfor

 BADHD:  

 close,unit
 if status LT 0 then begin
      message,'Bad File: ' + files[i],/Con
      if N_elements(errmsg) NE 0 then message,errmsg,/CON
      endif 
 endfor
 DONE: 
 free_lun, unit
 vallen = lonarr(nkey)
 for k=0,nkey-1 do vallen[k]  = max(strlen(keyvalue[*,k]))


 textopen, 'fitsdir', TEXTOUT=textout,/STDOUT 
 printf,!TEXTUNIT,' '
 printf,!TEXTUNIT,'FITS File Directory ' + systime()
 printf,!TEXTUNIT, direct
  printf,!TEXTUNIT, ' '

 pheader = ' NAME '
 if namelen GT 5 then pheader += string(replicate(32b,namelen-5))
 if ~keyword_set(nosize) then begin 
    pheader += 'SIZE '
    naxislen = max(strlen(bignaxis))+1
    if naxislen GT 5 then pheader += string(replicate(32b,naxislen-5))
 endif
 for k=0,nkey-1 do begin
     pheader += keys[k] + ' '
     if vallen[k] GT keylen[k] then  $
        pheader += string(replicate(32b,vallen[k]-keylen[k]))
 endfor
  printf,!TEXTUNIT, pheader
  printf,!TEXTUNIT, ' '
  xx = namelen + 2
 fmt = '(A' 
 if ~keyword_set(nosize) then begin 
   fmt += ',T' + strtrim(xx,2)
   xx += (naxislen>4) + 1
 endif 
   fmt +=  ',A'  
 remchar,keyvalue,"'"

 for k=0,nkey-1 do begin 
  
   fmt += ',T' + strtrim(xx,2) + ',A'
   xx += (vallen[k]>keylen[k]) +1
 endfor
 fmt += ')'

 for i=0,n-1 do printf, f= fmt, $
      !TEXTUNIT,fname[i],bignaxis[i], keyvalue[i,*]
    
 textclose,textout=textout 
 return      ;Normal return   
 end
