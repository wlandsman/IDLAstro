;+
; NAME:   
;       CONCAT_DIR()
;               
; PURPOSE:     
;       To concatenate directory and file names for current OS.
; EXPLANATION:
;       The given file name is appended to the given directory name with the 
;       format appropriate to the current operating system.
;
; CALLING SEQUENCE:               
;       result = concat_dir( directory, file) 
;
; INPUTS:
;       directory  - the directory path (string)
;       file       - the basic file name and extension (string)
;                                   can be an array of filenames.
;
; OUTPUTS:     
;       The function returns the concatenated string.  If the file input
;       is a string array then the output will be a string array also.
;               
; EXAMPLES:         
;       IDL> pixfile = concat_dir('$DIR_GIS_MODEL','pixels.dat')
;
;       IDL> file = ['f1.dat','f2.dat','f3.dat']
;       IDL> dir = '$DIR_NIS_CAL'
;       IDL> 

;
; RESTRICTIONS: 
;               
;       The version of CONCAT_DIR available at 
;       http://sohowww.nascom.nasa.gov/solarsoft/gen/idl/system/concat_dir.pro
;       includes (mostly) additional VMS-specific keywords.
;
; CATEGORY    
;        Utilities, Strings
;               
; REVISION HISTORY:
;       Prev Hist. : Yohkoh routine by M. Morrison
;       Written     : CDS version by C D Pike, RAL, 19/3/93
;       Version     : Version 1  19/3/93
;       Documentation modified Nov-94   W. Landsman 
;       Add V4.0 support for Windows    W. Landsman   Aug 95
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Changed loops to long integer   W. Landsman   December 1998
;       Added Mac support, translate Windows environment variables, 
;       & treat case where dirname ends in '/' W. Landsman  Feb. 2000
;       Assume since V5.5, remove VMS support W. Landsman  Sep. 2006
;-            
;
function concat_dir, dirname, filnam
;
;  Check number of parameters
;
 if N_params() lt 2 then begin
   print,'Syntax - out_string = concat_dir( directory, filename)'
   print,' ' 
   return,''
 endif
;
;  remove leading/trailing blanks
;
 dir0 = strtrim(dirname, 2)     
 n_dir = N_Elements(dir0)
;
;  Act according to operating system
;  Under Windows, if the directory starts with a dollar sign, then check to see
;  the if it's really an environment variable.  If it is, then substitute the
;  the environment variable for the directory name.
;
    IF !VERSION.OS_FAMILY EQ 'Windows' THEN BEGIN
      FOR i = 0l, n_dir-1 DO BEGIN
         FIRST = STRMID(DIR0[I], 0, 1)
         IF FIRST EQ '$' THEN BEGIN
             SLASH = STRPOS(DIR0[I]+'/','/') < STRPOS(DIR0[I]+'\','\')
             TEST = GETENV(STRMID(DIR0[I],1,SLASH-1))
             IF TEST NE '' THEN BEGIN
                 IF STRLEN(DIR0[I]) GT SLASH THEN TEST = TEST + $
                         STRMID(DIR0[I],SLASH,STRLEN(DIR0[I])-SLASH)
                 DIR0[I] = TEST
             ENDIF
         ENDIF
;
         last = STRMID(dir0[i], STRLEN(dir0[i])-1, 1)
         IF (last NE '\') AND (last NE '/') AND (last NE ':') THEN BEGIN
            dir0[i] = dir0[i] + '\' ;append an ending '\' 
         ENDIF
      ENDFOR

; Macintosh/UNIX  section

 endif else  begin
   psep = path_sep()
    for i = 0l, n_dir-1 do begin
        last = strmid(dir0[i], strlen(dir0[i])-1, 1)
        if(last ne psep) then dir0[i] = dir0[i] + psep  ;append path separator 
    endfor
endelse 

;
;  no '/' needed when using default directory
;
 g  = where(dirname EQ '', Ndef) 
 if Ndef GT 0 then dir0[g] = '' 
 
 return, dir0 + filnam

 end
