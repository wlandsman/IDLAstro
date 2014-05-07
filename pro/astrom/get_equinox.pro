FUNCTION GET_EQUINOX,HDR,CODE, ALT = alt                                       
;+
; NAME:
;       GET_EQUINOX
; PURPOSE:
;       Return the equinox value from a FITS header.  
; EXPLANATION:
;       Checks for 4 possibilities:
;
;       (1)  If the EQUINOX keyword is found and has a numeric value, then this
;               value is returned
;       (2)  If the EQUINOX keyword has the values 'J2000' or 'B1950', then
;               either 2000. or 1950. is returned.
;       (3)  If the EQUINOX keyword is not found, then GET_EQUINOX will return
;               the EPOCH keyword value.   This usage of EPOCH is disparaged.
;       (4)  If neither EQUINOX no EPOCH is found, then the RADESYS keyword 
;               (or the deprecated RADECSYS keyword) is checked.   If the value 
;               is 'ICRS' or 'FK5' then 2000 is is returned, if it is 'FK4' then
;               1950 is returned.
;
;       According Calabretta & Greisen (2002, A&A, 395, 1077) the EQUINOX should
;       be written as a numeric value, as in format (1).   However, in older 
;       FITS headers, the EQUINOX might have been written using formats (2) or 
;       (3).      
; CALLING SEQUENCE:
;       Year = GET_EQUINOX( Hdr, [ Code ] )   
;
; INPUTS:
;       Hdr - FITS Header, string array, will be searched for the EQUINOX
;               (or EPOCH) keyword.
;
; OUTPUT:
;       Year - Year of equinox in FITS header, numeric scalar
; OPTIONAL OUTPUT:
;       Code - Result of header search, scalar
;               -1 - EQUINOX, EPOCH or RADECSYS keyword not found in header
;               0 - EQUINOX found as a numeric value
;               1 - EPOCH keyword used for equinox (not recommended)
;               2 - EQUINOX found as  'B1950'
;               3 - EQUINOX found as  'J2000'
;               4 - EQUINOX derived from value of RADESYS or RADECSYS keyword
;                   'ICRS', 'FK5' ==> 2000,  'FK4' ==> 1950
; OPTIONAL KEYWORD INPUT: 
;       ALT -  single character 'A' through 'Z' or ' ' specifying which  
;             astrometry system to use in the FITS header.    The default is
;             to use the primary astrometry or ALT = ''.   If /ALT is set, 
;             then this is equivalent to ALT = 'A'.   See Section 3.3 of 
;             Greisen & Calabretta (2002, A&A, 395, 1061) for information about
;             alternate astrometry keywords.
; PROCEDURES USED:
;       ZPARCHECK, SXPAR()  
; NOTES:
;       Technically, RADESYS = 'ICRS' does not specify any equinox, but can be 
;       assumed to be equivalent to J2000 for all but highest-precision work.      
; REVISION HISTORY:                                               
;       Written  W. Landsman        STX              March, 1991
;       Don't use !ERR          W. Landsman   February 2000
;       N = 1 for check of EPOCH keyword, not 0 S. Ott July 2000
;       Added ALT keyword, recognize RADESYS along with deprecated RADECSYS
;              W. Landsman   Sep 2011
;-     
 compile_opt idl2
 On_error,2
 
 if N_elements(alt) EQ 0 then alt = '' else if (alt EQ '1') then alt = 'A' $
    else alt = strupcase(alt)
 zparcheck, 'GET_EQUINOX', hdr, 1, 7, 1, 'FITS Header array'
 code = -1                      ;Not found yet

 year = SXPAR( Hdr, 'EQUINOX' + alt, Count = n )    ;YEAR of Initial equinox
 if n EQ 0 then begin

     year = sxpar( Hdr, 'EPOCH', Count = n )  ;Check EPOCH if EQUINOX not found
     if n EQ 1 then code = 1 else begin       ;EPOCH keyword found
            
     sys = sxpar( Hdr, 'RADESYS'+alt, Count = n)
     if n EQ 0 then sys = sxpar( Hdr, 'RADECSYS', Count = n) 
              if n EQ 1 then begin
                  code = 4 
                  case strmid(sys,0,3) of
                  'ICR': year = 2000
                  'FK5': year = 2000
                  'FK4': year = 1950
                  else: 
                  endcase
               endif
         endelse
 endif else begin  

    tst = strmid(year,0,1)     ;Check for 'J2000' or 'B1950' values
    if (tst EQ 'J') || (TST EQ 'B') then begin 
           year = float(strmid(year,1,strlen(year)-1) )
           if tst EQ 'J' then code = 3
           if tst EQ 'B' then code = 2 
    endif else code = 0

 endelse     

 return, year
 end

