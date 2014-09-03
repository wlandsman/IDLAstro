function f_format, minval, maxval, factor, length
;+
; NAME:
;	F_FORMAT
; PURPOSE:
;	Choose a nice floating format for displaying an array of REAL data.
; EXPLANATION:
;	Called by TVLIST, IMLIST.
;
; CALLING SEQUENCE:
;	fmt = F_FORMAT( minval, maxval, factor, [ length ] )
;
; INPUTS:
;	MINVAL - REAL scalar giving the minimum value of an array of numbers
;		for which one desires a nice format.
;	MAXVAL - REAL scalar giving maximum value in array of numbers
;
; OPTIONAL INPUT:
;	LENGTH - length of the output F format (default = 5)
;		must be an integer scalar > 2
;
; OUTPUT:
;	FMT - an F or I format string, e.g. 'F5.1'
;	FACTOR - factor of 10 by which to multiply array of numbers to achieve
;		a pretty display using format FMT.
;
; EXAMPLE:
;	Find a nice format to print an array of numbers with a minimum of 5.2e-3
;	and a maximum  of 4.2e-2.
;
;		IDL> fmt = F_FORMAT( 5.2e-3, 4.2e-2, factor )
;         
;	yields fmt = '(F5.2)' and factor = .01, i.e. the array can be displayed
;	with a F5.2 format after multiplication by 100.
;
; REVISION HISTORY:
;	Written W. Landsman              December 1988
;	Deal with factors < 1.           August 1991
;	Deal with factors < 1. *and* a large range    October 1992
;	Now returns In format rather than Fn.0    February, 1994
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
 On_error,2

 if N_params() LT 3 then begin                                         
    print,'Syntax  - fmt = F_FORMAT( minval, maxval, factor, [ length ])'
    return,''
 endif

 if N_params() LT 4 then length = 5 else length = length > 2
 factor = 1.

 RANGE: if ( maxval GT 0) then begin 
       mxlog = fix( alog10( maxval ) ) 
       mxval =  (mxlog>0) + 1 
 endif else if ( maxval LT 0) then begin
       mxlog =   fix( alog10( abs( maxval ) ) ) 
       mxval =  (mxlog>0) + 2 
 endif else begin
        mxlog = 0
        mxval = 1
 endelse

 if ( minval GT 0 ) then begin 
       mnlog = fix( alog10( minval ))
       mnval =  (mnlog>0) + 1 
 endif else if ( minval LT 0) then begin
       mnlog =   fix(alog10(abs(minval))) 
       mnval =   (mnlog>0) + 2 
 endif else begin
        mnlog = 0
        mnval = 1
 endelse

 if ( mnlog LT 0 ) and ( mxlog LT 0 ) then begin        ;All numbers are < 1.0
      expon = max( [ mnlog,mxlog ] ) -1
      factor = factor*10.^(expon)
      maxval = maxval / factor
      minval = minval / factor
      goto, RANGE  
 endif

 dif = abs( mxlog - mnlog )
 if ( dif GE length-3 ) then begin

     factor =  factor*10.^(mxlog-(length-3))    
     abs = 0

 endif else begin

 TEST:  tpairv = abs( [mxval,mnval] ) 
        test   = max( tpairv )          

 if ( test LE length-3 ) then begin        ;No factor needed
      abs = length - test - 2         
 endif else begin
     expon = min( [mxlog, mnlog] ) 
     if expon EQ 0 then expon = 1         ;Avoid infinite loop
     factor = factor*10.^(expon)
     mxval = mxval - expon
     mnval = mnval - expon
     goto, TEST 
 endelse 
 endelse

 if abs EQ 0 then begin
        factor = factor/10
	return,'I' + strtrim(length,2)
 endif else return,'F' + strtrim( length, 2 ) + '.' + strtrim( abs, 2 )
 
 end
