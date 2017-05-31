      FUNCTION tenv,dd,mm,ss
;+
; NAME:
;	TENV()
; PURPOSE:
;	Converts sexagesimal number or string vector to decimal.  
; EXPLANATION:
;	Like TEN() but allows vector input.
;
; CALLING SEQUENCES:
;	Result = TENV( dd, mm )           ; result = dd + mm/60.
;	Result = TENV( dd, mm, ss)        ; result = dd + mm/60. + ss/3600.
;                       or
;       Result = TENV(ddmmss_string)
; INPUTS:
;	dd - sexagesimal element(s) corresponding to hours or degrees
;	mm - sexagesimal element(s) corresponding to minutes
;	ss - sexagesimal element(s) corresponding to seconds (optional)
;		The input parameters can be scalars or vectors.   However, the
;		number of elements in each parameter must be the same.
;
;       HRMNSC_STRING - String scalar or vector giving sexagesmal quantity 
;               separated by spaces or colons e.g. "10 23 34" or "-3:23:45.2"
;               Any negative values should begin with a minus sign.
; OUTPUTS:
;	Result -  double, decimal equivalent of input sexagesimal 
;		quantities.  Same number of elements as the input parameters.
;		If the nth element in any of the input parameters is negative 
;		then the nth element in Result will also be negative.
;
; EXAMPLE:
;	If dd = [60,60,0], and mm = [30,-30,-30], then
;
;	IDL> Result = TENV(dd,mm)  ====>   Result =  [60.5,-60.5,-0.5]
;       
;       Alternatively, the input could be written as the string vector
;       IDL> str = ['60:30','-60:30','-0:30'] 
;       IDL> print,tenv(str)   ====>   Result =  [60.5,-60.5,-0.5]
;
; WARNING: 
;       TENV() will recognize floating point values of -0.0 as negative numbers.
;       However,  there is no distinction in the binary representation of -0 
;       and 0  (integer values), and so TENV will treat both values as positive.
; PROCEDURES USED:
;       GETTOK(), REPCHR()  for string processing.
; PROCEDURE:
;	Mostly involves checking arguments and setting the sign.
;
;   MODIFICATION HISTORY:
;	Written by W.B. Landsman           April, 1991
;       Recognize -0.0   W. Landsman/B. Stecklum   Dec 2005
;       Work with string input   W. Landsman Feb 2009
;       Accept comma separator in string input W. Landsman May 2017
;
;-
 compile_opt idl2

 npar = N_params()
 npts = N_elements(dd)
 if npts EQ 0 then begin
     print,'Syntax -  RESULT = TENV( dd, mm, ss)'
     return, 0.0d
 endif

 if ( npar EQ 1 ) then begin 
 if size(dd,/TNAME) EQ 'STRING' then begin 
       temp = strtrim(dd,2)
       temp = repchr(temp,':',' ')
       temp = repchr(temp,',',' ')
       neg = where( strmid(temp,0,1) EQ '-', Nneg)
       value = abs(double(gettok(temp,' ')))
       mm = double(gettok(temp,' '))
       decimal =  value + mm/60. + double(temp)/3600.0d
       if Nneg GT 0 then decimal[neg] = -decimal[neg]
       return,decimal
         
 endif else return,double( dd )   ;No need to check for neg values.
 endif

 value = double( abs(dd) ) 

 if ( npar GT 1 ) then begin               ;Add minutes/60., check for <0

      if N_elements(mm) NE npts then $
           message,'ERROR - Number of elements in each parameter must be equal'
      nd=(strpos(string(dd),'-') ge 0)
      nm=(strpos(string(mm),'-') ge 0)
      neg =  nd OR nm
      value = value + abs(mm)/60.0d

 endif

 if ( npar GT 2 ) then begin               ;Add sec/3600., check for <0

      if N_elements(ss) NE npts then $
           message,'ERROR - Number of elements in each parameter must be equal'
      ns=(strpos(string(ss),'-') ge 0)
      neg = neg OR ns
      value = value + abs(ss)/3600.0d

 endif

 neg = where( neg, Nfound )                  ;Account for negative values
 if ( Nfound GT 0 ) then value[neg] = -value[neg]

 return,value      
 end
