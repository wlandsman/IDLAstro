;+
; NAME: 
;     VALID_NUM()
; PURPOSE:               
;     Check if a string is a valid number representation.
; EXPLANATION:              
;     The input string is parsed for characters that may possibly
;     form a valid number.  It is more robust than simply checking
;     for an IDL conversion error because that allows strings such
;     as '22.3qwert' to be returned as the valid number 22.3
;
;     This function had a major rewrite in August 2008 to use STREGEX
;     and allow vector input.    It should be backwards compatible.
; CALLING SEQUENCE: 
;     IDL> status = valid_num(string  [,value]  [,/integer])
;    
; INPUTS:
;     string  -  the string to be tested, scalar or array
;               
; RETURNS
;     status - byte scalar or array, same size as the input string
;              set to 1 where the string is a  valid number, 0 for invalid
; OPTIONAL OUTPUT:               
;     value     - The value the string decodes to, same size as input string.
;           This will be returned as a double precision number unless 
;           /INTEGER is present, in which case a long integer is returned.
;           
; OPTIONAL INPUT KEYWORD:          
;    /INTEGER   -  if present code checks specifically for an integer.
; EXAMPLES:
;     (1) IDL> print,valid_num(3.2,/integer) 
;        --> 0     ;Since 3.2 is not an integer 
;     (2) IDL> str =['-0.03','2.3g', '3.2e12']
;         IDL> test = valid_num(str,val)
;              test = [1,0,1]    &  val =  [-0.030000000 ,NaN ,3.2000000e+12]
; REVISION HISTORY:
;          Version 1, C D Pike, RAL, 24-May-93
;          Version 2, William Thompson, GSFC, 14 October 1994
;                       Added optional output parameter VALUE to allow
;                       VALID_NUM to replace STRNUMBER in FITS routines.
;          Version 3 Wayne Landsman rewrite to use STREGEX, vectorize
;          Version 4 W.L. (fix from C. Markwardt) Better Stregex expression, 
;                    was missing numbers like '134.' before Jan 1 2010
;-            

FUNCTION valid_num, string, value, INTEGER=integer
 On_error,2
 compile_opt idl2 
 
; A derivation of the regular expressions below can be found on 
; http://wiki.tcl.tk/989

   if keyword_set(INTEGER) then $ 
    st = '^[-+]?[0-9][0-9]*$'  else $                    ;Integer
     st = '^[-+]?([0-9]+\.?[0-9]*|\.[0-9]+)([eEdD][-+]?[0-9]+)?$' ;F.P.
   
;Simple return if we just need a boolean test.
    if N_params() EQ 1 then return, stregex(strtrim(string,2),st,/boolean)

   
      vv = stregex(strtrim(string,2),st,/boolean)      
      if size(string,/N_dimen) EQ 0 then begin     ;Scalar
         if vv then $
            value= keyword_set(integer) ? long(string) : double(string) 
      endif else begin                             ;Array 
         
      g = where(vv,Ng)
      if Ng GT 0 then begin      ;Need to create output vector
        if keyword_set(integer) then begin 
              value = vv*0L 
              value[g] = long(string[g])
        endif else begin 
                value = replicate(!VALUES.D_NAN,N_elements(vv))
                value[g] = double(string[g])
        endelse 
        endif   
        endelse 
     
       return,vv
      end
