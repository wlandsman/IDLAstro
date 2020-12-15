      FUNCTION ten,dd,mm,ss
;+
; NAME:
;	TEN()
; PURPOSE:
;	Converts a sexagesimal number or string to decimal.
; EXPLANATION:
;	Inverse of the SIXTY() function.
;
; CALLING SEQUENCES:
;	X = TEN( [ HOUR_OR_DEG, MIN, SEC ] )
;	X = TEN( HOUR_OR_DEG, MIN, SEC )
;	X = TEN( [ HOUR_OR_DEG, MIN ] )
;	X = TEN( HOUR_OR_DEG, MIN )
;	X = TEN( [ HOUR_OR_DEG ] )      <--  Trivial cases
;	X = TEN( HOUR_OR_DEG )          <--
;
;        or
;       X = TEN(HRMNSC_STRING)
;
; INPUTS:
;	HOUR_OR_DEG,MIN,SEC -- Scalars giving sexagesimal quantity in 
;		in order from largest to smallest.    
;                         or
;   HRMNSC_STRING - String giving sexagesmal quantity separated by
;               spaces, commas or colons e.g. "10 23 34" or "-3:23:45.2"
;               Any negative values should begin with a minus sign.
; OUTPUTS:
;	Function value returned = double real scalar, decimal equivalent of
;	input sexigesimal quantity.  For numeric input, a minus sign on any 
;   nonzero element of the input vector causes all the elements to be taken 
;   as < 0.
;
; EXAMPLES:
;       IDL> print,ten(0,-23,34)
;                 --> -0.39277778
;       IDL> print,ten("-0,23,34")
;                 --> -0.39277778
; PROCEDURE:
;	Mostly involves checking arguments and setting the sign.
;
;	The procedure TENV can be used when dealing with a vector of 
;	sexigesimal quantities.
;
; MODIFICATION HISTORY:
;	Written by R. S. Hill, STX, 21 April 87       
;	Modified to allow non-vector arguments.  RSH, STX, 19-OCT-87
;   Recognize -0.0   W. Landsman/B. Stecklum   Dec 2005
;   Work with string input  W. Landsman Dec 2008
;   Accept comma separator in string input W. Landsman May 2017
;-
      compile_opt idl2
      np = N_params()

      if (np eq 1) then begin
         if size(dd,/TNAME) EQ 'STRING' then begin  
	      temp = strtrim(dd,2)
	      neg = strmid(dd,0,1) EQ '-'
	      temp = repchr(temp,':',' ')
	      temp = repchr(temp,',',' ')
	      value = abs(double(gettok(temp,' ')))
	       mm = double(gettok(temp,' '))
	       decimal =  value + mm/60. + double(temp)/3600.0d 
              if neg then decimal = -decimal
	      return,decimal
         endif else vector=dd
      endif else begin
         if (np lt 1) || (np gt 3) then goto,bad_args
         vector=dblarr(3)
         vector[0]=dd
         vector[1]=mm
         if np gt 2 then vector[2]=ss
      endelse
      sz = size(vector)
      ndim = sz[0]
      if (ndim eq 0) then return,double(vector)
      facs=[1.0d0,60.0d0,3600.0d0]
      nel = sz[1]
      sign = +1.0d0
       dummy=where(strpos(string(vector),'-') ge 0,cnt)
       if cnt gt 0 then sign = -1.0d0
      vector = abs(vector)
      decim = double(vector[0])
      i = 1
      while (i le nel-1) do begin
         decim = decim + double(vector[i])/facs[i]
         i++
      endwhile
      return,decim*sign
bad_args:    
      print,'Argument(s) should be hours/degrees, minutes (optional),'
      print,'seconds (optional)   in vector or as separate arguments.'
      print,'If any one number negative, all taken as negative.'
      return,0.0d0     
      end
