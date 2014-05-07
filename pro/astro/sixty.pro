      FUNCTION sixty,scalar, Trailsign = trailsign
;+
; NAME:
;	SIXTY()
; PURPOSE:
;	Converts a decimal number to sexagesimal.
; EXPLANATION:
;	Reverse of the TEN() function.
;
; CALLING SEQUENCE:
;	X = SIXTY( SCALAR, [ /TrailSign ] ) 
;
; INPUTS:
;	SCALAR -- Decimal quantity.  
; OUTPUTS:
;	Function value returned = real vector of three elements, 
;	sexagesimal equivalent of input decimal quantity.    Double
;       precision if the input is double, otherwise floating point.
;	By default, a negative number is signified by making the first non-zero
;	element of the output vection negative, but this can be modified with
;       the /TrailSign keyword.
;
; OPTIONAL INPUT KEYWORD:
;      /TrailSign - By default, SIXTY() returns a negative sign in the first
;         nonzero element.   If /TrailSign is set, then SIXTY() will return
;         always return a negative sign in the first element, even if it is
;         zero
; PROCEDURE:
;	Mostly involves checking arguments and setting the sign.
;
; EXAMPLE:
;	If x = -0.345d then sixty(x) = [0.0, -20.0, 42.0]
;                      and sixty(x,/trail) = [-0.0, 20.0, 42.0]
; MODIFICATION HISTORY:
;	Written by R. S. Hill, STX, 19-OCT-87         
;	Output changed to single precision.  RSH, STX, 1/26/88
;	Accept single element vector   W. Landsman   Sep. 1996
;	Converted to IDL V5.0   W. Landsman   September 1997
;       Added /TrailSign keyword, preserve data type  
;                 B. Stecklum/ W. Landsman   March 2006
;-

      if N_elements(scalar) NE 1 then begin
	      message,'ERROR - First parameter must contain 1 element',/CON
	      return,replicate(100.0e0,3)
      endif	

      ss=abs(3600.0d0*scalar)
      mm=abs(60.0d0*scalar) 
      dd=abs(scalar) 
      if size(scalar,/tname) EQ 'DOUBLE' then result = dblarr(3) else $
                                             result=fltarr(3)
      result[0]= fix(dd) 
      result[1]= fix(mm-60.0d0*result[0])
      result[2]= ss - 3600.d0*result[0] - 60.0d0*result[1]
     
      if scalar[0] lt 0.0d0 then begin 
         if keyword_set(trailsign) then result[0] = -result[0] else begin
            if result[0] ne 0 then result[0] = -result[0] else $
            if result[1] ne 0 then result[1] = -result[1] else $
            result[2] = -result[2]
	 endelse 
      endif

      return,result
      end
