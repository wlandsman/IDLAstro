function is_ieee_big
;+
; NAME:
;	IS_IEEE_BIG
; PURPOSE:
;	Determine if the current machine uses IEEE, big-endian numbers.
; EXPLANATION:
;       (Big endian implies that byteorder XDR conversions are no-ops).
; CALLING SEQUENCE:
;	flag = is_ieee_big()
; INPUT PARAMETERS:
;       None
; RETURNS:
;       1 if the machine appears to be IEEE-compliant, 0 if not.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None
; RESTRICTIONS:
; PROCEDURE:
;       The first byte of the two-byte representation of 1 is examined.
;       If it is zero, then the data is stored in big-endian order.
; MODIFICATION HISTORY:
;       Written 15-April-1996 by T. McGlynn for use in MRDFITS.
;	13-jul-1997	jkf/acc	- added calls to check_math to avoid
;				  underflow messages in V5.0 on Win32 (NT).
;	Converted to IDL V5.0   W. Landsman   September 1997
;       Follow RSI and just do a single test  W. Landsman   April 2003
;-

      return, 1b - (byte(1,0,1))[0]
      end								    
