function fits_ascii_encode, sum32
;+ 
; NAME:
;    FITS_ASCII_ENCODE()
; PURPOSE:
;    Encode an unsigned longword as an ASCII string to insert in a FITS header
; EXPLANATION:
;     Follows the July 2007 version of the FITS checksum proposal at 
;       http://fits.gsfc.nasa.gov/registry/checksum.html
; CALLING SEQUENCE:
;     result = FITS_ASCII_ENCODE( sum32)
; INPUTS:
;     sum32 - 32bit *unsigned longword* (e.g. as returned by CHECKSUM32)
; RESULT:
;     A 16 character scalar string suitable for the CHECKSUM keyword
; EXAMPLE:
;      A FITS header/data unit has a checksum of 868229149.  Encode the 
;      complement of this value (3426738146) into an ASCII string
;
;      IDL> print,FITS_ASCII_ENCODE(3426738146U)
;           ===> "hcHjjc9ghcEghc9g"
;
; METHOD:
;      The 32bit value is interpreted as a sequence of 4 unsigned 8 bit 
;      integers, and divided by 4.    Add an offset of 48b (ASCII '0'). 
;      Remove non-alphanumeric ASCII characters (byte values 58-64 and 91-96)
;      by simultaneously incrementing and decrementing the values in pairs.
;      Cyclicly shift the string one place to the right.
;                  
; REVISION HISTORY:
;     Written  W. Landsman  SSAI              December 2002
;     Use V6.0 notation  W.L.                 August 2013
;-
 if N_Params() LT 1 then begin
      print,'Syntax -  result = FITS_ASCII_ENCODE( sum32)'
      return,'0'
 endif
 
; Non-alphanumeric ASCII characters  
 exclude = [58b,59b,60b,61b,62b,63b,64b,91b,92b,93b,94b,95b,96b]
 ch = bytarr(16)
 t = byte(sum32,0,4)
 byteorder,t,/htonl
 quot = t/4 + 48b
 for i=0,12,4 do ch[i] = quot

 remain = t mod 4
 ch[0] = ch[0:3] + remain    ;Insert the remainder in the first 4 bytes

;Step through the 16 bytes, 8 at a time, removing nonalphanumeric  characters
 repeat begin           
 check = 0b
  for j=0,1 do begin
 il = j*8
 for i=il,il+3 do begin
  bad = where( (exclude EQ ch[i]) or (exclude Eq ch[i+4]) , Nbad) 
   if Nbad GT 0 then begin
       ch[i]++
       ch[i+4]--
       check=1b
  endif
 endfor
 endfor
 endrep until (check EQ 0b)

  return, string( shift(ch,1))
  end

