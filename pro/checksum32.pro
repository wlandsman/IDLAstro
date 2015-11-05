pro checksum32, array, checksum, FROM_IEEE = from_IEEE, NOSAVE = nosave
;+
; NAME:
;       CHECKSUM32
;
; PURPOSE:
;       To compute the 32bit checksum of an array (ones-complement arithmetic)
;
; EXPLANATION:
;       The 32bit checksum is adopted in the FITS Checksum convention
;       http://fits.gsfc.nasa.gov/registry/checksum.html
;
; CALLING SEQUENCE:
;       CHECKSUM32, array, checksum, [/FROM_IEEE, /NoSAVE]
;
; INPUTS:
;       array - any numeric idl array.  If the number of bytes in the array is 
;               not a multiple of four then it is padded with zeros internally
;               (the array is returned unchanged).   Convert a string array 
;               (e.g. a FITS header) to bytes prior to calling CHECKSUM32.
;
; OUTPUTS:
;       checksum - unsigned long scalar, giving sum of array elements using 
;                  ones-complement arithmetic
; OPTIONAL INPUT KEYWORD:
;
;      /FROM_IEEE - If this keyword is set, then the input is assumed to be in
;           big endian format (e.g. an untranslated FITS array).   This keyword
;           only has an effect on little endian machines (e.g. Linux boxes).
;
;      /NoSAVE - if set, then the input array is not saved upon exiting.   Use 
;           the /NoSave keyword to save time if the input array is not needed 
;           in further computations. 
; METHOD:
;       Uses TOTAL() to sum the array into an unsigned integer variable.  The
;       overflow bits beyond 2^32 are then shifted back to the least significant
;       bits.    The summing is done in chunks of 2^31 numbers to avoid loss
;      of precision.    Adapted from FORTRAN code in
;      heasarc.gsfc.nasa.gov/docs/heasarc/ofwg/docs/general/checksum/node30.html
;
; RESTRICTIONS:
;       (1) Not valid for object or pointer data types
; EXAMPLE:
;       Find the 32 bit checksum of the array x = findgen(35)
;
;       IDL> checksum32, x, s    ===> s =  2920022024
; FUNCTION CALLED:
;       HOST_TO_IEEE, IS_IEEE_BIG(), N_BYTES()
; MODIFICATION HISTORY:
;       Written    W. Landsman          June 2001
;       Work correctly on little endian machines, added /FROM_IEEE and /NoSave
;                  W. Landsman          November 2002
;       Pad with zeros when array size not a multiple of 4 W.Landsman Aug 2003
;       Always copy to new array, somewhat slower but more robust algorithm
;           especially for Linux boxes   W. Landsman Sep. 2004 
;       Sep. 2004 update not implemented correctly (sigh) W. Landsman Dec 2004         
;       No need to byteswap 4 byte datatypes on little endian W. L. May 2009
;       Use /INTEGER keyword to TOTAL() function W.L. June 2009
;       
;-
 if N_params() LT 2 then begin
      print,'Syntax - CHECKSUM32, array, checksum, /FROM_IEEE, /NoSAVE'
      return
 endif
 idltype = size(array,/type)

; Convert data to byte.  If array size is not a multiple of 4, then we pad with
; zeros 

 N = N_bytes(array)
 Nremain = N mod 4
 if Nremain GT 0 then begin 
     if keyword_set(nosave) then $
           uarray = [ byte(temporary(array),0,N), bytarr(4-Nremain)]  $
           else uarray =  [ byte(array,0,N), bytarr(4-Nremain)] 
      N = N + 4 - Nremain 
 endif else  begin 
      if keyword_set(nosave) then $
           uarray =  byte( temporary(array) ,0,N) else $
           uarray =  byte( array ,0,N) 
 endelse
 	    
; Get maximum number of base 2 digits available in an unsigned long array, 
; without losing any precision.    Since we will sum unsigned longwords, the 
; original  array must be byteswapped as longwords.

 maxnum = long64(2)^31       
 Niter =  (N-1)/maxnum
 checksum = long64(0)
  word32 =  long64(2)^32
  bswap  = ~is_ieee_big()
  if bswap then begin
       if ~keyword_set( from_ieee) then begin 
            if (idltype NE 3) && (idltype NE 4) then begin 
	         if idltype NE 1 then host_to_ieee, uarray,idltype=idltype   
                 byteorder,uarray,/NTOHL
	   endif	 
       endif else byteorder,uarray,/NTOHL	     
 endif
 
 for i=0, Niter do begin

   if i EQ Niter then begin 
           nbyte = (N mod maxnum) 
           if nbyte EQ 0 then nbyte = maxnum
   endif else nbyte = maxnum

   checksum += total(ulong(  uarray,maxnum*i,nbyte/4), /integer)
; Fold any overflow bits beyond 32 back into the word.

   hibits = long(checksum/word32)
   while hibits GT 0 do begin
     checksum = checksum - (hibits*word32) + hibits    
     hibits = long(checksum/word32)
  endwhile

   checksum = ulong(checksum)

 endfor

 return
 end
