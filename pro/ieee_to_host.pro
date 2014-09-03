pro ieee_to_host, data, IDLTYPE = idltype
;+
; NAME:
;     IEEE_TO_HOST
; PURPOSE:
;     Translate an IDL variable from IEEE-754 to host representation 
; EXPLANATION:
;     The variable is translated from IEEE-754 ("big-endian" as used, for
;     example, in FITS data ), into the host machine architecture.
;
;     Duplicates most of the functionality of the SWAP_ENDIAN_INPLACE procedure
;     introduced in V5.6, with the addition of the IDLTYPE keyword.
; CALLING SEQUENCE:
;     IEEE_TO_HOST, data, [ IDLTYPE = , ]
;
; INPUT-OUTPUT PARAMETERS:
;     data - any IDL variable, scalar or vector.   It will be modified by
;             IEEE_TO_HOST to convert from IEEE to host representation.  Byte 
;             and string variables are returned by IEEE_TO_HOST unchanged
;
; OPTIONAL KEYWORD INPUTS:
;     IDLTYPE - scalar integer (1-15) specifying the IDL datatype according
;               to the code given by the SIZE function.     This keyword
;               is usually when DATA is a byte array to be interpreted as
;               another datatype (e.g. FLOAT).
;
; EXAMPLE:
;       A 2880 byte array (named FITARR) from a FITS record is to be 
;       interpreted as floating and converted to the host representaton:
;
;       IDL> IEEE_TO_HOST, fitarr, IDLTYPE = 4     
;
; METHOD:
;       The BYTEORDER procedure is called with the appropriate keyword
;
; MODIFICATION HISTORY:
;      Written, W. Landsman   Hughes/STX   May, 1992
;      Under VMS check for IEEE -0.0 values   January 1998
;      VMS now handle -0.0 values under IDL V5.1    July 1998
;      Added new integer datatypes  C. Markwardt/W. Landsman  July 2000
;      Post-V5.1 version, no VMS negative zero check  W. Landsman July 2001
;      Use size(/type)  W. Landsman December 2002
;      Use /SWAP_IF_LITTLE_ENDIAN keyword for 64bit types W. Landsman Feb 2003
;      Do not use XDR keywords to BYTEORDER for much improved speed
;                               W. Landsman   April 2006
;      Update cosmetic typo for structures W. Landsman  October 2006
;-
 On_error,2 

 if N_params() EQ 0 then begin
    print,'Syntax - IEEE_TO_HOST, data, [ IDLTYPE = ]'
    return
 endif  

 npts = N_elements( data )
 if npts EQ 0 then $
     message,'ERROR - IDL data variable (first parameter) not defined'

 if N_elements(idltype) EQ 0 then idltype = size(data,/type)
 
 case idltype of

      1: return                             ;byte
      
            2: byteorder, data, /SSWAP,/SWAP_IF_LITTLE            ;integer

      3: byteorder, data, /LSWAP,/SWAP_IF_LITTLE            ;long

      4: byteorder, data, /LSWAP, /SWAP_IF_LITTLE           ;float

      5: byteorder,data,/L64SWAP, /SWAP_IF_LITTLE              ;double
 
      6: byteorder, data, /LSWAP, /SWAP_IF_LITTLE
     
      7: return                             ;string

      8: BEGIN                              ;structure

        Ntag = N_tags( data )

        for t=0,Ntag-1 do  begin
          temp = data.(t)
          ieee_to_host, temp
          data.(t) = temp
        endfor 
       END

     9: byteorder, data, /L64SWAP, /SWAP_IF_LITTLE
 
     12: byteorder, data, /SSWAP, /SWAP_IF_LITTLE

     13: byteorder, data, /LSWAP, /SWAP_IF_LITTLE

     14: byteorder, data, /L64swap, /SWAP_IF_LITTLE

     15: byteorder, data, /L64swap, /SWAP_IF_LITTLE

     else: message,'Unrecognized datatype ' + strtrim(idltype,2)

 ENDCASE


 return
 end 
