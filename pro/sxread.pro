function sxread,unit,group,par
;+
; NAME:
;	SXREAD
; PURPOSE:
;	Read a Space Telescope STSDAS image file     
;
; CALLING SEQUENCE:
;	result = sxread( Unit, group , [par] )
;
; INPUTS:
;	UNIT  =  Unit number of file, must be from 1 to 9.
;		Unit must have been opened with SXOPEN.
;	GROUP  =  group number to read.  if omitted, read first record.
;		The first record is number 0.
; OUTPUTS:
;	Result of function  =  array constructed from designated record.
;
; OPTIONAL OUTPUT:
;	PAR  =  Variable name into which parameter values from STSDAS
;		group parameter block are read.  It is a byte array
;		which may contain multiple data types.  The function
;		SXGPAR can be used to retrieve values from it.
;
; COMMON BLOCKS:
;	Uses IDL Common STCOMMN to access parameters.
;
; NOTES:
;	Use the function SXGREAD to read the group parameter blocks without
;	having to read the group array.
;
;	If the STSDAS file does not contain groups, then the optional output
;	parameter PAR is returned undefined, but no error message is given.
;
; SIDE EFFECTS:
;	IO is performed. 
; MODIFICATION HISTORY:
;	WRITTEN, Don Lindler, July, 1 1987
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
 On_error,2

; common block containing description of file (see SXOPEN)

	common stcommn,result,filename

; check if unit open

 if ( unit LT 1 ) or ( unit GT 9 ) then $
     message,'Invalid unit number, must be between 1 and 9'

 if N_elements(result) EQ 0 then result = 0

 if ( N_elements(result) NE 200 ) or ( result[0,unit] NE 121147 ) then $
        message,'Specified unit is not open'

 desc = result[*,unit]				;description for unit

; default group number is 0 (first group)

 if N_params() eq 1 then group = 0

; read group parameters if requested

 if (N_params() GT 2) and ( desc[7] GT 0 ) then begin
	parrec = assoc(UNIT, bytarr(desc[7]),(group+1)*desc[9]-desc[7])
	par = parrec[0]
 end

; read data with dimensions specified in desc.

 ndimen = desc[3]
 dtype  =  desc[8]
 dimen = desc[10:9+ndimen]
 sbyte = long(group)*desc[9]

 rec  =  assoc(unit,make_array(size=[ndimen,dimen>1,dtype,0],/nozero),sbyte)

 return,rec[0]

 end
