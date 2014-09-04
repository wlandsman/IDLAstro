function sxgread,unit,group
;+
; NAME:
;	SXGREAD
; PURPOSE:
;	Read group parameters from a Space Telescope STSDAS image file     
;
; CALLING SEQUENCE:
;	grouppar = sxgread( unit, group )
;
; INPUTS:
;	UNIT   = Supply same unit as used in SXOPEN.
;	GROUP  =  group number to read.  if omitted, read first group.
;		The first group is number 0.
;
; OUTPUTS:
;	GROUPPAR  =  parameter values from fits group parameter block.
;		It is a byte array which may contain multiple data types.
;		The function SXGPAR can be used to retrieve values from it.
;
; COMMON BLOCKS:
;	Uses IDL Common STCOMMN to access parameters.
; SIDE EFFECTS:
;	IO is performed. 
; MODIFICATION HISTORY:
;	WRITTEN, Don Lindler, July, 1 1987
;	MODIFIED, Don Neill, Jan 11, 1991 - derived from sxread.pro
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
 On_error,2
;
; common block containing description of file (see SXOPEN)
;
	common stcommn,result,filename
;
; check if unit open
;
 if (unit lt 1) or (unit gt 9) then $
     message,'Invalid unit number, must be between 1 and 9'
 if N_elements(result) eq 0 then result = 0
 if (N_elements(result) ne 200) or (result[0,unit] ne 121147) then $
        message,'Specified unit is not open'
 desc = result[*,unit]				;description for unit
;
; default group number is 0 (first group)
;
 if N_params() eq 1 then group = 0
;
; read group parameters
;
 parrec = assoc(UNIT,bytarr(desc[7]),(group+1)*desc[9]-desc[7])
 par = parrec[0]
;
 return,par
 end
