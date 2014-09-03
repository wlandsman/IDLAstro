pro fthmod,h,field,parameter,value
;+
; NAME:
;       FTHMOD
; PURPOSE:
;       Procedure to modify header information for a specified field
;       in a FITS table.
;
; CALLING SEQUENCE:
;       fthmod, h, field, parameter, value
;       
; INPUT:
;       h - FITS header for the table
;       field - field name or number
;       parameter - string name of the parameter to modify.  Choices
;               include:
;                       TTYPE - field name
;                       TUNIT - physical units for field (eg. 'ANGSTROMS')
;                       TNULL - null value (string) for field, (eg. '***')
;                       TFORM - format specification for the field
;                       TSCAL - scale factor
;                       TZERO - zero offset
;               User should be aware that the validity of the change is
;               not checked.  Unless you really know what you are doing,
;               this routine should only be used to change field names,
;               units, or another user specified parameter.
;       value - new value for the parameter.  Refer to the FITS table
;               standards documentation for valid values.
;
; EXAMPLE:
;      Change the units for a field name "FLUX" to "Janskys" in a FITS table
;        header,h
;
;      IDL> FTHMOD, h, 'FLUX', 'TUNIT','Janskys' 
; METHOD:
;       The header keyword <parameter><field number> is modified
;       with the new value.
; HISTORY:
;       version 1, D. Lindler  July 1987
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Major rewrite to use new FTINFO call   W. Landsman   May 2000
;-
;-----------------------------------------------------------------------
on_error,2

 ftinfo,h,ft_str
 sz = size(field)
 if ((sz[0] ne 0) or (sz[1] EQ 0)) then $
      message,'Invalid field specification, it must be a scalar'

 if sz[1] EQ 7 then begin
    field = strupcase(strtrim(field,2))
    ttype = strtrim(ft_str.ttype,2)
    ipos = where(ttype EQ field, Npos)
    if Npos EQ 0 then message, $ 
        'Specified field ' + strupcase(strtrim(field,2)) + ' not in table'
 endif else ipos = field -1

;
 par = parameter+strtrim(ipos[0]+1,2)
 sxaddpar,h,par,value
return
end
