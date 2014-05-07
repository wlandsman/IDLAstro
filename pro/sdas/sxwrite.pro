pro SXWRITE, Unit, Data, Par
;+
; NAME:
;	SXWRITE
; PURPOSE:
;	Write a group of data and parameters in ST format
;	to a STSDAS data file.
;
; CALLING SEQUENCE:
;	SXWRITE, Unit, Data,[ Par]
;
; INPUTS:
;	Unit = unit number of file.  The file must have been
;		previously opened by SXOPEN.
;	Data = Array of data to be written.  The dimensions
;		must agree with those supplied to SXOPEN and written
;		into the FITS header.  The type is converted if
;		necessary.
;
; OPTIONAL INPUT PARAMETERS:
;	Par = parameter block.  The size of this array must
;		agree with the Psize parameter in the FITS header.
;
; OUTPUTS:
;	None.
; COMMON BLOCKS:
;	STCOMMN - Contains RESULT(20,10) where RESULT(i,LUN) =
;	0 - 121147 for consistency check, 1 - Unit for consistency,
;	2 - bitpix, 3 - naxis, 4 - groups (0 or 1), 5 - pcount,
;	6 - gcount, 7 - psize, 8 - data type as idl type code,
;	9 - bytes / record, 10 to 10+N-1 - dimension N,
;	18 - # of groups written, 19 = gcount.
;
; SIDE EFFECTS:
;	The data are written into the next group.
;
; RESTRICTIONS:
;	SXOPEN must have been called to initialize the
;	header and the common block.
;
; MODIFICATION HISTORY:
;	DMS, July, 1983.
;	D.Lindler July, 1986 - changed block size of file to 512
;			moved group parameters after the groups data.
;	D.Lindler July, 1987 - modified to allow any size parameter block
;			(in bytes).
;	D. Lindler  April, 1990 - converted to new VMS IDL
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;----------------------------------------------------------------------------
;
	common stcommn, result, filename
	if N_params() LT 2 then begin
		print,'Syntax - SXWRITE, Unit, Data,[ Par]
		return
        endif	
;
	if N_elements(result) ne 200 then begin
		print,'SXWRITE - Sxopen not called'
		return
		endif
	if result[1,unit] ne unit then begin
		print,'SXWRITE - unit not opened with SXOPEN'
		return
		endif
;
	on_error,2			;return to caller on error
	s = size(data)			;get data dims
;
; determine position in file to write
;
	start=result[18,unit]*result[9,unit]
;
; create assoc variable for data
;
	rec = assoc(unit,data,start)
;
; write data
;
	rec[0]=data
;
; write pblk
;
	if result[7,unit] gt 0 then begin
		if n_params(0) lt 3 then par=bytarr(result[7,unit])
		p=byte(par,0,result[7,unit])
		rec=assoc(unit,p,start+result[9,unit]-result[7,unit])
		rec[0]=p
	end
	result[18,unit] = result[18,unit]+1 ;did one more group
	return
end
