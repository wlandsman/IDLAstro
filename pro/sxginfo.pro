pro sxginfo,h,par,type,sbyte,nbytes
;+
; NAME:
;	SXGINFO
;
; PURPOSE:
;	Return information on all group parameters in an STSDAS header.
; EXPLANATION:
;	Return datatype, starting byte, and number bytes for all group
;	parameters in an STSDAS file.     Obtaining these values 
;	greatly speed up execution time in subsequent calls to SXGPAR.
;
; CALLING SEQUENCE:
;	sxginfo, h, par, type, sbyte, nbytes
;
; INPUTS:
;	h - header returned by SXOPEN
;	par - parameter block returned by SXREAD or multiple
;		parameter blocks stored in array of dimension
;		greater than one.
;
; OUTPUT:
;	type - data type (if not supplied or null string, the
;		header is searched for type,sbyte, and nbytes)
;	sbyte - starting byte in parameter block for data
;	nbytes - number of bytes in parameter block for data
;
;	The number of elements in type,sbyte and nbytes equals the total
;	number of group parameters.
;
; METHOD:
;	The parameter type for each parameter is obtained
;	from PDTYPEn keyword.  If not found then DATATYPE keyword
;	value is used.  If that is not found then BITPIX is
;	used.  BITPIX=8, byte; BITPIX=16 integer*2; BITPIX=32
;	integer*4.
;
; NOTES:
;	For an example of the use of SXGINFO, see CONV_STSDAS
;
; HISTORY:
;	version 1  W. Landsman   Apr. 93
;
;	Converted to IDL V5.0   W. Landsman   September 1997
;------------------------------------------------------------
 On_error,2

 if N_params() LT 3 then begin
	print,'Syntax - sxginfo,h,par,type,sbyte,nbytes'
	return
 endif

; determine size of output result

	s = size(par)
	ndim = s[0]
	dtype = s[ndim+1]
	case 1 of
	    (ndim eq 0) or (dtype ne 1) : message, $
			'Invalid parameter block specified'	

	    ndim eq 1 : begin
			scalar = 1	; output will be scalar
			dimen = intarr(1)+1
			end
	    else: begin
			scalar = 0	; output will be vector
			dimen = s[2:ndim]
		 	end
	endcase
	plen = s[1]		;length of parameter blocks
;
; check remaining input parameters
;
	s=size(h)
	!err=-1
	if (s[0] ne 1) or (s[2] ne 7) then message, $
		'Header array must be string array'

	if strlen(h[0]) ne 80 then message, $
		'Header must contain 80 character strings'
;
; get number of group parameters and size
;
;
	pcount = sxpar(h,'PCOUNT')	;get number of group parameters
	if pcount eq 0 then begin
		message,'No group parameters present',/INFO
		return
	endif

        sbyte = intarr(pcount)
        nbytes = intarr(pcount)
        type = strarr(pcount)

; Determine BITPIX and DATATYPE in case PSIZE or PDTYPE is undefined

	nbits=0		;number of bits to skip
	dtype = strtrim(sxpar(h, 'DATATYPE') )
	bitpix = sxpar(h,'BITPIX')
	if !err lt 0 then begin
		case bitpix of
			8: dtype = 'BYTE'
			16: dtype = 'INTEGER*2'
			32: dtype = 'INTEGER*4'
                       -32: dtype = 'REAL*4'
		       -64: dtype = 'REAL*8'	
		endcase
	endif

	for i = 1,pcount do begin
		nbit = sxpar(h,'PSIZE'+strtrim(i,2))
		if !err lt 0 then nbit = bitpix
		nbits=nbits+nbit
		if i NE pcount then sbyte[i]=nbits/8   ;number of bytes to skip
		pdtype = strtrim(sxpar(h,'PDTYPE' + strtrim(i,2)))
		if !ERR LT 0 then pdtype = dtype
		type[i-1] = pdtype
		aster = strpos(pdtype,'*')
		if aster gt 0 then $
		nbytes[i-1]=fix(strmid(pdtype,aster+1,strlen(pdtype)-aster-1)) $
		else nbytes[i-1]=4
	endfor

  return
  end
