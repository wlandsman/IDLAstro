function sxgpar,h,par,name,type,sbyte,nbytes
;
;+
; NAME:
;	SXGPAR                           
;
; PURPOSE:
;	Obtain group parameter value in SDAS/FITS file
;
; CALLING SEQUENCE:
;	result = sxgpar( h, par, name, [ type, sbyte, nbytes] )
;
; INPUTS:
;	h - header returned by SXOPEN
;	par - parameter block returned by SXREAD or multiple
;		parameter blocks stored in array of dimension
;		greater than one.
;	name - parameter name (keyword PTYPEn) or integer
;		parameter number.
;
; OPTIONAL INPUT/OUTPUT
;	type - data type (if not supplied or null string, the
;		header is searched for type,sbyte, and nbytes)
;	sbyte - starting byte in parameter block for data
;	nbytes - number of bytes in parameter block for data
;
; OUTPUT:
;	parameter value or value(s) returned as function value
;
; SIDE EFFECTS:
;	If an error occured then !err is set to -1
;
; OPERATIONAL NOTES:
;	Supplying type, sbyte and nbytes greatly decreases execution
;	time.  The best way to get the types is on the first call
;	pass undefined variables for the three parameters or set
;	type = ''.  The routine will then return their values for
;	use in subsequent calls.
;	
; METHOD:
;	The parameter type for parameter n is obtained
;	from PDTYPEn keyword.  If not found then DATATYPE keyword
;	value is used.  If that is not found then BITPIX is
;	used.  BITPIX=8, byte; BITPIX=16 integer*2; BITPIX=32
;	integer*4.
;
; HISTORY:
;	version 1  D. Lindler  Oct. 86
;	version 2  D. Lindler Jan. 90  added ability to process
;		multiple parameter blocks in single call
;	version 3  D. Lindler  (converted to New vaxidl)
;       Apr 14 1991      JKF/ACC - fixed make_array datatypes(float/double)
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;------------------------------------------------------------
 On_error,2

 if N_params() lt 3 then $
    message,'Syntax - result = sxgpar( h, par, name, [ type, sbyte, nbytes ])'
;
; determine size of output result
;
	s = size(par)
	ndim = s[0]
	dtype = s[ndim+1]
	case 1 of
	    (ndim eq 0) or (dtype ne 1) : begin
			print,'SXGPAR - invalid parameter block specified'	
			return,0
			end
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
; check if type, sbyte and nbytes supplied
;
	if n_elements(type) ne 0 then if strtrim(type) ne '' then goto,bypass
;
; check remaining input parameters
;
	s=size(h)
	!err=-1
	if (s[0] ne 1) or (s[2] ne 7) then begin
		print,'SXGPAR -- Header array must be string array'
		return,0
	end
	if strlen(h[0]) ne 80 then begin
		print,'SXGPAR -- header must contain 80 character strings'
		return,0
	end
;
	if n_elements(name) eq 0 then begin
		print,'SXGPAR -- parameter name must be a scalar'
 		return,0
	endif
;
; get number of group parameters and size
;
;
	pcount=sxpar(h,'PCOUNT')	;get number of group parameters
	if pcount eq 0 then begin
		print,'sxgpar -- No group parameters present'
		return,0
	endif
	psize=sxpar(h,'PSIZE')	;number of bits in parameter block
	if psize eq 0 then psize=sxpar(h,'BITPIX')*pcount
;
; determine if name supplied or parameter number
;
	s=size(name)
	if s[1] eq 7 then begin	;is it a string?
		nam=strtrim(strupcase(name)) ;convert to upper case and trim
;
; search for parameter name
;
		for i=1,pcount do begin
			if strtrim(sxpar(h,'PTYPE'+strtrim(i,2))) eq nam then $
								goto,found
		endfor
		!err=-1
		print,'SXGPAR -- group parameter ',name,' not found'
		return,0
found:
		ipar=i
	    end else begin		;integer
		ipar=fix(name)
		if ipar gt pcount then begin
			!err=-1
			print,'SXGPAR -- parameter number',name,' is too large'
			print,'       -- only ',pcount,' group parameters'
			return,0
		endif
	endelse
;
; find starting position of parameter in parameter block
;
	nbits=0		;number of bits to skip
	if ipar gt 1 then begin
		for i=1,ipar-1 do begin
			nbit=sxpar(h,'PSIZE'+strtrim(i,2))
			if !err lt 0 then nbit=sxpar(h,'bitpix')
			nbits=nbits+nbit
		endfor
	endif
	sbyte=nbits/8		;number of bytes to skip
;
; determine type of output data
;
	charn=strtrim(ipar,2)	;convert ipar to string
	type=strtrim(sxpar(h,'pdtype'+charn))
	if !err lt 0 then type=strtrim(sxpar(h,'datatype'))
	if !err lt 0 then begin
		case sxpar(h,'bitpix') of
			8: type = 'BYTE'
			16: type = 'INTEGER*2'
			32: type = 'INTEGER*4'
                       -32: type = 'REAL*4'
		endcase
	endif
;
; get number of bytes from type
;
	aster=strpos(type,'*')
	if aster gt 0 then $
		nbytes=fix(strmid(type,aster+1,strlen(type)-aster-1)) $
		else nbytes=4

BYPASS:		
;-------------------------------------------------------------
;
; get first character of type
;
	c=strupcase(strmid(type,0,1))
;
; create output vector
;
	if c eq 'L' then c = 'I'	;change LOGICAL to INTEGER
	case c of
		'R' : if nbytes eq 8 then $
			val = make_array(dimension=dimen,/double) $
			else val = make_array(dimension=dimen,/float)
		'I' : case nbytes of
			1: val=make_array(dimension=dimen,/byte)
			2: val=make_array(dimension=dimen,/int)
			4: val=make_array(dimension=dimen,/long)
		      endcase
		'B' : val = make_array(dimension=dimen,/byte)
		'C' : val = make_array(dimension=dimen,/string)
		else: begin
			print,'sxgpar -- unsupported group parameter data type'
			!err=-1
			return,0
		      end
	endcase
	nval = n_elements(val)
;
; extract data
;
	for i=0,nval-1 do begin
	    ssbyte = sbyte + plen*i
   	    case c of
		'R' : begin
			if nbytes eq 4 then val[i]=float(par,ssbyte)
			if nbytes eq 8 then val[i]=double(par,ssbyte)
		      end
		'I' : begin
			if nbytes eq 1 then val[i]=byte(par,ssbyte)
			if nbytes eq 2 then val[i]=fix(par,ssbyte)
			if nbytes eq 4 then val[i]=long(par,ssbyte)
		      end
		'B' :val=byte(par,ssbyte,1)
		'C' : begin
			val[i]=string(byte(par,ssbyte,nbytes))
		      end
	    endcase
	endfor
;
	if scalar then val=val[0]
	!err=0
	return,val
end
