pro mrd_skip, unit, nskip
;+
; NAME:
;       MRD_SKIP
; PURPOSE:
;       Skip a number of bytes from the current location in a file or a pipe
; EXPLANATION:
;       First tries using POINT_LUN and if this doesn't work, perhaps because
;       the unit is a pipe or a socket, MRD_SKIP will just read in the 
;       requisite number  of bytes.    
; CALLING SEQUENCE:
;       MRD_SKIP, Unit, Nskip
;
; INPUTS:
;       Unit - File unit for the file or pipe in question, integer scalar
;       Nskip - Number of bytes to be skipped, positive integer
; NOTES:
;       This routine should be used in place of POINT_LUN wherever a pipe
;       or socket may be the input unit (see the procedure FXPOSIT for an 
;       example).   Note that it assumes that it can only work with nskip >= 0 
;       so it doesn't even try for negative values.      
;
;       For reading a pipe, MRD_SKIP currently uses a maximum buffer size
;       of 8 MB.   This chunk value can be increased for improved efficiency
;       (or decreased if you really have little memory.)
; REVISION HISTORY:
;       Written, Thomas A. McGlynn    July 1995
;	Don't even try to skip bytes on a pipe with POINT_LUN, since this
;	might reset the current pointer     W. Landsman        April 1996
;       Increase buffer size, check fstat.compress W. Landsman  Jan 2001
;       Only a warning if trying read past EOF   W. Landsman   Sep 2001
;       Use 64bit longword for skipping in very large files W. Landsman Sep 2003
;       Assume since V5.4, fstat.compress available W. Landsman April 2006
;       POINT_LUN for compressed files is as fast as any W. Landsman Oct 2006
;       Don't try to use POINT_LUN on compressed files W. Landsman Dec. 2006
;       
;-
        On_error,2

	if nskip le 0 then return
        compress = (fstat(unit)).compress

; We try to use POINT_LUN but if an error ocurrs, we just read in the bytes 

          if ~compress then begin
 	  On_IOerror, byte_read
	  point_lun, -unit, curr_pos
	  On_IOerror, null
          if curr_pos NE -1 then point_lun, unit, long64(curr_pos) + nskip
           return
	  endif 

; Otherwise, we have to explictly read the number of bytes to skip
; If the number is very large we don't want to create a array so skip
; in chunks of 8 Megabyte

byte_read:

        chunk = 8000000L
	buf = bytarr(nskip<chunk, /nozero)
	nleft = nskip
	on_ioerror, DONE
	while (nleft gt 0) do begin
		readu, unit, buf
		nleft = nleft - chunk
	        if (nleft gt 0) && (nleft lt chunk) then buf = buf[0:nleft-1]	
	endwhile
	return
DONE:  message,'Warning - Byte padding in FITS file may not be correct',/CON
       return		
end

