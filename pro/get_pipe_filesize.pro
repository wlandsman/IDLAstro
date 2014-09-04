pro get_pipe_filesize, unit, nbytes, buffer = buffer
;+
; NAME:
;       GET_PIPE_FILESIZE
;
; PURPOSE:
;       Determine the number of bytes in a unit opened as a pipe with SPAWN
;
; EXPLANATION:
;      Reads into a buffer until the end of file is reached and then counts the
;      number of bytes read.   Needed because the fstat.size field is not 
;      automatically set for a unit opened as a pipe.
;
; CALLING SEQUENCE:
;       GET_PIPE_FILESIZE,unit, nbytes_in_file, BUFFER = 
;
; INPUTS:
;       unit -   IDL unit number of a previously opened file.    For example,
;         an FPACK  ( http://heasarc.gsfc.nasa.gov/fitsio/fpack/ ) compressed 
;         FITS file could be opened as follows:
;
;        IDL> spawn,'funpack -S test.fits.fz', unit=unit
; OUTPUTS:
;       nbytes_in_file - Unsigned long64 integer giving number of bytes in 
;         the file.
;
; INPUT KEYWORD PARAMETERS:
;       BUFFER  Integer giving number of bytes in the buffer.  Default = 
;     .          1000000
; NOTES:
;      Unite must be opened prior to calling GET_PIPE_FILESIZE, and the number
;      of bytes is counted from the current pointer position.  The pointer is 
;      left at the end of the file upon return. 
; PROCEDURES USED:
;     SETDEFAULTVALUE  
; REVISION HISTORY:
;      Written, W. Landsman  Adnet   Dec 2010 

 On_error,2
 compile_opt idl2

 nbytes = 0ULL
 setdefaultvalue, buffer, 1000000
 ON_IOerror,Done
 b= bytarr(buffer,/noz)

 while 1 do begin
   readu,unit,b
   nbytes += buffer
 endwhile

Done:
 On_IOError, null
 nbytes += (fstat(unit)).transfer_count
 
  return
 end
