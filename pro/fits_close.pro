pro fits_close,fcb,no_abort=no_abort,message=message
;+
; NAME:
;      FITS_CLOSE
;
;*PURPOSE:
;       Close a FITS data file
;
;*CATEGORY:
;       INPUT/OUTPUT
;
;*CALLING SEQUENCE:
;       FITS_CLOSE,fcb
;
;*INPUTS:
;       FCB: FITS control block returned by FITS_OPEN.
;
;*KEYWORD PARAMETERS:
;       /NO_ABORT: Set to return to calling program instead of a RETALL
;               when an I/O error is encountered.  If set, the routine will
;               return  a non-null string (containing the error message) in the
;               keyword MESSAGE.   If /NO_ABORT not set, then FITS_CLOSE will 
;               print the message and issue a RETALL
;       MESSAGE = value: Output error message
;       
;*EXAMPLES:
;       Open a FITS file, read some data, and close it with FITS_CLOSE
;
;               FITS_OPEN,'infile',fcb
;               FITS_READ,fcb,data
;               FITS_READ,fcb,moredata
;               FITS_CLOSE,fcb
;
;*HISTORY:
;       Written by:     D. Lindler      August, 1995
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Do nothing if fcb an invalid structure D. Schlegel/W. Landsman Oct. 2000
;       Return Message='' for to signal normal operation W. Landsman Nov. 2000
;-
;----------------------------------------------------------------------------
;
; print calling sequence if no parameters supplied
;
        if N_params() lt 1 then begin
                print,'Syntax -  FITS_CLOSE, fcb'
                print,'KEYWORD PARAMETERS: /No_abort, message='
                return
        end
;
; close unit
;
        on_ioerror,ioerror
        message = ''

        sz_fcb = size(fcb)             ;Valid structure?
        if sz_fcb[2] EQ 8 then free_lun,fcb.unit
        return
;
; error exit (probably should never occur)
;
ioerror:
        message = !error_state.msg
         if keyword_set(no_abort) then return
        message,' ERROR: '+message,/CON
        retall
end
