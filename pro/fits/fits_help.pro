pro fits_help,file_or_fcb
;+
; NAME:
;       FITS_HELP
;
; PURPOSE:
;       To print a summary of the primary data units and extensions in a
;       FITS file.
;;
; CALLING SEQUENCE:
;       FITS_HELP,filename_or_fcb
;
; INPUTS:
;       FILENAME_OR_FCB - name of the fits file or the FITS Control Block (FCB)
;               structure returned by FITS_OPEN.     The  file name is allowed 
;               to be gzip compressed (with a .gz  extension)
;
; OUTPUTS:
;       A summary of the FITS file is printed.   For each extension, the values
;       of the XTENSION, EXTNAME EXTVER EXTLEVEL BITPIX GCOUNT, PCOUNT NAXIS 
;       and NAXIS* keywords are displayed. 
; 
;
; EXAMPLES:
;       FITS_HELP,'myfile.fits'
;
;       FITS_OPEN,'anotherfile.fits',fcb
;       FITS_HELP,fcb
;
; PROCEDURES USED:
;       FITS_OPEN, FITS_CLOSE
; HISTORY:
;       Written by:     D. Lindler      August, 1995
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Don't truncate EXTNAME values at 10 chars  W. Landsman Feb. 2005
;       Use V6.0 notation W. Landsman Jan 2012
;-
;-----------------------------------------------------------------------------
 compile_opt idl2
;
; print calling sequence
;
        if N_params() eq 0 then begin
          print,'Syntax -  FITS_HELP,file_or_fcb'
          return
        endif
;
; Open file if file name is supplied
;
        fcbtype = size(file_or_fcb,/type) 
        fcbsize = n_elements(file_or_fcb)
        if (fcbsize ne 1) || ((fcbtype ne 7) && (fcbtype ne 8)) then begin
                message, 'Invalid Filename or FCB supplied',/con
                return
        end

        if fcbtype eq 7 then fits_open,file_or_fcb,fcb $
                        else fcb = file_or_fcb
                        
; EXTNAME will always be displayed with a length of at least 10 characters
; but allow for possibility that lengths might be longer than this 

        maxlen = max(strlen(fcb.extname)) > 10 
        if maxlen EQ 10 then space = '' else $
            space = string(replicate(32b, maxlen -10))                  
;
; print headings
;
        print,' '
        print,FCB.FILENAME
        print,' '
        print,'     XTENSION  EXTNAME  '+ space + $
              'EXTVER EXTLEVEL BITPIX GCOUNT  PCOUNT NAXIS  NAXIS*'
        print,' '
;
; loop on extensions
;
        for i=0,fcb.nextend do begin
                st = string(i,'(I4)')
;
; xtension, extname, extver, extlevel (except for i=0)
;
                if i gt 0 then begin
                        t = fcb.xtension[i]
                        while strlen(t) lt 8 do t += ' '
                        st +=  ' '+ strmid(t,0,8)
                        t = fcb.extname[i]
                        while strlen(t) lt maxlen do t += ' '
                        st += ' '+ strmid(t,0,maxlen)               
                        t = fcb.extver[i]
                        if t eq 0 then st += '     ' $
                                  else st += string(t,'(I5)')
                        t = fcb.extlevel[i]
                        if t eq 0 then st +=  '        ' $
                                  else st += string(t,'(I8)')
                end else st += '                                 ' + space
;
; bitpix, gcount, pcount, naxis
;
                st += string(fcb.bitpix[i],'(I6)')
                st += string(fcb.gcount[i],'(I7)')
                st += string(fcb.pcount[i],'(I7)')
                st += string(fcb.naxis[i],'(I6)')
;
; naxis*
;
                st += '  '
                if fcb.naxis[i] gt 0 then begin
                    nax1 = fcb.naxis[i] - 1
                    st += strjoin(strtrim(fcb.axis[0:nax1,i],2),' x ')
                endif
;
; print the info
;
                print,st
        end
        if fcbtype eq 7 then fits_close,fcb
return
end
