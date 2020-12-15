pro sxhwrite,name,h
;+
; NAME:
;       SXHWRITE
; PURPOSE:
;       Procedure to write an STSDAS or FITS header to disk as a *.hhh file.
;
; CALLING SEQUENCE:
;       SXHWRITE,name,h
;
; INPUTS:
;       name - file name. If an extension is supplied it must be 3 characters
;               ending in "h".
;       h - FITS header, string array
;
; SIDE EFFECTS:
;       File with specified name is written.  If qualifier not specified
;       then .hhh is used
;   
;       SXHWRITE will modify the header in the following ways, if necessary
;       (1)  If not already present, an END statement is added as the 
;               last line.   Lines after an existing END statment are
;               deleted.
;       (2)  Spaces are appended to force each line to be 80 characters.
;       (3)  On Unix machines, a carriage return is appended at the end
;               of each line.   This is consistent with STSDAS and allows
;               the file to be directly displayed on a stream device
;
; PROCEDURES USED:
;       zparcheck, fdecomp
; HISTORY:
;       version 1  D. Lindler  June 1987
;       conversion cleaned up.  M. Greason, June 1990
;       Add carriage return at the end of Unix files   W. Landsman Oct 1991
;       Use SYSTIME() instead of !STIME for V5.0 compatibility Aug 1997
;       Assume since V55, remove VMS support
;-
;----------------------------------------------------------------
 compile_opt idl2
 On_error,2 
 if N_params() LT 2 then begin
    print,'Syntax - SXHWRITE, name, hdr'
    return
 endif

; Create output file name

 ZPARCHECK, 'SXHWRITE', name, 1, 7, 0, 'Disk file name'  ;Check for valid param
 FDECOMP,name, disk, dir, file, qual
 if ( qual EQ '' ) then qual = 'hhh'                    ;default qualifier

; Check for valid qualifier

 if ( strlen(qual) NE 3 ) || ( strupcase(strmid(qual,2,1)) NE 'H' ) then $
        message,'Qualifier on file name must be 3 characters, ending in h'

 hname = disk + dir + file + '.' + qual           ;header file name

; Check that valid FITS header was supplied

 ZPARCHECK, 'SXHWRITE', h, 2, 7, 1, 'FITS header'

 sxdelpar,h,'XTENSION'       ;For SDAS header SIMPLE must be the first line
 SXADDPAR, h, 'SIMPLE', 'F', ' Written by IDL:  ' + systime()

; Determine if an END line occurs, and add one if necessary

 endline = where( strtrim( strmid(h,0,8), 2) EQ 'END', Nend)
 if Nend EQ 0 then begin

    message, /INF, $
        'WARNING - An END statement has been appended to the FITS header'
    h = [ h, 'END' + string( replicate(32b,77) ) ]
    endline = N_elements(h) - 1 

 endif
 nmax = endline[0] + 1

; Convert to byte and force into 80 character lines

 temp = replicate( 32b, 80, nmax)
 for n = 0, endline[0] do temp[0,n] = byte( h[n] )

; Under Unix append a carriage return ( = string(10b) )

 temp = [ temp, rotate( replicate(10b,nmax), 1 ) ]

; Open the output file and write as byte array.

 openw, unit, hname, 80, /GET_LUN
 writeu, unit, temp
 free_lun,unit

 return
 end
