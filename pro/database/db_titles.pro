pro db_titles,fnames,titles
;+
; NAME:
;	DB_TITLES
;
; PURPOSE:
;	Print database name and title.  Called by DBHELP
;
; CALLING SEQUENCE:
;	db_titles, fnames, titles
;
; INPUT:
;	fnames - string array of data base names
;
; SIDE EFFECT:
;	Database name is printed along with the description in the .dbh file
;
; HISTORY:
;	version 2  W. Landsman May, 1989
;	modified to work under Unix, D. Neill, ACC, Feb 1991.
;	William Thompson, GSFC/CDS (ARC), 1 June 1994
;		Added support for external (IEEE) representation.
;	William Thompson, GSFC, 3 November 1994
;			Modified to allow ZDBASE to be a path string.
;	Converted to IDL V5.0   W. Landsman   September 1997
;       Assume since V5.5,      W. Landsman   September 2006
;-
;
;-----------------------------------------------------------------------------
 compile_opt idl2
 n = N_elements(fnames)
 get_lun,unit
 b = bytarr(59)
 npar = N_params()
 if npar eq 2 then titles = strarr(n)
 for i = 0,n-1 do begin
     dbh_file = find_with_def(strtrim(fnames[i])+'.dbh', 'ZDBASE')
     openr,unit,dbh_file,error=err
     if err lt 0 then $               ;Does database exist?
        printf,!TEXTUNIT,'Unable to locate database ',fnames[i] $
 else begin
        readu,unit,b
        if npar eq 1 then begin
            printf,!TEXTUNIT,format='(A,T20,A)',fnames[i],strtrim(b[19:58],2) 
        endif else titles[i] = string(b[19:58])
   endelse

   close,unit

 endfor

 free_lun,unit
 return
end
