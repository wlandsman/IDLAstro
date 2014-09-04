pro ftaddcol,h,tab,name,idltype,tform,tunit,tscal,tzero,tnull
;+
; NAME:
;      FTADDCOL
; PURPOSE:
;      Routine to add a field to a FITS ASCII table
;
; CALLING SEQUENCE:
;      ftaddcol, h, tab, name, idltype, [ tform, tunit, tscal, tzero, tnull ]
;
; INPUTS:
;      h - FITS table header.  It will be updated as appropriate
;      tab - FITS table array.  Number of columns will be increased if
;               neccessary.
;      name - field name, scalar string
;      idltype - idl data type (as returned by SIZE function) for field,
;               For string data (type=7) use minus the string length.
;
; OPTIONAL INPUTS:
;       tform - format specification 'qww.dd' where q = A, I, E, or D
;       tunit - string giving physical units for the column.
;       tscal - scale factor
;       tzero - zero point for field
;       tnull - null value for field
;
;       Use '' as the value of tform,tunit,tscal,tzero,tnull if you want
;       the default or no specification of them in the table header.
;
; OUTPUTS:
;       h,tab - updated to allow new column of data
;
; PROCEDURES USED:
;       FTINFO, FTSIZE, GETTOK(), SXADDPAR
; HISTORY:
;       version 1  D. Lindler   July, 1987
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Updated call to new FTINFO   W. Landsman   April 2000
;-
  On_error,2
  if N_params() LT 2 then begin
      print,'Syntax - FTADDCOL, h, tab, name, idltype, ' 
      print,'                [ tform, tunit, tscal, tzero, tnull ]'
      return
  endif

; get table size

 ftsize,h,tab,ncols,nrows,tfields,allcols,allrows

; check to see if column name is a string

 s = size(name)
 if (s[0] NE 0) or (s[1] NE 7) then $
        message,'Column name must be a string'

; check to see if column already exists

 ftinfo,h,ft_str, Count = count
 if Count GT 0 then begin
    g = where(strtrim(ft_str.ttype,2) EQ strupcase(name), Ng)
    if Ng GT 0 then message,'ERROR - Column '+name+' already exists'
 endif

; set non specified inputs to ''

 npar = N_params()
 if npar lt 5 then tform = ''
 if npar lt 6 then tunit = ''
 if npar lt 7 then tscal = ''
 if npar lt 8 then tzero = ''
 if npar lt 9 then tnull = ''

; create default format if not supplied

 if tform eq '' then begin
        case idltype of
                1:      tform = 'I4'            ;byte
                2:      tform = 'I6'            ;integer*2
                4:      tform = 'E15.8'         ;real*4
                3:      tform = 'I11'           ;longword
                5:      tform = 'D23.8'         ;real*8
                else: begin
                        if idltype LT 0 then begin      ;string
                            tform = 'A'+strtrim(fix(abs(idltype)),2)
                            idltype = 7
                          end else message,'Invalid idltype specified'
                      end
        end; case
 end

; get field width from format

 width = fix(gettok(strmid(tform,1,strlen(tform)-1),'.'))

;
; is present allocated table size large enough?
;
;  If the new field is not a string, put a zero in the leftmost position
;  of the record so that a "Type conversion error" won't occur.
;
 if (width+ncols) GT allcols then begin
    tab = [ tab, replicate(32B,width,allrows)]          ;increase size  
    if (idltype NE 7) then tab[allcols,*] = 48B
 endif

;
; update header
;
 tfields = tfields+1
 apos = strtrim(tfields,2)
 ttype = strupcase(name)                                        ;ttype
 while strlen(ttype) lt 8 do ttype = ttype+' '
 sxaddpar,h,'TTYPE'+apos,ttype,'','HISTORY'

;
 sxaddpar,h,'TBCOL'+apos,ncols+1,'','HISTORY'           ;tbcol (WBL 2-88)

;
 while strlen(tform) lt 8 do tform = tform+' '          ;tform
 sxaddpar,h,'TFORM'+apos,tform,'','HISTORY'


 if tunit NE '' then begin                              ;tunit
        while strlen(tunit) lt 8 do tunit = tunit+' '
        sxaddpar,h,'tunit'+apos,tunit,'','HISTORY'
 end

 if string(tscal) NE '' then $
        sxaddpar,h,'tscal'+apos,tscal,'','HISTORY'      ;tscal


 if string(tzero) NE '' then $
        sxaddpar,h,'tzero'+apos,tzero,'','HISTORY'      ;tzero

 if string(tnull) NE '' then begin                      ;tnull
        s = size(tnull) & type = s[s[0]+1]
        if type NE 1 then stnull = string(tnull,'('+strtrim(tform)+')') $
                     else stnull = tnull
        while strlen(stnull) LT 8 do stnull = stnull+' '
        sxaddpar, h, 'TNULL' + apos, stnull, '', 'HISTORY'
 end

;
; increase table size in header
;
 sxaddpar,h,'TFIELDS',tfields
 sxaddpar,h,'NAXIS1',ncols+width

 return
 end
