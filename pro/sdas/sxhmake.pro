Pro sxhmake,data,groups,header
;+
; NAME:
;       SXHMAKE
; PURPOSE:
;       Create a basic STSDAS header file from an IDL data array
;
; CALLING SEQUENCE:
;       sxhmake, Data, Groups, Header
;
; INPUTS:
;       Data = IDL data array of the same type, dimensions and
;               size as are to be written to file.
;       Groups = # of groups to be written.
;
; OUTPUTS:
;       Header = String array containing ST header file.
;
; PROCEDURE:
;       Call sxhmake to create a header file.  Then call sxopen to
;       open output image, followed by sxwrite to write the data.
;       If you do not plan to change the header created by sxhmake
;       before calling sxopen, you might consider using sxmake which
;       does both steps.
;
; MODIFICATION HISTORY:
;       Don Lindler  Feb 1990 modified from SXMAKE by DMS, July, 1983.
;       D. Lindler April 90  Converted to new VMS IDL
;       M. Greason May 1990  Header creation bugs eliminated.
;       W. Landsman Aug 1997 Use SYSTIME() instead of !STIME for V5.0 
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Recognize unsigned datatype    January 2000   W. Landsman 
;-
;-----------------------------------------------------------------------------
 On_error,2
 if N_Params() LT 3 then begin
     print,'Syntax - sxhmake, Data, Groups, Header'
     return
 endif

        s = size(data)                  ;obtain size of array.
        stype = s[s[0]+1]               ;type of data.
        if (groups eq 0) and (stype LT 6) then $
                sxaddpar,header,'simple','T','Written by IDL:  '+ systime() $
            else $
                sxaddpar,header,'simple','F','Written by IDL:  '+ systime()

        case stype of
0:      message,'Data parameter is not defined'
7:      message,"Can't write strings to ST files'
1:      begin& bitpix= 8 & d='INTEGER*1' & endcase
2:      begin& bitpix= 16 & d = 'INTEGER*2' & endcase
4:      begin& bitpix= 32 & d='REAL*4' & endcase
3:      begin& bitpix= 32 & d='INTEGER*4' & endcase
5:      begin& bitpix= 64 & d='REAL*8' & endcase
6:      begin& bitpix= 64 & d='COMPLEX*8' & endcase
12:     begin & bitpix=16 & d='UNSIGNED*2' & endcase
13:     begin & bitpix=32 & d='UNSIGNED*4' & endcase
else:   message,'ERROR -- Unrecoginized input data type'
        endcase
        sxaddpar,header,'BITPIX',bitpix
        sxaddpar,header,'NAXIS',S[0]    ;# of dimensions
        for i=1,s[0] do sxaddpar,header,'NAXIS'+strtrim(i,2),s[i]
        sxaddpar,header,'DATATYPE',d,'Type of data'
        Get_date,dte                    ;Get current date as CCYY-MM-DD
        sxaddpar,header,'DATE',dte
        if groups eq 0 then $           ;true if not group fmt.
                sxaddpar,header,'GROUPS','F','No groups' $
           else begin                   ;make group params.
                sxaddpar,header,'GROUPS','T'
                sxaddpar,header,'PCOUNT',0
                sxaddpar,header,'GCOUNT',groups
                sxaddpar,header,'PSIZE',0,'# of bits in parm blk'
           endelse
        return
end
