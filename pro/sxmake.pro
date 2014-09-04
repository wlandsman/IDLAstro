Pro sxmake, unit, File, Data, Par, Groups, Header, PSIZE = psize
;+
; NAME:
;       SXMAKE
; PURPOSE:
;       Create a basic ST header file from an IDL array prior to writing data.
;
; CALLING SEQUENCE:
;       sxmake, Unit, File, Data, Par, Groups, Header, [ PSIZE = ]
;
; INPUTS:
;       Unit = Logical unit number from 1 to 9.
;       File = file name of data and header files to create.   If no file name
;              extension is supplied then the default is to use .hhh for the
;              header file extension and .hhd for the data file extension    
;              If an extension is supplied, it should be of the form .xxh
;              where xx are any alphanumeric characters.
;       Data = IDL data array of the same type, dimensions and
;               size as are to be written to file.
;       Par = # of elements in each parameter block for each data record.  If 
;             set equal to 0, then parameter blocks will not be written.  The 
;             data type of the parameter blocks must be the same as the data 
;             array.   To get around this restriction, use the PSIZE keyword.
;       Groups = # of groups to write.  If 0 then write in basic
;               format without groups.  
;
; OPTIONAL INPUT PARAMETERS:
;       Header = String array containing ST header file.  If this
;               parameter is omitted, a basic header is constructed.
;               If included, the basic parameters are added to the
;               header using sxaddpar.  The END keyword must terminate
;               the parameters in Header.
;
; OPTIONAL KEYWORD INPUT PARAMETER:
;        PSIZE - Integer scalar giving the number of bits in the parameter 
;               block.    If the PSIZE keyword is given, then the Par input
;               parameter is ignored.
;                
; OPTIONAL OUTPUT PARAMETERS:
;       Header = ST header array, an 80 by N character array.
;
; COMMON BLOCKS:
;       Stcommn - as used in sxwrite, sxopen, etc.
;
; SIDE EFFECTS:
;       The header file is created and written and then the
;       data file is opened on the designated unit.
;
; RESTRICTIONS:
;       Header files must be named .xxh and data files must be
;       named .xxd, where xx are any alphanumeric characters.
;
; PROCEDURE:
;       Call sxmake to create a header file.  Then call sxwrite
;       to output each group.
; 
; PROCEDURES USED:
;       GET_DATE, SXADDPAR, SXOPEN
; MODIFICATION HISTORY:
;       DMS, July, 1983.
;       converted to new VMS IDL  April 90
;       Use SYSTIME() instead of !STIME   W. Landsman   Aug 1997
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Added optional PSIZE keyword   August 1999 W. Landsman 
;       Recognize unsigned datatype    January 2000   W. Landsman 
;-
        common stcommn, result, filename
;
        if N_params() LT 2 then begin
           print,'Syntax - SXMAKE,unit,file,data,par,groups,header, [PSIZE = ]'
           return
        endif
;
        if N_elements(result) ne 200 then begin
                result = lonarr(20,10)  ;define common blks
                filename = strarr(10)
                endif
;
        if (unit lt 1) or (unit gt 9) then $  ;unit ok?
                message,'Unit number must be from 1 to 9.'
;
        close,unit
        result[unit,*]=0
;
        if N_elements(par) EQ 0 then par = 0
        if N_elements(groups) EQ 0 then groups = 0
;
        s = size(data)                  ;obtain size of array.
        stype = s[s[0]+1]               ;type of data.
        if (par eq 0) and (groups eq 0) and (stype LT 6) then $
                sxaddpar,header,'simple','T','Written by IDL:  '+ systime() $
            else $
                sxaddpar,header,'simple','F','Written by IDL:  '+ systime()
        case stype of
0:      message,'Data parameter is not defined'
7:      message,"Can't write strings to ST files"
1:      begin& bitpix=  8 & d = 'INTEGER*1' & endcase
2:      begin& bitpix= 16 & d = 'INTEGER*2' & endcase
4:      begin& bitpix= 32 & d = 'REAL*4' & endcase
3:      begin& bitpix= 32 & d = 'INTEGER*4' & endcase
5:      begin& bitpix= 64 & d = 'REAL*8' & endcase
6:      begin& bitpix= 64 & d = 'COMPLEX*8' & endcase
12:     begin & bitpix=16 & d='UNSIGNED*2' & endcase
13:     begin & bitpix=32 & d='UNSIGNED*4' & endcase
else:   message,'ERROR -- Unrecognized input data type'

        endcase
;
        sxaddpar,header,'BITPIX',bitpix
        sxaddpar,header,'NAXIS',S[0]    ;# of dimensions
        for i=1,s[0] do sxaddpar,header,'NAXIS'+strtrim(i,2),s[i]
        sxaddpar,header,'DATATYPE',d,'Type of data'
        Get_date,dte
        sxaddpar,header,'DATE',dte
;
        if groups eq 0 then $           ;true if not group fmt.
                sxaddpar,header,'GROUPS','F','No groups' $
           else begin                   ;make group params.
                sxaddpar,header,'GROUPS','T'
                sxaddpar,header,'PCOUNT',par
                sxaddpar,header,'GCOUNT',groups
                if N_elements(psize) EQ 0 then psize = bitpix*par
                sxaddpar,header,'PSIZE',psize,'# of bits in parm blk'
           endelse
;
        sxopen,unit,file,header,hist,'W' ;make header file, etc.
        return
end
