pro SXOPEN,unit,fname,header,history,access
;+
; NAME:
;       SXOPEN
; PURPOSE:
;       Open a Space Telescope formatted (STSDAS) header file.
; EXPLANATION:
;       Saves the parameters required subsequent SX routines in
;       the common block Stcommn.  Optionally save the header in 
;       the string array Header, and the history in the string array
;       History.  Open the data file associated with this
;       header on the same unit.
;
; CALLING SEQUENCE:
;       SXOPEN, Unit, Fname [, Header [,History] [,Access]]
;
; INPUTS:
;       Unit = IDL unit used for IO.  Must be from 1 to 9.
;       Fname = File name of header file.  Default extension
;               is .hhh for header files and .hhd for data
;               files.    If an extension is supplied it must have the 
;               form .xxh where xx are any alphanumeric characters. The
;               data file must have extension .xxd
;               No version number is allowed.  Most recent versions
;               of the files are used.
;
; OPTIONAL INPUT PARAMETER:
;       Access = 'R' to open for read, 'W' to open for write.
;
; OUTPUTS:
;       Stcommn = Common block containing ST parameter blocks.
;               (Long arrays.)
;
; OPTIONAL OUTPUT PARAMETERS:
;       Header = 80 char by N string array containing the
;               names, values and comments from the FITS header.
;               Use the function SXPAR to obtain individual
;               parameter values.
;       History = String array containing the value of the
;               history parameter.
;
; COMMON BLOCKS:
;       STCOMMN - Contains RESULT(20,10) where RESULT(i,LUN) =
;       0 - 121147 for consistency check, 1 - Unit for consistency,
;       2 - bitpix, 3 - naxis, 4 - groups (0 or 1), 5 - pcount,
;       6 - gcount, 7 - psize, 8 - data type as idl type code,
;       9 - bytes / record, 10 to 10+N-1 - dimension N,
;       17 = record length of file in bytes.
;       18 - # of groups written, 19 = gcount.
;
; SIDE EFFECTS:
;       The data and header files are accessed.
;
; RESTRICTIONS:
;       Works only for disc files.  The data file must have
;       must have the extension ".xxd" and the header file must
;       have the extension ".xxh" where x is any alphanumeric character
;
; PROCEDURE:
;       The header file is opened and each line is read.
;       Important parameters are stored in the output
;       parameter.  If the last two parameters are specified
;       the parameter names and values are stored.  The common
;       block STCOMMN is filled with the type of data, dimensions,
;       etc. for use by SXREAD.
;
;       If access is for write, each element of the header
;       array, which must be supplied, is written to the
;       header file.  The common block is filled with
;       relevant parameters for SXWRITE.  A keyword of "END"
;       ends the header.
;
; MODIFICATION HISTORY:
;       Written, DMS, May, 1983.
;       D. Lindler Feb. 1990
;               Modified to allow var. record length header files.
;       D. Lindler April 1990   Conversion to new VMS IDL
;       Added /BLOCK when opening new .hhd file
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Recognize unsigned datatype for V5.1 or greater   W. Landsman Jan 2000
;       Assume since V5.5  W. Landsman Sep 2006
;-
;------------------------------------------------------------------------------
        On_error,2
        common stcommn,result,filename
;
     if N_params() LT 2 then begin
         print, 'Syntax: SXOPEN, unit, fname, [ header, history, access]'
         return
     endif
;
        if N_elements(result) NE 200 then begin ;defined?
                result = lonarr(20,10)
                filename = strarr(10)
                endif
;
        if (unit lt 1) OR (unit gt 9) then $
                message,'Unit number must be from 1 to 9.'
;
        close,unit              ;close unit first
;
        n = N_params(0)              ;# of parameters we have
        if n LT 5 then access = 'R'   ;read access if unspecified
;
; Add default extension (.hhh) if not specified       
;
        xname=strtrim(fname,2)
        if strmid(xname,strlen(xname)-4,1) NE '.' then xname = xname + '.hhh'
        t=xname                         ;Open keywords.
        CASE strupcase(access) OF
'R':    sxhread,fname,header               ;Read FITS header
'W':    sxhwrite,fname,header              ;Write FITS header
ELSE:   message,'Illegal access value, must be R or W'
        ENDCASE
;
        result[*,unit]=0        ;Zero our block     
        filename[unit]=fname    ;Save file name   
        result[0,unit]=121147L  ;Code for descr block   
        result[1,unit] = unit   ;Save unit number    
        result[6,unit]=1        ;Default value of GCOUNT is 1
;
; Get keyword names and values from header array
;
 name =  strtrim(strmid(header,0,8),2)   ;param name
 value = strtrim(strmid(header,10,20),2) ;param value
;
 L_bitpix = where(name EQ 'BITPIX',nfound)
      if nfound GT 0 then result[2,unit] = value[L_bitpix[0]] else $
       message,'Required Keyword BITPIX not found',/CON
;
 l_naxis = where(strmid(name,0,5) EQ 'NAXIS',nfound)         
      IF nfound GT 0 then BEGIN
           axis = fix(strtrim(strmid(name[l_naxis],5,3),2))
           for i=0,nfound-1 do begin
                if axis[i] EQ 0 then  $
                       result[3,unit]=value[l_naxis[i]] else  $  ;# of dimensions
                       result[9+axis[i],unit]=value[l_naxis[i]] ;each dimension
            endfor
       endif else message,'Required Keyword NAXIS not found'
;           
 if n GE 4 then BEGIN                ;Create history parameter?
   L_hist = where(name EQ 'HISTORY',nfound)  
   IF nfound then history = strtrim(strmid(header[l_hist],8,72),2) else $
                  history = ''  
ENDIF
;
 L_groups = where(name EQ 'GROUPS',nfound)
   if nfound GT 0 then result[4,unit] = value[L_groups[0]] eq 'T'
;
 L_pcount = where(name EQ 'PCOUNT',nfound)
   if nfound GT 0 then result[5,unit] = value[L_pcount[0]]
;
 L_gcount = where(name EQ 'GCOUNT',nfound)
if nfound GT 0 then result[6,unit] = value[L_gcount[0]]
;
 L_psize = where(name EQ 'PSIZE',nfound)
 if nfound GT 0 then result[7,unit] = value[L_psize[0]]/8 $
               else result[7,unit] = result[5,unit]*result[2,unit]
;
 L_datatype = where(name EQ 'DATATYPE',nfound)
 if nfound GT 0 then begin 
                v = value[L_datatype[0]]      ;Process data type.
                v = strmid(v,1,strlen(v)-2)   ;Remove apostrophes
                v = strtrim(v,2)                    ;trim blanks
                CASE v OF       ;Cvt datatype to IDL type code    
                'BYTE':                 result[8,unit]=1
                'LOGICAL*1':            result[8,unit]=1        ;Byte
                'INTEGER*1':            result[8,unit]=1
                'REAL*4':               result[8,unit]=4
                'INTEGER*2':            result[8,unit]=2
                'UNSIGNED*2':           result[8,unit]=12
                'INTEGER*4':            result[8,unit]=3
                'UNSIGNED*4':           result[8,unit]=13 
                'REAL*8':               result[8,unit]=5
                'COMPLEX*8':            result[8,unit]=6
                ELSE:                   message,'Undefined Datatype value'
                ENDCASE         ;V OF
 endif                       ;DATATYPE
;
;
; If DATATYPE not specified assume integer of size specified by BITPIX
;
        if result[8,unit] EQ 0 then begin
                CASE result[2,unit] OF
                        8: result[8,unit]=1             ;byte
                       16: result[8,unit]=2             ;integer*2
                       32: result[8,unit]=3             ;integer*4
                      -32: result[8,unit]=4
                      -64: result[8,unit]=5
                     else: message,'Unable to determine data type'
                ENDCASE
        endif
;      
        bytes = abs(result[2,unit])/8l  ;bytes/datum
        for j=1,result[3,unit] do $     ;accum bytes/record
                        bytes=bytes*result[9+j,unit]
        bytes = bytes + result[7,unit]     ;+ header.
        result[9,unit]=bytes               ;Save bytes/record. 
;
        xname=strmid(xname,0,strlen(xname)-1)+'d'   ;Change to data filename  
;
        If result[3,unit] GT 0 then begin      ;NAXIS non-zero?
          close,unit
          if strupcase(access) eq 'R' then $
                openr,unit,xname  $
          else begin
                nrecs = (result[6,unit]*result[9,unit]+511)/512
                openw, unit, xname
          endelse
        result[17,unit] = 512           ;Save record length    
        endif else result[17,unit]=0    ;NAXIS = 0
        return
end  
