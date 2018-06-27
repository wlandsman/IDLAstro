Pro sxaddpar, Header, Name, Value, Comment, Location, before=before, $
                 savecomment = savecom, after=after , format=format, pdu = pdu, $
                 missing = missing, null = null
;+
; NAME:
;       SXADDPAR
; PURPOSE:
;       Add or modify a parameter in a FITS header array.
;
; CALLING SEQUENCE:
;       SXADDPAR, Header, Name, Value, [ Comment,  Location, /SaveComment, 
;                               BEFORE =, AFTER = , FORMAT= , /PDU
;                               /SAVECOMMENT, Missing=, /Null
; INPUTS:
;       Header = String array containing FITS header.    The
;               length of each element must be 80 characters.    If not 
;               defined, then SXADDPAR will create an empty FITS header array.
;
;       Name = Name of parameter. If Name is already in the header the value 
;               and possibly comment fields are modified.  Otherwise a new 
;               record is added to the header.  If name is equal to 'COMMENT'
;               or 'HISTORY' or a blank string then the value will be added to 
;               the record without replacement.  For these cases, the comment 
;               parameter is ignored.
;
;       Value = Value for parameter.  The value expression must be of the 
;               correct type, e.g. integer, floating or string.  String values
;                of 'T' or 'F' are considered logical values.
;
; OPTIONAL INPUT PARAMETERS:
;       Comment = String field.  The '/' is added by this routine.  Added 
;               starting in position 31.    If not supplied, or set equal to 
;               '', or /SAVECOMMENT is set, then the previous comment field is 
;               retained (when found) 
;
;       Location = Keyword string name.  The parameter will be placed before the
;               location of this keyword.    This parameter is identical to
;               the BEFORE keyword and is kept only for consistency with
;               earlier versions of SXADDPAR.
;
; OPTIONAL INPUT KEYWORD PARAMETERS:
;       BEFORE  = Keyword string name.  The parameter will be placed before the
;               location of this keyword.  For example, if BEFORE='HISTORY'
;               then the parameter will be placed before the first history
;               location.  This applies only when adding a new keyword;
;               keywords already in the header are kept in the same position.
;
;       AFTER   = Same as BEFORE, but the parameter will be placed after the
;               location of this keyword.  This keyword takes precedence over
;               BEFORE.
;
;       FORMAT  = Specifies FORTRAN-like format for parameter, e.g. "F7.3".  A
;               scalar string should be used.  For complex numbers the format
;               should be defined so that it can be applied separately to the
;               real and imaginary parts.  If not supplied then the default is
;               'G19.12' for double precision, and 'G14.7' for floating point.
;       /NULL   = If set, then keywords with values which are undefined, or
;                 which have non-finite values (such as NaN, Not-a-Number) are
;                 stored in the header without a value, such as
;
;                       MYKEYWD =                      /My comment
;
;       MISSING = A value which signals that data with this value should be
;                 considered missing.  For example, the statement
;
;                       FXADDPAR, HEADER, 'MYKEYWD', -999, MISSING=-999
;
;                 would result in the valueless line described above for the
;                 /NULL keyword.  Setting MISSING to a value implies /NULL.
;                 Cannot be used with string or complex values.
;       /PDU    = specifies keyword is to be added to the primary data unit
;               header. If it already exists, it's current value is updated in
;               the current position and it is not moved.
;       /SAVECOMMENT = if set, then any existing comment is retained, i.e. the
;               COMMENT parameter only has effect if the keyword did not 
;               previously exist in the header.
; OUTPUTS:
;       Header = updated FITS header array.
;
; EXAMPLE:
;       Add a keyword 'TELESCOP' with the value 'KPNO-4m' and comment 'Name
;       of Telescope' to an existing FITS header h.
;
;       IDL> sxaddpar, h, 'TELESCOPE','KPNO-4m','Name of Telescope'
; NOTES:
;       The functions SXADDPAR() and FXADDPAR() are nearly identical, with the
;       major difference being that FXADDPAR forces required FITS keywords
;       BITPIX, NAXISi, EXTEND, PCOUNT, GCOUNT to appear in the required order
;       in the header, and FXADDPAR supports the OGIP LongString convention.   
;       There is no particular reason for having two nearly identical 
;       procedures, but both are too widely used to drop either one.
;
;       All HISTORY records are inserted in order at the end of the header.
;
;       All COMMENT records are also inserted in order at the end of the header
;       header, but before the HISTORY records.  The BEFORE and AFTER keywords
;       can override this.
;
;       All records with no keyword (blank) are inserted in order at the end of
;       the header, but before the COMMENT and HISTORY records.  The BEFORE and
;       AFTER keywords can override this.

; RESTRICTIONS:
;       Warning -- Parameters and names are not checked
;               against valid FITS parameter names, values and types.
;
; MODIFICATION HISTORY:
;       DMS, RSI, July, 1983.
;       D. Lindler Oct. 86  Added longer string value capability
;       Added Format keyword, J. Isensee, July, 1990
;       Added keywords BEFORE and AFTER. K. Venkatakrishna, May '92
;       Pad string values to at least 8 characters   W. Landsman  April 94
;       Aug 95: added /PDU option and changed routine to update last occurrence
;               of an existing keyword (the one SXPAR reads) instead of the
;               first occurrence.
;       Comment for string data can start after column 32 W. Landsman June 97
;       Make sure closing quote supplied with string value  W. Landsman  June 98
;       Increase precision of default formatting of double precision floating
;               point values.   C. Gehman, JPL  September 1998
;       Mar 2000, D. Lindler, Modified to use capital E instead of lower case
;               e for exponential formats.
;       Apr 2000, Make user-supplied format upper-case  W. Landsman 
;       Oct 2001, Treat COMMENT or blank string like HISTORY keyword W. Landsman
;       Jan 2002, Allow BEFORE, AFTER to apply to COMMENT keywords W. Landsman
;       June 2003, Added SAVECOMMENT keyword    W. Landsman
;       Jan 2004, If END is missing, then add it at the end W. Landsman
;       May 2005 Fix SAVECOMMENT error with non-string values W. Landsman
;       Oct 2005 Jan 2004 change made SXADDPAR fail for empty strings W.L.
;       May 2011 Fix problem with slashes in string values W.L. 
;       Aug 2013 Only use keyword_set for binary keywords W. L. 
;       Sep 2015 Added NULL and MISSING keywords W.L.
;       Sep 2016 Allow writing of byte or Boolean variables  W.L.
;       Nov 2016 Allow value to be a 1 element scalar  W.L.
;       Jun 2018 W. Thompson, for backward compatibility, save non-finite
;                values (e.g. NaN) as strings if /NULL not set
;       
;-
 compile_opt idl2
 if N_params() LT 3 then begin             ;Need at least 3 parameters
      print,'Syntax - Sxaddpar, Header, Name,  Value, [Comment, Postion'
      print,'                      BEFORE = ,AFTER = , FORMAT =, /SAVECOMMENT'
      print,'                      MISSING =, /NULL'
      return
 endif

; Define a blank line and the END line

 ENDLINE = 'END' +string(replicate(32b,77))     ;END line
 BLANK = string(replicate(32b,80))             ;BLANK line
;
;  If Location parameter not defined, set it equal to 'END     '
;
 if ( N_params() GT 4 ) then loc = strupcase(location) else $
 if N_elements( BEFORE) GT 0 then loc = strupcase(before) else $
 if N_elements( AFTER) GT 0  then loc = strupcase(after) else $
 if N_elements( PDU) GT 0  then loc = 'BEGIN EX' else $
                             loc = 'END'

 while strlen(loc) lt 8 do loc += ' '

 if N_params() lt 4 then comment = ''      ;Is comment field specified?

 n = N_elements(header)                  ;# of lines in FITS header
 if (n EQ 0) then begin                  ;header defined?
          header=strarr(10)              ;no, make it.
          header[0]=ENDLINE
          n=10
 endif else begin
          s = size(header)               ;check for string type
              if (s[0] ne 1) || (s[2] ne 7) then $
                  message,'FITS Header (first parameter) must be a string array'
 endelse

;  Make sure Name is 8 characters long

        nn = string(replicate(32b,8))   ;8 char name
        strput,nn,strupcase(name) ;insert name
;
;  Check to see if the parameter should be saved as a null value.
;
        stype = size(value,/type)
        save_as_null = 0
        if stype EQ 0 then begin        
            if (n_elements(missing) eq 1) || keyword_set(null) then $
              save_as_null = 1 else $
                message,'Keyword value (third parameter) is not defined'
        endif else if (stype NE 6) && (stype NE 7) && (stype NE 9) then begin
            if N_elements(missing) eq 1 then $
              if value eq missing then save_as_null = 1
              if ~save_as_null then if ~finite(value) then begin
                if ((n_elements(missing) eq 1) || keyword_set(null)) then $
                  save_as_null = 1 else begin
                    message,/continue,'Keyword value (third parameter) ' + $
                            'is not finite, saving as string.'
                    stype = 7
                    save_as_string = 1
                endelse
            endif
        endif
;
;  Extract first 8 characters of each line of header, and locate END line

 keywrd = strmid(header,0,8)                 ;Header keywords
 iend = where(keywrd eq 'END     ',nfound)
;
;  If no END, then add it.  Either put it after the last non-null string, or
;  append it to the end.
;
        if nfound EQ 0 then begin
                ii = where(strtrim(header) ne '',nfound)
                ii = max(ii) + 1
                if ii eq n_elements(header) then begin
                        header = [header,endline]
                        n++ 
                endif else header[ii] = endline
                keywrd = strmid(header,0,8)
                iend = where(keywrd eq 'END     ',nfound)
        endif
;
        iend = iend[0] > 0                      ;make scalar

;  History, comment and "blank" records are treated differently from the
;  others.  They are simply added to the header array whether there are any
;  already there or not.

 if (nn EQ 'HISTORY ') || (nn EQ 'COMMENT ') || $
    (nn EQ '        ')  then begin             ;add history record?
;
;  If the header array needs to grow, then expand it in increments of 5 lines.
;

     if iend GE (n-1) then begin
                 header = [header,replicate(blank,5)] ;yes, add 5.
                 n = N_elements(header)
      endif

; Format the record

      newline = blank
      strput,newline,nn+string(value),0

;
;  If a history record, then append to the record just before the end.
;
      if nn EQ 'HISTORY ' then begin
             header[iend] = newline             ;add history rec.
             header[iend+1] = endline
;
;  The comment record is placed immediately after the last previous comment
;  record, or immediately before the first history record, unless overridden by
;  either the BEFORE or AFTER keywords.
;
      endif else if nn EQ 'COMMENT ' then begin
            if loc EQ 'END     ' then loc = 'COMMENT '
            iloc = where(keywrd EQ loc, nloc)
            if nloc EQ 0 then iloc = where(keywrd EQ 'HISTORY ', nloc)
            if nloc gt 0 then begin
               i = iloc[nloc-1]
               if keyword_set(after) or (loc EQ 'COMMENT ') then i = i+1 < iend 
               if i gt 0 then header=[header[0:i-1],newline,header[i:n-1]] $
                        else header=[newline,header[0:n-1]]
            endif else begin
                header[iend] = newline
                header[iend+1] = endline
            endelse

;
;  The "blank" record is placed immediately after the last previous "blank"
;  record, or immediately before the first comment or history record, unless
;  overridden by either the BEFORE or AFTER keywords.
;
          ENDIF ELSE BEGIN
            if loc EQ 'END     ' then loc = '       '
            iloc = where(keywrd[0:iend] EQ loc, nloc)
            if nloc gt 0 then begin
               i = iloc[0]
               if keyword_set(after) and loc ne 'HISTORY ' then i = i+1 < iend 
               if i gt 0 then header=[header[0:i-1],newline,header[i:n-1]] $
                        else header=[newline,header[0:n-1]]
            endif else begin
                iloc = where(keywrd EQ 'COMMENT ', nloc)
                if nloc Eq 0 then iloc = where(keywrd EQ 'HISTORY ', nloc)
                if nloc GT 0 then begin
                   i = iloc[0]
                   if i gt 0 then header=[header[0:i-1],newline,header[i:n-1]] $
                        else header=[newline,header[0:n-1]]
                endif else begin
                  header[iend] = newline
                  header[iend+1] = endline
            endelse
            endelse
           endelse
            RETURN
 endif

; Find location to insert keyword.   Save the existing comment if user did
; not supply a new one.   Comment starts after column 32 for numeric data,
; after the slash (but at least after final quote) for string data. 

 ncomment = comment
 ipos  = where(keywrd eq nn,nfound)
 if nfound gt 0 then begin
         i = ipos[nfound-1]
         if comment eq '' or keyword_set(savecom) then begin  ;save comment?
         if strmid(header[i],10,1) NE "'" then $
                 ncomment=strmid(header[i],32,48) else begin
		 quote = strpos(header[i],"'",11)
		
                 if quote EQ -1 then slash = -1 else $
		       slash = strpos(header[i],'/',quote)  		
                 if slash NE -1 then $
                        ncomment =  strmid(header[i], slash+1, 80) else $
                        ncomment = string(replicate(32B,80))
                endelse
        endif 
         goto, REPLACE    
 endif

 if loc ne '' then begin
          iloc =  where(keywrd eq loc,nloc)
          if nloc gt 0 then begin
             i = iloc[0]
             if keyword_set(after) && (loc ne 'HISTORY ') then i = i+1 < iend 
             if i gt 0 then header=[header[0:i-1],blank,header[i:n-1]] $
                        else header=[blank,header[0:n-1]]
             goto, REPLACE  
          endif
 endif

; At this point keyword and location parameters were not found, so a new
; line is added at the end of the FITS header

        if iend lt (n-1) then begin     ;Not found, add more?
                header[iend+1] = ENDLINE        ;no, already long enough.
                i = iend                ;position to add.
           endif else begin             ;must lengthen.
                header = [header,replicate(blank,5)] ;add an element on the end
                header[n]=ENDLINE               ;save "END"
                i =n-1                  ;add to end
        end

; Now put value into keyword at line i

REPLACE:    
        h=blank                 ;80 blanks
        strput,h,nn+'= '        ;insert name and =.
        apost = "'"             ;quote a quote
        if N_elements(value) NE 1 then $
                message,'Keyword Value (third parameter) must be a scalar'

        case stype of         ;which type?

7:      begin
          upval = strupcase(value)      ;force upper case.
          if (upval eq 'T') || (upval eq 'F') then begin
                strput,h,upval,29  ;insert logical value.
            end else begin              ;other string?
                if keyword_set(save_as_string) then $
                  svalue = strtrim(value,2) else svalue = value
                if strlen(svalue) gt 18 then begin       ;long string
                    strput, h, apost + strmid(svalue,0,68) + apost + $
                        ' /' + ncomment,10
                    header[i] = h
                    return
                endif
                strput, h, apost + svalue,10       ;insert string val
                strput, h, apost, 11 + (strlen(svalue)>8)  ;pad string vals
          endelse                                          ;to at least 8 chars
          endcase

5:      BEGIN
        IF (N_ELEMENTS(format) EQ 1) THEN $             ; use format keyword
            v = string(value, FORMAT='('+strupcase(format)+')') $
        ELSE v = STRING(value, FORMAT='(G19.12)')
        s = strlen(v)                                   ; right justify
        strput, h, v, (30-s)>10
        END               

 else:  begin
        if ~save_as_null then begin
        if stype EQ 1 then begin
             if !VERSION.RELEASE GE '8.4' && ISA(value,/boolean) then begin
                upval = ['F','T']
                strput,h,upval[value],29 
                break
             endif else v = strtrim(fix(value),2) 
        endif else begin
        if (N_elements(format) eq 1) then $            ;use format keyword
            v = string(value, FORMAT='('+strupcase(format)+')' ) else $
            v = strtrim(strupcase(value),2)      
                                      ;convert to string, default format
        endelse                              
        s = strlen(v)                 ;right justify
        strput,h,v,(30-s)>10          ;insert
        endif
        end
 endcase

 if (~save_as_null) || (strlen(strtrim(comment)) GT 0) then begin
   strput,h,' /',30       ;add ' /'
   strput, h, ncomment, 32 ;add comment
 endif  
   header[i] = h          ;save line
 
 return
 end
