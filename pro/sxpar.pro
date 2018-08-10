function SXPAR, hdr, name, abort, COUNT=matches, COMMENT = comments, $
                IFound = number, NoContinue = NoContinue, SILENT = silent, $
                DUP = dup, NULL = K_Null, NAN = NaN, MISSING = Missing
;+
; NAME:
;      SXPAR
; PURPOSE:
;      Obtain the value of a parameter in a FITS header
;
; CALLING SEQUENCE:
;      result = SXPAR( Hdr, Name, [ Abort, COUNT=, COMMENT =, /NoCONTINUE, 
;                                        DUP=,   /SILENT  ])   
;
; INPUTS:
;      Hdr =  FITS header array, (e.g. as returned by READFITS) 
;             string array, each element should have a length of 80 characters      
;
;      Name = String name of the parameter to return.   If Name is of the
;             form 'keyword*' then an array is returned containing values of
;             keywordN where N is a positive (non-zero) integer.  The value of 
;             keywordN will be placed in RESULT[N-1].  The data type of RESULT 
;             will be the type of the first valid match of keywordN found.
;
; OPTIONAL INPUTS:
;       ABORT - string specifying that SXPAR should do a RETALL
;               if a parameter is not found.  ABORT should contain
;               a string to be printed if the keyword parameter is not found.
;               The default string is 'FITS Header' if abort is a integer or 
;               single character.     If ABORT is not supplied, SXPAR will return
;               quietly with COUNT = 0  if a keyword is not found.
;
; OPTIONAL INPUT KEYWORDS: 
;       DUP =  When the FITS keyword exists more than once in a header, set 
;                 DUP to a positive integer to specify which value is to be 
;                 read.   For example, set DUP = 2 to read the value of the 
;                 second appearance of a keyword in the header.    If DUP is not
;                 supplied, then by default, SXPAR() reads the *last* appearance and
;                 issues a warning.
;       MISSING = By default, this routine returns 0 when keyword values are
;                 not found.  This can be overridden by using the MISSING
;                 keyword, e.g. MISSING=-1.
;       /NAN    = If set, then return Not-a-Number (!values.f_nan) for missing
;                 values.  Ignored if keyword MISSING is present.
;       /NOCONTINUE = If set, then continuation lines will not be read, even
;                 if present in the header
;       /NULL   = If set, then return !NULL (undefined) for missing values.
;                 Ignored if MISSING or /NAN is present, or if earlier than IDL
;                 version 8.0.  If multiple values would be returned, then
;                 MISSING= or /NAN should be used instead of /NULL, making sure
;                 that the datatype is consistent with the non-missing values,
;                 e.g. MISSING='' for strings, MISSING=-1 for integers, or
;                 MISSING=-1.0 or /NAN for floating point.  /NAN should not be
;                 used if the datatype would otherwise be integer.
;       /SILENT - Set this keyword to suppress warning messages about duplicate
;                 keywords in the FITS header.
;
; OPTIONAL OUTPUT KEYWORDS:
;       COUNT - Optional keyword to return a value equal to the number of 
;               parameters found by SXPAR, integer scalar
;
;       COMMENT - Array of comments associated with the returned values
;       IFOUND - Array of found keyword indicies when Name is of the form keyword*
;              For example, one searches for 'TUNIT*' and the FITS header contains
;              TUNIT1, TUNIT2, TUNIT4, and TUNIT6 then IFOUND woud be returned as
;              [1,2,4,6].    Set to zero if Name is not of the form keyword*.
;
; OUTPUTS:
;       Function value = value of parameter in header.
;               If parameter is double precision, floating, long or string,
;               the result is of that type.  Apostrophes are stripped
;               from strings.  If the parameter is logical, 1b is
;               returned for T, and 0b is returned for F.
;               If Name was of form 'keyword*' then a vector of values
;               are returned.
;
; SIDE EFFECTS:
;       !ERR is set to -1 if parameter not found, 0 for a scalar
;       value returned.  If a vector is returned !ERR is set to the
;       number of keyword matches found.    The use of !ERR is deprecated, and
;       instead the COUNT keyword is preferred
;
;       If a keyword (except HISTORY or COMMENT) occurs more than once in a 
;       header, and the DUP keyword is not supplied, then a warning is given, 
;       and the *last* occurrence is used.
;
; EXAMPLES:
;       Given a FITS header, h, return the values of all the NAXISi values
;       into a vector.    Then place the history records into a string vector.
;
;       IDL> naxisi = sxpar( h ,'NAXIS*')         ; Extract NAXISi value
;       IDL> history = sxpar( h, 'HISTORY' )      ; Extract HISTORY records
;
; PROCEDURE:
;       The first 8 chacters of each element of Hdr are searched for a 
;       match to Name.  The value from the last 20 characters is returned.  
;       An error occurs if there is no parameter with the given name.
;
;       If a numeric value has no decimal point it is returned as type
;       LONG.   If it has a decimal point, and contains more than 8 numerals, or 
;       contains the character 'D', then it is returned as type DOUBLE.  Otherwise
;       it is returned as type FLOAT.    Very large integer values, outside
;       the range of valid LONG, are returned as LONG64.
;
;       If the value is too long for one line, it may be continued on to the
;       the next input card, using the CONTINUE convention.  For more info,
;       see http://fits.gsfc.nasa.gov/registry/continue_keyword.html
;
;       Complex numbers are recognized as two numbers separated by one or more
;       space characters.
;
; NOTES:
;       The functions SXPAR() and FXPAR() are nearly identical, although
;       FXPAR() has slightly more sophisticated parsing, and additional keywords
;       to specify positions in the header to search (for speed), and to force
;       the output to a specified data type..   There is no
;       particular reason for having two nearly identical functions, but
;       both are too widely used to drop either one.
;
; PROCEDURES CALLED:
;       cgErrorMsg(), GETTOK(), VALID_NUM()
; MODIFICATION HISTORY:
;       DMS, May, 1983, STPAR Written.
;       D. Lindler Jan 90 added ABORT input parameter
;       J. Isensee Jul,90 added COUNT keyword
;       W. Thompson, Feb. 1992, added support for FITS complex values.
;       W. Thompson, May 1992, corrected problem with HISTORY/COMMENT/blank
;               keywords, and complex value error correction.
;       W. Landsman, November 1994, fix case where NAME is an empty string 
;       W. Landsman, March 1995,  Added COMMENT keyword, ability to read
;               values longer than 20 character
;       W. Landsman, July 1995, Removed /NOZERO from MAKE_ARRAY call
;       T. Beck May 1998, Return logical as type BYTE
;       W. Landsman May 1998, Make sure integer values are within range of LONG
;       W. Landsman Feb 1998, Recognize CONTINUE convention 
;       W. Landsman Oct 1999, Recognize numbers such as 1E-10 as floating point
;       W. Landsman Jan 2000, Only accept integer N values when name = keywordN
;       W. Landsman Dec 2001, Optional /SILENT keyword to suppress warnings
;       W. Landsman/D. Finkbeiner  Mar 2002  Make sure extracted vectors 
;             of mixed data type are returned with the highest type.
;       W.Landsman Aug 2008  Use vector form of VALID_NUM()
;       W. Landsman Jul 2009  Eliminate internal recursive call
;       W. Landsman Apr 2012  Require vector numbers be greater than 0
;       W. Landsman Apr 2014  Don't convert Long64 numbers to double
;       W. Landsman Nov 2014  Use cgErrorMsg rather than On_error,2
;       W. Landsman Dec 2014  Return Logical as IDL Boolean in IDL 8.4 or later
;       W. Landsman May 2015  Added IFound output keyword
;       J. Slavin Aug 2015 Allow for 72 character par values (fixed from 71)
;       W. Landsman Sep 2015  Added Missing, /NULL and /NaN keywords
;       W. Landsman Oct 2017   Added DUP keyword,  Needed to support distortion
;			table lookup parameters
;       W. Landsman Jan 2018 Return ULONG64 integer if LONG64 will overflow
;       W. Landsman/Y. Yang May 2018 MISSING keyword was always returning 0
;       W. Landsman/M. Knight Aug 2018 Clean up use of Abort parameter
;-
;----------------------------------------------------------------------
 compile_opt idl2

 if N_params() LT 2 then begin
     print,'Syntax -  result =  sxpar( hdr, name, [abort])'
     print,'   Input Keywords:    /NOCONTINUE, /SILENT, MISSING=, /NAN, /NULL'
     print,'   Output Keywords:   COUNT=,  COMMENT= '
     return, -1
 endif 
 
 ;
;  Determine the default value for missing data.
;
        CASE 1 OF 
            N_ELEMENTS(MISSING) EQ 1: MISSING_VALUE = MISSING
            KEYWORD_SET(NAN): MISSING_VALUE = !VALUES.F_NAN
            KEYWORD_SET(K_NULL) AND !VERSION.RELEASE GE '8.': $
              DUMMY = EXECUTE('MISSING_VALUE = !NULL')
            ELSE: MISSING_VALUE = 0
        ENDCASE
        VALUE = MISSING_VALUE
;
 
 if N_elements(abort) EQ 0 then begin
      abort_return = 0
      abort = 'FITS Header'
 endif else begin 
       if strlen(strtrim(abort,2)) LE 1 then abort = 'FITS header'
       abort_return = 1
 endelse
 
 if abort_return then On_error,1 else begin
      Catch, theError
      if theError NE 0 then begin
           Catch,/Cancel
	   void = cgErrorMsg(/quiet)
	   return,-1
	   endif
   endelse
;       Check for valid header

;Check header for proper attributes.
  if ( size(hdr,/N_dimen) NE 1 ) || ( size(hdr,/type) NE 7 ) then $
           message,'FITS Header (first parameter) must be a string array'

  nam = strtrim( strupcase(name) )      ;Copy name, make upper case     


;  Determine if NAME is of form 'keyword*'.  If so, then strip off the '*', and
;  set the VECTOR flag.  One must consider the possibility that NAM is an empty
;  string.

   namelength1 = (strlen(nam) - 1 ) > 1         
   if strpos( nam, '*' ) EQ namelength1 then begin    
            nam = strmid( nam, 0, namelength1)  
            vector = 1                  ;Flag for vector output  
            name_length = strlen(nam)   ;Length of name 
            num_length = 8 - name_length        ;Max length of number portion  
            if num_length LE 0 then  $ 
                  message, 'Keyword length must be 8 characters or less'

;  Otherwise, extend NAME with blanks to eight characters.

    endif else begin  
                while strlen(nam) LT 8 do nam += ' ' ;Make 8 chars long
                vector = 0      
    endelse


;  If of the form 'keyword*', then find all instances of 'keyword' followed by
;  a number.  Store the positions of the located keywords in IFOUND, and the
;  value of the number field in NUMBER.

        histnam = (nam eq 'HISTORY ') || (nam eq 'COMMENT ') || (nam eq '') 
        keyword = strmid( hdr, 0, 8)
	    number = 0
 
        if vector then begin
            ifound = where(strpos(keyword,nam) GE 0, matches)
            if  matches GT 0  then begin
                numst= strmid( hdr[ifound], name_length, num_length)
	        	igood = where(VALID_NUM(numst,/INTEGER), matches)

				if matches GT 0 then begin 
		     			ifound = ifound[igood]
             			number = long(numst[igood])
		     			g = where(number GT 0, matches)
 		     			if matches GT 0 then number = number[g]
				endif 
           endif

;  Otherwise, find all the instances of the requested keyword.  If more than
;  one is found, and NAME is not one of the special cases, then check if DUP keyword 
;  supplied to determine which one to use.   If DUP not supplied then issue a
;  warning and use the *last* appearance.
;  

        endif else begin
            ifound = where(keyword EQ nam, matches)
             if (matches GT 1) && ~histnam then begin
                if N_elements(dup) EQ 1 then begin
                if dup LE matches then begin 
                        ifound = ifound[dup-1] 
                        matches = 1
                endif else begin
                   message,/inf,'Warning - keyword ' + strtrim(nam,2) + $
                   ' located ' + strtrim(matches,2) + ' times in FITS header'
                   message,/inf,'But DUP specified as ' + strtrim(dup,2)
                endelse 
                endif else begin  
                if ~keyword_set(silent) then $
                message,/informational, 'Warning - keyword ' +   $
                strtrim(nam,2) + ' located more than once in ' + abort
                endelse
        endif
        endelse


; Process string parameter 

 if matches GT 0 then begin
  line = hdr[ifound]
  svalue = strtrim( strmid(line,9,71),2)
  if histnam then $
       value = strtrim(strmid(line,8,72),2) else for i = 0,matches-1 do begin
      if ( strmid(svalue[i],0,1) EQ "'" ) then begin   ;Is it a string?
                  test = strmid( svalue[i],1,strlen( svalue[i] )-1)
                  next_char = 0
                  off = 0
                  value = '' 
          NEXT_APOST:
                  endap = strpos(test, "'", next_char)      ;Ending apostrophe  
                  if endap LT 0 then $ 
                            MESSAGE,'Value of '+name+' invalid in '+abort
                  value += strmid( test, next_char, endap-next_char )  

;  Test to see if the next character is also an apostrophe.  If so, then the
;  string isn't completed yet.  Apostrophes in the text string are signalled as
;  two apostrophes in a row.

                 if strmid( test, endap+1, 1) EQ "'" then begin    
                    value += "'"
                    next_char = endap+2         
                    goto, NEXT_APOST
                 endif      

; Extract the comment, if any
                
                slash = strpos( test, "/", endap )
                if slash LT 0 then comment = '' else    $
                        comment = strmid( test, slash+1, strlen(test)-slash-1 )

; This is a string that could be continued on the next line.  Check this
; possibility with the following four criteria: *1) Ends with '&'
; (2) Next line is CONTINUE  (3) LONGSTRN keyword is present (recursive call to
; SXPAR) 4. /NOCONTINE is not set

    if ~keyword_set(nocontinue) then begin
                off++
                val = strtrim(value,2)

                if (strlen(val) gt 0) && $
                  (strmid(val, strlen(val)-1, 1) EQ '&') && $
                  (strmid(hdr[ifound[i]+off],0,8) EQ 'CONTINUE') then $
		      if ~array_equal(keyword EQ 'LONGSTRN',0b) then begin 
                  value = strmid(val, 0, strlen(val)-1)
                  test = hdr[ifound[i]+off]
                  test = strmid(test, 8, strlen(test)-8)
                  test = strtrim(test, 2)
                  if strmid(test, 0, 1) NE "'" then message, $
                    'ERROR: Invalidly CONTINUEd string in '+ abort
                  next_char = 1
                  GOTO, NEXT_APOST
                ENDIF
    ENDIF


; Process non-string value  

          endif else begin
               value = missing_value
               test = svalue[i]
               if test EQ '' then begin
                        comment = ''
                        GOTO, got_value
                endif
                slash = strpos( test, "/" )
                if slash GE 0 then begin
                        comment = strmid( test, slash+1, strlen(test)-slash-1 )
                        if slash GT 0 then test = strmid(test, 0, slash) else $
                            GOTO, got_value
                endif else comment = ''

; Find the first word in TEST.  Is it a logical value ('T' or 'F') ?

                test2 = test
                value = gettok(test2,' ')
                true = 1b
                false = 0b
                if !VERSION.RELEASE GE 8.4 then begin
                	true =  boolean(true) 
                	false = boolean(false)
               endif                      

               if ( value EQ 'T' ) then value = true else $
               if ( value EQ 'F' ) then value = false else begin

;  Test to see if a complex number.  It's  a complex number if the value and
;  the next word, if any, are both valid values.

                if strlen(test2) EQ 0 then goto, NOT_COMPLEX
                value2 = gettok( test2, ' ') 
                if value2 EQ '' then goto, NOT_COMPLEX
                On_ioerror, NOT_COMPLEX
                value2 = float(value2)
                value = complex(value,value2)
                goto, GOT_VALUE

;  Not a complex number.  Decide if it is a floating point, double precision,
;  or integer number.

NOT_COMPLEX:
                On_IOerror, GOT_VALUE
                 
                  if (strpos(value,'.') GE 0) || (strpos(value,'E') GT 0) $
                  || (strpos(value,'D') GE 0) then begin  ;Floating or double?
                      if ( strpos(value,'D') GT 0 ) || $  ;Double?
                         ( strlen(value) GE 8 ) then value = double(value) $
                                                else value = float(value)
                       endif else begin                   ;Long integer
                            lmax = 2.0d^31 - 1.0d
                            lmin = -2.0d^31      ;Typo fixed Feb 2010

                            if strmid(value,0,1) NE '-' then begin 
                            		value =ulong64(value) 
                            		if value lt ulong64(2)^63-1 then value = long64(value)
                            endif else value = long64(value)
                            if (value GE lmin) && (value LE lmax) then $
                                value = long(value) 
                       endelse

GOT_VALUE:
                On_IOerror, NULL
                endelse
             endelse; if c eq apost

;  Add to vector if required

         if vector then begin
               if ( i EQ 0 ) then begin
                     maxnum = max(number)
                     dtype = size(value,/type)
                     result = make_array( maxnum, TYPE = dtype )
                     comments = strarr( maxnum )
               endif 
               if size(value,/type) GT dtype then begin   ;Do we need to recast?
                    result = result + 0*value
                    dtype = size(value,/type)
               endif
               result[ number[i]-1 ] =  value
               comments[ number[i]-1 ] = comment
          endif else $
                comments = comment
  endfor

  if vector then begin
         !ERR = matches     
         return, result
  endif else !ERR = 0

endif  else  begin  

     if abort_return then message,/CON, $
        'Keyword '+ strtrim(nam,2) + ' not found in '+abort
     !ERR = -1
endelse     

return, value       

END                 
