function getopt,input,type,numopt,count =count
;+
; NAME:
;	GETOPT
; PURPOSE:
;	Convert a string supplied by the user into a valid scalar or vector
; EXPLANATION:
;	Distinct elements in the string may be
;	separated by either a comma or a space.  The output scalar
;	or vector can be specified to be  either  integer or floating
;	point.   A null string is converted to a zero.   
; CALLING SEQUENCE:
;     option = GETOPT( input, [ type, numopt, COUNT = ])
;
; INPUTS:
;	input   - string that was input by user in response to a prompt
;		Arithmetic operations can be included in the string (see
;		examples)
;
; OPTIONAL INPUTS:
;	type    - Either an "I" (integer) or an "F" (floating point) specifying 
;		the datatype of the output vector.  Default is floating point
;
;	numopt  - number of values expected by calling procedure
;		If less than NUMOPT values are supplied the output
;		vector will be padded with zeros.  
; OUTPUTS:
;	option  - scalar or vector containing the numeric conversion of
;		the fields in the string INPUT.  If NUMOPT is not
;		supplied, the number of elements in OPTION will 
;		equal the number of distinct fields in INPUT.
; OPTIONAL INPUT KEYWORD:
;       Count - integer giving the number of values actually returned by
;               GETOPT.   If the input is invalid then COUNT is set to -1
; NOTES:
;	(1) If an input is invalid, Count is set to -1 and the result is set 
;		to 999.
;	(2) GETOPT uses the execute function to interpret the user string.   
;	 	Therefore GETOPT itself cannot be called with the EXECUTE 
;		function.
;	(3) GETOPT has a hard limit of 10 tokens in the input string. 
;
; EXAMPLES:
;	(1)   a = getopt( '3.4,5*4 ', 'I' )    yields   a = [ 3, 20]
;	(2)   a = getopt( '5/2.', 'F', 5)      yields   a = [2.5,0.,0.,0.,0.]
;	(3)   a = getopt( '2*3,5,6')           yields   a = [6.,5.,6.]
;
; REVISON HISTORY:
;	written by B. Pfarr, STX, 5/6/87
;	change value of !ERR W. Landsman   STX,  6/30/88
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
 On_error,2

 Err = 0
 inp = strtrim(input,2)               ;Remove leading & trailing blanks
 comma = strpos(inp,',')              ;look for comma

 if comma GT 0 then char = ',' else char = ' '  ;Delineator is comma or space

 if N_params() LT 2 then option = fltarr(10) else  $
 if strupcase(type) EQ 'I' then option = intarr(10) $
                          else option = fltarr(10) ;Default type is float

 if strlen(inp) EQ 0 then return,0.0   $            ;Null string is 0.0
 else begin
   i =0                                            ;Counts number of tokens
   while inp NE '' do begin

      token = strtrim( gettok(inp,char), 2 )
      if token NE '' then begin

          test = execute( 'option[i] = ' + token) 
          if test NE 1 then begin
                count = -1
                return, 999.9
          endif       
         i = i+1
      endif

   endwhile
 endelse
;

 if N_params() LT 3 then begin

    if i EQ 1 then option = option[0] else $
            option = option[0:i-1]    ;Trim output vector

 endif else option = option[0:numopt-1] 

 count = N_elements(option)
 return,option       ;Successful completion

 end
