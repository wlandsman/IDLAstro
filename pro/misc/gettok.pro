function gettok,st,char, exact=exact, notrim=notrim
;+
; NAME:
;	GETTOK                                    
; PURPOSE:
;	Retrieve the first part of a (vector) string up to a specified character
; EXPLANATION:
;	GET TOKen - Retrieve first part of string until the character char 
;	is encountered.   
;
; CALLING SEQUENCE:
;	token = gettok( st, char, [ /EXACT, /NOTRIM ] )
;
; INPUT:
;	char - character separating tokens, scalar string
;
; INPUT-OUTPUT:
;	st - string to get token from (on output token is removed unless
;            /NOTRIM is set), scalar or vector
;
; OUTPUT:
;	token - extracted string value is returned, same dimensions as st
; OPTIONAL INPUT KEYWORD:
;       /EXACT -  The default behaviour of GETTOK is to remove any leading 
;              blanks and (if the token is a blank) convert tabs to blanks.    
;              Set the /EXACT keyword to skip these steps and leave the 
;              input string unchanged before searching for the  character 
;              tokens. 
;
;      /NOTRIM - if set, then the input string is left unaltered 
; EXAMPLE:
;	If ST is ['abc=999','x=3.4234'] then gettok(ST,'=') would return
;	['abc','x'] and ST would be left as ['999','3.4234'] 
;
; PROCEDURE CALLS:
;       REPCHR()
; HISTORY
;	version 1  by D. Lindler APR,86
;	Remove leading blanks    W. Landsman (from JKF)    Aug. 1991
;       V5.3 version, accept vector input   W. Landsman February 2000
;       Slightly faster implementation  W. Landsman   February 2001
;       Added EXACT keyword  W. Landsman March 2004
;       Assume since V5.4, Use COMPLEMENT keyword to WHERE W. Landsman Apr 2006
;       Added NOTRIM keyword W. L. March 2011
;-
;----------------------------------------------------------------------
  On_error,2                           ;Return to caller
  compile_opt idl2

   if N_params() LT 2 then begin
       print,'Syntax - token = gettok( st, char, [ /EXACT, /NOTRIM] )'
       return,-1
   endif

; if char is a blank treat tabs as blanks

 if ~keyword_set(exact) then begin
    st = strtrim(st,1)              ;Remove leading blanks and tabs
    if char EQ ' ' then begin 
       tab = string(9b)                 
       if max(strpos(st,tab)) GE 0 then st = repchr(st,tab,' ')
    endif
  endif
  token = st

; find character in string

  pos = strpos(st,char)
  test = pos EQ -1
  bad = where(test, Nbad, Complement = good, Ncomplement=Ngood)
  if Nbad GT 0 && ~keyword_set(notrim) then st[bad] = ''
 
; extract token
 if Ngood GT 0 then begin
    stg = st[good]
    pos = reform( pos[good], 1, Ngood )
    token[good] = strmid(stg,0,pos)
    if ~keyword_set(notrim) then st[good] = strmid(stg,pos+1)
 endif

;  Return the result.

 return,token
 end
