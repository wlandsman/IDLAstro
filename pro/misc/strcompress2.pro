function strcompress2, str, chars
;+
; NAME:
;	STRCOMPRESS2
; PURPOSE:
;	Remove blanks around specified characters in a string
; CALLING SEQUENCE
;	newstring = strcompress2( st, chars)
; INPUTS:
;       st - any scalar string
;      chars - scalar  or vector string specifing which characters around which 
;             blanks should be removed.    For example, if chars=['=','-','+'] 
;              then spaces around the three characters "=', '-', and '+' will 
;             be removed.
; OUTPUTS:
;       newstring - input string with spaces removed around the specified 
;        characters.   
; EXAMPLE:
;       The Vizier constraint string (see queryvizier.pro) does not allow 
;       blanks around the operators '=','<', or '>'.     But we do not want
;       to remove blanks around names (e.g. 'NGC 5342'):
; 
;       IDL> st = 'name = NGC 5342, v< 23'
;       IDL> print,strcompress2(st, ['=','<','>'])
;            name=NGC 5342, v<23
; MODIFICATION HISTORY:
;	Written by W.Landsman                   July 2008
;-

 On_error,2
 compile_opt idl2
 st = strcompress(str)    ;Ok to compress to a single space
 if N_elements(chars) GT 1 then op = '(' + strjoin(chars,'|') + ')'   $
                           else op = chars
 
 op1 = ' ' + op      ;first look for Leading space
 n = stregex(st, op1)     
 while n GT 0 do begin
    st = strmid(st,0,n) + strmid(st,n+1)   ;piece string together
    n = stregex(st,op1)      ; Look for another occurrence since stregex just 
 endwhile                    ; gives the first

 op2 = op + ' '    ;Now look for Following space
 n = stregex(st, op2)
 while n GT 0 do begin
    st = strmid(st,0,n+1) + strmid(st,n+2)
    n = stregex(st,op2)
 endwhile 

   return,st
 end
