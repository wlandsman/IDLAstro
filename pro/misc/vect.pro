function VECT,vctr,form,Format=Format,delim=delim
;+
; NAME:
;	VECT
; PURPOSE:
;	Print a set of numbers as a string with delimiters included
; EXPLANATION:
;	This function returns the given vector in parenthesized coordinates
;	as in the form (X,Y).  No limit on the number of dimensions.  Also
;	note that the vector does not need to be numbers.  It may also be a
;	string vector.  e.g. ['X','Y']
;
; CALLING SEQEUNCE:
;	tmp = VECT( vctr, [ form, FORMAT = , DELIM =  ] )
; INPUT:
;	VCTR      The vector to be displayed  e.g. [56,44]
;
; OPTIONAL KEYWORD INPUT:
;	FORMAT    This KEYWORD allows the specification of a format for the
;		elements.  e.g.: VECT([2,3],format='(f7.1)') gives '(2.0,3.0)'
;	DELIM     This KEYWORD specifies the delimeter.  The default is ',' but
;		other useful examples might be ', ' or ':'
;
; OPTIONAL INPUT
;	FORM      This parameter may be used instead of the keyword FORMAT
;
; OUTPUT:
;	tmp       A returned string of the parenthesized vector
;
; Other Procedures/Functions Called:
;	STRN
;
; HISTORY:
;	03-JUL-90 Version 1 written by Eric W. Deutsch
;	24-AUG-91 Format='' keyword added (E. Deutsch)
;	29-AUG-91 FORM parameter added (E. Deutsch)
;	Converted to IDL V5.0   W. Landsman   September 1997
;-

  if (n_params(0) lt 1) then begin
    print,'Call: IDL> stringvar=VECT(vector,[FORMAT],[FORMAT=])'
    print,"e.g.: IDL> tmp=VECT([512,512]) & print,'Center: ',tmp"
    return,''
    endif
  if (n_params(0) lt 2) then FORM=''
  if (n_elements(vctr) lt 1) then return,''
  if (n_elements(Format) eq 0) then Format=''
  if (n_elements(delim) eq 0) then delim=','
  if (FORM ne '') then Format=FORM

  tmp='('
  for i=0,n_elements(vctr)-1 do begin
    sep=delim
    if (i eq 0) then sep=''
    if (Format eq '') then tmp=tmp+sep+strn(vctr[i]) $
    else tmp=tmp+sep+strn(vctr[i],Format=Format)
    endfor
  tmp=tmp+')'

  return,tmp
end
