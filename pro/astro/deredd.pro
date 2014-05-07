pro deredd,Eby,by,m1,c1,ub,by0,m0,c0,ub0, update = update
;+
; NAME:
;     DEREDD
;
; PURPOSE:
;     Deredden stellar Stromgren parameters given for a value of E(b-y)
; EXPLANATION:
;     See the procedure UVBYBETA for more info.
;
;  CALLING SEQUENCE:
;     deredd, eby, by, m1, c1, ub, by0, m0, c0, ub0, /UPDATE
;
;  INPUTS:
;     Eby - color index E(b-y),scalar  (E(b-y) = 0.73*E(B-V) )
;     by - b-y color (observed)
;     m1 - Stromgren line blanketing parameter (observed)
;     c1 - Stromgren Balmer discontinuity parameter (observed)
;     ub - u-b color (observed)
;
;     These input values are unaltered unless the /UPDATE keyword is set
;  OUTPUTS:
;     by0 - b-y color (dereddened)
;     m0 - Line blanketing index (dereddened)
;     c0 - Balmer discontinuity parameter (dereddened)
;     ub0 - u-b color (dereddened)
;
;  OPTIONAL INPUT KEYWORDS:
;     /UPDATE - If set, then input parameters are updated with the dereddened
;           values (and output parameters are not used).
;  REVISION HISTORY:
;     Adapted from FORTRAN routine DEREDD by T.T. Moon 
;     W. Landsman          STX Co.        April, 1988
;     Converted to IDL V5.0   W. Landsman   September 1997
;-   
 if N_Params() LT  2 then begin
       print,'Syntax - DEREDD, eby, by, m1, c1, ub, by0, m0, c0, ub0'
       return
 endif            

 Rm1 = -0.33 & Rc1 = 0.19 & Rub = 1.53 
 Eby0 = Eby >0
 if keyword_set(update) then begin
       by = by - eby0
       if N_elements(m1) GT 0 then m1 = m1 - Rm1*Eby0
       if N_elements(c1) GT 0 then c1 = c1 - Rc1*Eby0
       if N_elements(ub) GT 0 then ub = ub - Rub*Eby0
 endif  else begin  
       by0 = by - Eby0
       m0 = m1 - Rm1*Eby0
       c0 = c1 - Rc1*Eby0
       ub0 = ub - Rub*Eby0
 endelse
 return
 end
