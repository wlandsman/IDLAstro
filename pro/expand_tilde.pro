;+
; NAME:
;      EXPAND_TILDE()
;               
; PURPOSE: 
;       Expand tilde in UNIX directory names
;               
; CALLING SEQUENCE: 
;       IDL> output=expand_tilde(input)
;    
; INPUTS: 
;       INPUT = input file or directory name, scalar string
;
; OUTPUT:
;       Returns expanded filename, scalar string
;               
; EXAMPLES: 
;       output=expand_tilde('~zarro/test.doc')
;               ---> output='/usr/users/zarro'
;
; NOTES:
;       This version of EXPAND_TILDE differs from the version in the Solar
;       Library in that it does not call the functions EXIST and IDL_RELEASE.
;       However, it should work identically.
; PROCEDURE CALLS:
;       None.
; REVISION HISTORY: 
;       Version 1,  17-Feb-1997,  D M Zarro.  Written
;       Transfered from Solar Library   W. Landsman   Sep. 1997
;       Made more robust  D. Zarro/W. Landsman  Sep. 2000
;       Made even more robust (since things like ~zarro weren't being expanded)
;       Zarro (EITI/GSFC, Mar 2001)
;-            

 function expand_tilde,name
 if N_elements(name) EQ 0 then return,''
 if size(name,/TNAME) ne 'STRING' then return,name
 tpos=strpos(name,'~')
 if tpos eq -1 then return,name
 apos = strpos(name,'~/')
 bpos = strpos(name,'/~')

 tilde=name
 if apos GT -1 then begin
    tilde = strmid(name,0,apos+1)
    post = strmid(name,apos+1,strlen(name))
 endif else begin
   if bpos gt -1 then begin
            pre = strmid(name,0,bpos+1)
            tilde = strmid(name,bpos+1,strlen(name))
   endif
 endelse
 
  error=0
  catch,error
  if error ne 0 then begin
     catch,/cancel
     return,name
  endif
 
 cd,tilde,curr=curr
 cd,curr,curr=dcurr
 tname = dcurr
 if N_elements(pre) GT 0 then tname = pre+tname else $
    if N_elements(post) GT 0 then tname = tname + post

 return,tname & end
