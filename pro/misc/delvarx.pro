;+
; NAME: 
;	DELVARX
; PURPOSE: 
; 	Delete up to 10 variables for memory management (can call from routines) 
; EXPLANATION:
;	Like intrinsic DELVAR function, but can be used from any calling level
;   
;       Modified in January 2012 to always free memory associated with
;       pointers/objects and remove the use of EXECUTE()
;       Also look at the Coyote routine UNDEFINE
;          http://www.idlcoyote.com/programs/undefine.pro
; 
; CALLING SEQUENCE:
; 	DELVARX,  p0, [p1, p2......p9]
;
; INPUTS: 
;	p0, p1...p9 - variables to delete
;
; OBSOLETE KEYWORD:
;       /FREE_MEM -  formerly freed memory associated with pointers 
;                   and objects.  Since this is now the DELVARX default this 
;                   keyword does nothing.   
;           
; METHOD: 
;	Uses HEAP_FREE and PTR_NEW(/NO_COPY) to delete variables and free
;       memory   
;
; REVISION HISTORY:
;	Copied from the Solar library, written by slf, 25-Feb-1993
;	Added to Astronomy Library,  September 1995
;       Modified, 26-Mar-2003, Zarro (EER/GSFC) 26-Mar-2003
;       - added FREE_MEM to free pointer/objects
;       Modified, 28-Jan-2012, E. Rykoff (SLAC), W. Landsman - 
;               replace EXECUTE calls with SCOPE_VARFETCH.
;-

PRO delvarx, p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,free_mem = free_mem

 npar = N_params()      ; Number of parameters
 pp = 'p'+strtrim(indgen(npar),1)

 for i=0,npar-1 do begin
    defined = N_elements( SCOPE_VARFETCH(pp[i],LEVEL=0))   
    if LOGICAL_TRUE(defined) then $
             heap_free, ptr_new( SCOPE_VARFETCH(pp[i],LEVEL=0),/no_copy) 
        
 endfor

 return
 end

