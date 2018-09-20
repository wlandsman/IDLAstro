;+
; NAME: 
;	DELVARX
; PURPOSE: 
; 	Undefine up to 10 variables for memory management (can call from routines) 
; EXPLANATION:
;	Similar to the intrinsic DELVAR function, but can be used from any calling level.
;   (DELVAR can only be used at the main level.)    Note, however, that unlike DELVAR,
;   DELVARX does not delete the variables (they will be listed as UNDEFINED when 
;   viewed with HELP), but only makes them undefined and frees their memory
;   
;   Also look at the similar Coyote routine UNDEFINE
;          http://www.idlcoyote.com/programs/undefine.pro
; 
; CALLING SEQUENCE:
; 	DELVARX,  p0, [p1, p2......p9]
;
; INPUTS: 
;	p0, p1...p9 - variables to delete
;
; OBSOLETE KEYWORD:
;   /FREE_MEM -  free memory associated with pointers and objects.  Since this is now the 
;                 DELVARX default (since 2012) this keyword now does nothing.   
;           
; METHOD: 
;	Uses HEAP_FREE and PTR_NEW(/NO_COPY) to undefine variables and free memory   
;
; REVISION HISTORY:
;	Copied from the Solar library, written by slf, 25-Feb-1993
;	Added to Astronomy Library,  September 1995
;   Modified, 26-Mar-2003, Zarro (EER/GSFC) 26-Mar-2003
;       - added FREE_MEM to free pointer/objects
;   Modified, 28-Jan-2012, E. Rykoff (SLAC), W. Landsman - 
;               replace EXECUTE calls with SCOPE_VARFETCH.
;   Clarified documentation  W. Landsman Sep 2018
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

