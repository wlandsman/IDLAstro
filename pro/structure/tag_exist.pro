;+
; NAME:        
;       TAG_EXIST()
; PURPOSE:              
;       To test whether a tag name exists in a structure.
; EXPLANATION:               
;       Routine obtains a list of tagnames and tests whether the requested one
;       exists or not. The search is recursive so if any tag names in the 
;       structure are themselves structures the search drops down to that level.
;       (However, see the keyword TOP_LEVEL).
;               
; CALLING SEQUENCE: 
;       status = TAG_EXIST(str, tag, [ INDEX =, /TOP_LEVEL, /QUIET ] )
;    
; INPUT PARAMETERS:     
;       str  -  structure variable to search
;       tag  -  tag name to search for, scalar string
;
; OUTPUTS:
;       Function returns 1b if tag name exists or 0b if it does not.
;                              
; OPTIONAL INPUT KEYWORD:
;       /TOP_LEVEL = If set, then only the top level of the structure is
;                           searched.
;       /QUIET - if set, then do not print messages if invalid parameters given
;       /RECURSE - does nothing but kept for compatibility with the
;                  Solarsoft version for which recursion is not the default 
;        http://sohowww.nascom.nasa.gov/solarsoft/gen/idl/struct/tag_exist.pro
; OPTIONAL OUTPUT KEYWORD:
;       INDEX = index of matching tag, scalar longward, -1 if tag name does
;               not exist
;
; EXAMPLE:
;       Determine if the tag 'THICK' is in the !P system variable
;       
;       IDL> print,tag_exist(!P,'THICK')
;
; PROCEDURE CALLS:
;       None.
;
; MODIFICATION HISTORY:     : 
;       Written,       C D Pike, RAL, 18-May-94               
;       Passed out index of matching tag,  D Zarro, ARC/GSFC, 27-Jan-95     
;       William Thompson, GSFC, 6 March 1996    Added keyword TOP_LEVEL
;       Zarro, GSFC, 1 August 1996    Added call to help 
;       Use SIZE(/TNAME) rather than DATATYPE()  W. Landsman  October 2001
;       Added /RECURSE and /QUIET for compatibility with Solarsoft version
;                W. Landsman  March 2009
;       Slightly faster algorithm   W. Landsman    July 2009
;       July 2009 update was not setting Index keyword  W. L   Sep 2009.
;       Use V6.0 notation W.L. Jan 2012 
;        Not setting index again, sigh  W.L./ K. Allers  Jan 2012
;-            

function tag_exist, str, tag,index=index, top_level=top_level,recurse=recurse, $
         quiet=quiet

;
;  check quantity of input
;
compile_opt idl2
if N_params() lt 2 then begin
   print,'Use:  status = tag_exist(structure, tag_name)'
   return,0b
endif

;
;  check quality of input
;

if size(str,/TNAME) ne 'STRUCT' or size(tag,/TNAME) ne 'STRING' then begin
 if ~keyword_set(quiet) then begin 
   if size(str,/TNAME) ne 'STRUCT' then help,str
   if size(tag,/TNAME) ne 'STRING' then help,tag
   print,'Use: status = tag_exist(str, tag)'
   print,'str = structure variable'
   print,'tag = string variable'
  endif 
   return,0b
endif

  tn = tag_names(str)

  index = where(tn eq strupcase(tag), nmatch)

 if ~nmatch && ~keyword_set(top_level) then begin
       status= 0b
       for i=0,n_elements(tn)-1 do begin
        if size(str.(i),/TNAME) eq 'STRUCT' then $
                status=tag_exist(str.(i),tag,index=index)
        if status then return,1b
      endfor
    return,0b

endif else begin
    index = index[0] 
    return,logical_true(nmatch)
 endelse
end
