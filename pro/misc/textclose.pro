pro textclose,textout=textout
;+
; NAME:
;	TEXTCLOSE                   
;
; PURPOSE:
;	Close a text outpu file previously opened with TEXTOPEN 
; EXPLANATION:
;	procedure to close file for text output as specifed
;	by the (non-standard) system variable !TEXTOUT. 
;
; CALLING SEQUENCE:
;	textclose, [ TEXTOUT = ]
;
; KEYWORDS:
;	textout - Indicates output device that was used by
;		TEXTOPEN
;
; SIDE EFFECTS:
;	if !textout is not equal to 5 and the textunit is
;	opened.   Then unit !textunit is closed and released
;
; HISTORY:
;	D. Lindler  Dec. 1986  (Replaces PRTOPEN)
;	Test if TEXTOUT is a scalar string   W. Landsman   August 1993
; Can't close unit -1 (Standard Output) I. Freedman  April  1994
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;-----------------------------------------------------------
; CLOSE PROPER UNIT
;

 if N_elements( textout ) EQ 0 then textout = !textout ;use default

 ptype = size( textout )           ;Test if TEXTOUT is a scalar string
 if ptype[1] EQ 7  then text_out = 6 else text_out = textout

 if ( text_out NE 5 ) then begin
	if !textunit ne 0 AND !textunit ne -1 then begin
		free_lun, !TEXTUNIT  
                !textunit = 0
	end
 end

 return
 end
