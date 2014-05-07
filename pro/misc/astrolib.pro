PRO ASTROLIB
;+
; NAME:
;       ASTROLIB
; PURPOSE:
;       Add the non-standard system variables used by the IDL Astronomy Library
; EXPLANATION: 
;       Also defines the environment variable ASTRO_DATA pointing to the 
;       directory containing data files  associated with the IDL Astronomy 
;       library (system dependent -- user must edit the third line in the
;       program below).
;
; CALLING SEQUENCE:
;       ASTROLIB
;
; INPUTS:
;       None.
;
; OUTPUTS:
;       None.
;
; METHOD:
;       The non-standard system variables !PRIV, !TEXTUNIT, and 
;       !TEXTOUT are added using DEFSYSV.
;
; REVISION HISTORY:
;       Written, Wayne Landsman, July 1986.
;       Use DEFSYSV instead of ADDSYSVAR           December 1990
;       Test for system variable existence before definition    July 2001
;       Assume since V55, remove VMS support  W. Landsman   Sep 2006
;       Remove !Debug, comment out ASTRO_DATA definition  WL  Jan 2009 
;-
  On_error,2
  compile_opt idl2

;  User should edit the folowing line and uncomment it to give the location of 
;  ASTRO_DATA on their own system (or define it in their .cshrc or .bashrc file).     
;  setenv,'ASTRO_DATA=/export/home/ftp/pub/data/'

  defsysv, '!PRIV', exist = exist 
     if ~exist then defsysv, '!PRIV', 0
  defsysv, '!TEXTUNIT', exist = exist
     if ~exist then  defsysv, '!TEXTUNIT', 0
  defsysv, '!TEXTOUT', exist = exist 
     if ~exist then defsysv, '!TEXTOUT', 1 

   message,'Astronomy Library system variables have been added',/INF

  return
  end
 
