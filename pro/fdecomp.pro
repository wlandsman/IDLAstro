pro fdecomp, filename, disk, dir, name, qual, version, OSfamily = osfamily
;+
; NAME:
;     FDECOMP
; PURPOSE:
;     Routine to decompose file name(s) for any operating system.
;
; CALLING SEQUENCE:
;     FDECOMP, filename, disk, dir, name, qual, [OSFamily = ]
;
; INPUT:
;     filename - string file name(s), scalar or vector
;
; OUTPUTS:
;     All the output parameters will have the same number of elements as 
;       input filename 
;
;       disk - disk name, always '' on a Unix machine, scalar or vector string
;       dir - directory name, scalar or vector string
;       name - file name, scalar or vector string 
;       qual - qualifier, set equal to the characters beyond the last "."
;
; OPTIONAL INPUT KEYWORD:
;     OSFamily -  scalar string specifying the operating system, must be either
;             'Windows' or 'unix'.    If not supplied,
;             then !VERSION.OS_FAMILY is used to determine the OS.
; EXAMPLES:
;     Consider the following file names 
;
;     unix:    file = '/itt/idl71/avg.pro' 
;     Windows: file =  'd:\itt\idl71\avg.pro'
;       
;     then IDL> FDECOMP,  file, disk, dir, name, qual
;       will return the following
;
;                 Disk             Dir          Name        Qual    
;       Unix:      ''            '/itt/idl71/'  'avg'       'pro'   
;       Windows:    'd:'         \itt\idl71\    'avg'       'pro'   
;
; NOTES:
;     (1) The period is removed between the name and qualifier 
;     (2) Unlike the intrinsic FILE_BASENAME() and FILE_DIRNAME() functions,
;         one can use FDECOMP to decompose a Windows file name on a Unix machine
;         or a Unix filename on a Windows machine.
;
; ROUTINES CALLED:
;     None.
; HISTORY
;     version 1  D. Lindler  Oct 1986
;     Include VMS DECNET machine name in disk    W. Landsman  HSTX  Feb. 94
;     Converted to Mac IDL, I. Freedman HSTX March 1994          
;     Major rewrite to accept vector filenames V5.3   W. Landsman June 2000
;     Fix cases where disk name not always present  W. Landsman  Sep. 2000
;     Make sure version defined for Windows  W. Landsman April 2004
;     Include final delimiter in directory under Windows as advertised
;                W. Landsman   May 2006
;     Remove VMS support, W. Landsman    September 2006
;     Remove MacOS branch (same as Unix) W. Landsman  August 2009
;-
;--------------------------------------------------------
;
  On_error,2                            ;Return to caller
  compile_opt idl2

  if N_params() LT 2 then begin
     print, 'Syntax - FDECOMP, filename, disk, [dir, name, qual ] '
     return
  endif
    

  if ~keyword_set(osfamily) then osfamily = !Version.OS_Family
       st = filename
     disk = st
     replicate_inplace,disk,''
     dir = disk
     qual = disk


 if OSFAMILY EQ "Windows" then begin 
 
     lpos = strpos( st, ':')                 ; DOS diskdrive (i.e. c:)
     good = where(lpos GT 0, Ngood) 
     if Ngood GT 0 then begin
         stg = st[good]
         lpos = reform( lpos[good], 1, Ngood)
         disk[good] = strmid( stg, 0, lpos+1) 
         st[good] = strmid(stg,lpos+1 )
     endif

;  Search the path name (i.e. \dos\idl\) and locate last backslash

     lpos = strpos(st,'\',/reverse_search)
     good = where(lpos Gt 0, Ngood)

 
 endif ELSE begin                 ;Unix

 
; Unix directory name ends at last slash

    lpos = strpos(st,'/',/reverse_search)
    good = where(lpos GE 0, Ngood)
 
  endelse
    
  if Ngood GT 0 then begin      ;Extract directory name if present
          stg = st[good] 
          lpos = reform( lpos[good],1, Ngood )
 
             dir[good] = strmid(stg,0, lpos+1) 
             st[good] = strmid(stg,lpos+1 )
    endif
  
; get  name and qualifier (extension)...qual is optional

    lpos = strpos(st,'.',/reverse_search)
    good = where(lpos GE 0, Ngood)
    name = st

    if Ngood GT 0 then begin
             stg = st[good]
             lpos = reform(lpos[good], 1, Ngood)
 
             name[good] = strmid(stg,0,lpos )
             qual[good] = strmid(stg,lpos+1 )
     endif


 return
  end
