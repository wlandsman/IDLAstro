function spec_dir,filename,extension
;+
; NAME:
;     SPEC_DIR()
; PURPOSE:
;     Complete a file specification by appending the default disk or directory
;
; CALLING SEQUENCE:                      
;     File_spec = SPEC_DIR( filename, [ extension ] )
; INPUT:
;     filename - character string giving partial specification of a file
;               name.  Examples for different operating systems include the
;                       following:
;               Unix: 'pro/test.dat', '$IDL_HOME/test','~/subpro'
;               MacOS: ':Programs:test'
;               Windows: '\pro\test.dat','d:\pro\test'
;
; OPTIONAL INPUT:
;     exten - string giving a default file name extension to be used if
;             filename does not contain one.  Do not include the period.
;
; OUTPUT:
;     File_spec - Complete file specification using default disk or 
;               directory when necessary.  
;
; EXAMPLE:
;      IDL> a = spec_dir('test','dat')
;
;      is equivalent to the commands
;      IDL> cd, current=cdir
;      IDL> a = cdir + delim + 'test.dat'
;
;      where delim is the OS-dependent separator 
; METHOD:
;      SPEC_DIR() decomposes the file name using FDECOMP, and appends the 
;      default directory (obtained from the FILE_EXPAND_PATH) if necessary.   
;
;      SPEC_DIR() does not check whether the constructed file name actually
;      exists.
; PROCEDURES CALLED:
;      FDECOMP, EXPAND_TILDE()
; REVISION HISTORY:
;      Written W. Landsman         STX         July, 1987
;      Expand Unix tilde if necessary              W. Landsman  September 1997
;      Assume since V5.5, use FILE_EXPAND_PATH, remove VMS support        
;              W. Landsman   September 2006
;-
 On_error,2                                     ;Return to user
 compile_opt idl2
 fdecomp,filename,disk,dir,name,ext
 if N_elements(extension) GT 0 then $ 
      if (ext EQ '') then ext =  extension
     
 dir = disk+ dir    
 if !VERSION.OS_FAMILY EQ 'unix' then $ 
     if strpos(dir,'~') GE 0 then dir = expand_tilde(dir) 
     
 dir = file_expand_path(disk+dir)
 return, dir + path_sep() + name + '.' + ext
  end
