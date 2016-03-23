; docformat = 'rst'
;+ 
;  NAME:
;      FILE_LAUNCH
;
;  PURPOSE: 
;      Launch a file using the default application of the operating system 
;
;  EXPLANATION:
;      The FILE_LAUNCH procedure procedure will launch a file (e.g. a .pdf, .docx or .html
;      file)  using the default application of the operating system.    By default, it 
;      first  tries to use the Java desktop class.   
;              https://docs.oracle.com/javase/tutorial/uiswing/misc/desktop.html 
;      If this fails, it uses the appropriate Spawn command for the oS to launch  
;
;  CALLING SEQUENCE:
;     file_launch, file, [ buseJava,  ojDesktop = ojDesktop, /QUIET ] 
;
;  INPUT PARMAMTER:
;     file: in, required, type=string
;          scalar filename (with path if required) to launch   
;
;  OPTIONAL INPUT KEYWORD:
;       bUseJava: in, optional, type=boolean, default=1
;           Flag to indicate if java should be used to launch browser.
;           True by default. Routine falls back to spawn commands if desktop is
;           not supported.
;
;       /NoWait - if set, then if using Spawn, wait for the command to return
;           This is slower but is useful for debugging
;
;       /quiet - if set, then don't print a message when forced to use SPAWN
;
;   OPTIONAL OUTPUT KEYWORD:
;     ojDesktop : in, out, optional, type=object
;             reference to a java AWT desktop instance
;       
;  EXAMPLE:
;
;       Open a PDF file test.pdf in the current directory
;       IDL> file_launch, 'test.pdf'
;
;
;  HISTORY:
;     First release W. Landsman      March 2016
;          Heavily based on code by Derek Sabatke
;     
;-
;-----------------------------------------------------------------------------

pro file_launch, file, ojDesktop = ojDesktop, bUseJava = bUseJava, quiet=quiet, $
          Nowait = nowait
  COMPILE_OPT idl2, HIDDEN
  
  if ~file_test(file) then begin
    message,/CON,'ERROR -- File not found ' + file
    return
  endif
  ;set option defaults
  setdefaultvalue, bUseJava, 1L
  setdefaultvalue, NoWait, 0

  Catch,theError
  if theError NE 0 then begin
      Catch,/Cancel
      if bUseJava EQ 1 then bUseJava = 0 else begin    ;If Java failed then use Spawn
             void = cgErrorMsg(/quiet)
             return
      endelse       
  endif    

  ;initialize variables
  if bUseJava && ((N_elements(ojDesktop) eq 0) || (~obj_valid(ojDesktop))) then begin 
    oJavaAWTDesktop = OBJ_NEW('IDLJavaObject$Static$JAVA_AWT_DESKTOP', 'java.awt.Desktop')
    if oJavaAWTDesktop.isDesktopSupported() then ojDesktop = ojavaAWTDesktop.getDesktop() $
    else bUseJava = 0L
  endif
  
  if bUseJava && ojDesktop.isDesktopSupported() then begin ; have java do the launching if possible
    if !VERSION.OS_FAMILY NE 'WINDOWS' then fname = file_search(file,/full) $
                                       else fname = file	
    sCleanOutputFN = strjoin(strsplit(fname, '\\', /extract), '/') ;purge (possible) backslashes
    oJURI = OBJ_NEW('IDLJavaObject$Static$JAVA_NET_URI', 'java.net.URI')
    oJString = OBJ_NEW('IDLJavaObject$JAVA_LANG_STRING', 'java.lang.String', 'file://'+sCleanOutputFN)
    oURI = oJURI.create(oJString)
    
    ojDesktop.browse, oURI

  endif else begin; no java, so try spawning a command
       if ~keyword_set(quiet) then message,'Using Spawn',/INF
   if !VERSION.OS_NAME EQ 'Mac OS X' then begin 
          cmd = 'open "'+ file +'" '
          if ~nowait then cmd += '&'
          spawn,cmd
    endif else begin
    case StrUpCase(!Version.OS_Family) OF
      'WINDOWS': spawn, 'start "" "'+ file +'"', nowait = nowait
         'UNIX': begin 
                cmd = 'xdg-open "'+ file +'" '
                if ~nowait then cmd+= '&'
                spawn,cmd
                end
           else: print, 'Unable to launch ' + file + ' automatically.'
    endcase
    endelse

  endelse
end
