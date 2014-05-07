PRO TEXTOPEN,PROGRAM,TEXTOUT=TEXTOUT, STDOUT = STDOUT, MORE_SET = more_set, $
             SILENT = silent, WIDTH = width
;+
; NAME:
;       TEXTOPEN
; PURPOSE:
;       Open a device specified by TEXTOUT with unit !TEXTUNIT 
; EXPLANATION:
;       Procedure to open file for text output.   The type of output 
;       device (disk file or terminal screen) is specified by the 
;       TEXTOUT keyword or the (nonstandard) system variable !TEXTOUT.
;
; CALLING SEQUENCE:
;       textopen, program, [ TEXTOUT =, /STDOUT, /SILENT, MORE_SET=, WIDTH= ]
;
; INPUTS:
;       program - scalar string giving name of program calling textopen
;
; OPTIONAL INPUT KEYWORDS:
;       TEXTOUT - Integer scalar (0-7) specifying output file/device to be 
;               opened (see below) or scalar string giving name of output file.
;               If TEXTOUT is not supplied, then the (non-standard) system 
;               variable !TEXTOUT is used.
;       /SILENT - By default, TEXTOPEN prints an informational message when
;                opening a file for hardcopy output.   Set /SILENT (or !QUIET)
;                to suppress this message.
;       /STDOUT - if this keyword is set and non-zero, then the standard output
;               (unit = -1) is used for TEXTOUT=1 or TEXTOUT=2.   The use
;               of STDOUT has  2 possible advantages:
;               (1) the output will appear in a journal file
;               (2) Many Unix machines print spurious control characters when
;               printing to /dev/tty.   These characters are eliminated by 
;               setting /STDOUT
;
;               The disadvantage of /STDOUT is that the /MORE option is not
;               available.
;
;         WIDTH - Specify line width for hardcopy output line wrapping (passed onto OPENW).
;
; OPTIONAL OUTPUT KEYWORD:
;       MORE_SET - Returns 1 if the output unit was opened with /MORE.   This
;               occurs if (1) TEXTOUT = 1 and (2) the device is a tty, and 
;               (3) /STDOUT is not set.      User can use the returned value
;                of MORE_SET to determine whether to end output when user
;                presses 'Q'.
; SIDE EFFECTS:
;       The following dev/file is opened for output.    Different effects
;       occur depending whether the standard output is a GUI (Macintosh,
;       Windows, Unix/IDLTool) or a TTY
;
;               textout=0       Nowhere
;               textout=1       if a TTY then TERMINAL using /more option
;                                   otherwise standard (Unit=-1) output
;               textout=2       if a TTY then TERMINAL without /more option
;                                   otherwise standard (Unit=-1) output
;               textout=3       <program>.prt
;               textout=4       laser.tmp
;               textout=5      user must open file
;               textout=7      same as 3 but text is appended to <program>.prt
;                               file if it already exists.
;               textout = filename (default extension of .prt)
;
;       The unit to be opened is obtained with the procedure GET_LUN
;       unless !TEXTOUT=5.  The unit number is placed in system variable 
;       !TEXTUNIT.  For !TEXTOUT=5 the user must set !TEXTUNIT to the 
;       appropriate unit number.
;
; NOTES:
;       When printing to a TTY terminal, the output will *not* appear in an 
;       IDL JOURNAL session, unlike text printed with the PRINT command.
;
; NON-STANDARD SYSTEM VARIABLES:
;       TEXTOPEN will automatically define the following system variables if
;       they are not previously defined:
;
;       DEFSYSV,'!TEXTOUT',1
;       DEFSYSV,'!TEXTUNIT',0
; HISTORY:
;       D. Lindler  Dec. 1986  
;       Keyword textout added, J. Isensee, July, 1990
;       Made transportable, D. Neill, April, 1991
;       Trim input PROGRAM string W. Landsman  Feb 1993
;       Don't modify TEXTOUT value   W. Landsman   Aug 1993
;       Modified for MacOS  I. Freedman April 1994
;       Modified for output terminals without a TTY  W. Landsman  August 1995
;       Added /STDOUT keyword   W. Landsman    April 1996
;       added textout=7 option, D. Lindler, July, 1996
;       Exit with RETURN instead of RETALL  W. Landsman  June 1999
;       In IDL V5.4 filepath(/TERMINAL) not allowed in the IDLDE WL August 2001
;       Added MORE_SET output keyword   W.Landsman   January 2002
;       Added /SILENT keyword  W. Landsman  June 2002  
;	Define !TEXTOUT and !TEXTUNIT if needed.  R. Sterner, 2002 Aug 27
;       Return Calling Sequence if no parameters supplied W.Landsman Nov 2002
;       Remove VMS specific code  W. Landsman Sep 2006
;       Make sure MORE_SET is always defined   W. Landsman Jan 2007
;       Added WIDTH keyword   J. Bailin Nov 2010
;       Use V6.0 notation  W. Landsman April 2011
;-
;-----------------------------------------------------------
  On_Error,2
  compile_opt idl2

  if N_params() LT 1 then begin
      print,'Syntax - TEXTOPEN, program, [ TEXTOUT =, /STDOUT, /SILENT,' 
      print,'                              MORE_SET=, WIDTH= ]' 
      return
  endif

  defsysv,'!TEXTOUT',exists=ex			; Check if !TEXTOUT exists.
  if ex eq 0 then defsysv,'!TEXTOUT',1		; If not define it.
  defsysv,'!TEXTUNIT',exists=ex			; Check if !TEXTUNIT exists.
  if ex eq 0 then defsysv,'!TEXTUNIT',0		; If not define it.
  more_set = 0                                  
  ;
  ; Open proper unit.
  ;
  if N_elements( textout ) NE 1 then textout = !textout ;use default output dev.

  ; keywords for openw
  if n_elements(width) gt 0 then openw_keywords = {width: width}

  if size(textout,/tname) EQ 'STRING' then begin  ;test if filename entered
        filename = textout
        j = strpos(filename,'.')        ;test if file extension given
        if j lt 0 then filename = filename + ".prt"
        text_out = 6
  endif else text_out = textout     

  if TEXT_OUT eq 5 then begin
     if !TEXTUNIT eq 0 then begin
         print,' '
         print,' You must set !TEXTUNIT to the desired unit number...'
         print,'                    ...see following example'
         print,' '
         print,'                    OPENW, LUN, filename, /GET_LUN
         print,'                    !TEXTUNIT = LUN
         print,'                    DBPRINT...
         print,'
         print,' Action: returning'
         print,' '
         return
     end
     return
  end
   stndout = fstat(-1)
   isatty = (stndout.isatty) && (~stndout.isagui) && $
             (~keyword_set(STDOUT))

   if isatty || (text_out GT 2) then begin 

        if !TEXTUNIT GT 0 then free_lun,!TEXTUNIT 
        get_lun,unit
        !TEXTUNIT = unit

    endif else !TEXTUNIT = -1                     ;standard output

  more_set = (text_out EQ 1) && isatty
  
  case text_out of
     1: if isatty then openw, !TEXTUNIT, filepath(/TERMINAL), /MORE, _extra=openw_keywords

     2: if isatty then openw, !TEXTUNIT, filepath(/TERMINAL) , _extra=openw_keywords

     3: begin
        oname = strlowcase( strtrim( PROGRAM,2) +'.prt')
         openw, !TEXTUNIT, oname, _extra=openw_keywords
        if ~keyword_set(SILENT) then $
        message,'Output is being directed to a file ' + oname,/INFORM
        end

     4: openw, !TEXTUNIT, 'laser.tmp', _extra=openw_keywords

     6: begin
        openw,!TEXTUNIT,filename, _extra=openw_keywords
        if ~keyword_set(SILENT) then $
        message,'Output is being directed to a file ' + filename,/INFORM
        end

     7: begin
        oname = strlowcase(strtrim( PROGRAM,2) +'.prt')
        openw, !TEXTUNIT, oname, /append, _extra=openw_keywords
        if ~keyword_set(SILENT) then $
        message,'Output is being appended to file ' + oname,/INFORM
        for i=0,3 do printf,!textunit,' '       ;added a couple of blank lines
        end

     0: openw,!TEXTUNIT, strtrim(PROGRAM,2) + '.tmp',/DELETE, _extra=openw_keywords

     else: begin
        !textunit = 0
        print,' '
        print,' Invalid value for TEXTOUT =',TEXTOUT
        print,' '
        print,'                 ...the possibilities are:
        print,' '
        print,'                 textout=0      nowhere
        if isatty then begin
                print,'                 textout=1      terminal with /more 
                print,'                 textout=2      terminal without /more 
        endif else begin
                print,'                 textout=1      terminal
                print,'                 textout=2      terminal
        endelse
        print,'                 textout=3      file   <program>.prt
        print,'                 textout=4      file   laser.tmp
        print,'                 textout=5      User supplied file
        print,'                 textout = filename (default extension of .prt)
        print,'                 textout=7      Same as 3 but append the file
        print,' '
        print,' Action: returning
        print,' '
        return
    end
 endcase

 return
 end   ; textout
