pro curs, sel
;+
; NAME:
;       CURS
; PURPOSE:
;       Selects an X windows cursor shape
; CALLING SEQUENCE:
;       curs            ;Interactively select a cursor shape.
;       curs, sel       ;Make the given CURSOR_STANDARD value the cursor
;                        shape.
; OPTIONAL INPUT:
;       sel  -  Either an integer giving the CURSOR_STANDARD value (usually an 
;               even value between 0 and 152) indicating the cursor shape, or 
;               a string from the following menu
;       a -- Up arrow              
;       b -- Left-angled arrow
;       c -- Right-angled arrow
;       d -- Crosshair
;       e -- Finger pointing left 
;       f -- Finger pointing right
;       g -- Narrow crosshair
;       h -- Cycle through all possible standard cursor shapes
; 
;       The full list of available cursor values is given in 
;      /usr/include/X11/cursorfont.h
; OUTPUTS:
;       None.
; RESTRICTIONS:
;       Uses the CURSOR_STANDARD keyword of the DEVICE procedure.  Although 
;       this keyword is available in Windows IDL, the values
;       used by this procedure are specific to the X windows device.
;
; PROCEDURE:
;       If the user supplies a valid cursor shape value, it is set.  Otherwise,
;       an interactive command loop is entered; it will continue until a valid
;       value is given.
; MODIFICATION HISTORY:
;       Converted to VAX 3100 workstations / IDL V2.  M. Greason, STX, May 1990.
;       Avoid bad cursor parameter values  W. Landsman   February, 1991
;       Don't change value of input param        W. Landsman   August 1995
;       Use SIZE(/TNAME) instead of DATATYPE()   W. Landsman  October 2001
;-
On_error,2
if !D.NAME NE 'X' then message, $
     'ERROR - Requires an X-windows display, current device is ' + !D.NAME
;                       Check parameter.
;
isel = indgen(76)*2
nsel = n_elements(isel)
;
IF N_elements( sel ) EQ 0 THEN sel = 0
;
;                       Get the selection interactively, if not already
;                       specified.
;
;                               Initialize.
;
mnu = ["  a -- Up arrow", "  b -- Left-angled arrow", $
       "  c -- Right-angled arrow", "  d -- Crosshair", $
       "  e -- Finger pointing left", "  f -- Finger pointing right", $
       "  g -- Narrow crosshair", $
       "  h -- Cycle through all possible standard cursor shapes", $
       "  i -- Enter cursor shape number directly", "  j -- Quit"]
nmnu = n_elements(mnu)
fmt = "($,'Code ',I3,'      ',I3,' of ',I3,'      ')"
IF size(sel,/TNAME) EQ 'STRING' then begin
             cmd = strupcase(sel)
             csel = -99
ENDIF ELSE csel = sel
;
;                               While loop until a selection is made.
;
WHILE (csel LE 0) OR (csel GT isel[nsel-1]) DO BEGIN
;
;                                       Get command.
;
if csel NE -99 then begin
        print, "Cursor selection:"
        print, "   "
        FOR i = 0, (nmnu-1) DO print, mnu[i]
        print, "   "
        cmd = ''
        read, "Enter the letter of the desired command: ",cmd
endif
;
;                                       Perform the command.
;
MENU:   CASE strupcase(cmd) OF
                 'A' : csel = 22                        ; Up arrow
                 'B' : csel = 132               ; Left arrow
                 'C' : csel = 2                 ; Right arrow
                 'D' : csel = 34                        ; X-hair.
                 'E' : csel = 56                        ; Left hand.
                 'F' : csel = 58                        ; Right hand.
                 'G' : csel = 33                        ; Narrow crosshair.
                 'H' : BEGIN                    ; Cycle thru all cursors.
                          print, "  "
                          print, "  "
                          print, "Cycling through the possible cursors."
                          print, "  "
                          print, "Strike the space bar to select, any other"
                          print, "key to reject." 
                          print, "  "
                          print, "  "
                          scr_curmov, 0, 1
                          cont = 1
                          FOR i = 0, (nsel-1) DO BEGIN
                                IF cont THEN BEGIN
                                        csel = isel[i]
                                        print, format=fmt, csel, i+1, nsel
                                        scr_curmov, 2, 31
                                        device, cursor_standard=csel
                                        IF get_kbrd(1) EQ ' ' THEN cont = 0
                                ENDIF
                          ENDFOR
                       END
                 'I' : BEGIN                    ; Get # from user.
                          print, "  "
                          print, "  "
                          print, format="(A14,$)", "Enter cursor #"
                          read, csel
                          IF (csel LE 0) OR (csel GT isel[nsel-1]) THEN $
                                print, "Invalid entry."
                       END
                 'J' : csel = 34                ; Quit.  Set to X-hair.
                ELSE : csel = 0                 ; Invalid command.
        ENDCASE
ENDWHILE
;
;                       Set the cursor shape
;
device, cursor_standard=csel
;
RETURN
END
