FUNCTION qget_string, dummy
;+
; NAME:
;     QGET_STRING
; PURPOSE:
;     To get a string from the keyboard without echoing it to the screen.
;
; CALLING SEQUENCE:
;     string = QGET_STRING() 
;
; INPUTS:
;     None.
;
; OUTPUTS:
;     string   The string read from the keyboard.
;
; SIDE EFFECTS:
;     A string variable is created and filled.
;
; PROCEDURE:
;     The IDL GET_KBRD functions is used to get each character in
;     the string.  Each character is added to the string until a
;     carriage return is struck.  The carriage return is not appended
;     to the string.  Striking the delete key or the backspace key
;     removes the previous character from the string.
;
; NOTES:
;     For a widget password procedure see 
;     http://idlcoyote.com/tip_examples/password.pro
; MODIFICATION HISTORY:
;     Written by Michael R. Greason, STX, 8 January 1991.
;     Work for Mac and Windows IDL  W. Landsman    September 1995
;-
 compile_opt idl2

;                       Variable definitions.
;
 st = bytarr(1)                                 ; String variable.
 n = 0

 IF !VERSION.OS_FAMILY EQ "unix" THEN dun = 10B $       ; Unix version of CR.
                           ELSE dun = 13B       ; All other version of CR.
wt = 1                                          ; Wait for key to be struck?
del = 127B & bs = 8B                            ; Delete, backspace keys.
;
;                       Loop, gathering characters into the string until
;                       a carriage return has been struck.
;
REPEAT BEGIN
;
;                               Get next character.
;
        ch = byte(get_kbrd(wt))
        ch = ch[0]
;
;                               If it isn't a carriage return, process it.
;
        IF (ch NE dun) THEN BEGIN
;
;                                       If it isn't a delete or backspace,
;                                       append it to the string.
;
                IF ((ch NE del) && (ch NE bs)) THEN BEGIN
                        IF (n LE 0) THEN BEGIN
                                st[0] = ch
                                n = 1
                        ENDIF ELSE BEGIN
                                st = [st, ch]
                                n++
                        ENDELSE
                ENDIF ELSE BEGIN
;
;                                       It's a delete/backspace.  Remove the
;                                       previous character.
;
                        IF (n GT 0) THEN BEGIN
                                n--
                                IF (n GT 0) THEN st = st[0:(n-1)]
                        ENDIF
                ENDELSE
        ENDIF
;
ENDREP UNTIL (ch EQ dun)
;
;                       Finished.
;
IF (n LE 0) THEN st = '' ELSE st = string(st)
RETURN, st
END
