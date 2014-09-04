FUNCTION read_key, wait
;+
; NAME:
;	READ_KEY
; PURPOSE:
;	To read a keystroke and return its ASCII equivalent
; EXPLANATION:
;	If an ESCAPE sequence was produced and  the sequence is
;	recognized (e.g. up arrow), then a code is returned.
;
;       This functionality is mostly made obsolete by the addition of the
;       ESCAPE and KEY_NAME keywords to GET_KBRD in IDL V6.2
;
; CALLING SEQUENCE:
;	key = READ_KEY(Wait)
;
; INPUTS:
;	Wait  -  The wait flag.  If non-zero, execution is halted until a
;	         key is struck.  If zero, execution returns immediately and
;	         a zero is returned if there was no keystroke waiting in the
;	         keyboard buffer.  If not specified, zero is assumed.
;
; OUTPUT:
;	Returned - The key struck.  The ASCII code for non-escape sequences.
;	           Escape sequence equivalents:
;			Up Arrow     --  128
;			Down Arrow   --  130
;			Left Arrow   --  129
;			Right Arrow  --  131
;			Else         --    0
;
;	The return value is a byte value.
;
; MODIFICATION HISTORY:
;	Written by Michael R. Greason, STX, 22 June 1990.
;	Rewritten for a SUN workstation.  MRG, STX, 23 August 1990.
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;			Check the input parameter.
;
IF (n_params(0) LT 1) THEN wait = 0
;
;			Get the keystroke.
;
key = byte(get_kbrd(wait))
key = key[0]
;
;			If it is an ESCAPE, get the rest of it and 
;			then decode it.
;
IF (key EQ 27B) THEN BEGIN
	st = bytarr(10)
;
;				Get the rest of the escape sequence.
;
	i = 0
	REPEAT BEGIN
		key = byte(get_kbrd(0))
		st[i] = key[0]
		i = i + 1
	ENDREP UNTIL (st[i-1] EQ 0B)
;
;				Decode the escape sequence.
;
	CASE string(st) OF
		'[A' : key = 128B
		'[B' : key = 130B
		'[D' : key = 129B
		'[C' : key = 131B
		ELSE : BEGIN
				 IF (i GT 1) THEN key = 0B ELSE key = 27B
		       END
	ENDCASE
ENDIF
;
;			If it is a CSI, get the rest of it and 
;			then decode it.
;
IF (key EQ '9B'XB) THEN BEGIN
	st = bytarr(10)
;
;				Get the rest of the sequence.
;
	i = 0
	REPEAT BEGIN
		key = byte(get_kbrd(0))
		st[i] = key[0]
		i = i + 1
	ENDREP UNTIL (st[i-1] EQ 0B)
;
;				Decode the sequence.
;
	CASE string(st) OF
		'A' : key = 128B
		'B' : key = 130B
		'D' : key = 129B
		'C' : key = 131B
		ELSE : BEGIN
				 IF (i GT 1) THEN key = 0B ELSE key = '9B'XB
		       END
	ENDCASE
ENDIF
;
;			If it is a SS3, get the rest of it and 
;			then decode it.
;
IF (key EQ '8F'XB) THEN BEGIN
	st = bytarr(10)
;
;				Get the rest of the sequence.
;
	i = 0
	REPEAT BEGIN
		key = byte(get_kbrd(0))
		st[i] = key[0]
		i = i + 1
	ENDREP UNTIL (st[i-1] EQ 0B)
;
;				Decode the sequence.
;
	CASE string(st) OF
		ELSE : BEGIN
				 IF (i GT 1) THEN key = 0B ELSE key = '8F'XB
		       END
	ENDCASE
ENDIF
;
RETURN, key
END
