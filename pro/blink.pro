PRO BLINK, wndw, t
;+
; NAME:
;	BLINK
; PURPOSE:
;	To allow the user to alternatively examine two or more windows within
;	a single window.
;
; CALLING SEQUENCE:
;	BLINK, Wndw [, T]
;
; INPUTS:
;	Wndw  A vector containing the indices of the windows to blink.
;	T     The time to wait, in seconds, between blinks.  This is optional
;	      and set to 1 if not present.  
;
; OUTPUTS:
;	None.
;
; PROCEDURE:
;	The images contained in the windows given are written to a pixmap.
;	The contents of the the windows are copied to a display window, in 
;	order, until a key is struck.
;
; EXAMPLE:
;	Blink windows 0 and 2 with a wait time of 3 seconds
;
;	IDL> blink, [0,2], 3 
;
; MODIFICATION HISTORY:
;	Written by Michael R. Greason, STX, 2 May 1990.
;	Allow different size windows   Wayne Landsman    August, 1991
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;			Check the parameters.
;
On_error,2                             ;Return to caller
n = n_params(0)
cflg = 0
IF (n LT 2) THEN BEGIN
	IF (n LT 1) THEN cflg = 1
	t = 1.0
ENDIF
IF (cflg NE 1) THEN BEGIN
	s = size(wndw)
	cflg = 2
	IF (s[0] GT 0) THEN BEGIN
		IF (s[1] GT 1) THEN cflg = 0
                n_wndw = s[1]
	ENDIF
ENDIF
;
;			Check to see if a window is open.  If so, save the 
;			index for later use. 
;
IF (cflg EQ 0) THEN BEGIN
	whld = !d.window
	IF (whld LT 0) THEN cflg = 3
ENDIF
;
;			If not enough or incorrect parameters were given, 
;			complain and return.
;
IF (cflg NE 0) THEN BEGIN
	IF (cflg EQ 1) THEN BEGIN
		print, " Insufficient parameters given to BLINK."
		print, " Syntax:  BLINK, WIN_INDICES [, TIME]"
	ENDIF
	IF (cflg EQ 2) THEN print, " The array of window indices is invalid."
	IF (cflg EQ 3) THEN print, " No windows are open."
ENDIF ELSE BEGIN
;
;
;			Get the size of each window in the array.
;
device, window = opnd
ncol = intarr(n_wndw)
nrow = ncol
for i=0,n_wndw-1 do begin
        if ~opnd[wndw[i]] then $
            message,'ERROR - Window '+ strtrim(wndw[i],2) + ' is not open'
	wset, wndw[i]
	ncol[i] = !d.x_vsize
	nrow[i] = !d.y_vsize
endfor
;
;			Write a message explaining how to terminate BLINK.
;
	print, "     "
	print, "To exit BLINK, strike any key."
	print, "     "
;
;			Create the display window and display the images.
;
	window, /free, retain=2, xsize = max(ncol), ysize=max(nrow), $
                   xpos=0, ypos=0, $ 
                   title="Blink window - Press any key to exit"
        whd = !d.window
	i = 0L
	WHILE (get_kbrd(0) EQ '') DO BEGIN
		device, copy=[0, 0, ncol[i], nrow[i], 0, 0, wndw[i]]
		i = (i + 1) mod n_wndw
		wait, t
	ENDWHILE
;
;			Clear up and terminate.  Close windows/pixmaps and
;			restore the originally active window.
;
	wdelete, whd
	wset, whld
ENDELSE
;
RETURN
END
