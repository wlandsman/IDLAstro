pro RESET_RDPLOT
;
;   If the user crashes out of the RDPLOT program, they can call this procedure
; to reset the graphics device functions to default values.
;
device, /CURSOR_CROSSHAIR, SET_GRAPHICS_FUNCTION=3, BYPASS_TRANSLATION=0
end



pro RDPLOT, x, y, WaitFlag, DATA=Data, DEVICE=Device, NORMAL=Normal, $
   NOWAIT=NoWait, WAIT=Wait, DOWN=Down, CHANGE=Change, Err=Err, $
   PRINT=Print, XTITLE=XTitle,YTITLE=YTitle, XVALUES=XValues,YVALUES=YValues, $
   FULLCURSOR=FullCursor, NOCLIP=NoClip, LINESTYLE=Linestyle, THICK=Thick, $
   COLOR=Color, BACKGROUND=BackGround, CROSS=Cross, ACCUMULATE=Accumulate, $
   CURSOR_STANDARD=cursor_standard
   
;*******************************************************************************
;+
; NAME:
;   RDPLOT
;
; PURPOSE:
;   Like CURSOR but with a full-screen cursor and continuous readout option
;
; EXPLANATION:
;   This program is designed to essentially mimic the IDL CURSOR command,
;   but with the additional options of continuously printing out the data
;   values of the cursor's position, and using a full-screen cursor rather 
;   than a small cross cursor.  The full screen cursor uses OPLOT and 
;   X-windows graphics masking to emulate the cursor.
;      One difference is that IF the PRINT keyword is set but the DOWN,
;   WAIT, CHANGE, or NOWAIT keywords are not set, then the leftmost mouse
;   button will print a "newline" line-feed, but not exit.
;
;   Mac users may need to set their X windows preferences to (1) Emulate 3
;    button mouse and (2) Click through inactive windows, to make cursor
;    work properly.
;
; CALLING SEQUENCE:
;   RDPLOT [, X, Y] [, WaitFlag] [, /DATA | /DEVICE | /NORMAL]
;      [, /NOWAIT | /WAIT | /DOWN | /CHANGE] 
;      [, /FULLCURSOR] [, /NOCLIP] [, /CROSS] [, /ACCUMULATE]
;      [, ERR=, PRINT=, XTITLE=, YTITLE=, XVALUES=, YVALUES=
;       , LINESTYLE=, THICK=, COLOR=, BACKGROUND=, CURSOR_STANDARD=]
;
; REQUIRED INPUTS:
;   None.
;
; OPTIONAL INPUTS: 
;   WAITFLAG = Uses the same table as the intrinsic CURSOR command, But note
;	that unlike the CURSOR command, there is no UP keyword.
;		WaitFlag=0 sets the NOWAIT keyword
;		WaitFlag=1 sets the WAIT keyword {default}
;		WaitFlag=2 sets the CHANGE keyword
;		WaitFlag=3 sets the DOWN keyword
;
; OPTIONAL OUTPUTS:
;    X - a named variable to receive the final cursor X position, scalar
;        or vector (if /ACCUMULATE is set)
;    Y - a named variable to receive the final cursor Y position, scalar
;        or vector (if /ACCUMULATE is set)
; OPTIONAL KEYWORD INPUT PARAMETERS:
;   /DATA - data coordinates are displayed and returned.
;   /DEVICE - device coordinates are displayed and returned.
;   /NORMAL - normal coordinates are displayed and returned.
;      Default is to use DATA coordinates if available (see notes).
;   /NOWAIT = if non-zero the routine will immediately return the cursor's
;      present position.
;   /WAIT - if non-zero will wait for a mouse key click before returning.  If
;      cursor key is already down, then procedure immediately exits.
;   /DOWN - equivalent to WAIT *except* that if the mouse key is already down
;      when the procedure is called, the procedure will wait until the mouse
;      key is clicked down again.
;   /CHANGE - returns when the mouse is moved OR a key is clicked up or down.
;   PRINT = if non-zero will continuously print out (at the terminal) the data 
;      values of the cursor's position.  If PRINT>1, program will printout a 
;      brief header describing the mouse button functions.  However, note that 
;      the button functions are overridden if any of the DOWN, WAIT, or
;      CHANGE values are non-zero.
;   XTITLE = label used to describe the values of the abscissa if PRINT>0.
;   YTITLE = label used to describe the values of the ordinate if PRINT>0.
;   XVALUES = a vector corresponding to the values to be printed when the
;	PRINT keyword is set.  This allows the user the option of printing
;	out other values rather than the default X coordinate position of
;	the cursor.  E.g., if XVALUES is a string vector of dates such as
;	['May 1', 'May 2', ...], then those dates will be printed rather than
;	the X value of the cursor's position: if X=1 then 'May 2' would be
;	printed, etc.  This requires that the values of the X coordinate read
;	by the cursor must be positive (can't access negative elements).
;       If XVALUES=-1, then NO values for X will be printed.
;   YVALUES = analogous to the XVALUES keyword.
;   /FULLCURSOR - if non-zero default cursor is blanked out and full-screen 
;      (or full plot window, depending on the value of NOCLIP) lines are
;      drawn; their intersecton is centered on the cursor position.
;   /NOCLIP - if non-zero will make a full-screen cursor, otherwise it will
;      default to the value in !P.NOCLIP.
;   LINESTYLE = style of line that makes the full-screen cursor.
;   THICK = thickness of the line that makes the full-screen cursor.
;   COLOR = color of the full-screen cursor.
;   BACKGROUND = color of the background of the plot device.  If this has
;      been set to !P.BackGround, then this keyword is unnecessary.
;   /CROSS = if non-zero will show the regular cross AND full screen cursors.
;   /ACCUMULATE - all of the positions for which the left button was
;      clicked are stored in the X and Y variables.  Has no effect if X and Y 
;      are not present.
;   CURSOR_STANDARD = this keyword can be used to select the cursor
;      appearance if /CROSS is set and will set the cursor to this value
;      when the full-screen cursor is turned off if /FULLCURSOR has been
;      set. See IDL help for the DEVICE keyword CURSOR_STANDARD to see
;      possible cursors for X Windows and MS Windows.  The default
;      behavior, if this keyword is not set, is to set the cursor to the
;      window system's default cursor, which might not be the user's
;      preferred cursor.
;
; OPTIONAL KEYWORD OUTPUT PARAMETER:
;   ERR = returns the most recent value of the !mouse.button value.
;
; NOTES:
;   Note that this procedure does not allow the "UP" keyword/flag...which 
;   doesn't seem to work too well in the origianl CURSOR version anyway.
;   Note: this might have been the case back in the day, but Robishaw
;   hasn't experienced any problems with CURSOR, /UP in the last 10
;   years.  Even so, it would be somewhat tricky to implement the /UP
;   behavior in this routine, which explains why it's still missing.
;
;   If a data coordinate system has not been established, then RDPLOT
;   will create one identical to the device coordinate system.  Note that
;   this kluge is required even if the user specified /NORMAL coordinates,
;   since RDPLOT makes use of the OPLOT procedure.  This new data
;   coordinate system is effectively "erased" (!X.CRange and !Y.CRange are
;   both set to zero) upon exit of the routine so as to not change the plot
;   status from the user's point of view.
;
;   Only tested on X-windows systems.  If this program is interrupted, the
;   graphics function might be left in a non-standard state; in that case,
;   run the program RESET_RDPLOT to return the standard graphics functions,
;   or type the command:   DEVICE, /CURSOR_CROSS, SET_GRAPHICS=3, BYPASS=0
;
;   Robishaw added /ACCUMULATE keyword to pass back all the positions at
;   which the mouse was left-clicked.  In addition, the value of the exit
;   click is returned unless the cursor did not change position between the
;   last left-click and the exit click.
;
;
;
; PROCEDURE:
;   Basically is a bells-n-whistles version of the CURSOR procedure.  All
;   the details are covered in the above discussion of the keywords.
;
; EXAMPLES:
;   A silly, but informative one:
;   Months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', $
;             'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
;   plot, indgen(12), xrange=[-5, 15]
;   rdplot, /FULL, /PRINT, $
;      XTITLE='Month: ', YTITLE='Y-value per month = ', $
;      xvalues=Months
;
;   If your plot has a non-black background color, be sure to set either
;   !p.background or the BACKGROUND keyword.  Here are examples of how to
;   use a blue full-screen cursor on a plot with a red background and
;   yellow axes and data. First, deal with color decomposition off:
;   device, decomposed=0
;   tvlct, [255,255,0], [0,255,0], [0,0,255], 1
;   plot, randomn(seed,1024), XSTYLE=19, PSYM=3, COLOR=2, BACK=1
;   rdplot, /PRINT, /FULL, THICK=5, /NOCLIP, BACK=1, COLOR=3
;
;   For decomposition on (TrueColor or DirectColor only):
;   device, decomposed=1
;   plot, randomn(seed,1024), XSTYLE=19, PSYM=3, COLOR=65535l, BACK=255l
;   rdplot, /PRINT, /FULL, THICK=5, /NOCLIP, BACK=255l, COLOR=16711680l
;
; MODIFICATION HISTORY:
;   Written (originally named CURFULL) by J.Wm.Parker  1993 Nov 22 
;   Created data coordinates if not already present, W. Landsman Nov. 93
;   Added continuous printout of data values, COLOR and FULLCURSOR keywords
;      (so that default is that it acts just like the cursor command).
;      Changed name from CURFULL to RDPLOT.   J.Wm.Parker  1994 Apr 20
;   Modified (with some translation table assistance from the IDL support 
;      group) to correctly plot the crosshair with the desired IDL 
;      color using the device's translation table to determine the XOR 
;      function and using the BYPASS function.  Added the RESET_RDPLOT
;      procedure to cleanup crashes that might occur while running
;      RDPLOT.  Other minor changes/bug fixes.  J.Wm.Parker  1994 May 21
;   Modified DOWN, WAIT, CHANGE functions to behave more similar to the
;      generic CURSOR procedure.   J.Wm.Parker  1995 April 24
;   Added XVALUES, YVALUES keywords and cleanup.   J.Wm.Parker  1995 April 24
;   Convert to IDL V5.0,  W. Landsman    July 1998
;   Change !D.NCOLORS to !D.TABLE_SIZE for 24 bit displays W. Landsman May 2000
;   Skip translation table for TrueColor visuals   W. Landsman  March 2001
;   Fixed /FULLCURSOR ghosts. Fixed to properly deal with background colors
;      in 24-bit visual classes (TrueColor and DirectColor).  Added
;      BACKGROUND keyword. Tim Robishaw 2005 Jan 27       
;   Added /ACCUMULATE keyword. T. Robishaw 2006 Nov 8
;   Corrected following problems. When /CHANGE and /PRINT were set,
;      returned X & Y were different than those printed.  When /PRINT and
;      /NOWAIT were set, or /PRINT and /WAIT were set and the routine was
;      entered with a mouse button clicked, nothing was printed. When
;      /PRINT and /DOWN were set, if routine was started with button down,
;      advertised behavior was that routine would exit on next down click;
;      in practice if cursor was not moved, successive down clicks had no
;      effect.  Now, if X is passed as an output variable, requires that Y
;      is also passed, like CURSOR.  Bottom line is that RDPLOT now really
;      does behave like CURSOR and when /PRINT is set, the values printed
;      correspond to those returned in X & Y.  T. Robishaw 2006 Nov 12
;   Fixed misbehavior when color decomposition was set to off for
;      TrueColor and DirectColor.  Now thoroughly tested on PseudoColor
;      displays as well as both decomposition states for TrueColor and
;      DirectColor.  Also made the default cursor color white when
;      decomposition is on (this has been its default value for
;      decomposition off). T. Robishaw 2006 Nov 16
;   Fixed misbehavior when /FULLCURSOR not set; was checking for
;      non-existent variable VisualName. T. Robishaw 2007 Jul 01
;   Added the CURSOR_STANDARD keyword because I hate how this routine
;      changes my default cursor. Also, it was crashing when /FULL not set:
;      small fix, now works. T. Robishaw  2007 Jul 03
;   Fixed bug where moving mouse with button pressed or releasing button
;      would return values even if DOWN was set. The checks for this were
;      only being done if PRINT was set also. T.V. Wenger 2013 May 14
;   Fix problem exiting when X,Y not supplied W. Landsman June 2013
;-
;*******************************************************************************
On_error,2

;;;
;   If the device does not support windows, then this program can not be used.
;
if ((!D.Flags and 256) ne 256) then message, $
  'ERROR - Current graphics device ' + !D.NAME + ' does not support windows'

;;;
;   Like cursor, require that if present, both X and Y be specified...
;
if (N_Params() eq 1) then message, $
   'Incorrect number of arguments. Both X & Y must be present.'

;;;
;   Keywords, keywords.
;
if (N_Params() eq 3) then begin
   case WaitFlag of
      0 : NoWait = 1
      1 : Wait = 1
      2 : Change = 1
      3 : Down = 1
      else : Wait = 1
   endcase
endif

NoWait = keyword_set(NoWait)
Wait = keyword_set(Wait)
Down = keyword_set(Down); or Wait
Change = keyword_set(Change)
FullCursor = keyword_set(FullCursor)

;;;
;   If plotting coordinates are not already established, and the NORMAL keyword
; is not set, then use device coordinates.
;   Note that even if this procedure was called with the DATA keyword set, that
; the DEVICE keyword will always take precedence over the DATA keyword in the
; cursor command.  However, if the NORMAL and DEVICE keywords are both set,
; then very strange values are returned.
;
UndefinedPlot = ((!X.CRange[0] eq 0) and (!X.CRange[1] eq 0))
if UndefinedPlot then plot, [0,!D.X_Size], [0,!D.Y_Size], /NODATA, $
   XSTYLE=5, YSTYLE=5, XMARGIN=[0,0], YMARGIN=[0,0], /NOERASE

;;;
;   Initialize the !mouse.button variable.  The value of !mouse.button 
; corresponds to the BYTE  value of the buttons on the mouse from left to right,
; lowest bit first.  So, the left button gives !mouse.button = 1, next button 
; gives !mouse.button = 2, then 4.
;  Read in the cursor with no wait.  If the user does not want to wait, or if 
; the DOWN or WAIT keywords are set AND the mouse key is depressed, then we're
; done (I hate GOTO's, but it is appropriate here).
; NOTE: Robishaw gets rid of GOTO statement... if user asks for value to be
;       printed, it should be printed!
;
!mouse.button = 0
cursor, X, Y, /NOWAIT, DATA=Data, DEVICE=Device, NORMAL=Normal
;if (keyword_set(NoWait) or (Wait and (!mouse.button gt 0))) then $
;            goto, LABEL_DONE
;;;
;   PRINTOUT SETUP SECTION ==================================================
;;;

;;;
;   Is the PRINT keyword set?  Then we have a lot of things to set up.  First,
; set up carriage return and line feed variables for the formatted printout,
; and define the titles for the printed values.
;
if keyword_set(Print) then begin 
   if not(keyword_set(XTitle)) then XTitle = "X = "
   if not(keyword_set(YTitle)) then YTitle = "Y = "
   Blanks  = "                    "

;;;
;   Now, if the XValues and/or YValues keywords are set, then deal with them.
; Also, we may want to suppress the printing of the X or Y values (e.g.,
; XValues=-1 or YValues=-1 sets the ShowX and ShowY variables).
;
   ShowX = 1
   UseXV = keyword_set(XValues)
   if UseXV then begin
      XVSt = string(XValues)
      XVtop = n_elements(XValues) - 1
      XVfmt = "(A" + strtrim(max(strlen(XVst))+3,2) + ")"
      if ((XVtop eq 0) and (strtrim(XVSt[0],2) eq '-1')) then ShowX = 0
   endif else XVfmt = "(A13)"
   if not(ShowX) then XTitle = ''

   ShowY = 1
   UseYV = keyword_set(YValues)
   if UseYV then begin
      YVSt = string(YValues)
      YVtop = n_elements(YValues) - 1
      YVfmt = "(A" + strtrim(max(strlen(YVst)),2) + ")"
      if ((YVtop eq 0) and (strtrim(YVSt[0],2) eq '-1')) then ShowY = 0
   endif else YVfmt = "(A13)"
   if not(ShowY) then YTitle = ''

;;;
;   If Print>1, then printout the informative header, which will vary depending
; on the values of the DOWN and CHANGE keywords.
;
   if (Print gt 1) and not(NoWait) then begin
      print
      if Change then begin
         print, " Hit any mouse button or move the mouse to exit."
      endif else begin
         if Down or Wait then begin
            print, " Hit any mouse button to exit."
         endif else begin
            print, '  Mouse Button:   LEFT         MIDDLE        RIGHT'
            print, ' Result Action:   New Line     Exit          Exit'
         endelse
      endelse
      print
   endif

endif else Print = 0


;;;
;   FULL-SCREEN CURSOR SETUP SECTION =======================================
;;;

;;;;
; If using the full-screen cursor:
;   Determine the data range for the full screen.
;   Blank out the regular cross cursor if the CROSS keyword is not set.
;   Set up the linestyle, thickness, clipping, and color parameters for the 
; oplot commands.
;   Set up the graphics to be XOR with the overplotted crosshair, and figure
; out the color to use for plotting the crosshair {details below}.
;
if FullCursor then begin
   Yfull = convert_coord([0.0,1.0], [0.0,1.0], /NORMAL, /TO_DATA)
   Xfull = Yfull[0,*]
   Yfull = Yfull[1,*]

   device, GET_GRAPHICS=OldGraphics, SET_GRAPHICS=6
   if not(keyword_set(Cross)) then device, CURSOR_IMAGE=intarr(16)

   if not(keyword_set(Linestyle)) then Linestyle = 0
   if not(keyword_set(Thick)) then Thick = 1
   NoClip = keyword_set(NoClip)

;;;
;   I think the best way to make the fullscreen cursor work is to use the XOR
; graphics function - overplotting a line will XOR with the data already on
; the screen, then overplotting the same line again will XOR again, effectively
; erasing the line and returning the device to its original state/appearance.
;    But first, let me present a quick primer on plotting colors in IDL and the 
; related color tables and translation table:
;   Normally, when a color N (a number between 0 and 255 which refers to a
; particular color in the currently loaded IDL color table) is used in one of
; the plotting or tv commands, the value that is actually sent to the display is
; the value in the N-th bin of the translation table.  E.g., if the background
; color is 0, then the actual (device) color value of the background is the
; value in the zeroth bin of the translation table.  Similarly, if the user
; wants to plot the color defined by number 147 in the IDL color table, the
; actual (device) color value of that color is the value in the 147th bin
; of the translation table.
;  So in the following example, let's pretend we have the following situation:
;   IDL> PRINT, !D.N_Colors
;            222
;   IDL> PRINT, !P.Background
;              0
;   IDL> DEVICE, TRANSLATION=TTab
;   IDL> PRINT, TTab[0]
;             34
;   IDL> PRINT, TTab[147]
;            181
;   When we set DEVICE,SET_GRAPHICS=6, and do an overplot, it performs an XOR
; function between the overplot's translated color value and the background's
; translated color value.
;   If we want the resulting color to be the IDL color 147, then we have to 
; overplot with the color whose translated color value XOR'ed with the 
; background's translated color value (34) will equal 181, which is the 
; translated color value of the desired IDL color 147.
;
; Symbolically:
; *  TTab[Desired Color] = TTab[OPLOT color] XOR TTab[Background]
; *  OPLOT Color = where( TTab eq (TTab[Desired Color] XOR TTab[Background]) )
;
; Numerically {using the above example}:
; *  OPLOT Color = where( TTab eq (TTab[147] XOR TTab[0]) )
; *  OPLOT Color = where( TTab eq (181 XOR 34) )
; *  OPLOT Color = where( TTab eq 151 )
;
;   Fine.
;   HOWEVER...since the translation table often does NOT contain the full range
; of possible numbers (e.g., 0 to 255), the result of the XOR function between 
; the background and the oplot color may be a value that does NOT appear in the 
; translation table.  This is particularly a problem for colors near the bottom
; of the translation table where the result of the XOR function may be less than
; the lowest value in TTab.
;   To fix this problem, I bypass the translation table, and directly send the
; device color (e.g., the value 151 in the above example) to the OPLOT command.
;   There is still some bug here - sometimes the color still isn't right.  I'll
; have to talk to the IDL support people about this {as soon as our support
; license is renewed!}
; NOTE: Took a while to figure out how to make the full cursor work with
;       both a specified cursor color and a non-black background.  We stick
;       with the XOR graphics function.  However, we need to deal with the
;       complex case of an indexed color model (Decompositon off) for the
;       TrueColor and DirectColor visual classes.  For TrueColor, we get
;       the RGB triplet stored in the color table at the indices specified
;       by Color and BackGround and convert them to 24-bit decomposed color
;       indices.  Then we turn on color decomposition.  Before we exit, we
;       turn it back off.  For DirectColor, we just need to XOR the 8-bit
;       color table indices. -Robishaw
;

   ; CHECK FOR THE VISUAL CLASS AND COLOR DECOMPOSITION STATE...
   device, Get_Visual_Name=VisualName, Get_Decomposed=Decomposed

   ; SET COLOR KEYWORDS IF NOT DEFINED...
   if ((size(Color))[1] eq 0) then $   ;  if undefined
      Color = Decomposed ? !D.N_Colors - 1 : !D.Table_Size - 1
   if (N_elements(BACKGROUND) eq 0) then BackGround = !P.BackGround
   
   ; Are we using a TrueColor or DirectColor visual class...
   if (VisualName eq 'TrueColor') OR (VisualName eq 'DirectColor') then begin
      if (VisualName eq 'TrueColor') AND not(Decomposed) then begin
         ; For TrueColor with color decomposition off, we need to...
         ; Turn on Color Decomposition...
         device, Decomposed=1
         ; Get the RGB triplets stored in our color table...
         tvlct, rct, gct, bct, /GET
         ; Find the corresponding 24-bit decomposed color indices...
         CTab = long(rct) + ishft(long(gct),8) + ishft(long(bct),16)
         DevColor = CTab[Color]
         DevBack = CTab[BackGround]
      endif else begin
         ; If TrueColor or Directcolor with Decomposition On, or
         ; DirectColor with Decomposition Off...
         DevColor = Color
         DevBack  = BackGround
      endelse
   endif else begin
      ; If we're not using TrueColor or DirectColor, then we'll
      ; access the translation table...
      device, TRANSLATION=TTab, BYPASS_TRANSLATION=1
      if (Color ge !D.Table_size) then $
         message, /INFO, $
                  'Trying to draw cursor with color table index GT Table Size'
      DevColor = TTab[Color < (!D.Table_size - 1)]
      if (BackGround ge !D.Table_size) then $
         message, /INFO, $
                  'Specified background has color table index GT Table Size'
      DevBack  = TTab[BackGround < (!D.Table_size - 1)]
   endelse
   OColor = DevColor xor DevBack
endif


;;;
;   FINALLY...THE PLOT READING SECTION  ====================================
;;;

;;;
;   If the cursor is beyond the boundaries of the window (device coordinates of
; X=-1 and Y=-1), then wait until the cursor is moved into the window.
;
cursor, X, Y, /NOWAIT, /DEVICE
if ((X lt 0) or (Y lt 0)) then cursor, X, Y, /CHANGE


;;;
;   Begin the loop that will repeat until a button is clicked (or a change if
; that is what the user wanted).   Err0 is used to keep track if the procedure
; was entered with a key already down, then it will be non-zero until that
; key has been released, at which point it will be permanantly set to zero.
; NOTE: Robishaw's edits make Err0 obsolete so these lines are commented.
;   Wait for a change (movement or key click).  Delete the old lines, and
; if we don't exit the loop, repeat and draw new lines.
;
cursor, X, Y, /NOWAIT, DATA=Data, DEVICE=Device, NORMAL=Normal
;Err0 = !mouse.button

NClicks = 0l
repeat begin    ; here we go!

;;;
;   This wait is a kludge to prevent ghosts from being left when /FULLCURSOR
;   is set.
;
    if FullCursor then wait, 0  ; black magic

;;;
;   If doing a full-screen cursor, overplot two full-screen lines intersecting 
; at that position.
;
   if FullCursor then begin
      XY = convert_coord(X,Y, DATA=Data,DEVICE=Device,NORMAL=Normal, /TO_DATA)
      Xdata = XY[0] * [1.0,1.0]
      Ydata = XY[1] * [1.0,1.0]
      oplot,Xdata,Yfull,LINE=Linestyle,THICK=Thick,NOCLIP=NoClip,COLOR=OColor
      oplot,Xfull,Ydata,LINE=Linestyle,THICK=Thick,NOCLIP=NoClip,COLOR=OColor
   endif

;;;
;   If printing out data values, do so.
;   !mouse.button=1 is the signal for a new line.
;
   if (Print gt 0) then begin

      if ShowX then begin
         if UseXV then Xst = XVSt[(X+0.5) > 0 < XVtop] else Xst = strtrim(X,2)
         XSt = XTitle + string(Xst + Blanks, FORMAT=XVfmt)
      endif else Xst = ''
      if ShowY then begin
         if UseYV then Yst = YVSt[(Y+0.5) > 0 < YVtop] else Yst = strtrim(Y,2)
         YSt = YTitle + string(Yst + Blanks, FORMAT=YVfmt)
      endif else Yst = ''

      print, Xst, Yst, format='($,2A,%"\R")'

      ; If left button pressed, then print out a new line; accumulate
      ; position if /ACCUMULATE set...
      if (!mouse.button eq 1) and $
         not(Down or Wait or Change or NoWait) then begin ;  new line?
         print, format='($,%"\n")'
         NClicks++
         if Arg_Present(y) then begin
            if keyword_set(ACCUMULATE) && (NClicks gt 1) then begin
               xout = [xout,x]
               yout = [yout,y]
            endif else begin
               xout = x
               yout = y
            endelse
         endif
      endif
   endif

   ; If button is held down, don't continue until button is released...
   if ( (!mouse.button eq 1) and not(Wait or Change or NoWait) ) $
      ; if entered with a button down, wait for next down click before
      ; returning...
      or ( (!mouse.button gt 1) and Down) then begin
      while (!mouse.button gt 0) do begin
         wait, 0.1
         cursor, XX, YY, /NOWAIT
      endwhile
   endif

   ;Err0 = Err0 < !mouse.button

;;;
;  Check to see that the cursor's current position is really the last measured 
; position (the mouse could have moved during a delay in the last section).  If
; so, then go on.  If not, then wait for some change in the mouse's status 
; before going on.
;  In either case, once we are going on, then if doing a full-screen cursor, 
; overplot the previous lines {the XOR graphics function will return the plot
; to its original appearance}.  Repeat until exit signal.
;

   ; There are a few cases where we just want to exit immediately...
   InstantOut = ( NoWait ) OR $  ; if /NoWait is set
                ; if /WAIT is set and *any* button is pressed, even if
                ; a button is being held down when the routine is called...
                ( Wait AND (!mouse.button gt 0) ) OR $
                ; if /CHANGE is set and *any* button is pressed...
                ( Change AND (NClicks gt 0) )

   if ~(InstantOut) then begin
      cursor, XX, YY, /NOWAIT, DATA=Data, DEVICE=Device, NORMAL=Normal
      if ((XX eq X) and (YY eq Y)) then $
         cursor, XX, YY, /CHANGE, DATA=Data, DEVICE=Device, NORMAL=Normal
      ; Load the new XX and YY values into the X and Y variables...
      X = XX
      Y = YY
   endif

   ; Erase the full cursor...
   if FullCursor then begin
      oplot,Xdata,Yfull,LINE=Linestyle,THICK=Thick,NOCLIP=NoClip,COLOR=OColor
      oplot,Xfull,Ydata,LINE=Linestyle,THICK=Thick,NOCLIP=NoClip,COLOR=OColor
   endif

   ; Handle case of /CHANGE but cursor was moved rather than a button
   ; clicked; we use kludge of incrementing NClicks counter...
   ; this will force the new position to be printed...
   if Change AND (NClicks eq 0) then begin
      XOut = X
      YOut = Y
      NClicks++
      ExitFlag = 0
      continue
   endif

   Err = !mouse.button

   ExitFlag = (Down AND (Err gt 0)) OR (Err gt 1) OR InstantOut
   print,down,instantout,err,exitflag
endrep until ExitFlag
;;;
; If exit click was at a position different from last left-click, then add
; this to the list of positions...
;
if (NClicks gt 0) then begin
   last_left_click = keyword_set(ACCUMULATE) ? NClicks-1 : 0
   if N_elements(Xout) Gt 0 THEN $
   if ~((X eq XOut[last_left_click]) and $
          (Y eq YOut[last_left_click])) then begin
      XOut = [XOut,X]
      YOut = [YOut,Y]
   endif ELSE BEGIN
      Xout = x
      YOut = y
   endELSE   
endif else begin
   XOut = X
   YOut = Y
endelse

if (Print gt 0) then print ; clear the last printed line

;LABEL_DONE:

;;;
;  Done!  Go back to the default Graphics and cursor in case they were changed.
;  Also erase the plot ranges if they originally were not defined.
;
if FullCursor then begin
   if (N_elements(CURSOR_STANDARD) eq 0) $
      then device,/CURSOR_CROSSHAIR,SET_GRAPHICS=OldGraphics,Bypass=0 $
      else device,CURSOR_STANDARD=cursor_standard,SET_GRAPHICS=OldGraphics,$
                  Bypass=0

   ; If the color decomposition was off when we started, shut it off again...
   if (VisualName eq 'TrueColor') && ~Decomposed then device, Decomposed=0
endif

if UndefinedPlot then begin
   !X.CRange = 0
   !Y.CRange = 0
endif

;;;
;  Assign X & Y to the accumulated values if /ACCUMULATE is set...
if keyword_set(ACCUMULATE) and Arg_Present(Y) then begin
   X = temporary(XOut)
   Y = temporary(YOut)
endif
end   ;   RDPLOT
