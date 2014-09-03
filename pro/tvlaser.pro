PRO TVLASER, hdr, Image, BARPOS=BarPos, CARROWS=CArrows, CLABELS=CLabels, $
	COLORPS=ColorPS, COMMENTS=Comments, CSIZE=CSize, CTITLE=CTitle, $
 	DX=dX, DY=dY, ENCAP=encap, FILENAME=filename, HEADER=Header, HELP=Help,$
	IMAGEOut=ImageOut, INTERP=Interp, MAGNIFY=Magnify, NoClose=noclose, $
        NODELETE=NoDelete, NO_PERS_INFO=No_Pers_Info, NOEIGHT=NoEight, $ 
        NOPRINT=NoPrint, NORETAIN = NoRetain, PORTRAIT=Portrait, $
        PRINTER = Printer, REVERSE=Reverse, SCALE=Scale, TITLE=Title, $
        XSTART=XStart, YSTART=YStart, XDIM=XDim, YDIM=YDim, $
        TrueColor=TrueColor, BOTTOMDW=bottomdw, NCOLORSDW=ncolorsdw
;+
; NAME:
;      TVLASER
; PURPOSE:
;      Prints screen or image array onto a Postscript file or printer.
;      Information from FITS header is optionally used for labeling.  
;
; CALLING SEQUENCE:     
;      TVLASER, [header, Image, BARPOS = ,CARROWS =, CLABELS = ,/COLORPS, 
;             COMMENTS = ,CSIZE = ,CTITLE = , DX = , DY =, /ENCAP, FILENAME =
;             HEADER = ,/HELP, IMAGEOUT = ,/INTERP, /MAGNIFY, /NoCLOSE, 
;             /NoDELETE, /NO_PERS_INFO, /NoEIGHT, /NoPRINT, /NoRETAIN, 
;             /PORTRAIT, PRINTER = , /REVERSE, /SCALE, TITLE = , /TrueColor, 
;             XDIM=, XSTART=, YDIM=, YSTART=, BOTTOMDW=, NCOLORSDW= ]	
;
;       Note that the calling sequence was changed in May 1997
; OPTIONAL INPUTS: 
;       HEADER - FITS header string array.   Object and astrometric info from
;               the FITS header will be used for labeling, if available
;       IMAGE - if an array is passed through this parameter, then this image
;               will be used rather than reading off the current window.  This
;		allows easy use of large images.     It is usually preferable
;               to optimally byte scale IMAGE before supplying it to TVLASER   
;
; OPTIONAL KEYWORD INPUT PARAMETERS: 
;       BARPOS - A four- or five-element vector giving the position and
;            orientation of the color bar.  The first four elements
;            [X0,Y0,XSize,YSize] indicate the position and size of the color
;            bar in INCHES, relative to origin of the displayed image.
;            (X0,Y0) are the position of the lower left corner and 
;            (XSize,YSize) are the width and height.  The fifth element is
;            optional, and if present, the color bar will be printed
;            horizontally rather than vertically.  If BARPOS is set to
;            anything but a four- or five-element vector, the bar is NOT
;            printed.  The default value is BARPOS = [-0.25, 0.0, 0.2, 2.0] 
;       BOTTOMDW - The lowest value to use in building the density
;            wedge.  Used with NCOLORSDW.  Compatible with BOTTOM and
;            NCOLORS keywords of XLOADCT.
;       CARROWS - The color to print the North-East arrows.  Default is dark.
;            Three types of values can be passed:
;                 SCALAR: that value's color in the current color table
;                 3-ELEMENT VECTOR: the color will be [R,G,B]
;                 STRING: A letter indicating the color.  Valid names are:  
;                 'W' (white), 'D' (dark/black), 'R' (red),    'G' (green), 
;                 'B' (blue),  'T' (turquoise),  'V' (violet), 'Y' (yellow), 
;             If the keyword is set to a value of -1, the arrows are
;             NOT printed.
;       COLORPS - If present and non-zero, the idl.ps file is written using
;             color postscript.
;       COMMENTS - A string that will be included in the comment line below the
;                image.  For multi-line comments you can either use "!C" in the
;                string as a carriage return {although the vertical spacing
;                might be a little off} or, preferably, make the COMMENTS a
;                string array with each line as a separate element. 
;       CLABELS - Color to print the labels, same format as for CARROWS.
;       CSIZE - Color to print the size-scale bar and label, same format as for
;                CARROWS.
;       CTITLE - Color to print the title, same format as for CARROWS.
;       DX,DY - offsets in INCHES added to the position of the figure on the
;               paper.  As is the case for the device keywords XOFFSET and
;               YOFFSET, when in landscape mode DX and DY are the same
;               *relative to the paper*, not relative to the plot (e.g., DX is
;               the horizontal offset in portrait mode, but the *vertical*
;               offset in landscape mode).
;       ENCAP - If present and non-zero, the IDL.PS file is written in
;               encapsulated postscript for import into LaTeX documents
;       FILENAME - scalar string giving name of output postscript file.
;               Default is idl.ps.   Automatically sets /NODELETE
;       HEADER = FITS header.   This is an alternative to supplying the FITS
;                header in the first parameter.
;       HELP - print out the sytax for this procedure.
;       INTERP - If present and non-zero, current color table will be
;                interpolated to fill the full range of the PostScript color
;                table (256 colors).  Otherwise, the current color table will be
;                directly copied.   You probably will want to use this if you
;                are using IMAGE keyword and a shared color table.
;       MAGNIFY - The net magnification of the entire figure.  At this point,
;                the figure is not automatically centered on the paper if the
;                value of MAGNIFY is not equal to 1, but the DX and DY keywords
;                can be used to shift location.  For example, to fit a full plot
;                on the printable area (8.5x8.5 inches) of the Tek PhaserIISD
;                color printer use:  MAGNIFY=0.8, DX=0.5, DY=0.5.;       
;       NCOLORSDW - The number of values to include in the density
;                wedge.  Used with BOTTOMDW.  Compatible with
;                BOTTOM/NCOLORS keywords of XLOADCT.
;       NoCLOSE - If present and non-zero, then the postscript file is not
;             closed (or printed), the device is set to 'PS', and the data 
;             coordinate system is set to match the image size.  This allows the
;             user to add additional plotting commands before printing.  For 
;             example, to include a 15 pixel circle around a source at 
;             coordinates (150,160), around an image, im, with FITS header 
;             array, h
;
;                IDL> tvlaser,h,im,/NoClose      ;Write image & annotation
;                IDL> tvcircle,15,150,160,/data  ;Draw circle
;                IDL> device,/close              ;Close postscript file & print
;
;       NoDELETE - If present and non-zero, the postscript file is kept AND is 
;                 also sent to the printer
;       NoEIGHT - if set then only four bits sent to printer (saves space)
;       NO_PERS_INFO - if present and non-zero, output notation will NOT
;                 include date/user block of information.
;       NoPRINT - If present and non-zero, the output is sent to a file (default
;                name 'idl.ps'), which is NOT deleted and is NOT sent to the 
;                printer.
;       NoRETAIN - In order to avoid possible problems when using TVRD with
;                 an obscured window, TVLASER will first copy the current window
;                 to a temporary RETAIN=2 window.    Set /NORETAIN to skip this
;                 step and improve performance
;       PORTRAIT - if present and non-zero, the printer results will be in
;                 portrait format; otherwise, they will be in landscape format.
;                 If labels are requested, image will be in portrait mode,
;                 regardless
;       PRINTER - scalar string giving the OS command to send a the postscript
;               file to the printer.   Under Unix, the default value of PRINTER
;               is 'lpr ' while for other OS it is 'print ' 
;       REVERSE - if present and non-zero, color table will be fliped, so black
;               and white are reversed.
;       SCALE - if present and non-zero, image will be bytscaled before being
;               sent to postscript file.      
;       TITLE - if present and non-zero, the string entered here will be the
;               title of the picture.  Default is the OBJECT field in the
;               header (if present).
;       TRUECOLOR - if present and non-zero, the postscript file is created
;               using the truecolor switch (i.e. true=3). The colorbar is
;               not displayed in this mode.  
;       XDIM,YDIM - Number of pixels.  Default is from !d.x_size and !d.y_size,
;               or size of image if passed with IMAGE keyword.
;       XSTART,YSTART - lower left corner (default of (0,0))
;
; OPTIONAL KEYWORD OUTPUT PARAMETER
;        IMAGEOUT = the image byte array actually sent to the postscript file.
;
; SIDE EFFECTS: 
;        A postscript file is created in the current directory.  User must have 
;        write privileges in the current directory.  The file is named idl.ps
;        unless the FILENAME keyword is given.   The file is directed to the
;        printer unless the /ENCAP, /NoCLOSE, or /NOPRINT keywords are given.
;        After printing, the file is deleted unless the /NODELETE or FILENAME 
;        keywords are given. 
; PROCEDURE:  
;       Read display or take IMAGE and then redisplay into a postscript file.
;       If a header exists, printout header information.  If header has
;       astrometry, then print out orientation and scale information.
; PROCEDURES USED:
;        ARROWS, EXTAST, FDECOMP, GETROT, PIXCOLOR, SXPAR(), XYAD, ZPARCHECK
;
;*EXAMPLE:
;       1) Send a true color image (xsize,ysize,3) to a printer (i.e. print23l),
;                tvlaser,huv,cpic,/colorps,/truecolor,printer="print23l"
;                % TVLASER: Now printing image: $print23l idl.ps
;
; MODIFICATION HISTORY:     
;       Major rewrite from UIT version   W. Landsman   Dec 94
;       Massive rewrite.  Added North-East arrows, pixel scale bar, color bar,
;       and keywords DX, DY, MAGNIFY, INTERP, HELP, and COMMENTS.
;       Created ablility to define colors for annotation and
;       text.  Repositioned text labels.     J.Wm.Parker, HITC, 5/95
;       Make Header and Image parameters instead of keywords.   Add PRINTER
;       keyword.   Include alternate FITS keywords.   W. Landsman May 97      
;       Copy to a RETAIN=2 window, work without FITS header W. Landsman June 97
;       Cleaner output when no astrometry in header  W. Landsman  June 97
;       Added /INFO to final MESSAGE  W. Landsman   July 1997
;       12/4/97	jkf/acc	- added TrueColor optional keyword.
;       Added /NoClose keyword, trim Equinox format  W. Landsman 9-Jul-1998
;       Don't display coordinate labels if no astrometry, more flexible
;       formatting of exposure time W. Landsman 30-Aug-1998
;       BottomDW and NColorsDW added.  R. S. Hill, 1-Mar-1999
;       Apply func tab to color bar if not colorps.  RSH, 21 Mar 2000
;       Fix problem with /NOCLOSE and unequal X,Y sizes  W. Landsman Feb 2001
;       Use TVRD(True=3) if /TRUECOLOR set    W. Landsman   November 2001
;       More synonyms, check for header supplied W. Landsman November 2007
;-
 compile_opt idl2
 on_error,2

 if keyword_set(Help) then begin
   print, 'Syntax:  TVLASER, [ Header, Image ]'
   print, 'Keywords:  BARPOS= ,CARROWS= , CLABELS= ,/COLOPS, COMMENTS= ,'
   print, '           CSIZE= , CTITLE= , DX= , DY= , /ENCAP, FILENAME= ,'
   print, '           HEADER= ,/HELP, IMAGEOUT= , /INTERP, /MAGNIFY,/NoCLOSE ,'
   print, '           /NoDELETE, NO_PERS_INFO, /NoEIGHT, /NoPRINT, /NORETAIN,'
   print, '           /PORTRAIT,PRINTER=,/REVERSE, /SCALE, TITLE= , /TRUECOLOR,' 
   print, '           XDIM= ,XSTART=, YDIM= , YSTART= ] '
   print, '   '        
   return
 endif

;----------------------------;
;  SECTION:  INITIALIZATION  ;
;----------------------------;

;;;
;   Save some info and set some variables.  LogoDir may need to be changed
; depending on where the GIF logos are.
;
 sv_device = !D.NAME
 sv_color = !P.Color
 if !D.NAME EQ 'PS' then set_plot,'X'     ;Return to X terminal
 tvlct,sv_rr,sv_gg,sv_bb,/get

 if keyword_set(NoEight)  THEN NBits = 4 ELSE NBits = 8
 if keyword_set(Portrait) THEN Lands = 0 ELSE Lands = 1
 ColorPS  = keyword_set(ColorPS)
 Encap    = keyword_set(Encap)
 NoPrint  = keyword_set(NoPrint)
 NoDelete = keyword_set(NoDelete)
 TrueColor= keyword_set(TrueColor)
 if TrueColor then TrueValue =3 else TrueValue =0
 
 if N_elements(hdr) EQ 0 then $
	if N_elements(header) NE 0 then hdr = header
 if (N_params() GE 1) and (N_elements(hdr) EQ 0) then message,/INF, $
        'Warning - No valid FITS header supplied'	
 if N_elements(hdr) NE 0 then zparcheck,'TVLASER',hdr,1,7,1,'FITS image header'
;;;
;   If no image was passed in the IMAGE keyword, then we will be reading the
; image from the screen.  Default values are to start at 0,0 and read the
; entire window.
;
 FromTV = N_elements(Image) eq 0
 if FromTV then begin
   if !D.WINDOW EQ -1 then begin
	tvlaser,/help
	return
   endif
   message,'Reading image from window ' + strtrim(!D.WINDOW,2) + $
        ' ... Please be patient', /INF
   if not keyword_set(XStart) then XStart = 0
   if not keyword_set(YStart) then YStart = 0
   if not keyword_set(XDim) then XDim = !d.x_size
   if not keyword_set(YDim) then YDim = !d.y_size
   if not keyword_set(noretain) then begin
	chan = !D.WINDOW
	xsize = !D.X_SIZE & ysize = !D.Y_SIZE
	window,/free,xsize=xsize,ysize=ysize
	wset,!D.WINDOW
	device,copy=[0,0,xsize,ysize,0,0,chan]
   endif
   ImageOut = tvrd(XStart,YStart,XDim,YDim,true = truevalue)
   if not keyword_set(noretain) then begin
	wdelete,!D.WINDOW
	wset,chan
   endif
 endif else begin
   XStart = 0
   YStart = 0
   XDim   = (size(Image))[1]
   YDim   = (size(Image))[2]
   ImageOut = Image
 endelse
;;;
;   YSpace is used to scale the vertical spacing of text and the title.
;
 YSpace  = (float(Xdim) / Ydim) > 1.              ;Modified December 1994 WBL
 XSpace  = (float(Ydim) / Xdim) > 1.

;;;
;   If using B/W PostScript, use NTSC color -> B/W formula, J Brinkmann
;   Scale and/or reverse if desired.
;
 if not(ColorPS) then ImageOut = $
   0.299 * sv_rr[ImageOut] + 0.587 * sv_gg[ImageOut] + 0.114 * sv_bb[ImageOut]
 if keyword_set(Scale)   then ImageOut = bytscl(ImageOut)
 if keyword_set(Reverse) then ImageOut = 255b - temporary(ImageOut)

;;;
;   If a header is given, put in portrait mode regardless. 
;
 if N_elements(hdr) NE 0 then Lands = 0

;;;
;   Set up colors for density wedge.
;
 if N_elements(BottomDW) LE 0 then BottomDW = 0
 nc = !D.table_size - BottomDW
 if n_elements(NColors) GT 0 then nc = nc < ncolors
 if nc LE 0 then begin
   message, /INFO, 'Bad color spec; using default'
   BottomDW = 0
   nc = !D.table_size
 endif


;------------------------------;
;  SECTION:  POSTSCRIPT SETUP  ;
;------------------------------;

;;;
;   Redirect output to Postscript printer file, which may be printed.
;   Size of image is restricted to 7.5 inches in the paper's narrow direction
; for MAGNIFY=1.  If we will be printing out header info, then restrict the
; Y size to be no more than 7.5 also.
;
if (Lands eq 1) then begin
   inx = 10.0
   iny = float(YDim)/float(XDim)*float(inx)
   if (iny gt 7.5) then begin
     iny = 7.5
     inx = (float(XDim)/float(YDim))*float(iny)
   endif
 endif

 if (Lands eq 0) then begin
   if N_elements(hdr) NE 0 then iny = 7.5 else iny = 10.0
   inx = float(XDim)/float(YDim)*float(iny)
   if (inx gt 7.5) then begin
     inx = 7.5
     iny = (float(YDim)/float(XDim))*float(inx)
   endif
 endif

;;;
;   Some info for the user, and setting the filename.
;
 pstype = ' '
 if Encap then pstype = pstype + 'encapsulated '
 if ColorPS then pstype = pstype + 'color '
 if not keyword_set(filename) then fname = 'idl.ps' else begin
   fdecomp,filename,disk,dir,name,ext
   if ext EQ '' then ext = 'ps'
   fname = disk + dir + name + '.' + ext
   NoDelete = 1
 endelse 
 if keyword_set(NoDelete) or keyword_set(EnCap) or keyword_set(NoPrint) then $ 
 message,'Writing image to' + pstype + 'postscript file ' + fname, /INF

;;;
;   Set plot to the PostScript printer.  Set all the device keywords.
;
set_plot, 'ps', INTERPOLATE=keyword_set(Interp)
sv_font = !P.FONT
!p.font = 0

 if not keyword_set(dX) then dX = 0
 if not keyword_set(dY) then dY = 0

 XOff =  0.75 + dX
 YOff = 10.25 + dY
 if Lands then begin
   device, /landscape
   YOff = inx + ((11 - inx) / 2.0) + dY   ; centered
 endif else begin
   device, /portrait
   YOff = Yoff - iny
 endelse

 device, xsize=inx, ysize=iny, xoffset=XOff, yoffset=YOff, /inches, $
   bits=NBits, filename=fname, /helvetica, encapsulated=Encap, color=ColorPS

 if keyword_set(Magnify) then device, scale=Magnify else device, scale=1


;-----------------------;
;  SECTION:  TV OUTPUT  ;
;-----------------------;

 tv, ImageOut,true=TrueValue

;   If the BarPos keyword has four or five elements, then show the color bar.

 if (not(TrueValue)) then begin 
   if (N_elements(BarPos) eq 0) then BarPos = [-0.25, 0.0, 0.2, 2.0]
   NumEls = N_elements(BarPos)
   if ( (NumEls eq 4) or (NumEls eq 5) ) then begin
    ColorBar = byte(round(congrid(findgen(nc)+BottomDW, 256))) $
       # make_array(20,val=1b)
    if not(ColorPS) then $
       ColorBar = 0.299 * sv_rr[ColorBar] + 0.587 * sv_gg[ColorBar] $
                  + 0.114 * sv_bb[ColorBar]
    ColorBar[0:*,[0,19]]  = 0
    ColorBar[[0,255],0:*] = 0
    if (NumEls eq 4) then ColorBar = transpose(ColorBar)
    tv, ColorBar, BarPos[0],BarPos[1], xsize=BarPos[2],ysize=BarPos[3], /INCHES
   endif
 endif

;;;
;   Now that the image has been displayed with the desired color table, we will 
; play with the color table a bit to get the appropriate colors for the text,
; arrows, and scale bar.  The three RGB values for each one will be loaded into
; vectors called things like 'CArrowsRGBN', 'CSizeRGBN', etc.  The last value
; in this vector will be the location of that color in the color table.
;   "Colors" is a string array of the keyword names, then via the EXECUTE
; function, we determine what the content of each variable is: a string to be
; used inthe pixcolor procedure, a single number indicating the location in the
; current color table, or a 3-element vector with RGB values.  One reason for
; doing it this way, is that if more objects to be colored are added to the
; keywords, only the variable COLORS need be changed here by adding those
; keyword names.
;   "Val" is where we will be temporarily putting the new colors (usually in
; the bottom bin).
;
 Colors = ['CArrows','CSize','CTitle','CLabels']
 r_new = bytarr(n_elements(Colors))
 g_new = r_new
 b_new = r_new

 for N=0,(n_elements(Colors) -1) do begin
  tvlct, sv_rr, sv_gg, sv_bb
  Val = 0

  dummy = execute( 'NumEls = n_elements(' + Colors[N] + ')' )
  if (NumEls eq 0) then begin
    dummy = execute( Colors[N] + ' = "D"' )
    NumEls = 1
  endif
  dummy = execute( 'C = ' + Colors[N] )
  if (NumEls eq 1) then begin  ; string or color value
    if ((size(C))[1] eq 7) then pixcolor, Val, C else Val = C
  endif else begin
    if (NumEls eq 3) then tvlct,transpose(C) else pixcolor, Val, 'D'
  endelse

  tvlct, r, g, b, /get
  if (Val[0] ne -1) then begin
     r_new[N] = r[Val]
     g_new[N] = g[Val]
     b_new[N] = b[Val]
     dummy = execute(Colors[N]+'RGBN = [r[Val],g[Val],b[Val],N]')
  endif
endfor

 tvlct, r_new, g_new, b_new


;-------------------------------;
;  SECTION:  HEADER and LABELS  ;
;-------------------------------;

;;;
;   If a FITS header was given then include whatever of the following FITS
; keywords that are present as annotation:  OBJECT (becomes the title if none
; given), TELESCOP, IMAGE, EXPTIME, EQUINOX, CRVAL1 (Right Ascension), CRVAL2
; (Declination), NAXIS1, NAXIS2, CD (Rotation angle and pixel size), PDSDATIM
; (Date of Microdensitometry).  Also will include the name of the user and the
; current date.  Some blocks can be suppressed...see description of keywords
; above.  Also prints directional arrows and scale.
;  
if (N_elements(Hdr) NE 0) then begin
 

;;;
;   Does the header have astrometry?
;
  extast, hdr, astr, NoAstrom
  if NoAstrom GT 0 then begin
    ast_type = strmid( strupcase( strtrim(astr.ctype[0],2) ), 0 ,4)
    if  ((ast_type NE 'RA--') and (ast_type NE 'GLON') and $ ;Valid projection?
         (ast_type NE 'ELAT') ) then NoAstrom = -1
  endif
	
  if (NoAstrom LT 0) then begin
    rga      = 'N/A'
    decl     = 'N/A'
    equi     = ''
    ROTATE   = 'N/A'
    CDELT    = [0.0,0.0]
    CDELTAS  = 'N/A'
  endif else begin
    xcen = (XDim-XStart-1)/2.
    ycen = (YDim-YStart-1)/2.
    if FromTV then zoom_xy,xcen,ycen ;In case TV image has non-zero zoom or roam
    xyad,hdr, xcen, ycen, ra_cen, dec_cen
    str = adstring(ra_cen,dec_cen,1)
    rga = strmid( str, 1, 11)
    decl = strmid( str, 14, 11)
    equi = sxpar( hdr, 'EQUINOX', Count = N_equi)
    if N_equi EQ 0 then equi = '' else $ 
              equi = '(' + strmid(strtrim(equi,2),0,7) + ')'
    getrot, hdr ,ROTATE, CDELT
    ROTATE  = strtrim(string(ROTATE,  format='(f7.2)'),2) + ' degrees'
    CDELT   = abs(CDELT*60.*60.)
    if CDELT[0] LT 0.1 then fmt = '(f7.3)' else fmt = '(f7.2)'
    CDELTAS = strtrim(string(CDELT[0],format=fmt ),2)
    if (abs(CDELT[0] - CDELT[1]) GT 0.05*CDELT[0]) THEN $
       CDELTAS = CDELTAS + ' by ' + strtrim(string(CDELT[1],format=fmt),2)
    CDELTAS = CDELTAS + ' arcsec/pixel'
  endelse

;;;
;   Printout the image information?  YSpace is used to scale the spacing of the
; linformation lines in NORMAL units.  dY is one line height.  LabXs and LabYs
; are arrays that define the placement of Label/Value pairs in the NORMAL
; coordinates.  So to increment to the next line, simply use:
;   LabYs = LabYs + dY
;
if (strtrim(CLabels[0],2) ne '-1') then begin
    dY     = -0.025 * YSpace
    LabYs  = [-0.05, -0.05] * YSpace
    LabX1s = [ 0.01,  0.21] * XSpace
    LabX2s = [ 0.64,  0.74] * XSpace

;;;
;  Set the label color and print out each label/value.
;
  !P.Color = CLabelsRGBN[3]

;OBJECT
    OBJ = strtrim( sxpar(hdr,'OBJECT', Count = N_Obj),2 )
    if N_Obj EQ 0 then begin 
    OBJ = strtrim( sxpar( hdr,'TARGNAME', Count = N_Obj),2)
    if N_Obj EQ 0 then OBJ = 'N/A'
    endif
    XYOUTS, LabX1s, LabYs, ['OBJECT:',OBJ],/ NORMAL
    LabYs = LabYs + dY

;TITLE (set here, but print out later in case no header was given)
    if NOT keyword_set(TITLE) then begin
      if (N_Obj NE 0) then TITLE=OBJ else TITLE = ''
    endif 

;IMAGE ID
    imname = 'N/A'
    imname = sxpar(hdr,'IMAGE', Count = N_image)
    if N_image EQ 0 then imname = sxpar(hdr,'EXPNAME', Count = N_image)
    if N_image EQ 0 then imname = sxpar(hdr,'OBS_ID', Count = N_image)
    if N_image EQ 0 then imname = sxpar(hdr,'ROOTNAME', Count = N_image)
    imname = strtrim(imname,2)
 
  
    XYOUTS,LabX1s,LabYs,['IMAGE:',IMNAME],/NORMAL
    LabYs = LabYs + dY

  LabYs = LabYs + dY

;TELESCOPE
    scop = sxpar( hdr,'INSTRUME', Count = N_Scop)
    if N_Scop EQ 0 then scop = sxpar( hdr,'TELESCOP', Count = N_Scop)
    if N_Scop EQ 0 then scop = sxpar( hdr,'OBSERVAT', Count = N_Scop)
    if N_Scop EQ 0 then scop = '' else scop = strtrim(scop,2)
    detector = sxpar( hdr,'DETECTOR', Count = N_det)
    if N_det EQ 0 then detector = '' else detector = strtrim(detector,2)
    if scop EQ '' then scop = detector else $
    if detector NE '' then scop = scop + '/' + detector
    XYOUTS,LabX1s,LabYs,['INSTRUMENT:',scop],/NORMAL

;SIZE
    SIZ = strtrim(XDim,2) +' by ' + strtrim(YDim,2) + ' pixels'
    XYOUTS,LabX2s,LabYs,['SIZE:',SIZ],/NORMAL
    LabYs = LabYs + dY

;FILTER
    filter = sxpar(hdr, 'FILTER', Count= N_filter)
    if N_filter EQ 0 then filter = sxpar(hdr, 'FILTNAM1', Count= N_filter)
    if N_filter EQ 0 then filter = sxpar(hdr, 'FILTER1', Count= N_filter)
    if N_filter EQ 0 then FILTER = 'N/A' else filter = strtrim(filter,2)
    XYOUTS,LabX1s,LabYs,['CAMERA/FILTER:',FILTER],/NORMAL

;SCALE
    if NoAstrom GE 0 then XYOUTS,LabX2s,LabYs,['SCALE:',CDELTAS],/NORMAL
    LabYs = LabYs + dY

;EXPOSURE TIME   First try 'EXPTIME' then 'EXPOSURE' then 'INTEG'
    exptime = sxpar(hdr, 'EXPTIME', Count = N_time)
    if N_time EQ 0 then exptime = sxpar(hdr, 'EXPOSURE', Count = N_time)
    if N_time EQ 0 then exptime = sxpar(hdr, 'INTEG', Count = N_time)
    if N_time EQ 0 then exptime = 'N/A' else $
	exptime = strmid( strtrim(exptime,2),0,6) + ' seconds'
    XYOUTS,LabX1s,LabYs,['EXPOSURE TIME:',EXPTIME],/NORMAL	
    LabYs = LabYs + dY

    LabYs = LabYs + dY

    if noastrom GE 0 then begin
;CENTER COORDINATES
    XYOUTS, LabX1s, LabYs,['CENTER '+ equi + ':', $
     'RA = ' + RGA + '    DEC = ' + DECL], /NORMAL
    LabYs = LabYs + dY

;ROTATION
    XYOUTS,LabX1s,LabYs,['ROTATION:',strtrim(ROTATE,2)],/NORMAL
    LabYs = LabYs + dY
    endif



;COMMENTS
    if keyword_set(Comments) then begin
      XYOUTS,LabX1s[0],LabYs[0],'COMMENTS:',/NORMAL
      for N=0,(n_elements(Comments)-1) do $
        XYOUTS,LabX1s[1],(LabYs[1] + (dY * N)),Comments[N],/NORMAL
    endif
    LabYs = LabYs + dY

;USER and DATE/TIME
    if not keyword_set(No_pers_info) then begin
      XYOUTS, LabX2s[0],LabYs[0], GetEnv('USER') + '  (' + $
      STRMID(systime(),4,20) + ')' ,SIZE=0.9, /NORMAL
    endif

  endif


;ARROWS
;   The calculations AX and XY allow the smallest use of space for the arrows
; for all possible rotation angles.  To test the extent of the circle, add
; code like the following in before the "R = float(..." line:
;   hextract,ImageOut,h,i1,h1,0,5,0,5 & for N=0,18 do begin
;   hrot,i1,h1,i2,h2,N*20,-1,-1,0  & getrot, h2 ,Rotate
;
  if ((strtrim(CArrows[0],2) ne '-1') and (NoAstrom ne -1)) then begin
    R = float(rotate) * !pi / 180
    AX = ( 0.50 + (0.05 * (cos(R) + sin(R)))) * XSpace
    AY = (-0.10 - (0.05 * (cos(R) - sin(R)))) * YSpace

    !P.Font  = -1
    !P.Color = CArrowsRGBN[3]
    arrows, hdr, AX, AY, /NORMAL, FONT=13, COLOR=!P.Color, arrowlen=3, charsize=2
    !P.Font  = 0
  endif 


;SIZE SCALE BAR
;   This is probably more complicated than necessary, but the idea is to find
; the best size scale bar for any image, where the scale may be a few arcsec
; or a few degrees.
;   "BarLength" is the length of a 1 arcsecond bar in normal coordinates
;   "BarScale" is the list of standard sizes for the bar in arcsec or arcmin.
;   "BarLength" is the length in normal coordiates of the "best" scale bar.
;
  if ((strtrim(CSize[0],2) ne '-1') and (NoAstrom ne -1)) then begin
    BarLength = 1.0 / (CDelt[0] * XDim)
    BarScale = [1,2,3,5,10,15,20,25,30,40]
    MinBar   = 0.1 * XSpace

    BS = where((BarLength * BarScale) gt MinBar)        ; bar scale in arcsec?
    if (BS[0] ne -1) then begin
      BarLength = BarLength * BarScale[BS[0]]
      BarLabel  = strtrim(BarScale[BS[0]], 2) + '"'
    endif else begin
      BS = where((BarLength * BarScale * 60) gt MinBar) ; bar scale in arcmin?
      if (BS[0] ne -1) then begin
        BarLength = BarLength * BarScale[BS[0]] * 60
        BarLabel  = strtrim(BarScale[BS[0]], 2) + "'"
      endif else begin
        BarLength = BarLength * 3600
        BarLabel  = '1 degree'
      endelse
    endelse

;    Barlength = BarLength * XSpace
    BarX      =   0.7 * XSpace             ; left end of bar
    BarY      = -0.03 * YSpace             ; Y position of bar
    BarDY     = 0.01 * [-1,1] * YSpace     ; height of bar's endpoints
    LabY      = BarY - (0.025 * YSpace)    ; position of label

    !P.Color = CSizeRGBN[3]
    plots, BarX+[0,BarLength], [BarY,BarY], /NORMAL
    plots, [BarX,BarX], BarY+BarDY, /NORMAL
    plots, BarLength+[BarX,BarX], BarY+BarDY,/NORMAL
    xyouts, ((BarX + (BarX + BarLength)) / 2.0), LabY, /NORMAL, ALIGN=0.5, $
      '!6'+BarLabel+'!X', FONT=-1

  endif

endif 

;;;
; TITLE  (handle here in case no header was given but TITLE keyword was used.)
;
 if (keyword_set(TITLE) and (strtrim(CTitle[0],2) ne '-1')) then begin
   !P.Color = CTitleRGBN[3]
   XYOUTS, 0.50*XSpace, 1+(0.01*YSpace), TITLE,SIZE=2.0, /NORMAL, ALIGN=0.5
 endif

 if keyword_set(NoClose) then begin
       plot,[0,xdim-1],[0,ydim-1],/noerase,xsty=5,ysty=5,/nodata, $
       pos = [0,0,1,1]
       return
 endif 

 Device,/close

;-------------------------------;
;  SECTION:  PRINTING THE FILE  ;
;-------------------------------;

 if not(NoPrint or Encap) then begin        ;Should the file be printed out?
 if not keyword_set(PRINTER) then begin
	 case !VERSION.OS_FAMILY of
	 'unix': printer = 'lpr'
	 else: printer = 'print'
	 endcase
 endif
 spawn,printer + ' ' + fname
 message,/INFO,'Now printing image: $' + printer + ' ' + fname
 endif

;  Reset output direction to X-windows, and restore some variables.

 tvlct,sv_rr,sv_gg,sv_bb
 set_plot, sv_device
 !P.font = sv_font
 !P.Color = sv_color

 return
 end
