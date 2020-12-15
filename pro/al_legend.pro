;+
; NAME:
;       AL_LEGEND
; PURPOSE:
;       Create an annotation legend for a plot.
; EXPLANATION:
;           
;       This procedure makes a legend for a plot.  The legend can contain
;       a mixture of symbols, linestyles, Hershey characters (vectorfont),
;       and filled polygons (usersym).  A test procedure, al_legendtest.pro,
;       shows legend's capabilities.  Placement of the legend is controlled
;       with keywords like /right, /top, and /center or by using a position
;       keyword for exact placement (position=[x,y]) or via mouse (/position).
;
;       The procedure CGLEGEND in the Coyote library provides a similar 
;       capability.     https://www.idlcoyote.com/idldoc/cg/cglegend.html
; CALLING SEQUENCE:
;       AL_LEGEND [,items][,keyword options]
; EXAMPLES:
;       The call:
;               al_legend,['Plus sign','Asterisk','Period'],psym=[1,2,3]
;         produces:
;               -----------------
;               |               |
;               |  + Plus sign  |
;               |  * Asterisk   |
;               |  . Period     |
;               |               |
;               -----------------
;         Each symbol is drawn with a cgPlots command, so they look OK.
;         Other examples are given in optional output keywords.
;
;       lines = indgen(6)                       ; for line styles
;       items = 'linestyle '+strtrim(lines,2)   ; annotations
;       al_legend,items,linestyle=lines         ; vertical legend---upper left
;       items = ['Plus sign','Asterisk','Period']
;       sym = [1,2,3]
;       al_legend,items,psym=sym                   ; ditto except using symbols
;       al_legend,items,psym=sym,/horizontal       ; horizontal format
;       al_legend,items,psym=sym,box=0             ; sans border
;       al_legend,items,psym=sym,delimiter='='     ; embed '=' betw psym & text
;       al_legend,items,psym=sym,margin=2          ; 2-character margin
;       al_legend,items,psym=sym,position=[x,y]    ; upper left in data coords
;       al_legend,items,psym=sym,pos=[x,y],/norm   ; upper left in normal coords
;       al_legend,items,psym=sym,pos=[x,y],/device ; upper left in device coords
;       al_legend,items,psym=sym,/position         ; interactive position
;       al_legend,items,psym=sym,/right            ; at upper right
;       al_legend,items,psym=sym,/bottom           ; at lower left
;       al_legenditems,psym=sym,/center           ; approximately near center
;       al_legend,items,psym=sym,number=2          ; plot two symbols, not one
;     Plot 3 filled colored squares
;       al_legend,items,/fill,psym=[8,8,8],colors=['red','green','blue']
;
;        Another example of the use of AL_LEGEND can be found at 
;        http://www.idlcoyote.com/cg_tips/al_legend.php
; INPUTS:
;       items = text for the items in the legend, a string array.
;               For example, items = ['diamond','asterisk','square'].
;               You can omit items if you don't want any text labels.  The
;               text can include many LaTeX symbols (e.g. $\leq$) for a less
;               than equals symbol) as described in cgsymbol.pro. 
; OPTIONAL INPUT KEYWORDS:
;
;       linestyle = array of linestyle numbers  If linestyle[i] < 0, then omit
;               ith symbol or line to allow a multi-line entry.     If 
;               linestyle = -99 then text will be left-justified.  
;       psym = array of plot symbol numbers or names.  If psym[i] is negative, 
;               then a line connects pts for ith item.  If psym[i] = 8, then the
;               procedure USERSYM is called with vertices defined in the
;               keyword usersym.   If psym[i] = 88, then use the previously
;               defined user symbol.    If 11 <= psym[i] <= 46 then David
;               Fanning's function CGSYMCAT() will be used for additional 
;               symbols.   Note that PSYM=10 (histogram plot mode) sets
;               poly_fill.
;       vectorfont = vector-drawn characters for the sym/line column, e.g.,
;               ['!9B!3','!9C!3','!9D!3'] produces an open square, a checkmark,
;               and a partial derivative, which might have accompanying items
;               ['BOX','CHECK','PARTIAL DERIVATIVE'].
;               There is no check that !p.font is set properly, e.g., -1 for
;               X and 0 for PostScript.  This can produce an error, e.g., use
;               !20 with PostScript and !p.font=0, but allows use of Hershey
;               *AND* PostScript fonts together.
;       N. B.: Choose any of linestyle, psym, and/or vectorfont.  If none is
;               present, only the text is output.  If more than one
;               is present, all need the same number of elements, and normal
;               plot behaviour occurs.
;               By default, if psym is positive, you get one point so there is
;               no connecting line.  If vectorfont[i] = '',
;               then cgPlots is called to make a symbol or a line, but if
;               vectorfont[i] is a non-null string, then cgText is called.
;       /help = flag to print header
;       /horizontal = flag to make the legend horizontal
;       /vertical = flag to make the legend vertical (D=vertical)
;       background_color - color name or number to fill the legend box.
;              Automatically sets /clear.    (D = -1)
;       box = flag to include/omit box around the legend (D=include)
;		  outline_color = color of box outline (D = !P.color)
;       bthick = thickness of the legend box (D = !P.thick)
;       charsize = just like !p.charsize for plot labels
;       charthick = just like !p.charthick for plot labels
;       clear = flag to clear the box area before drawing the legend
;       colors = array of colors names or numbers for plot symbols/lines 
;          See cgCOLOR for list of color names.   Default is 'Opposite'
;          If you are using index colors (0-255), then supply color as a byte,
;          integer or string, but not as a long, which will be interpreted as 
;          a decomposed color. See http://www.idlcoyote.com/cg_tips/legcolor.php
;       delimiter = embedded character(s) between symbol and text (D=none)
;       font = scalar font graphics keyword (-1,0 or 1) for text
;       linsize = Scale factor for line length (0-1), default = 1
;                 Set to 0 to give a dot, 0.5 give half default line length   
;       line_orientation = Filled line orientation (-180 to 180). Set to
;                          a value < -180 for solid fill (default).
;                          See poly_fill.
;       line_thick = Filled line thickness (D=!P.thick). See poly_fill.
;       margin = margin around text measured in characters and lines
;       number = number of plot symbols to plot or length of line (D=1)
;       spacing = line spacing (D=bit more than character height)
;       polycolor = Fill color for polygons (D='background').
;                   See poly_fill.
;       polyspace = Spacing between filled lines. See poly_fill.
;       poly_fill = Draw a bar and fill it. Implied by polycolor,
;                   polyspace, and line_orientation.
;       position = data coordinates of the /top (D) /left (D) of the legend
;       pspacing = psym spacing (D=3 characters) (when number of symbols is
;             greater than 1)
;       textcolors = array of color names or numbers for text.  See cgCOLOR
;          for a list of color names.   Default is 'Opposite' of background
;       thick = array of line thickness numbers (D = !P.thick), if used, then 
;               linestyle must also be specified
;       normal = use normal coordinates for position, not data
;       device = use device coordinates for position, not data
;       /window - if set then send legend to a resizeable graphics window
;       usersym = 2-D array of vertices, cf. usersym in IDL manual. 
;             (/USERSYM =square, default is to use existing USERSYM definition)
;       /fill = flag to fill the usersym
;       /left_legend = flag to place legend snug against left side of plot
;                 window (D)
;       /right_legend = flag to place legend snug against right side of plot
;               window.    If /right,pos=[x,y], then x is position of RHS and
;               text runs right-to-left.
;       /top_legend = flag to place legend snug against top of plot window (D)
;       /bottom = flag to place legend snug against bottom of plot window
;               /top,pos=[x,y] and /bottom,pos=[x,y] produce same positions.
;
;       If LINESTYLE, PSYM, VECTORFONT, SYMSIZE, THICK, COLORS, or 
;       TEXTCOLORS are supplied as scalars, then the scalar value is set for 
;       every line or symbol in the legend.
; Outputs:
;       legend to current plot device
; OPTIONAL OUTPUT KEYWORDS:
;       corners = 4-element array, like !p.position, of the normalized
;         coords for the box (even if box=0): [llx,lly,urx,ury].
;         Useful for multi-column or multi-line legends, for example,
;         to make a 2-column legend, you might do the following:
;           c1_items = ['diamond','asterisk','square']
;           c1_psym = [4,2,6]
;           c2_items = ['solid','dashed','dotted']
;           c2_line = [0,2,1]
;           al_legend,c1_items,psym=c1_psym,corners=c1,box=0
;           al_legend,c2_items,line=c2_line,corners=c2,box=0,pos=[c1[2],c1[3]]
;           c = [c1[0]<c2[0],c1[1]<c2[1],c1[2]>c2[2],c1[3]>c2[3]]
;         cgplots,[c[0],c[0],c[2],c[2],c[0]],[c[1],c[3],c[3],c[1],c[1]],/norm
;
;         Useful also to place the legend.  Here's an automatic way to place
;         the legend in the lower right corner.  The difficulty is that the
;         legend's width is unknown until it is plotted.  In this example,
;         the legend is plotted twice: the first time in the upper left, the
;         second time in the lower right.
;
;         al_legend,['1','22','333','4444'],linestyle=indgen(4),corners=corners
;                       ; BOGUS LEGEND---FIRST TIME TO REPORT CORNERS
;           xydims = [corners[2]-corners[0],corners[3]-corners[1]]
;                       ; SAVE WIDTH AND HEIGHT
;           chdim=[!d.x_ch_size/float(!d.x_size),!d.y_ch_size/float(!d.y_size)]
;                       ; DIMENSIONS OF ONE CHARACTER IN NORMALIZED COORDS
;           pos = [!x.window[1]-chdim[0]-xydims[0] $
;                       ,!y.window[0]+chdim[1]+xydims[1]]
;                       ; CALCULATE POSITION FOR LOWER RIGHT
;           cgplot,findgen(10)    ; SIMPLE PLOT; YOU DO WHATEVER YOU WANT HERE.
;           al_legend,['1','22','333','4444'],linestyle=indgen(4),pos=pos
;                       ; REDO THE LEGEND IN LOWER RIGHT CORNER
;         You can modify the pos calculation to place the legend where you
;         want.  For example to place it in the upper right:
;           pos = [!x.window[1]-chdim[0]-xydims[0],!y.window[1]-xydims[1]]
; Common blocks:
;       none
; Procedure:
;       If keyword help is set, call doc_library to print header.
;       See notes in the code.  Much of the code deals with placement of the
;       legend.  The main problem with placement is not being
;       able to sense the length of a string before it is output.  Some crude
;       approximations are used for centering.
; Restrictions:
;       Here are some things that aren't implemented.
;       - An orientation keyword would allow lines at angles in the legend.
;       - An array of usersyms would be nice---simple change.
;       - An order option to interchange symbols and text might be nice.
;       - Somebody might like double boxes, e.g., with box = 2.
;       - Another feature might be a continuous bar with ticks and text.
;       - There are no guards to avoid writing outside the plot area.
;       - There is no provision for multi-line text, e.g., '1st line!c2nd line'
;         Sensing !c would be easy, but !c isn't implemented for PostScript.
;         A better way might be to simply output the 2nd line as another item
;         but without any accompanying symbol or linestyle.  A flag to omit
;         the symbol and linestyle is linestyle[i] = -1.
;       - There is no ability to make a title line containing any of titles
;         for the legend, for the symbols, or for the text.
; Notes:
;       This procedure was originally named LEGEND, but a distinct LEGEND() 
;       function was introduced into IDL V8.0.   Therefore, the      
;       original LEGEND procedure was renamed to AL_LEGEND to avoid conflict.  
;
; Modification history:
;       write, 24-25 Aug 92, F K Knight (knight@ll.mit.edu)
;       allow omission of items or omission of both psym and linestyle, add
;         corners keyword to facilitate multi-column legends, improve place-
;         ment of symbols and text, add guards for unequal size, 26 Aug 92, FKK
;       add linestyle(i)=-1 to suppress a single symbol/line, 27 Aug 92, FKK
;       add keyword vectorfont to allow characters in the sym/line column,
;         28 Aug 92, FKK
;       add /top, /bottom, /left, /right keywords for automatic placement at
;         the four corners of the plot window.  The /right keyword forces
;         right-to-left printing of menu. 18 Jun 93, FKK
;       change default position to data coords and add normal, data, and
;         device keywords, 17 Jan 94, FKK
;       add /center keyword for positioning, but it is not precise because
;         text string lengths cannot be known in advance, 17 Jan 94, FKK
;       add interactive positioning with /position keyword, 17 Jan 94, FKK
;       allow a legend with just text, no plotting symbols.  This helps in
;         simply describing a plot or writing assumptions done, 4 Feb 94, FKK
;       added thick, symsize, and clear keyword Feb 96, W. Landsman HSTX
;               David Seed, HR Wallingford, d.seed@hrwallingford.co.uk
;       allow scalar specification of keywords, Mar 96, W. Landsman HSTX
;       added charthick keyword, June 96, W. Landsman HSTX
;       Made keyword names  left,right,top,bottom,center longer,
;                                 Aug 16, 2000, Kim Tolbert
;       Added ability to have regular text lines in addition to plot legend 
;       lines in legend.  If linestyle is -99 that item is left-justified.
;       Previously, only option for no sym/line was linestyle=-1, but then text
;       was lined up after sym/line column.    10 Oct 2000, Kim Tolbert
;       Make default value of thick = !P.thick  W. Landsman  Jan. 2001
;       Don't overwrite existing USERSYM definition  W. Landsman Mar. 2002
;	     Added outline_color BT 24 MAY 2004
;       Pass font keyword to cgText commands.  M. Fitzgerald, Sep. 2005
;       Default spacing, pspacing should be relative to charsize. M. Perrin, July 2007
;       Don't modify position keyword  A. Kimball/ W. Landsman Jul 2007
;       Small update to Jul 2007 for /NORMAL coords.  W. Landsman Aug 2007
;       Use SYMCAT() plotting symbols for 11<=PSYM<=46   W. Landsman  Nov 2009
;       Make a sharper box edge T. Robishaw/W.Landsman July 2010
;       Added BTHICK keyword W. Landsman October 2010
;       Added BACKGROUND_COLOR keyword  W. Landsman February 2011
;       Incorporate Coyote graphics  W. Landsman  February 2011
;       Added LINSIZE keyword W.L./V.Gonzalez   May 2011
;       Fixed a small problem with Convert_Coord when the Window keyword is set. 
;                         David Fanning, May 2011.
;       Fixed problem when /clear and /Window are set J. Bailin/WL   May 2011
;       CGQUERY was called instead of CGCONTROL   W.L.  June 2011
;       Fixed typo preventing BTHICK keyword from working W.L. Dec 2011
;       Remove call to SYMCAT() W.L. Dec 2011
;       Changed the way the WINDOW keyword adds commands to cgWindow, and
;       now default to BACKGROUND for background color. 1 Feb 2012 David Fanning
;       Allow 1 element SYMSIZE for vector input, WL Apr 2012.
;       Allow to specify symbols by cgSYMCAT() name WL Aug 2012 
;       Fixed bug when linsize, /right called simultaneously, Dec 2012, K.Stewart
;       Added a check for embedded symbols in the items string array. March 2013. David Fanning
;       Implement histogram filling.  J. Sapp  Aug 2015.
;       Only set polycolor if using polygon filling W. Landsman Sep 2016
;       Set default polycolor in case psym=10.  J. Sapp  Sep 2017
;       
;-
pro al_legend, items, BOTTOM_LEGEND=bottom, BOX = box, CENTER_LEGEND=center, $
    CHARTHICK=charthick, CHARSIZE = charsize, CLEAR = clear, COLORS = colorsi, $
    CORNERS = corners, DATA=data, DELIMITER=delimiter, DEVICE=device, $
    FILL=fill, HELP = help, HORIZONTAL=horizontal,LEFT_LEGEND=left, $
    LINESTYLE=linestylei, LINE_ORIENTATION=line_orient, $
    LINE_THICK=line_thick, MARGIN=margin, NORMAL=normal, NUMBER=number, PATTERN=pattern, $
    POLYCOLOR=polycolor, POLYSPACE=polyspace, POLY_FILL=poly_fill, POSITION=position, $
    PSPACING=pspacing, PSYM=psymi, RIGHT_LEGEND=right, $
    SPACING=spacing, SYMSIZE=symsizei, TEXTCOLORS=textcolorsi, THICK=thicki, $
    TOP_LEGEND=top, USERSYM=usersym,  VECTORFONT=vectorfonti, $
    VERTICAL=vertical,OUTLINE_COLOR = outline_color, FONT = font, $
    BTHICK=bthick, background_color = bgcolor, WINDOW=window,LINSIZE = linsize
;
;       =====>> HELP
;
compile_opt idl2
;On_error,2
if keyword_set(help) then begin & doc_library,'al_legend' & return & endif
; Should this commnad be added to a resizeable graphics window?
IF (Keyword_Set(window)) && ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        cgWindow, 'al_legend', items, BOTTOM_LEGEND=bottom, BOX = box, CENTER_LEGEND=center, $
            CHARTHICK=charthick, CHARSIZE = charsize, CLEAR = clear, COLORS = colorsi, $
            CORNERS = corners, DATA=data, DELIMITER=delimiter, DEVICE=device, $
            FILL=fill, HELP = help, HORIZONTAL=horizontal,LEFT_LEGEND=left, $
            LINESTYLE=linestylei, LINE_ORIENTATION=line_orient, $
            LINE_THICK=line_thick, MARGIN=margin, NORMAL=normal, NUMBER=number, PATTERN=pattern, $
            POLYCOLOR=polycolor, POLYSPACE=polyspace, POLY_FILL=poly_fill, POSITION=position, $
            PSPACING=pspacing, PSYM=psymi, RIGHT_LEGEND=right, $
            SPACING=spacing, SYMSIZE=symsizei, TEXTCOLORS=textcolorsi, THICK=thicki, $
            TOP_LEGEND=top, USERSYM=usersym,  VECTORFONT=vectorfonti, $
            VERTICAL=vertical,OUTLINE_COLOR = outline_color, FONT = font, $
            BTHICK=thick, background_color = bgcolor, LINSIZE = linsize, ADDCMD=1
                            
         RETURN
    ENDIF
    ;

;
;       =====>> SET DEFAULTS FOR SYMBOLS, LINESTYLES, AND ITEMS.
;
 ni = n_elements(items)
 np = n_elements(psymi)
 nl = n_elements(linestylei)
 nth = n_elements(thicki)
 nsym = n_elements(symsizei)
 nv = n_elements(vectorfonti)
 nlpv = max([np,nl,nv])
 n = max([ni,np,nl,nv])                                  ; NUMBER OF ENTRIES
strn = strtrim(n,2)                                     ; FOR ERROR MESSAGES
if n eq 0 then message,'No inputs!  For help, type al_legend,/help.'
if ni eq 0 then begin
  items = replicate('',n)                               ; DEFAULT BLANK ARRAY
endif else begin
  if size(items,/TNAME) NE 'STRING' then message, $
      'First parameter must be a string array.  For help, type al_legend,/help.'
  if ni ne n then message,'Must have number of items equal to '+strn
endelse
 
 poly_fill = keyword_set(poly_fill) || (n_elements(polycolor) ne 0) || $
  (n_elements(polyspace) ne 0) || (n_elements(line_orient) ne 0)
 if (poly_fill && (np eq 0)) then begin
  psymi = replicate(10, n) ; use "histogram" symbol
  np = n
  nlpv >= np
 endif

items = cgCheckForSymbols(items) ; Check for embedded symbols in the items array.
symline = (np ne 0) || (nl ne 0)                        ; FLAG TO PLOT SYM/LINE
 if (np ne 0) && (np ne n) && (np NE 1) then message, $
        'Must have 0, 1 or '+strn+' elements in PSYM array.'
 if (nl ne 0) && (nl ne n) && (nl NE 1) then message, $
         'Must have 0, 1 or '+strn+' elements in LINESTYLE array.'
 if (nth ne 0) && (nth ne n) && (nth NE 1) then message, $
         'Must have 0, 1 or '+strn+' elements in THICK array.'

 case nl of 
 0: linestyle = intarr(n)              ;Default = solid
 1: linestyle = intarr(n)  + linestylei
 else: linestyle = linestylei
 endcase 
 
  case nsym of 
 0: symsize = replicate(!p.symsize,n)      ;Default = !P.SYMSIZE
 1: symsize = intarr(n) + symsizei
 else: symsize = symsizei
 endcase 

 
 case nth of 
 0: thick = replicate(!p.thick,n)      ;Default = !P.THICK
 1: thick = intarr(n) + thicki
 else: thick = thicki
 endcase

 if size(psymi,/TNAME) EQ 'STRING' then begin
    psym = intarr(n)
    for i=0,N_elements(psymi)-1 do psym[i] = cgsymcat(psymi[i])
 endif else begin    
     
 case np of             ;Get symbols
 0: psym = intarr(n)    ;Default = solid
 1: psym = intarr(n) + psymi
 else: psym = psymi
 endcase 
 endelse

 case nv of 
 0: vectorfont = replicate('',n)
 1: vectorfont = replicate(vectorfonti,n)
 else: vectorfont = vectorfonti
 endcase 
;
;       =====>> CHOOSE VERTICAL OR HORIZONTAL ORIENTATION.
;
if n_elements(horizontal) eq 0 then $              ; D=VERTICAL
  setdefaultvalue, vertical, 1 else $
  setdefaultvalue, vertical, ~horizontal

;
;       =====>> SET DEFAULTS FOR OTHER OPTIONS.
;
 setdefaultvalue, box, 1
 if N_elements(bgcolor) NE 0 then clear = 1
 setdefaultvalue, bgcolor, 'BACKGROUND'
 setdefaultvalue, clear, 0
 setdefaultvalue, linsize, 1.
 setdefaultvalue, margin, 0.5
 setdefaultvalue, delimiter, ''
 setdefaultvalue, charsize, !p.charsize
 setdefaultvalue, charthick, !p.charthick
 if charsize eq 0 then charsize = 1
 setdefaultvalue, number, 1
; Default color is opposite the background color
 case N_elements(colorsi) of 
 0: colors = replicate('opposite',n)    
 1: colors = replicate(colorsi,n)
 else: colors = colorsi
 endcase 

 case N_elements(textcolorsi) of 
 0: textcolors = replicate('opposite',n)     
 1: textcolors = replicate(textcolorsi,n)
 else: textcolors = textcolorsi
 endcase 

 case N_elements(line_thick) of
 0:    line_thick = replicate(!P.thick,n)
 1:    line_thick = replicate(line_thick,n)
 else: ;line_thick = line_thick
 endcase

 ; line_orient or polyspace imply line filling...otherwise, solid or
 ; pattern filling
 
 if poly_fill NE 0  then begin

  case N_elements(line_orient) of
  0:    line_orient = replicate(0,n)
  1:    line_orient = replicate(line_orient,n)
  else: ;line_orient = line_orient
  endcase

  case N_elements(polyspace) of
  0:    polyspace = replicate(0.05,n)
  1:    polyspace = replicate(polyspace,n)
  else: ;polyspace = polyspace
  endcase

endif

; Set default polycolor -- needed if psym=10
case N_elements(polycolor) of
0:    polycolor = replicate('opposite',n)
1:    polycolor = replicate(polycolor,n)
else: ;polycolor = polycolori
endcase

empty_polycolor = where(polycolor eq '', n_empty)
if (n_empty ne 0) then $
 polycolor[empty_polycolor] = 'background'
polycolor = cgcolor(temporary(polycolor))

fill = keyword_set(fill)

if (n_elements(usersym) eq 1) then usersym = 2*[[0,0],[0,1],[1,1],[1,0],[0,0]]-1

;
;       =====>> INITIALIZE SPACING
;
setdefaultvalue, spacing, 1.2*charsize
setdefaultvalue, pspacing , 3*charsize
xspacing = !d.x_ch_size/float(!d.x_size) * (spacing > charsize)
yspacing = !d.y_ch_size/float(!d.y_size) * (spacing > charsize)
ltor = 1                                        ; flag for left-to-right
if n_elements(left) eq 1 then ltor = left eq 1
if n_elements(right) eq 1 then ltor = right ne 1
ttob = 1                                        ; flag for top-to-bottom
if n_elements(top) eq 1 then ttob = top eq 1
if n_elements(bottom) eq 1 then ttob = bottom ne 1
xalign = ltor ne 1                              ; x alignment: 1 or 0
yalign = -0.5*ttob + 1                          ; y alignment: 0.5 or 1
xsign = 2*ltor - 1                              ; xspacing direction: 1 or -1
ysign = 2*ttob - 1                              ; yspacing direction: 1 or -1
if ~ttob then yspacing = -yspacing
if ~ltor then xspacing = -xspacing
;
;       =====>> INITIALIZE POSITIONS: FIRST CALCULATE X OFFSET FOR TEXT
;
xt = 0
if nlpv gt 0 then begin                         ; SKIP IF TEXT ITEMS ONLY.
if vertical then begin                          ; CALC OFFSET FOR TEXT START
  for i = 0,n-1 do begin
    if (psym[i] eq 0) and (vectorfont[i] eq '') then num = (number + 1) > 3 else num = number
    if (psym[i] lt 0) || (psym[i] eq 10) then num = number > 2       ; TO SHOW CONNECTING LINE
    if psym[i] eq 0 then expand = linsize else expand = 2
    thisxt = (expand*pspacing*(num-1)*xspacing)
    if ltor then xt = thisxt > xt else xt = thisxt < xt
    endfor
endif   ; NOW xt IS AN X OFFSET TO ALIGN ALL TEXT ENTRIES.
endif
;
;       =====>> INITIALIZE POSITIONS: SECOND LOCATE BORDER
;

if !x.window[0] eq !x.window[1] then begin
  cgplot,/nodata,xstyle=4,ystyle=4,[0],/noerase
endif
;       next line takes care of weirdness with small windows
pos = [min(!x.window),min(!y.window),max(!x.window),max(!y.window)]

case n_elements(position) of
 0: begin
  if ltor then px = pos[0] else px = pos[2]
  if ttob then py = pos[3] else py = pos[1]
  if keyword_set(center) then begin
    if ~keyword_set(right) && ~keyword_set(left) then $
      px = (pos[0] + pos[2])/2. - xt
    if ~keyword_set(top) && ~keyword_set(bottom) then $
      py = (pos[1] + pos[3])/2. + n*yspacing
    endif
  nposition = [px,py] + [xspacing,-yspacing]
  end
 1: begin       ; interactive
  message,/inform,'Place mouse at upper left corner and click any mouse button.'
  cursor,x,y,/normal
  nposition = [x,y]
  end
 2: begin       ; convert upper left corner to normal coordinates
 
  ; if keyword window is set, get the current graphics window.
  if keyword_set(window) then begin
     wid = cgQuery(/current)
     WSet, wid
  endif
  if keyword_set(data) then $
    nposition = convert_coord(position,/to_norm) $
  else if keyword_set(device) then $
    nposition = convert_coord(position,/to_norm,/device) $
  else if ~keyword_set(normal) then $
    nposition = convert_coord(position,/to_norm) else nposition= position
  end
 else: message,'Position keyword can have 0, 1, or 2 elements only. Try al_legend,/help.'
endcase

yoff = 0.25*yspacing*ysign                      ; VERT. OFFSET FOR SYM/LINE.

x0 = nposition[0] + (margin)*xspacing            ; INITIAL X & Y POSITIONS
y0 = nposition[1] - margin*yspacing + yalign*yspacing    ; WELL, THIS WORKS!
;
;       =====>> OUTPUT TEXT FOR LEGEND, ITEM BY ITEM.
;       =====>> FOR EACH ITEM, PLACE SYM/LINE, THEN DELIMITER,
;       =====>> THEN TEXT---UPDATING X & Y POSITIONS EACH TIME.
;       =====>> THERE ARE A NUMBER OF EXCEPTIONS DONE WITH IF STATEMENTS.
;
for iclr = 0,clear do begin
  y = y0                                                ; STARTING X & Y POSITIONS
  x = x0
  if ltor then xend = 0 else xend = 1           ; SAVED WIDTH FOR DRAWING BOX

 if ttob then ii = [0,n-1,1] else ii = [n-1,0,-1]

 for i = ii[0],ii[1],ii[2] do begin
  if vertical then x = x0 else y = y0           ; RESET EITHER X OR Y
  x = x + xspacing                              ; UPDATE X & Y POSITIONS
  y = y - yspacing
  if nlpv eq 0 then goto,TEXT_ONLY              ; FLAG FOR TEXT ONLY
  num = number
  if (psym[i] eq 0) && (vectorfont[i] eq '') then num = (number + 1) > 3 
  if (psym[i] lt 0) || (psym[i] eq 10) then num = number > 2         ; TO SHOW CONNECTING LINE
  if psym[i] eq 0 then expand = 1 else expand = 2
  xp = x + expand*pspacing*indgen(num)*xspacing
  if (psym[i] gt 0) && (num eq 1) && vertical then xp = x + xt/2.
  yp = y + intarr(num)
  if vectorfont[i] eq '' then yp +=  yoff
  if (psym[i] eq 0) then begin
      if ltor eq 1 then xp = [min(xp),max(xp) -(max(xp)-min(xp))*(1.-linsize)]   
      if ltor ne 1 then xp = [min(xp) +(max(xp)-min(xp))*(1.-linsize),max(xp)]
      yp = [min(yp),max(yp)]                      ; DITTO
  endif
  if (psym[i] eq 8) && (N_elements(usersym) GT 1) then $
    usersym,usersym,fill=fill,color=cgcolor(colors[i])
;; extra by djseed .. psym=88 means use the already defined usersymbol
 if psym[i] eq 88 then p_sym =8 $
 else p_sym= psym[i]

  if vectorfont[i] ne '' then begin
;    if (num eq 1) && vertical then xp = x + xt/2      ; IF 1, CENTERED.
     cgText,xp,yp,vectorfont[i],width=width,color=colors[i], $
      size=charsize,align=xalign,charthick = charthick,/norm,font=font
    xt = xt > width
    xp = xp + width/2.
  endif else begin
    if (p_sym ne 10) then begin
     if (symline && (linestyle[i] ge 0)) then $
      cgPlots,xp,yp,color=colors[i], $
       /normal,linestyle=linestyle[i],psym=p_sym,symsize=symsize[i], $
       thick=thick[i]
    endif $
    else begin
     xp = [xp[0], xp[0], xp[1], xp[1], xp[0]]
     if (yp[0] eq yp[1]) then begin
      yp[0] -= (yspacing / 4.)
      yp[1] += (yspacing / 4.)
     endif
     yp = [yp[0], yp[1], yp[1], yp[0], yp[0]]

     if ((N_elements(line_orient) ne 0) && finite(line_orient[i]) && $
         (line_orient[i] ge -180) && (polyspace[i] gt 0)) then $
      cgPolygon, xp, yp, /normal, $
       /fill, color=colors[i], fcolor=polycolor[i], $
       orientation=line_orient[i], pattern=pattern, $
       spacing=polyspace[i], noclip=0, thick=line_thick[i] $
     else $
      cgPolygon, xp, yp, /normal, $
       /fill, color=colors[i], fcolor=polycolor[i], $
       pattern=pattern, noclip=0, thick=line_thick[i]
    endelse
  endelse

  if vertical then x += xt else if ltor then x = max(xp) else x = min(xp)
  if symline then x += xspacing
  
  TEXT_ONLY:
  if vertical && (vectorfont[i] eq '') && symline && (linestyle[i] eq -99) then x=x0 + xspacing
  cgText,x,y,delimiter,width=width,/norm,color=textcolors[i], $
         size=charsize,align=xalign,charthick = charthick,font=font	 
  x += width*xsign
  if width ne 0 then x += 0.5*xspacing
  cgText,x,y,items[i],width=width,/norm,color=textcolors[i],size=charsize, $
            align=xalign,charthick=charthick,font=font
  x += width*xsign
  if ~vertical && (i lt (n-1)) then x += 2*xspacing; ADD INTER-ITEM SPACE
  xfinal = (x + xspacing*margin)
  if ltor then xend = xfinal > xend else xend = xfinal < xend   ; UPDATE END X
 endfor

 if (iclr lt clear ) then begin
;       =====>> CLEAR AREA
        x = nposition[0]
        y = nposition[1]
        if vertical then bottom = n else bottom = 1
        ywidth = - (2*margin+bottom-0.5)*yspacing
        corners = [x,y+ywidth,xend,y]
        cgColorfill,[x,xend,xend,x,x],y + [0,0,ywidth,ywidth,0],/norm, $
	   color=bgcolor
;       cgPlots,[x,xend,xend,x,x],y + [0,0,ywidth,ywidth,0], $
;                 thick=2
 endif else begin

;
;       =====>> OUTPUT BORDER
;
        x = nposition[0]
        y = nposition[1]
        if vertical then bottom = n else bottom = 1
        ywidth = - (2*margin+bottom-0.5)*yspacing
        corners = [x,y+ywidth,xend,y]
        if box then cgPlots,[x,xend,xend,x,x,xend],y + [0,0,ywidth,ywidth,0,0],$
	        /norm, color = outline_color,thick=bthick
        return
 endelse
endfor

end
