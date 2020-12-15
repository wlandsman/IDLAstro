;+
; Name:
;   MULTIPLOT
;
; Purpose:
;	Create multiple plots with simple control over the gaps between plots.
;   By default, the gap is zero but this can be set with the
;   gap= keyword, or xgap=, ygap= for individual control over different axes.
;   You can also place a single title along the x, y and top axes of the
;   matrix of plots using the mtitle, mxtitle and mytitle keywords.
;
;	It is good for data with one or two shared axes and retains all the
;	versatility of the plot commands (e.g. all keywords and log scaling).
;	The plots are connected with the shared axes, which saves space by
;	omitting redundant ticklabels and titles.  Multiplot does this by
;	setting !p.position, !x.tickname and !y.tickname automatically.
;	A call (multiplot,/reset) restores original values.
;
;   Coyote graphics users can find similar functionality in CGLAYOUT
;        http://www.idlcoyote.com/idldoc/cg/cglayout.html
;   Users of the post-8.0 IDL function graphics can find similar functionality
;   in Paulo Penteado's routine PP_MULTIPLOT
;        http://ppenteado.net/idl/pp_lib/doc/pp_multiplot__define.html
; CALLING SEQUENCE:
;	multiplot, pmulti, 
;       gap=, xgap=, ygap=, 
;       /square, 
;       /doxaxis, /doyaxis, 
;       mTitle=, mTitSize=, mTitOffset=, 
;       mxTitle=, mxTitSize=, mxTitOffset=, 
;       myTitle=, myTitSize=, myTitOffset=, 
;       xtickformat=, ytickformat=
;       /default, /reset, /rowmajor, /initialize
;
; INPUTS:
;   pmulti: Optional input. [Nx,Ny] array describing the shape of the
;       matrix of plots.  This is equivalent to the 2nd and 3rd elements
;       of !p.multi.  Or you can send all 5 elements of the !p.multi.
;
; KEYWORD INPUTS:
;   gap=: Set the gap between plots in normalized units.  Default is 0.
;       This input overrides the xgap and ygap inputs.
;   xgap=: Gap between plots in the x direction. Default 0. To set both
;       x and y gap to the same value just use the gap keyword.
;   ygap=: Gap between plots in the y direction. Default 0. To set both
;       x and y gap to the same value just use the gap keyword.
;
;   mTitle: A single title to go across the top of the matrix of plots,
;       as opposed to the plot over single plots you generate with the
;       plot command for example. 
;   mTitSize: The font size of the top title. Default is 1.25*!p.charsize
;   mTitOffset: Offset of the title in the y-direction.
;   mxTitle, mxTitSize, mxTitOffset: same as above but for the x-axis title
;   myTitle, myTitSize, myTitOffset: same as above but for the y-axis title
;
;   xtickformat, ytickformat: Set the default tick formats when the ticks
;       are plotted. This allows the user to avoid sending this to each
;       plotting command which can have unexpected results if that axis
;       was not to get tick labels in a given point in the matrix.
;
; KEYWORDS SWITCHES:
;   /square: Force the axis ratio of each plot to be square. Note if
;       xgap and ygap are set to different values, this axis ratio will
;       not be preserved.  It will be preserved if gap= is used.
;
;   /doxaxis: Put axis labels, etc on the axis. Default is to place labels
;       only on the left side and bottom sides of the plot matrix, but may
;       be useful when some cells are empty; for example the x-axis of
;       a 2x2 grid when only 3 total plots will be created.
;   /doyaxis: Put axis labels, etc on the yxis.  Default is to place labels
;       only on the left side and bottom sides of the plot matrix, but may
;       be useful when some cells are empty; for example the x-axis of
;       a 2x2 grid when only 3 total plots will be created.
;
;   /rowmajor: Like setting 5th element of !p.multi to 1. 
;   /reset: Set plotting parameters to their saved values from before
;       multiplot was initially called.
;   /default: Set plotting parameters to IDL defaults.  This is useful
;       when the saved parameters get in a whacky state.
;   /initialize: Just do the initialization. This is what happends when
;       you first call multiplot anyway.
;
; EXAMPLES:
;   ; Make an array of plots [4,3] with a gap of 0.1 (in norm. coords.)
;   ; and overall titles along the x and y axes as given.  Force the
;   ; plots to be square.
;
;       cgerase & multiplot, [4,3], /square, gap=0.1, mXtitle='R', mYtitle='F(R)'
;       for i=0,4*3-1 do begin
;           cgplot, struct[i].x, struct[i].y, psym=4
;           multiplot
;       endfor
;       multiplot,/reset
;
; Side Effects:
;   Multiplot sets a number of system variables: !p.position, !p.multi,
;	!x.tickname, !y.tickname, !P.noerase---but all can be reset with
;	the call: multiplot,/reset  
;
;   Things can get out of wack if your program crashes in the middle of 
;   making a matrix of plots, and often /reset will not fix it.  In those 
;   cases, calling multiplot,/default will often fix the problem.
;
; Restrictions:
;	1. If you use !p.multi as the method of telling how many plots
;	are present, you have to set !p.multi at the beginning each time you
;	use multiplot or call multiplot with the /reset keyword.
;	2. There is no way to make plots of different sizes; each plot
;	covers the same area on the screen or paper.
;
; Modification history:
;	write, 21-23 Mar 94, Fred Knight (knight@ll.mit.edu)
;	alter plot command that sets !x.window, etc. per suggestion of
;	  Mark Hadfield (hadfield@storm.greta.cri.nz), 7 Apr 94, FKK
;	add a /default keyword restore IDL's default values of system vars,
;	  7 Apr 94, FKK
;	modify two more sys vars !x(y).tickformat to suppress user-formatted
;	  ticknames, per suggestion of Mark Hadfield (qv), 8 Apr 94, FKK
;       
;   2001-03-20    Added /square keyword
;       Work in device coordinates so we can force aspect ratio to be square 
;       if requested. Erin Scott Sheldon UMichigan
;       
;   2007-06-18
;       Can now place titles on the overall x and y axes, as well as a 
;       top title using these new keywords. 
;           mTitle=, mTitSize=, mTitOffset=, 
;           mxTitle=, mxTitSize=, mxTitOffset=, 
;           myTitle=, myTitSize=, myTitOffset=, 
;       Can also control overall tick formats. Useful because can just call
;       multiplot initially and set this, while calling on each call to
;       the plotting program will have unexpected results if the ticks
;       are not to be labelled for that place in the matrix.
;           xtickformat, ytickformat
;       Erin Sheldon, NYU
;   2007-08-28:
;       Can now add gaps between the plots with these keywords:
;           gap=, xgap=, ygap=
;       where the values are in normalized coordinates. Erin Sheldon, NYU
;   2009-11-23
;       Initialize common block if M[X/Y]TITLE set W. Landsman 
;   2011-02-07
;        Use Coyote Graphics  W. Landsman    
;   2012-03-21
;        Use cgplot on initial call to get right background  W.L.
;   2014-02-04
;        Handle  [X/Y].OMargin   A. Negri, Bologna
;
;-

PRO multiplot, pmulti, help=help, $
        initialize=initialize, reset=reset, default=default, $
        rowmajor=rowmajor,verbose=verbose, square=square, $
        gap=gap_in, xgap=xgap_in, ygap=ygap_in, $
        doxaxis=doxaxis, doyaxis=doyaxis, $
        xtickformat=xtickformat_in, ytickformat=ytickformat_in, $
        mtitle=mtitle, mTitSize=mTitSize, mTitOffset=mTitOffset, $
        mxTitle=mxTitle, mxTitSize=mxTitSize, mxTitOffset=mxTitOffset, $
        myTitle=myTitle, myTitSize=myTitSize, myTitOffset=myTitOffset




    common multiplot $
        ,nplots $                   ; [# of plots along x, # of plots along y]
        ,nleft $                    ; # of plots remaining---like the first element of !p.multi
        ,pdotmulti $                ; saved value of !p.multi
        ,margins $                  ; calculated margins based on !p.multi or pmulti
        ,pposition $                ; saved value of !p.position
        ,colmajor $                 ; flag for column major order
        ,noerase $                  ; saved value of !p.noerase
        ,sqplot $                   ; should be make it square?
        ,xtickname $                ; Original value
        ,ytickname $                ; Original value
        ,xtickformat_orig $         ; Original value
        ,ytickformat_orig $        
        ,xtickformat $              ; Value we will use
        ,ytickformat $
        ,gap  $
        ,xgap $
        ,ygap

    ; help message
    if keyword_set(help) then begin
        doc_library,'multiplot' 
        return 
    endif
   
    
    ; restore idl's default values (kill multiplot's influence)
    if keyword_set(default) then begin
        !p.position = 0
        !x.tickname = ''
        !y.tickname = ''
        !x.tickformat = ''
        !y.tickformat = ''
        !p.multi = 0
        !p.noerase = 0
        nleft = 0
        nplots = [1,1]
        pdotmulti = !p.multi
        margins = 0
        sqplot=0
        pposition = !p.position
        noerase = !p.noerase
        xtickname = !x.tickname
        ytickname = !y.tickname
        xtickformat = !x.tickformat
        ytickformat = !y.tickformat

        gap=0.0
        xgap=0.0
        ygap=0.0
        if keyword_set(verbose) then begin
            message,/inform,$
                'Restore IDL''s defaults for affected system variables.'
            message,/inform,$
                'Reset multiplot''s common to IDL''s defaults.'
        endif
        return
    endif

    ; restore saved system variables
    if keyword_set(reset) then begin
         if n_elements(pposition) gt 0 then begin
             !p.position = pposition
             !x.tickname = xtickname
             !y.tickname = ytickname
             !x.tickformat = xtickformat_orig
             !y.tickformat = ytickformat_orig
             !p.multi = pdotmulti
             !p.noerase = noerase
             sqplot=0
        endif
        nleft = 0
        if keyword_set(verbose) then begin
            coords = '['+string(!p.position,form='(3(f4.2,","),f4.2)')+']'
            multi = '['+string(!p.multi,form='(4(i2,","),i2)')+']'
            message,/inform,'Reset.  !p.position='+coords+', !p.multi='+multi
        endif
        gap=0.0
        xgap=0.0
        ygap=0.0
        return
    endif

    ;
    ;  Now the user inputs
    ;

    ; How big are the gaps between the plots?
    if n_elements(gap) eq 0 then begin
        ; initial set up of common block values
        xgap=0.0
        ygap=0.0
        gap=0.0
    endif

    if n_elements(xgap_in) ne 0 then xgap=xgap_in
    if n_elements(ygap_in) ne 0 then ygap=ygap_in

    ; gap will override any previously set values
    if n_elements(gap_in) ne 0 then begin
        gap=gap_in
        xgap=gap
        ygap=gap
    endif


    ;
    ; Set up the plot layout
    ;

    ; Shall we force the individual plots to be square?
    if keyword_set(square) then sqplot=1 else begin
        if n_elements(sqplot) eq 0 then sqplot=0
    endelse 


    ; number of plots left in the grid
    if n_elements(nleft) eq 1 then init = (nleft eq 0) else init = 1
    if (n_elements(pmulti) eq 2) or (n_elements(pmulti) eq 5) then init = 1
    if (n_elements(!p.multi) eq 5) then begin
        if (!p.multi[1] gt 0) and (!p.multi[2] gt 0) then begin
            init = (!p.multi[0] eq 0) 
        endif
    endif
  
    if ~init then init = keyword_set(mxtitle) || keyword_set(mytitle) || $
                         keyword_set(mtitle)

    ; initialize if we are on the first plot
   
    if init or keyword_set(initialize) then begin
        case n_elements(pmulti) of
            0:begin
                if n_elements(!p.multi) eq 1 then return ; NOTHING TO SET
                if n_elements(!p.multi) ne 5 then begin
                    message,'Bogus !p.multi; aborting.'
                endif
                nplots = !p.multi[1:2] > 1
                if keyword_set(rowmajor) then begin
                    colmajor = 0 
                endif else begin
                    colmajor = !p.multi[4] eq 0
                endelse
            end
            2:begin
                nplots = pmulti
                colmajor = not keyword_set(rowmajor) 
            end
            5:begin
                nplots = pmulti[1:2]
                if keyword_set(rowmajor) then begin
                    colmajor = 0 
                endif else begin
                    colmajor = pmulti[4] eq 0
                endelse
            end
            else: message,'pmulti can only have 0, 2, or 5 elements.'
        endcase
	
        pposition = !p.position   ; save sysvar to be altered
        xtickname = !x.tickname
        ytickname = !y.tickname

        ; keep original values for resetting
        xtickformat_orig = !x.tickformat
        ytickformat_orig = !y.tickformat

        ; what will we actually plot when ticks are exposed?
        if n_elements(xtickformat_in) ne 0 then begin
            xtickformat=xtickformat_in
        endif else begin
            xtickformat=xtickformat_orig
        endelse
        if n_elements(ytickformat_in) ne 0 then begin
            ytickformat=ytickformat_in
        endif else begin
            ytickformat=ytickformat_orig
        endelse

        pdotmulti = !p.multi
        nleft = nplots[0]*nplots[1] ; total # of plots
 
        !p.position = 0           ; reset
        !p.multi = 0

        ; set window & region

        cgplot,/nodata,xstyle=4,ystyle=4,!x.range,!y.range,/noerase	

        px = !x.window*!d.x_vsize
        py = !y.window*!d.y_vsize
        xsize = px[1] - px[0]
        ysize = py[1] - py[0]

        ; in normlized coordinates

        ;Andrea Negri modification
        nmargins = [min(!x.window)-min(!x.region)  $
                     +!d.x_ch_size*!x.omargin[0]/double(!d.x_vsize), $
                    min(!y.window)-min(!y.region)  $
                     +!d.y_ch_size*!y.omargin[0]/double(!d.y_vsize), $
                    max(!x.region)-max(!x.window)  $
                     +!d.x_ch_size*!x.omargin[1]/double(!d.x_vsize), $
                    max(!y.region)-max(!y.window)  $
                     +!d.y_ch_size*!y.omargin[1]/double(!d.y_vsize)]

        ;in device coord
        margins = nmargins
        margins[0] = nmargins[0]*!d.x_vsize
        margins[2] = nmargins[2]*!d.x_vsize
        margins[1] = nmargins[1]*!d.y_vsize
        margins[3] = nmargins[3]*!d.y_vsize

        noerase = !p.noerase
        !p.noerase = 1            ; !p.multi does the same
        if keyword_set(verbose) then begin
            major = ['across then down (column major).',$
                     'down then across (row major).']
                 if colmajor then index = 0 else index = 1
                 message,/inform,'Initialized for '+strtrim(nplots[0],2) $
                     +'x'+strtrim(nplots[1],2)+', plotted '+major[index]
        endif

        if keyword_set(initialize) then return
    endif

    ;
    ; Define the plot region without using !p.multi.
    ;

    cols = nplots[0]              ; for convenience
    rows = nplots[1]
    nleft = nleft - 1             ; decrement plots remaining
    cur = cols*rows - nleft       ; current plot #: 1 to cols*rows

    ; device coords per plot
    idx = [(!d.x_vsize-margins[0]-margins[2])/cols, $
           (!d.y_vsize-margins[1]-margins[3])/rows] 

    ;; force to be square if requested
    if sqplot then begin 
        if idx[0] lt idx[1] then idx[1]=idx[0] else idx[0]=idx[1]
    endif 

    if colmajor then begin        ; location in matrix of plots
        col = cur mod cols
        if col eq 0 then col = cols
        row = (cur-1)/cols + 1
    endif else begin              ; here (1,2) is 1st col, 2nd row
        row = cur mod rows
        if row eq 0 then row = rows
        col = (cur-1)/rows + 1
    endelse


    pos = $
        [(col-1)*idx[0], (rows-row)*idx[1],   $
         col*idx[0],     (rows-row+1)*idx[1]] $
       +                                      $
        [margins[0], margins[1],              $
         margins[0], margins[1]]
  
    ; back to normalized coords
    pos[0] = pos[0]/!d.x_vsize
    pos[2] = pos[2]/!d.x_vsize
    pos[1] = pos[1]/!d.y_vsize
    pos[3] = pos[3]/!d.y_vsize

    ; add gaps
    pos[0] = pos[0] + xgap
    pos[2] = pos[2] - xgap

    pos[1] = pos[1] + ygap
    pos[3] = pos[3] - ygap

    ;
    ; Finally set the system variables; user shouldn't change them.
    ;
    
    !p.position = pos
    onbottom = (row eq rows) or (rows eq 1)
    onleft = (col eq 1) or (cols eq 1)
    IF keyword_set(doxaxis) THEN onbottom=1
    IF keyword_set(doyaxis) THEN onleft=1
    if onbottom then begin
        !x.tickname = xtickname 
    endif else begin
        !x.tickname = replicate(' ',30)
    endelse
    if onleft then !y.tickname = ytickname else !y.tickname = replicate(' ',30)
    if onbottom then !x.tickformat = xtickformat else !x.tickformat = ''
    if onleft then !y.tickformat = ytickformat else !y.tickformat = ''
    if keyword_set(verbose) then begin
        coords = '['+string(pos,form='(3(f4.2,","),f4.2)')+']'
        plotno = 'Setup for plot ['+strtrim(col,2)+','+strtrim(row,2)+'] of ' $
            +strtrim(cols,2)+'x'+strtrim(rows,2)
        message,/inform,plotno+' at '+coords
    endif



    ; Add titles to overall axes

    ; area covered by entire plot field in device coords
    allpos = $
        [0,                       0,      cols*idx[0], rows*idx[1]] +   $
        [margins[0],     margins[1],        margins[0], margins[1]]
    ;; back to normalized coords
    allpos[0] = allpos[0]/!d.x_vsize
    allpos[2] = allpos[2]/!d.x_vsize
    allpos[1] = allpos[1]/!d.y_vsize
    allpos[3] = allpos[3]/!d.y_vsize

    xCharSizeNorm = float(!d.x_ch_size) / float(!d.x_size)
    yCharSizeNorm = float(!d.y_ch_size) / float(!d.y_size)

    ; top title
    if n_elements(mTitle) ne 0 then begin
        if n_elements(mTitSize) eq 0 then mTitSize = 1.0
        if n_elements(mTitOffset) eq 0 then mTitOffset = 0.0

        ; align middle of region in x
        xpos = (allpos[2] - allpos[0])/2.0 + nmargins[0]
        ; align relative to the top.  Default is right there plus 
        ; one character size.
        ypos = allpos[3] + (mTitOffset+1.0)*yCharSizeNorm

        ; correct for gaps
        ypos = ypos - ygap
         cgtext, $
            xpos, $
            ypos, $
            mTitle, $
            /normal, $
            align = 0.5, $
            charsize = 1.25 * mTitSize
    endif

    ; x title
    if n_elements(mxTitle) ne 0 then begin
        if n_elements(mxTitSize) eq 0 then mxTitSize = 1.0
        if n_elements(mxTitOffset) eq 0 then mxTitOffset = 0.0

        ; align middle of region in x
        xpos = (allpos[2] - allpos[0])/2.0 + nmargins[0]

        ; align middle of region in x
        ypos = allpos[1] - (mxTitOffset+3.0)*yCharSizeNorm

        ; correct for gaps
        ypos = ypos + ygap
        cgtext, $
            xpos, $
            ypos, $
            mxTitle, $
            /normal, $
            align = 0.5, $
            charsize = mxTitSize
    endif



    ; y title
    if n_elements(myTitle) ne 0 then begin
        if n_elements(myTitSize) eq 0 then myTitSize = 1.0
        if n_elements(myTitOffset) eq 0 then myTitOffset = 0.0

        ; align relative to the left side.  Default is right there plus 
        ; one character size.
        xpos = allpos[0] - (myTitOffset+6.0)*xCharSizeNorm
        ;xpos = allpos[0] - (myTitOffset+4.0)*xCharSizeNorm

        ; align middle of region in x
        ypos = (allpos[3] - allpos[1])/2.0 + nmargins[1]


        ; correct for gaps
        xpos = xpos + xgap

        cgtext, $
            xpos, $
            ypos, $
            myTitle, $
            /normal, $
            align = 0.5, $
            orientation = 90.0, $
            charsize = myTitSize
    endif


return
end
