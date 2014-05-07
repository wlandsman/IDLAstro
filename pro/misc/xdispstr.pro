;+
;  NAME:      
;     XDISPSTR
;
;  PURPOSE:   
;     Display a string array in a text widget with a simple search capability.
;
; EXPLANATION:
;     Similar to the IDL XDISPLAYFILE procedure but includes a search capbility.
; CALLING SEQUENCE:    
;                 
;     xdispstr, array, [/BLOCK, WIDTH= , HEIGHT=, TITLE=, GROUP_LEADER=, FONT=
;                       TOP_LINE=, POS= ]
;
; INPUT PARAMETER:
;
;     array  - String array (.e.g. FITS header) to be displayed
;
;  OPTIONAL INPUT KEYWORD PARAMETERS:
;
;    block -  Set to 1 to make widget blocking.  Default = block=0
;
;    font  -     Display font for text.
;          
;    width, height  - Scalars giving number of characters per line, number
;                           of lines.  Default = 80x48
;
;    group_leader  -    Group leader for top level base.
;
;    title  - Scalar Title for outermost base widget.
;
;    pos - 2 element array containing the normalized X and Y position to
;            display the widget on the screen.    [0,0] is the upper left
;            hand corner.
;
;    top_line - first line in the string array to display (default is 0)
;
; PROCEDURES USED:
;     CGCENTERTLB
;
;  MODIFICATION HISTORY:
;     Written by R. S. Hill, RITSS, 17 Nov 2000
;     Use cumulative keyword to TOTAL   W. Landsman   May 2006
;     Made resizeable, default size now 48 lines  W. Landsman   July 2013
;     Added POS keyword W. Landsman  Sep 2013
;-


PRO XDISPSTR_EVENT, Event

widget_control, event.top, get_uvalue=info

search = 0b
destroy = 0b
if tag_names(event,/STRUCTURE_NAME) EQ 'WIDGET_BASE' then begin
    widget_control,(*info).array_text, $
          scr_ysize = event.Y,scr_xsize=event.X
endif else begin	  
CASE event.id OF
(*info).done_button:  destroy=1b
(*info).search_button:  search=1b
(*info).search_text:  search=1b
ELSE:
ENDCASE
endelse

IF search THEN BEGIN
    widget_control, (*info).search_text, get_value=seastr
    seastr = seastr[0]
    sp = strpos(strupcase(*(*info).arrayptr), strupcase(seastr))
    w = where(sp GE 0, c)
    IF c GT 0 THEN BEGIN
        tptr = sp[w] + (*(*info).clenptr)[w]
        tlen = strlen(seastr)
        ts = widget_info((*info).array_text, /text_select)
        this_line = max(where(ts[0] GE *(*info).clenptr, c3))
        line_frag = $
            strmid(strupcase((*(*info).arrayptr)[this_line]), $
                   ts[0] - (*(*info).clenptr)[this_line] + tlen)
        again = strpos(line_frag, strupcase(seastr))
        IF again GE 0 THEN BEGIN
            newtptr = again + tlen + ts[0]
        ENDIF ELSE BEGIN
            next = min(where(tptr GT ts[0], c2))
            IF c2 GT 0 THEN newtptr = tptr[next] ELSE newtptr = tptr[0]
        ENDELSE
        widget_control, (*info).array_text, set_text_select=[newtptr,tlen]
        new_line = max(where(newtptr GE *(*info).clenptr))
        middle = (*info).height/2
        nl = n_elements(*(*info).arrayptr)
        tl = ((new_line-middle)>0)<(nl-(*info).height)
        widget_control, (*info).array_text, set_text_top_line=tl
        widget_control, (*info).msg_text, set_value='Line '+strn(new_line)
    ENDIF ELSE BEGIN
        widget_control, (*info).msg_text, set_value='String not found'
    ENDELSE
ENDIF

IF destroy THEN widget_control, event.top, /destroy

RETURN
END

PRO XDISPSTR_CLEANUP, Id
widget_control, id, get_uvalue=info
IF ptr_valid(info) THEN BEGIN
    IF ptr_valid((*info).clenptr) THEN ptr_free, (*info).clenptr
    IF ptr_valid((*info).arrayptr) THEN ptr_free, (*info).arrayptr
    ptr_free, info
ENDIF
RETURN
END


PRO XDISPSTR, Array, BLOCK=block, WIDTH=width, HEIGHT=height, TITLE=title, $
                     GROUP_LEADER=group_leader, FONT=font,top_line=top_line, $
		     POS = pos

on_error, 2

IF N_params() LT 1 THEN BEGIN
    print, 'CALLING SEQUENCE:  XDISPSTR, Array'
    print, 'KEYWORD PARAMETERS:  BLOCK, WIDTH, HEIGHT, TITLE, ' $
            + 'GROUP_LEADER, FONT'
    RETURN
ENDIF

IF n_elements(block) LT 1 THEN block=0
IF n_elements(width) LT 1 THEN width=80
IF n_elements(height) LT 1 THEN height=48 < N_elements(array)
IF n_elements(title) LT 1 THEN title='XDISPSTR'

tlb = widget_base(title=title,col=1,group_leader=group_leader,/TLB_Size_Events)

controls = widget_base(tlb, frame=1, row=1)
done_button = widget_button(controls, value='Done', /no_release)
search_button = widget_button(controls, value='Search:', /no_release)
search_text = widget_text(controls, xsize=30, ysize=1, /editable, font=font)
msg_label = widget_label(controls, value='Message: ')
msg_text = widget_text(controls, xsize=20, ysize=1, font=font)

array_text = widget_text(tlb, value=array, $
                         xsize=width, ysize=height, /scroll, edit=0, font=font)

if N_elements(top_line) EQ 0 then top_line = 0
widget_control, array_text, set_text_top_line=top_line
widget_control, array_text, set_text_select=[0,0]

widget_control, tlb, /realize
    
linelen1 = strlen(array) + 1
cumul_len = [0, total(linelen1,/cumulative,/integer)]
geom = widget_info(tlb,/geometry)
info = ptr_new({done_button:done_button, $
                search_button:search_button, search_text:search_text, $
                array_text:array_text, arrayptr:ptr_new(array), $
                clenptr:ptr_new(cumul_len,/no_copy), $
                msg_text:msg_text, width:width, height:height})


widget_control, tlb, set_uvalue=info
widget_control, tlb,tlb_get_size = basesize
xmanager, 'xdispstr', tlb, cleanup='xdispstr_cleanup', $
          event_handler='xdispstr_event', no_block=1b-block, $
          group_leader=group_leader
if N_elements(pos) EQ 2 then cgcentertlb,tlb,pos[0],pos[1]	  

RETURN
END

