PRO select_w_event, event
;
;This procedure is the event handler for the CW_BGROUP widget below
COMMON select_w, val, exclusive

WIDGET_CONTROL, event.id, GET_VALUE = value

if exclusive then begin 
   val = event.value
   widget_control, event.top,/DESTROY
   return	
endif

done = ((size(value,/tname) EQ 'STRING') && (value EQ 'DONE'))

if done then begin 
       good  = where( val GE 0, nsel )
       if (nsel GT 0) THEN val = val[good] 
       widget_control, event.top,/DESTROY
       return
endif

; Get the selections
if (event.select EQ 1) then val = [val,event.value] $
                       else val = val[ where( val NE event.value) ]
		       
 	       
		       		       
END

PRO select_w, items, iselected, comments, command_line, only_one, $
	Count = count, GROUP_LEADER=GROUP, selectin = selectin, columns = columns, $
	y_scroll_size = y_scroll_size
;+
; NAME:
;	SELECT_W    
; PURPOSE:
;	Create a non-exclusive widget menu of items
; EXPLANATION:
;	More than one item may be selected or 'de-selected'.   
;
; CALLING SEQUENCE:
;	SELECT_W, items ,iselected, [ comments, command_line, only_one, 
;                             SELECTIN = , COLUMNS=, Y_SCROLL_SIZE= ]
;
; INPUTS:
;	items - string array giving list of items that can be selected.
;
; OPTIONAL INPUTS:
;	comments - string array of comments (same number of elements as items)
;               for each item in array selections.     Will be displayed as a
;               tooltip when passing the cursor over the button for that item. 
;               Should have the same number of elements as items; otherwise
;               will be ignored (and no tooltips will be displayed).
;                 
;	command_line - optional command line to be placed at the bottom
;		of the screen.  It is usually used to specify what the
;		user is selecting.
;	only_one - integer flag. If set to 1 then the user can only select
;		one item.  The routine returns immediately after the first
;		selection is made.
;	columns - number of columns (default = 8)
;       y_scroll_size - size of GUI in device coordinates for scrolling large lists.
; OPTIONAL KEYWORD INPUT
;       SELECTIN - vector of items to be pre-selected upon input (not used for
;               only_one option)
;
; OUTPUT:
;	iselected - list of indices in selections giving the selected
;		items, in the order they were selected.
;
; OPTIONAL OUTPUT KEYWORD:
;       COUNT  - Integer scalar giving the number of items selected 
;
; MODIFICATION HISTORY:
;	Written, K. Venkatakrishna & W. Landsman, Hughes/STX    January, 1992
;	Widgets made MODAL.  M. Greason, Hughes STX, 15 July 1992.
;       Changed handling of MODAL keyword for V5.0   W.Thompson  September 1997
;       Added selectin keyword  D. Lindler 01/12/99 
;       Added Columns, y_scroll_size keyword inputs, D. Lindler 6/20/2013
;       Use CW_BGROUP instead of obsolete XMENU, implement comments parameter
;         as tooltips.   W. Landsman Aug 2013
;       Restore SELECTIN capability  W. Landsman Aug 2013
;       Kluge for Unix systems when Y_SCROLL_SIZE set Nov 2013
;-
;
 common select_w, val, exclusive
 
 if N_elements(only_one) EQ 0 then only_one = 0
 if N_params() LT 5 then exclusive = 0 else exclusive = only_one
 if N_elements(columns) eq 0 then columns = 8

 if N_params() LT 4 then command_line = $ 
' Select by pressing the left mouse button once; To de-select press twice; finally QUIT'
    
        scroll = N_elements(y_scroll_size) NE 0
        MODAL = N_ELEMENTS(GROUP) GE 1
        base = WIDGET_BASE( TITLE = command_line, /COLUMN, MODAL=MODAL, $
                GROUP_LEADER=GROUP)
; On windows, IDL knows what X_scroll_size to set to get the specified number 
; of columns.   On Unix we need a kluge to estimate the required X_SCROLL_SIZE   
 if (!VERSION.OS_FAMILY EQ 'unix') && keyword_set(y_scroll_size) then $
     x_scroll_size = columns*90		
		
 if only_one then $
       bgroup = cw_bgroup(base,items, COLUMN=columns, /EXCLUSIVE, $
        y_scroll_size=y_scroll_size, ids = id, UNAME='BGROUP', $
	x_scroll_size=x_scroll_size) $
    else begin 
       donebut = WIDGET_BUTTON( base, VALUE = 'DONE', UVALUE= -1)
       if N_elements(selectin) GT 0 then begin
           preselect = bytarr(N_elements(items))
	   preselect[selectin] = 1b
	   val = selectin
       endif else val=-1 	   
       bgroup = cw_bgroup(base,items, COLUMN=columns, $
       /NONEXCLUSIVE,y_scroll_size=y_scroll_size, ids= id, $
                  X_SCROLL_SIZE=x_scroll_size, UNAME='BGROUP', $
		  set_value = preselect) 
     endelse		  

; Realize the widgets:
 WIDGET_CONTROL, base, /REALIZE
 
;In Unix one gets an error if trying to display a Tooltip of zero length 
  lencomm = strlen(comments)
  if N_elements(comments) EQ N_elements(items) then $
     for i= 0, N_elements(comments)-1  do $
         if lencomm[i] GT 0 then widget_control, id[i], ToolTip = comments[i]

; Hand off to the XMANAGER, i.e.,event-handler,:
  XMANAGER, 'select_w', base, GROUP_LEADER = GROUP
  if val[0] NE -1 then iselected = val
  count = N_elements( iselected)

 return
 end

