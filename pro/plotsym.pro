pro plotsym, psym, psize, FILL=fill,thick=thick,Color = color
;+
; NAME:
;     PLOTSYM
; PURPOSE:
;     Define useful plotting symbols not in the standard !PSYM definitions.
; EXPLANATION:
;     After a symbol has been defined with PLOTSYM, a plotting command should
;     follow with either PSYM = 8 or !P.PSYM = 8 (see USERSYM)
;
;     For additional rotationally symmetric plotting symbols, see VSYM.PRO
;     Also see CGSYMCAT in the/coyote directory.
; CALLING SEQUENCE:
;     PLOTSYM, PSYM,[ PSIZE, /FILL, THICK=, COLOR=]
;
; INPUTS:
;     PSYM -  The following integer values of PSYM will create the
;             corresponding plot symbols
;     0 - circle
;     1 - downward arrow (upper limit), base of arrow begins at plot value             value
;     2 - upward arrow (lower limt)
;     3 - 5 pointed star
;     4 - triangle
;     5 - upside down triangle
;     6 - left pointing arrow
;     7 - right pointing arrow
;     8 - square
;
;     Arrows are defined such that their base begins at their origin.
;
; OPTIONAL INPUTS:
;     PSIZE - Size of the plotting symbol in multiples of the default size
;               (default PSIZE=1).  Does not need to be an integer
;
; OPTIONAL INPUT KEYWORD:
;     FILL -  Parameter indicating whether to fill the symbol (see USERSYM)
;             The default is 0, unfilled symbol.  Does not affect arrows
;             or character symbols.
;     THICK -  Thickness of unfilled symbols. Default is 1.
;     COLOR - Color of the symbols, Default is !P.color
; OUTPUTS:
;     None
;
; EXAMPLES:
;     Plot Y vs. X with filled stars as the symbol, twice the default size
;     IDL> PLOTSYM, 3 ,2, /FILL       ;Plotting symbol is a filled star,
;                                       ;twice default size
;     IDL> PLOT,X,Y,PSYM=8            ;Set PSYM = 8 to get star symbol
;
;     Now plot Y vs. X with an open circle as the symbol
;
;      IDL> PLOTSYM, 0               ;Plotting symbol is a circle
;      IDL> PLOT,X,Y,PSYM=8
;
; METHOD:
;     Appropriate X,Y vectors are used to define the symbol and passed to the
;     USERSYM command.
;
; REVISION HISTORY
;      Written       W. Landsman         June 1992
;      18-JAN-1996    Added a square symbol, HCW.
;      98Aug20         Added keyword thick parameter - RCB.
;      April 2001     Added COLOR keyword    WBL
;-
 On_error,2

 if N_elements(psym) LT 1 then begin
     print,'Syntax - PLOTSYM, psym, [ size, /FILL, THICK= ]'
     print,'  PSYM values 0 - circle, 1 - down arrow, 2 - up arrow, 3 - star'
     print,'       4 - triangle, 5 - upside down triangle, 6 - left arrow'
     print,'       7 - right arrow, 8 - square'
     return
 endif

 if ( N_elements(psize) LT 1 ) then psize = 1 else psize = psize > 0.1

 if ~keyword_set(FILL) then fill = 0
 if ~keyword_set(thick) then thick=1

  case psym of
  0:  begin          ;Circle
    ang = 2*!PI*findgen(49)/48.     ;Get position every 5 deg
    xarr = psize*cos(ang)  &  yarr = psize*sin(ang)
    end
1:  begin                                     ;Down arrow
    xarr = [0,0,.5,0,-.5]*psize
    yarr = [0,-2,-1.4,-2,-1.4]*psize
    fill = 0
    end
2:  begin                                     ;Up arrow
    xarr = [0,0,.5,0,-.5]*psize
    yarr = [0,2,1.4,2,1.4]*psize
    fill = 0
    end
3:  begin                                     ;Star
   ang = (360. / 10 * findgen(11) + 90) / !RADEG  ;star angles every 36 deg
   r = ang*0
   r[2*indgen(6)] = 1.
   cp5 = cos(!pi/5.)
   r1 = 2. * cp5 - 1. / cp5
   r[2*indgen(5)+1] = r1
   r = r * psize / sqrt(!pi/4.) * 2. / (1.+r1)
   xarr = r * cos(ang)   &   yarr = r * sin(ang)
   end
4:  begin                                     ;Triangle
    xarr = [-1,0,1,-1]*psize
    yarr = [-1,1,-1,-1]*psize
    end
5:  begin                                     ;Upside down triangle
    xarr = [-1, 0, 1, -1]*psize
    yarr = [ 1,-1, 1, 1]*psize
    end
6:  begin                                     ;Left pointing arrow
    yarr = [0, 0, 0.5, 0, -.5]*psize
    xarr = [0,-2,-1.4,-2,-1.4]*psize
    fill = 0
    end
7:  begin                                     ;Left pointing arrow
    yarr = [ 0, 0, 0.5, 0, -.5] * psize
    xarr = [ 0, 2, 1.4, 2, 1.4] * psize
    fill = 0
    end
8:  begin                                     ;Square
    xarr = [-1,-1,1, 1,-1] * psize
    yarr = [-1, 1,1,-1,-1] * psize
    end
 else: message,'Unknown plotting symbol value of '+strtrim(psym,2)
 endcase

 if N_elements(color) GT 0 then $
 usersym, xarr, yarr, FILL = fill,thick=thick, color = color else $
 usersym, xarr, yarr, FILL = fill,thick=thick
 return
 end

