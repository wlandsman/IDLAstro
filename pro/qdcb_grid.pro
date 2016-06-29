;+
; NAME:
;	QDCB_GRID
;
; PURPOSE:
;	Produce an overlay of latitude and longitude lines over a plot or image
; EXPLANATION:
;	Grid is plotted on the current graphics device assuming that the 
;	current plot is a map  in the so called quad cube projection. The 
;	output plot range is assumed to go from 7.0 to -1.0 on the X axis and 
;	-3.0 to 3.0 on the Y axis. Within this plotting space, the quad cube 
;	faces are laid out as follows (X=Empty, Astronomical Layout shown - 
;	X axis can be swapped for geographic maps):
;
;	    3.0_
;		XXX0
;		4321
;	   -3.0_XXX5
;		|  |
;	      7.0  -1.0
;
; CATEGORY:
;	Mapping Support Routine
;
; CALLING SEQUENCE:
;
;	QDCB_GRID,[,DLONG,DLAT,[LINESTYLE=N,/LABELS]
;
; INPUT PARAMETERS:
;
;	DLONG	= Optional input longitude line spacing in degrees. If left
;		  out, defaults to 30.
;
;	DLAT    = Optional input lattitude line spacing in degrees. If left
;		  out, defaults to 30.
;
;
; OPTIONAL KEYWORD PARAMETERS:
;
;	LINESTYLE	= Optional input integer specifying the linestyle to
;			  use for drawing the grid lines.
;
;	LABELS		= Optional keyword specifying that the lattitude and
;			  longitude lines on the prime meridian and the
;			  equator should be labeled in degrees. If LABELS is
;			  given a value of 2, i.e. LABELS=2, then the longitude
;			  labels will be in hours and minutes instead of
;			  degrees.
;
; OUTPUT PARAMETERS:
;
;	NONE
;
; PROCEDURE:
;
;	Uses WCSSPH2XY.PRO with projection 23 ("QSC" - COBE Quadrilatieralized
;	Spherical Cube) to compute positions of grid lines and labels.
;
; COPYRIGHT NOTICE:
;       This software has been authored by an employee or employees of Los Alamos 
;       National Security, LLC, operator of the Los Alamos National Laboratory (LANL) 
;       under Contract No. DE-AC52-06NA25396 with the U.S. Department of Energy.  The U.S.
;       Government has rights to use, reproduce, and distribute this software.  The public
;       may copy, distribute, prepare derivative works and publicly display this software 
;       without charge, provided that this Notice and any statement of authorship are 
;       reproduced on all copies.  Neither the Government nor LANS makes any warranty, 
;       express or implied, or assumes any liability or responsibility for the use of this
;       software.  If software is modified to produce derivative works, such modified 
;       software should be clearly marked, so as not to confuse it with the version 
;       available from LANL.
;
; AUTHOR:
;
;	Jeff Bloch
;
; MODIFICATIONS/REVISION LEVEL:
;
;	%I%	%G%
;	Use WCSSPH2XY instead of QDCB   Wayne Landsman   December 1994
;	Converted to IDL V5.0   W. Landsman   September 1997
;-

PRO QDCB_GRID,DLONG,DLAT,LINESTYLE=N,LABELS=LABELS

	if ~keyword_set(n) then n=0
	if n_params() lt 2 then dlat = 30.0
	if n_params() lt 1 then dlong = 30.0
;
;	Set up offsets to cube face panes
;
	xfaceoff = [0.0,0.0,2.0,4.0,6.0,0.0]
	yfaceoff = [2.0,0.0,0.0,0.0,0.0,-2.0]
	face = 0
;
;	Do lines of constant longitude
;
	lat=findgen(180)-90
	lng=fltarr(180)
	lngtot = long(360.0/dlong)
	for i=0,lngtot do begin
		lng[*]=-180.0+(i*dlong)
                wcssph2xy, lng, lat, x, y, 23,face = face,north=0.,south=0.
		x = x/45. & y = y/45.
		for k=0,5 do begin
		    j=where(face eq k,nf)
		    if nf ne 0 then $
		      oplot,x[j]+xfaceoff[k],$
			y[j]+yfaceoff[k],linestyle=n
		endfor
	endfor
;
;	Do lines of constant latitude
;
	lng=findgen(360)-45.0
	lat=fltarr(360)
	lattot=long(180.0/dlat)
	for i=1,lattot do begin
		lat[*]=-90+(i*dlat)
		wcssph2xy, lng, lat, x, y, 23,face = face,north=0.,south=0.
		x = x/45. & y = y/45.
                for k=0,5 do begin
                    j=where(face eq k,nf) 
                    if nf ne 0 then $
                      oplot,x[j]+xfaceoff[k],$
                        y[j]+yfaceoff[k],linestyle=n
                endfor
	endfor

;
;	Do labeling if requested
;
	if keyword_set(labels) then begin
;
;	Label equator
;
	    for i=0,lngtot-1 do begin
		lng = (i*dlong)
		if lng ne 0.0 then begin
                    wcssph2xy, lng, 0.0, x, y, 23, face = face,north=0.,south=0.
		    x = x/45. & y = y/45.
		    if labels eq 1 then xyouts,x[0]+xfaceoff[face],$
					y[0]+yfaceoff[face],noclip=0,$
			strcompress(string(lng,format="(I4)"),/remove_all) $
		    else begin
		        tmp=sixty(lng*23.0/360.0)
		        xyouts,x[0]+xfaceoff[face[0]],y[0]+yfaceoff[face[0]],$
			    noclip=0,strcompress(string(tmp[0],tmp[1],$
			    format='(I2,"h",I2,"m")'),/remove_all),alignment=0.5
		    endelse
		endif
	    endfor
;
;	Label prime meridian
;
	    for i=1,lattot-1 do begin
		lat=-90+(i*dlat)
                wcssph2xy, 0.0, lat, x, y, 23, face = face
		x = x/45. & y = y/45.
		xyouts,x[0]+xfaceoff[face[0]],y[0]+yfaceoff[face[0]],noclip=0,$
			strcompress(string(lat,format="(I4)"),/remove_all)
	    endfor
	endif
	return
END
