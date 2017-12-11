;+
; NAME:
;       WCS_DEMO
;
; PURPOSE:
;       Demonstrate the basic capabilities of procedures WCSSPH2XY & WCSXY2SPH
;
; CATEGORY:
;       Mapping and Auxilary FITS Demo Routine
;
; CALLING SEQUENCE:
;
;       .run wcs_demo: compiles wcs_demo and the supporting demo routines
;       wcs_demo: run the demo
;
; INPUT PARAMETERS:
;
;       none
;
; OUTPUT PARAMETERS:
;       none
;
; PROCEDURE:
;
;       This is a demo program which is meant to call the routines 
;       wcssph2xy.pro and wcsxy2sph.pro.  Since the purpose of this
;       routine is both to show what the routines can do and what the
;       user has to do, a file is created with all of the commands 
;       needed to complete the desired operation.  Wcs_demo actually 
;       executes this command file, so the user can exactly duplicate
;       the results by simply re-executing this file.  Also, this 
;       allows a user to edit an already existing file which calls 
;       wcssph2xy.pro and wcsxy2sph.pro properly and extend the file's
;       usefulness.  This demo program allows several possible tests.
;       The first option is to simply draw a grid of evenly spaced
;       latitude and longitude lines in a particular map transformation.
;       Another possibility is to do a full loop, creating a Cartesian
;       grid of latitude and longitude lines and calling wcssph2xy.pro
;       to convert them to a particular map.  Then, wcsxy2sph.pro is
;       called to invert the process and the difference between the
;       original and final latitudes and longitudes can be plotted.
;       This allows one to assess the level of the numerical errors
;       introduced by the mapping routines.  A third possible option is to
;       look at some of the map transformations and include rotations of
;       the reference points so that a different perspective is given.
;
; COMMON BLOCKS:
;       none
;
; PROCEDURES CALLED:
;       SPHDIST(), WCSXY2SPH, WCSSPH2XY
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
; AUTHOR:
;
;       Rick Balsano
;
; MODIFICATIONS/REVISION LEVEL:
;
;       1.1     8/31/93
;       1.2     3/19/96 - J. Bloch - LANL
;                        - Made compatible with wcslib-2.2 by Calabretta.
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Updated for conical projections W. Landsman  July 2003
;-

; PROCEDURE FOR OPTION 1
pro wcssph2xy_plot,file_unit,map,param1,param2
printf,file_unit,";PLOTTING"
printf,file_unit,"; Plot the resulting map."
if ((map ge 0) and (map le 22)) then begin
  ; For all but the spherical cube projections, simply plot the results from
  ; wcssph2xy.pro as is.
  printf,file_unit,"xdelta = (max(xx) - min(xx))/20"
  printf,file_unit,"ydelta = (max(y) - min(y))/20"
  printf,file_unit,$
         "plot,xx,y,psym = 3,xrange = [min(xx) - xdelta,max(xx) + xdelta],$"
  printf,file_unit,$
         "yrange = [min(y) - ydelta,max(y) + ydelta],xstyle = 4,ystyle = 4"

  ; ZENITHAL PROJECTIONS.
  if ((map ge 1) and (map le 8)) then begin

    printf,file_unit,""
    printf,file_unit,$
    "; Only connect latitude lines in a full circle if the longitude"
    printf,file_unit,"; values cover the full circle."
    printf,file_unit,$
      "if (360 - abs(longitude(0,0) - longitude(n_elements(xx[*,0])-1)) $"
    printf,file_unit,"                             le lon_spacing) $"
    printf,file_unit,$
      "then for i = 0,num_lat - 1 do oplot,[xx[*,i],xx(0,i)],[y[*,i],y(0,i)] $"
    printf,file_unit,"else for i = 0,num_lat - 1 do oplot,xx[*,i],y[*,i]"

    printf,file_unit,""
    printf,file_unit,$
    "; Connect the longitude lines from the poles outward."
    printf,file_unit,"for i = 0,num_lon - 1 do oplot,xx[i,*],y[i,*]"

    printf,file_unit,""
    printf,file_unit,";LABELS"
    printf,file_unit,$
    "; Label the latitude and longitude lines and correctly orient the labels."
    printf,file_unit,"j = 0"
    printf,file_unit,"repeat begin"
    printf,file_unit,"  i = lon_index(j)"
    printf,file_unit,"  xyouts,xx(i,0)-xdelta*sin(longitude(i,0)/!radeg),$"
    printf,file_unit,"         y(i,0)-ydelta*cos(longitude(i,0)/!radeg),$"
    printf,file_unit,$
          "         strcompress(string(long(longitude(i,0)))),alignment=0.5,$"
    printf,file_unit,"         orientation=360-longitude(i,0)"
    printf,file_unit,"  j = j + 1"
    printf,file_unit,"endrep until (j eq n_elements(lon_index))"
    printf,file_unit,"if (lat_index[0] ne -1) then $"
    printf,file_unit,"  xyouts,xx(0,lat_index),y(0,lat_index),$"
    printf,file_unit,"       strcompress(string(long(latitude(0,lat_index))))"

  ; CYLINDRICAL PROJECTIONS
  endif else if (((map ge 9) and (map le 12)) or (map eq 0)) then begin
    printf,file_unit,""
    printf,file_unit,"; Draw lines connecting equal longitudes"
    printf,file_unit,"for i = 0,num_lon - 1 do oplot,xx[i,*],y[i,*]"
    printf,file_unit,"; Draw lines connecting equal latitudes"
    printf,file_unit,$
    "if ((min(longitude[*,0]) ge 180) or (max(longitude[*,0]) lt 180)) then $"
    printf,file_unit,"  for i = 0,num_lat - 1 do oplot,xx[*,i],y[*,i] $"
    printf,file_unit,"else begin"
    printf,file_unit,"  index = where(longitude[*,0] ge 180)"
    printf,file_unit,$
    "  if ((360 - max(longitude[*,0]) + min(longitude[*,0])) le lon_spacing) $"
    printf,file_unit,"    then begin"
    printf,file_unit,$
    "    for i = 0, num_lat - 1 do oplot,[xx(index,i),xx(0:index[0]-1,i)],$"
    printf,file_unit,$
    "                                    [y(index,i),y(0:index[0]-1,i)]"
    printf,file_unit,"  endif else begin"
    printf,file_unit,"    for i = 0,num_lat - 1 do begin"
    printf,file_unit,"      oplot,xx(0:index[0] - 1,i),y(0:index[0] - 1,i)"
    printf,file_unit,"      oplot,xx(index,i),y(index,i)"
    printf,file_unit,"    endfor"
    printf,file_unit,"  endelse"
    printf,file_unit,"endelse"

    printf,file_unit,""
    printf,file_unit,";LABELS"
    printf,file_unit,$
    "; Label the latitude and longitude lines and correctly orient the labels."
    printf,file_unit,$
    "xyouts,xx(lon_index,0),y(lon_index,0) - ydelta,orientation=90,$"
    printf,file_unit,$
    "       strcompress(string(long(longitude(lon_index,0)))),alignment=0.5"
    printf,file_unit,"y_index = where(longitude[0,*] eq max(longitude[0,*]))"
    printf,file_unit,"if (lat_index[0] ne -1) then $"
    printf,file_unit,$
    "xyouts,max(xx) + xdelta,y(y_index[0],lat_index),alignment=0.5,$"
    printf,file_unit,"       strcompress(string(long(latitude(0,lat_index))))"

  ; CONICAL PROJECTIONS
  endif else if ((map ge 13) and (map le 16)) then begin
    printf,file_unit,""
    printf,file_unit,"; Draw lines of longitude out from the poles."
    printf,file_unit,"for i = 0,num_lon - 1 do oplot,xx[i,*],y[i,*]"

    printf,file_unit,$
    "; Draw lines of latitude, making sure to break the line at 180 degrees."
    printf,file_unit,"index = where(longitude[*,0] ge 180)"
    printf,file_unit,"if (index[0] ne -1) then $"
    printf,file_unit,$
          "  for i = 0,num_lat - 1 do oplot,[xx(index,i),xx(0:index[0]-1,i)],$"
    printf,file_unit,"                        [y(index,i),y(0:index[0]-1,i)] $"
    printf,file_unit,"else begin"
    printf,file_unit,"  index = where(longitude[*,0] eq max(longitude[*,0]))"
    printf,file_unit,$
    "  for i = 0,num_lat - 1 do oplot,xx(0:index[0],i),y(0:index[0],i)"
    printf,file_unit,"endelse"

    printf,file_unit,""
    printf,file_unit,";LABELS"
    printf,file_unit,$
    "; Label latitude and longitude and correctly orient the labels."
    printf,file_unit,"j = 0"
    printf,file_unit,"if (min(longitude) lt 180) then begin"
    printf,file_unit,$
    "  lon_ind_1 = lon_index(where(longitude(lon_index,0) lt 180))"
    printf,file_unit,$
    "  lon_ind_1 = lon_ind_1(reverse(sort(longitude(lon_ind_1,0))))"
    printf,file_unit,"endif"
    printf,file_unit,"if (max(longitude) ge 180) then begin"
    printf,file_unit,$
    "  lon_ind_2 = lon_index(where(longitude(lon_index,0) ge 180))"
    printf,file_unit,$
    "  lon_ind_2 = lon_ind_2(reverse(sort(longitude(lon_ind_2,0))))"
    printf,file_unit,"endif"
    printf,file_unit,$
    "if ((n_elements(lon_ind_1) ne 0) and (n_elements(lon_ind_2) ne 0)) then $"
    printf,file_unit,"  lon_index = [lon_ind_1,lon_ind_2] $"
    printf,file_unit,"else if (n_elements(lon_ind_1) ne 0) then $"
    printf,file_unit,"  lon_index = lon_ind_1 $"
    printf,file_unit,"else if (n_elements(lon_ind_2) ne 0) then $"
    printf,file_unit,"  lon_index = lon_ind_2"
    if (param2 gt -param1) then begin
    printf,file_unit,"repeat begin"
    printf,file_unit,"  i = lon_index(j)"
    printf,file_unit,"  i1 = lon_index(j + 1)"
    printf,file_unit,"  angle = atan(y(i1,0) - y(i,0),xx(i1,0) - xx(i,0))"
    printf,file_unit,$
    "  xyouts,xx(i,0) + xdelta*sin(angle),y(i,0) - ydelta*cos(angle),$"
    printf,file_unit,$
    "         strcompress(string(long(longitude(i,0)))),alignment = 0.5,$"
    printf,file_unit,"         orientation = !radeg*angle"
    printf,file_unit,"  j = j + 1"
    printf,file_unit,"endrep until (j eq (n_elements(lon_index) - 1))"
    endif else begin
    printf,file_unit,"end_index = n_elements(xx[i,*]) - 1"
    printf,file_unit,"repeat begin"
    printf,file_unit,"  i = lon_index(j)"
    printf,file_unit,"  i1 = lon_index(j + 1)"
    printf,file_unit,"  angle = atan(y(i1,end_index) - y(i,end_index),$"
    printf,file_unit,"               xx(i1,end_index) - xx(i,end_index))"
    printf,file_unit,$
    "  xyouts,xx(i,end_index) - xdelta*sin(angle),y(i,end_index) + $"
    printf,file_unit,$
    "         ydelta*cos(angle),strcompress(string(long(longitude($"
    printf,file_unit,"i,end_index)))),alignment=0.5,orientation=!radeg*angle"
    printf,file_unit,"  j = j + 1"
    printf,file_unit,"endrep until (j eq n_elements(lon_index) - 1)"
    endelse
    printf,file_unit,$
    "if (lat_index[0] ne -1) then xyouts,xx(0,lat_index),y(0,lat_index),$"
    printf,file_unit,$
    "                        strcompress(string(long(latitude(0,lat_index))))"

  ; CONVENTIONAL PROJECTIONS
  endif else if ((map ge 17) and (map le 22)) then begin
    printf,file_unit,""
    printf,file_unit,"; Draw lines of longitude"
    printf,file_unit,"for i = 0,num_lon - 1 do oplot,xx[i,*],y[i,*]"

    printf,file_unit,$
    "; Draw lines of latitude, breaking the line at 180 degrees."
    printf,file_unit,$
    "if ((min(longitude[*,0]) ge 180) or (max(longitude[*,0]) lt 180)) then $"
    printf,file_unit,"  for i = 0,num_lat - 1 do oplot,xx[*,i],y[*,i] $"
    printf,file_unit,"else begin"
    printf,file_unit,"  index = where(longitude[*,0] ge 180)"
    printf,file_unit,$
    "  if ((360 - max(longitude[*,0]) + min(longitude[*,0])) le lon_spacing) $"
    printf,file_unit,"    then begin"
    printf,file_unit,$
    "    for i = 0, num_lat - 1 do oplot,[xx(index,i),xx(0:index[0]-1,i)],$"
    printf,file_unit,$
    "                                    [y(index,i),y(0:index[0]-1,i)]"
    printf,file_unit,"  endif else begin"
    printf,file_unit,"    for i = 0,num_lat - 1 do begin"
    printf,file_unit,"      oplot,xx(0:index[0] - 1,i),y(0:index[0] - 1,i)"
    printf,file_unit,"      oplot,xx(index,i),y(index,i)"
    printf,file_unit,"    endfor"
    printf,file_unit,"  endelse"
    printf,file_unit,"endelse"

    printf,file_unit,""
    printf,file_unit,";LABELS"
    printf,file_unit,$
    "; Label latitude and longitude lines and orient the labels correctly."
    printf,file_unit,"if (lat_index[0] ne -1) then $"
    printf,file_unit,"xyouts,xx(0,lat_index),y(0,lat_index),$"
    printf,file_unit,"       strcompress(string(long(latitude(0,lat_index))))"
    printf,file_unit,$
    "index = where(abs(latitude[0,*]) eq min(abs(latitude[0,*])))"
    printf,file_unit,$
    "xyouts,xx(lon_index,index[0]),y(lon_index,index[0]),orientation=90,$"
    printf,file_unit,$
"       strcompress(string(long(longitude(lon_index,index[0])))),alignment=0.5"
  endif

; SPHERICAL CUBE PROJECTIONS
endif else begin
  printf,file_unit,"xx = -x"
  printf,file_unit,"yy = y"

  printf,file_unit,""
  printf,file_unit,"; Make arrays with the locations of all points."
  printf,file_unit,"face_0 = where(face eq 0)"
  printf,file_unit,"face_1 = where(face eq 1)"
  printf,file_unit,"face_2 = where(face eq 2)"
  printf,file_unit,"face_3 = where(face eq 3)"
  printf,file_unit,"face_4 = where(face eq 4)"
  printf,file_unit,"face_5 = where(face eq 5)"

  printf,file_unit,""
  printf,file_unit,"; Define the size of the box around each face."
  printf,file_unit,"x_len = 2*45.0"
  printf,file_unit,"y_len = 2*45.0"

  printf,file_unit,""
  printf,file_unit,$
  "; Correctly adjust the x and y values for display purposes (they all start "
  printf,file_unit,$
  "; out on the same face)."
  printf,file_unit,"if (face_0[0] ne -1) then begin"
  printf,file_unit,"  x0 = -x(face_0) + 2.d0*x_len"
  printf,file_unit,"  y0 = y(face_0) + y_len"
  printf,file_unit,"  xx(face_0) = x0"
  printf,file_unit,"  yy(face_0) = y0"
  printf,file_unit,"endif"
  printf,file_unit,"if (face_1[0] ne -1) then begin"
  printf,file_unit,"  x1 = -x(face_1) + 2.d0*x_len"
  printf,file_unit,"  y1 = y(face_1)"
  printf,file_unit,"  xx(face_1) = x1"
  printf,file_unit,"  yy(face_1) = y1"
  printf,file_unit,"endif"
  printf,file_unit,"if (face_2[0] ne -1) then begin"
  printf,file_unit,"  x2 = -x(face_2) + x_len"
  printf,file_unit,"  y2 = y(face_2)"
  printf,file_unit,"  xx(face_2) = x2"
  printf,file_unit,"  yy(face_2) = y2"
  printf,file_unit,"endif"
  printf,file_unit,"if (face_3[0] ne -1) then begin"
  printf,file_unit,"  x3 = -x(face_3)"
  printf,file_unit,"  y3 = y(face_3)"
  printf,file_unit,"  xx(face_3) = x3"
  printf,file_unit,"  yy(face_3) = y3"
  printf,file_unit,"endif"
  printf,file_unit,"if (face_4[0] ne -1) then begin"
  printf,file_unit,"  x4 = -x(face_4) - x_len"
  printf,file_unit,"  y4 = y(face_4)"
  printf,file_unit,"  xx(face_4) = x4"
  printf,file_unit,"  yy(face_4) = y4"
  printf,file_unit,"endif"
  printf,file_unit,"if (face_5[0] ne -1) then begin"
  printf,file_unit,"  x5 = -x(face_5) + 2.d0*x_len"
  printf,file_unit,"  y5 = y(face_5) - y_len"
  printf,file_unit,"  xx(face_5) = x5"
  printf,file_unit,"  yy(face_5) = y5"
  printf,file_unit,"endif"

  printf,file_unit,""
  printf,file_unit,$
  "; Define plot ranges by finding which faces are actually used."
  printf,file_unit,"if (face_4[0] ne -1) then x_low = -3*x_len/2 $"
  printf,file_unit,"else if (face_3[0] ne -1) then x_low = -x_len/2 $"
  printf,file_unit,"else if (face_2[0] ne -1) then x_low = x_len/2 $"
  printf,file_unit,$
  "else if ((face_1[0] ne -1) or (face_0[0] ne -1) or (face_5[0] ne -1)) $"
  printf,file_unit,"then x_low = 3*x_len/2"
  printf,file_unit,$
  "if ((face_1[0] ne -1) or (face_5[0] ne -1) or (face_0[0] ne -1)) $"
  printf,file_unit,"  then x_high = 5*x_len/2 $"
  printf,file_unit,"else if (face_2[0] ne -1) then x_high = 3*x_len/2 $"
  printf,file_unit,"else if (face_3[0] ne -1) then x_high = x_len/2 $"
  printf,file_unit,"else if (face_4[0] ne -1) then x_high = -x_len/2"
  printf,file_unit,"if (face_5[0] ne -1) then y_low = -3*y_len/2 $"
  printf,file_unit,$
  "else if ((face_1[0] ne -1) or (face_3[0] ne -1) or (face_2[0] ne -1) or $"
  printf,file_unit,"   (face_4[0] ne -1)) then y_low = -y_len/2 $"
  printf,file_unit,"else if (face_0[0] ne -1) then y_low = y_len/2"
  printf,file_unit,"if (face_0[0] ne -1) then y_high = 3*y_len/2 $"
  printf,file_unit,$
  "else if ((face_1[0] ne -1) or (face_3[0] ne -1) or (face_2[0] ne -1) or $"
  printf,file_unit,"   (face_4[0] ne -1)) then y_high = y_len/2 $"
  printf,file_unit,"else if (face_5[0] ne -1) then y_high = -y_len/2"

  printf,file_unit,""
  printf,file_unit,"; Plot the points calculated by wcssph2xy."
  printf,file_unit,$
  "plot,xx,yy,psym=3,xrange=[x_low,x_high],yrange=[y_low,y_high],xstyle=4,$"
  printf,file_unit,"     ystyle=4"

  printf,file_unit,""
  printf,file_unit,$
  "; Set-up an array with the correct ordering of indices to connect the"
  printf,file_unit,"; latitude lines correctly on faces 1-4."
  printf,file_unit,"face_ind = intarr(1)"
  printf,file_unit,"if (face_4[0] ne -1) then face_ind = [face_ind,face_4]"
  printf,file_unit,"if (face_3[0] ne -1) then face_ind = [face_ind,face_3]"
  printf,file_unit,"if (face_2[0] ne -1) then face_ind = [face_ind,face_2]"
  printf,file_unit,"if (face_1[0] ne -1) then face_ind = [face_ind,face_1]"
  printf,file_unit,"; Draw the latitude lines on faces 1-4"
  printf,file_unit,"if (n_elements(face_ind) gt 1) then begin"
  printf,file_unit,"  face_ind = face_ind(1:*)"
  printf,file_unit,"  xxx = xx(face_ind)"
  printf,file_unit,"  yyy = yy(face_ind)"
  printf,file_unit,"  for i = 0,num_lat - 1 do begin"
  printf,file_unit,"    index = where(latitude(face_ind) eq latitude(0,i))"
  printf,file_unit,"    if (index[0] ne -1) then begin"
  printf,file_unit,"      tempx = xxx(index)"
  printf,file_unit,"      tempy = yyy(index)"
  printf,file_unit,"      index = sort(tempx)"
  printf,file_unit,$
  "         if (((360 - abs(longitude(0,0) - longitude(num_lon - 1,0))) le $"
  printf,file_unit,$
  "          lon_spacing) or (max(longitude(index)) le 135) or $"
  printf,file_unit,$
"       (min(longitude(index)) gt 135)) then oplot,tempx(index),tempy(index) $"
  printf,file_unit,"        else begin"
  printf,file_unit,"           lon_ind = 0"
  printf,file_unit,$
 "           repeat lon_ind=lon_ind+1 until (longitude(index(lon_ind)) gt 135)"
  printf,file_unit,"          index_1 = index(0:lon_ind - 1)"
  printf,file_unit,"          index_2 = index(lon_ind:*)
  printf,file_unit,"          oplot,tempx(index_1),tempy(index_1)"
  printf,file_unit,"          oplot,tempx(index_2),tempy(index_2)"
  printf,file_unit,"        endelse"
  printf,file_unit,"      endif"
  printf,file_unit,"    endfor"
  printf,file_unit,"  endif"
  printf,file_unit,""
  printf,file_unit,"; Draw latitude lines on faces 0 and 5"
  printf,file_unit,"  for i = 0,num_lat - 1 do begin"
  printf,file_unit,"    if (face_0[0] ne -1) then begin"
  printf,file_unit,"      index = where(latitude(face_0) eq latitude(0,i))"
  printf,file_unit,"      if (index[0] ne -1) then begin"
  printf,file_unit,$
  "        if ((360 - abs(longitude(0,0) - longitude(n_elements(x) - 1))) $"
  printf,file_unit,"                                    le lon_spacing) then $"
  printf,file_unit,$
  "          oplot,[x0(index),x0(index[0])],[y0(index),y0(index[0])] $"
  printf,file_unit,"        else oplot,x0(index),y0(index)"
  printf,file_unit,"      endif"
  printf,file_unit,"    endif"
  printf,file_unit,"    if (face_5[0] ne -1) then begin"
  printf,file_unit,"      index = where(latitude(face_5) eq latitude(0,i))"
  printf,file_unit,"      if (index[0] ne -1) then begin"
  printf,file_unit,$
  "        if ((360 - abs(longitude(0,0) - longitude(n_elements(x) - 1))) $"
  printf,file_unit,"                                    le lon_spacing) then $"
  printf,file_unit,$
  "          oplot,[x5(index),x5(index[0])],[y5(index),y5(index[0])] $"
  printf,file_unit,"        else oplot,x5(index),y5(index)" 
  printf,file_unit,"      endif"
  printf,file_unit,"    endif"
  printf,file_unit,"  endfor"
  printf,file_unit,""
  printf,file_unit,"; Draw boxes around each face and draw longitude lines"
  printf,file_unit,"  for i = 0,num_lon - 1 do begin"
  printf,file_unit,"    if (face_4[0] ne -1) then begin"
  printf,file_unit,"      index = where(longitude(face_4) eq longitude(i,0))"
  printf,file_unit,"      if (index[0] ne -1) then oplot,x4(index),y4(index)"
  printf,file_unit,"      plots,[-3*x_len/2,-x_len/2],[-y_len/2,-y_len/2]"
  printf,file_unit,"      plots,[-3*x_len/2,-x_len/2],[y_len/2,y_len/2]"
  printf,file_unit,"      plots,[-x_len/2,-x_len/2],[-y_len/2,y_len/2]"
  printf,file_unit,"      plots,[-3*x_len/2,-3*x_len/2],[-y_len/2,y_len/2]"
  printf,file_unit,"    endif"
  printf,file_unit,"    if (face_2[0] ne -1) then begin"
  printf,file_unit,"      index = where(longitude(face_2) eq longitude(i,0))"
  printf,file_unit,"      if (index[0] ne -1) then oplot,x2(index),y2(index)"
  printf,file_unit,"      plots,[x_len/2,3*x_len/2],[-y_len/2,-y_len/2]"
  printf,file_unit,"      plots,[x_len/2,3*x_len/2],[y_len/2,y_len/2]"
  printf,file_unit,"      plots,[x_len/2,x_len/2],[-y_len/2,y_len/2]"
  printf,file_unit,"      plots,[3*x_len/2,3*x_len/2],[-y_len/2,y_len/2]"
  printf,file_unit,"    endif"
  printf,file_unit,"    if (face_3[0] ne -1) then begin"
  printf,file_unit,"      index = where(longitude(face_3) eq longitude(i,0))"
  printf,file_unit,"      if (index[0] ne -1) then oplot,x3(index),y3(index)"
  printf,file_unit,"      plots,[-x_len/2,x_len/2],[-y_len/2,-y_len/2]"
  printf,file_unit,"      plots,[-x_len/2,x_len/2],[y_len/2,y_len/2]"
  printf,file_unit,"      plots,[-x_len/2,-x_len/2],[-y_len/2,y_len/2]"
  printf,file_unit,"      plots,[x_len/2,x_len/2],[-y_len/2,y_len/2]"
  printf,file_unit,"    endif"
  printf,file_unit,"    if (face_1[0] ne -1) then begin"
  printf,file_unit,"      index = where(longitude(face_1) eq longitude(i,0))"
  printf,file_unit,"      if (index[0] ne -1) then oplot,x1(index),y1(index)"
  printf,file_unit,"      plots,[3*x_len/2,5*x_len/2],[-y_len/2,-y_len/2]"
  printf,file_unit,"      plots,[3*x_len/2,5*x_len/2],[y_len/2,y_len/2]"
  printf,file_unit,"      plots,[3*x_len/2,3*x_len/2],[-y_len/2,y_len/2]"
  printf,file_unit,"      plots,[5*x_len/2,5*x_len/2],[-y_len/2,y_len/2]"
  printf,file_unit,"    endif"
  printf,file_unit,"    if (face_0[0] ne -1) then begin"
  printf,file_unit,"      index = where(longitude(face_0) eq longitude(i,0))"
  printf,file_unit,"      if (index[0] ne -1) then oplot,x0(index),y0(index)"
  printf,file_unit,"      plots,[3*x_len/2,5*x_len/2],[y_len/2,y_len/2]"
  printf,file_unit,"      plots,[3*x_len/2,5*x_len/2],[3*y_len/2,3*y_len/2]"
  printf,file_unit,"      plots,[3*x_len/2,3*x_len/2],[y_len/2,3*y_len/2]"
  printf,file_unit,"      plots,[5*x_len/2,5*x_len/2],[y_len/2,3*y_len/2]"
  printf,file_unit,"    endif"
  printf,file_unit,"    if (face_5[0] ne -1) then begin"
  printf,file_unit,"      index = where(longitude(face_5) eq longitude(i,0))"
  printf,file_unit,"      if (index[0] ne -1) then oplot,x5(index),y5(index)"
  printf,file_unit,"      plots,[3*x_len/2,5*x_len/2],[-3*y_len/2,-3*y_len/2]"
  printf,file_unit,"      plots,[3*x_len/2,5*x_len/2],[-y_len/2,-y_len/2]"
  printf,file_unit,"      plots,[3*x_len/2,3*x_len/2],[-3*y_len/2,-y_len/2]"
  printf,file_unit,"      plots,[5*x_len/2,5*x_len/2],[-3*y_len/2,-y_len/2]"
  printf,file_unit,"    endif"
  printf,file_unit,"  endfor"
  printf,file_unit,""
  printf,file_unit,";LABELS"
  printf,file_unit,"  if (lat_index[0] ne -1) then $"
  printf,file_unit,"  xyouts,xx(0,lat_index),yy(0,lat_index),$"
  printf,file_unit,"         strcompress(string(long(latitude(0,lat_index))))"
  printf,file_unit,$
  "  index = where(abs(latitude[0,*]) eq min(abs(latitude[0,*])))"
  printf,file_unit,$
  "  xyouts,xx(lon_index,index[0]),yy(lon_index,index[0]),orientation=90,$"
  printf,file_unit,$
"       strcompress(string(long(longitude(lon_index,index[0])))),alignment=0.5"
endelse
end

; PROCEDURE FOR OPTION 2
pro inversion_error,file_unit,map,param1,param2
printf,file_unit,";CONVERSION"
printf,file_unit,$
"; Convert the x-y coordinates into spherical coordinates by using wcsxy2sph."
if (map lt 23) then begin
  if (n_elements(param1) eq 0) then begin
    printf,file_unit,"wcsxy2sph,x,y,longitude_inv,latitude_inv,map"
  endif else if (n_elements(param2) eq 0) then begin
  printf,file_unit,"wcsxy2sph,x,y,longitude_inv,latitude_inv,map,pv2=param1"
  endif else begin
    printf,file_unit,$
     "wcsxy2sph,x,y,longitude_inv,latitude_inv,map,pv2= [param1, param2] "
  endelse
endif else begin
  printf,file_unit,$
 "; The variable face must be declared with the same structure as latitude and"
  printf,file_unit,"; longitude before calling wcsxy2sph." 
  printf,file_unit,"wcsxy2sph,x,y,longitude_inv,latitude_inv,map,face=face"
endelse

printf,file_unit,""
printf,file_unit,";PLOTTING"
printf,file_unit,"; Plot the resulting map."
printf,file_unit,"lon_delta = (max(longitude_inv) - min(longitude_inv))/20"
printf,file_unit,"lat_delta = (max(latitude_inv) - min(latitude_inv))/20"
printf,file_unit,$
    "plot,longitude_inv,latitude_inv,psym = 3,xrange = [min(longitude_inv) - $"
printf,file_unit,$
"    lon_delta,max(longitude_inv) + lon_delta],yrange = [min(latitude_inv) - $"
printf,file_unit,$
"    lat_delta,max(latitude_inv) + lat_delta],xstyle = 4,ystyle = 4"
printf,file_unit,"; Draw lines connecting equal longitudes"
printf,file_unit,$
       "for i = 0,num_lon - 1 do oplot,longitude_inv[i,*],latitude_inv[i,*]"
printf,file_unit,"; Draw lines connecting equal latitudes"
printf,file_unit,$
"if ((min(longitude[*,0]) ge 180) or (max(longitude[*,0]) lt 180)) then $"
printf,file_unit,$
      "  for i = 0,num_lat - 1 do oplot,longitude_inv[*,i],latitude_inv[*,i] $"
printf,file_unit,"else begin"
printf,file_unit,"  index = where(longitude[*,0] ge 180)"
printf,file_unit,$
"  if ((360 - max(longitude[*,0]) + min(longitude[*,0])) le lon_spacing) $"
printf,file_unit,"    then begin"
printf,file_unit,$
       "    for i = 0, num_lat - 1 do oplot,[longitude_inv(index,i),$"
printf,file_unit,$
       "      longitude_inv(0:index[0]-1,i)],[latitude_inv(index,i),$"
printf,file_unit,"      latitude_inv(0:index[0]-1,i)]"
printf,file_unit,"  endif else begin"
printf,file_unit,"    for i = 0,num_lat - 1 do begin"
printf,file_unit,$
   "      oplot,longitude_inv(0:index[0] - 1,i),latitude_inv(0:index[0] - 1,i)"
printf,file_unit,"      oplot,longitude_inv(index,i),latitude_inv(index,i)"
printf,file_unit,"    endfor"
printf,file_unit,"  endelse"
printf,file_unit,"endelse"

printf,file_unit,""
printf,file_unit,";LABELS"
printf,file_unit,$
"; Label the latitude and longitude lines and correctly orient the labels."
printf,file_unit,$
    "xyouts,longitude_inv(lon_index,0),latitude_inv(lon_index,0) - lat_delta,$"
printf,file_unit,$
    "       orientation=90,strcompress(string(long(longitude(lon_index,0)))),$"
printf,file_unit,"       alignment=0.5"
printf,file_unit,"lat1_index = where(longitude[0,*] eq max(longitude[0,*]))"
printf,file_unit,"if (lat_index[0] ne -1) then $"
printf,file_unit,$
"xyouts,max(longitude_inv) + lon_delta,latitude_inv(lat1_index[0],lat_index),$"
printf,file_unit,$
"       alignment=0.5,strcompress(string(long(latitude(0,lat_index))))"

printf,file_unit,"read,'Press return to continue',key"
print,"  In order to make the scripts wcssph2xy.pro and wcsxy2sph.pro"
print,"invertible and minimize the error in the process, it was necessary to"
print,"offset the latitude of all points at the poles by a small amount."
print,"When viewing the difference between the original longitude and"
print,"latitude and the longitude and latitude after points are run through"
print,"wcssph2xy.pro and wcsxy2sph.pro, the offset at the poles will show up"
print,"as vertical lines.  This overshadows any numerical error elsewhere"
print,"by orders of magnitude.  The default is to ignore these errors, but"
print,"to include them, enter n at the prompt"
print,""
key = ""
repeat $
  read,"Ignore offset at poles when plotting vector field (y or n):",key $
until ((key eq "y") or (key eq "n")) 

if (key eq "y") then begin
  printf,file_unit,"poles = where(abs(abs(latitude_inv) - 9.d1) le 573.d-4)"
  printf,file_unit,"if (poles[0] ne -1) then $"
  printf,file_unit,$
    "  latitude_inv(poles) = latitude_inv(poles)/abs(latitude_inv(poles))*9.d1"
endif

printf,file_unit, $
    "dist = sphdist(longitude,latitude,longitude_inv,latitude_inv,/degrees)"
printf,file_unit,"erase"
printf,file_unit,$
"print,'The largest arrow on the plot will represent a difference of '"
printf,file_unit,"print,max(dist),' degrees.'"
printf,file_unit,"read,'Press return to continue',key"
printf,file_unit,$
       "norm = sqrt((longitude-longitude_inv)^2 + (latitude-latitude_inv)^2)"
printf,file_unit,"lon_diff=dist*(longitude-longitude_inv)"
printf,file_unit,"good = where(norm ne 0.d0)"
printf,file_unit,"lon_diff(good) = lon_diff(good)/norm(good)"
printf,file_unit,"lat_diff = dist*(latitude-latitude_inv)"
printf,file_unit,"lat_diff(good) = lat_diff(good)/norm(good)"
printf,file_unit,"velovect,lon_diff,lat_diff,longitude[*,0],latitude[0,*]"
end

; PROCEDURE FOR OPTION 3
pro wcs_rot,file_unit,map,param1,param2
printf,file_unit,";PLOTTING"
printf,file_unit,"; Plot the resulting map."
if ((map ge 0) and (map le 22)) then begin
  ; For all but the spherical cube projections, simply plot the results from
  ; wcssph2xy.pro as is.
  printf,file_unit,"xdelta = (max(xx) - min(xx))/20"
  printf,file_unit,"ydelta = (max(y) - min(y))/20"
  printf,file_unit,$
         "plot,xx,y,psym = 3,xrange = [min(xx) - xdelta,max(xx) + xdelta],$"
  printf,file_unit,$
         "yrange = [min(y) - ydelta,max(y) + ydelta],xstyle = 4,ystyle = 4"
  printf,file_unit,"zero_ind = where(latitude[0,*] eq min(abs(latitude[0,*])))"
  printf,file_unit,$
  "xyouts,xx(lon_index,zero_ind[0]),y(lon_index,zero_ind[0]),$"
  printf,file_unit,$
  "       strcompress(string(long(longitude(lon_index,zero_ind[0])))),$"
  printf,file_unit,"       alignment = 0.5"
  printf,file_unit,$
  "zero_ind2 = where(longitude[*,0] eq min(abs(longitude[*,0])))"
  printf,file_unit,$
  "xyouts,xx(zero_ind2[0],lat_index),y(zero_ind2[0],lat_index),$"
  printf,file_unit,$
  "       strcompress(string(long(latitude(zero_ind2[0],lat_index)))),$"
  printf,file_unit,"       alignment = 0.5"
  printf,file_unit,$
  "non_zero_ind = where(longitude[*,0] ne min(abs(longitude[*,0])))
  printf,file_unit,$
  "for i = 0,zero_ind[0] - 1 do $"
  printf,file_unit,$
  "    oplot,xx(non_zero_ind,i),y(non_zero_ind,i),psym=4"
  printf,file_unit,$
  "for i = zero_ind[0] + 1,n_elements(longitude[0,*]) - 1 do $"
  printf,file_unit,"   oplot,xx(non_zero_ind,i),y(non_zero_ind,i),psym=4"
endif else begin
  printf,file_unit,"xx = -x"
  printf,file_unit,"yy = y"

  printf,file_unit,""
  printf,file_unit,"; Make arrays with the locations of all points."
  printf,file_unit,"face_0 = where(face eq 0)"
  printf,file_unit,"face_1 = where(face eq 1)"
  printf,file_unit,"face_2 = where(face eq 2)"
  printf,file_unit,"face_3 = where(face eq 3)"
  printf,file_unit,"face_4 = where(face eq 4)"
  printf,file_unit,"face_5 = where(face eq 5)"

  printf,file_unit,""
  printf,file_unit,"; Define the size of the box around each face."
  if (map eq 23) then begin
    printf,file_unit,"x_len = 90"
    printf,file_unit,"y_len = 90"
  endif else begin
    printf,file_unit,"x_len = 2*!radeg"
    printf,file_unit,"y_len = 2*!radeg"
  endelse

  printf,file_unit,""
  printf,file_unit,$
  "; Correctly adjust the x and y values for display purposes (they all start "
  printf,file_unit,$
  "; out on the same face)."
  printf,file_unit,"if (face_0[0] ne -1) then begin"
  printf,file_unit,"  x0 = -x(face_0)"
  printf,file_unit,"  y0 = y(face_0) - y_len"
  printf,file_unit,"  xx(face_0) = x0"
  printf,file_unit,"  yy(face_0) = y0"
  printf,file_unit,"endif"
  printf,file_unit,"if (face_1[0] ne -1) then begin"
  printf,file_unit,"  x1 = -x(face_1) + 2.d0*x_len"
  printf,file_unit,"  y1 = y(face_1)"
  printf,file_unit,"  xx(face_1) = x1"
  printf,file_unit,"  yy(face_1) = y1"
  printf,file_unit,"endif"
  printf,file_unit,"if (face_2[0] ne -1) then begin"
  printf,file_unit,"  x2 = -x(face_2) + x_len"
  printf,file_unit,"  y2 = y(face_2)"
  printf,file_unit,"  xx(face_2) = x2"
  printf,file_unit,"  yy(face_2) = y2"
  printf,file_unit,"endif"
  printf,file_unit,"if (face_3[0] ne -1) then begin"
  printf,file_unit,"  x3 = -x(face_3)"
  printf,file_unit,"  y3 = y(face_3)"
  printf,file_unit,"  xx(face_3) = x3"
  printf,file_unit,"  yy(face_3) = y3"
  printf,file_unit,"endif"
  printf,file_unit,"if (face_4[0] ne -1) then begin"
  printf,file_unit,"  x4 = -x(face_4) - x_len"
  printf,file_unit,"  y4 = y(face_4)"
  printf,file_unit,"  xx(face_4) = x4"
  printf,file_unit,"  yy(face_4) = y4"
  printf,file_unit,"endif"
  printf,file_unit,"if (face_5[0] ne -1) then begin"
  printf,file_unit,"  x5 = -x(face_5)"
  printf,file_unit,"  y5 = y(face_5) - y_len"
  printf,file_unit,"  xx(face_5) = x5"
  printf,file_unit,"  yy(face_5) = y5"
  printf,file_unit,"endif"

  printf,file_unit,""
  printf,file_unit,$
  "; Define plot ranges by finding which faces are actually used."
  printf,file_unit,"if (face_4[0] ne -1) then x_low = -3*x_len/2 $"
  printf,file_unit,"else if (face_3[0] ne -1) then x_low = -x_len/2 $"
  printf,file_unit,"else if (face_2[0] ne -1) then x_low = x_len/2 $"
  printf,file_unit,$
  "else if ((face_1[0] ne -1) or (face_5[0] ne -1) or (face_0[0] ne -1)) $"
  printf,file_unit,"  then x_low = 3*x_len/2"
  printf,file_unit,"if (face_4[0] ne -1) then x_high = -x_len/2 $"
  printf,file_unit,"else if (face_2[0] ne -1) then x_high = 3*x_len/2 $"
  printf,file_unit,"else if (face_3[0] ne -1) then x_high = x_len/2 $"
  printf,file_unit,$
  "else if ((face_1[0] ne -1) or (face_5[0] ne -1) or (face_0[0] ne -1)) $"
  printf,file_unit,"  then x_high = 5*x_len/2"
  printf,file_unit,"if (face_5[0] ne -1) then y_low = -3*y_len/2 $"
  printf,file_unit,$
  "else if ((face_4[0] ne -1) or (face_3[0] ne -1) or (face_2[0] ne -1) or $"
  printf,file_unit,"   (face_1[0] ne -1)) then y_low = -y_len/2 $"
  printf,file_unit,"else if (face_0[0] ne -1) then y_low = y_len/2"
  printf,file_unit,"if (face_0[0] ne -1) then y_high = 3*y_len/2 $"
  printf,file_unit,$
  "else if ((face_1[0] ne -1) or (face_3[0] ne -1) or (face_2[0] ne -1) or $"
  printf,file_unit,"   (face_4[0] ne -1)) then y_high = y_len/2 $"
  printf,file_unit,"else if (face_5[0] ne -1) then y_high = -y_len/2"

  printf,file_unit,""
  printf,file_unit,"; Plot the points calculated by wcssph2xy."
  printf,file_unit,$
  "plot,xx,yy,psym=3,xrange=[x_low,x_high],yrange=[y_low,y_high],xstyle=4,$"
  printf,file_unit,"     ystyle=4"
  printf,file_unit,"zero_ind = where(latitude[0,*] eq min(abs(latitude[0,*])))"
  printf,file_unit,$
  "xyouts,xx(lon_index,zero_ind[0]),yy(lon_index,zero_ind[0]),$"
  printf,file_unit,$
  "       strcompress(string(long(longitude(lon_index,zero_ind[0])))),$"
  printf,file_unit,"       alignment = 0.5"
  printf,file_unit,$
  "zero_ind2 = where(longitude[*,0] eq min(abs(longitude[*,0])))"
  printf,file_unit,$
  "xyouts,xx(zero_ind2[0],lat_index),yy(zero_ind2[0],lat_index),$"
  printf,file_unit,$
  "       strcompress(string(long(latitude(zero_ind2[0],lat_index)))),$"
  printf,file_unit,"       alignment = 0.5"
  printf,file_unit,$
  "non_zero_ind = where(longitude[*,0] ne min(abs(longitude[*,0])))
  printf,file_unit,$
  "for i = 0,zero_ind[0] - 1 do $"
  printf,file_unit,$
  "    oplot,xx(non_zero_ind,i),yy(non_zero_ind,i),psym=4"
  printf,file_unit,$
  "for i = zero_ind[0] + 1,n_elements(longitude[0,*]) - 1 do $"
  printf,file_unit,"   oplot,xx(non_zero_ind,i),yy(non_zero_ind,i),psym=4"
endelse
end

; MAIN DEMO PROGRAM
pro wcs_demo
print,""
print,"This demo program demonstrates the basic usage of the IDL procedures"
print,"wcssph2xy.pro and wcsxy2sph.pro.  You will be prompted for information"
print,"about the type of map projection you would like to try out and what"
print,"portion of the sky you would like to view.  All of the commands"
print,"actually issued to carry out these operations will be recorded in a"
print,"journal file so that the user may later reproduce the results from this"
print,"demo by issuing the commands him/herself.  Enjoy!"
key=''
print,""
repeat read,"Enter 'c' to continue or 'x' to exit:",key $
until ((key eq 'c') or (key eq 'x'))
if (key eq 'x') then stop
print,""

; Major loop of whole program.
repeat begin

print,""
print,"Your options are:"
print,"(1) Convert spherical (sky) coordinates to x and y coordinates"
print,"    (in other words, perform a map projection) and plot the results."
print,"(2) Do (1) without plotting, then perform the inverse operation."
print,"    Plot the results, then plot the difference between the original"
print,"    sky coordinates and the coordinates that have been produced by"
print,"    wcssph2xy and wcsxy2sph."
print,"(3) Do (1) with an added twist, rotating the coordinate system."
print,"(4) Exit"
print,""
repeat read,"Enter a number between 1 and 4:",option $
until ((option ge 1) and (option le 4))
print,""

if (option eq 4) then stop

file_name = ""
repeat begin
  read,"Please enter a name for the journal file:",file_name
  print,""
  suffix = strmid(file_name,strlen(file_name)-4,4)
  if (suffix ne ".pro") then file_name = string(file_name,".pro")
  file_test = file_search(file_name)
  if (file_test[0] ne "") then begin
    print,"The file ",file_name," already exists."
    print,"You can overwrite this file, but if you used this journal name"
    print,"previously in this IDL session, you will not get the desired"
    print,"results.  To avoid any conflicts, either quit and start a new"
    print,"session of IDL using this name (and ignore this message) or give a"
    print,"new name to the journal file.  NOTE:  This is due to IDL's"
    print,"inability to re-compile a procedure except from the interactive"
    print,"mode." 
    print,""
    read,"Type 'y' to overwrite the file:",key
    if (key ne 'y') then file_name = ""
  endif
endrep until (file_name ne "")
openw,file_unit,file_name,/get_lun

printf,file_unit,$
"; This is an IDL procedure created by running the IDL program wcs_demo.pro"
printf,file_unit,$
"; and can be executed from the IDL prompt by typing .run ",file_name,"."
printf,file_unit,$
"; This procedure may be far more complicated than what you need.  In order"
printf,file_unit,$
"; to make it more user-friendly, I have broken up the tasks performed into"
printf,file_unit,"; the following categories:"
printf,file_unit,";   (1) SET-UP -- sections declaring constants"
printf,file_unit,$
";   (2) CONVERSION -- section in which spherical to xy conversion is done"
printf,file_unit,$
";   (3) LABELS -- sections setting up and printing labels on the maps"
printf,file_unit,$
";   (4) PLOTTING -- sections in which data or lines are plotted"
printf,file_unit,$
";To find the appropriate section, simply search for one of these four"
printf,file_unit,";capitalized words."

printf,file_unit,""
printf,file_unit,string("pro ",strmid(file_name,0,strlen(file_name) - 4))

map = 0
print,""
print,"Which map projection would you like to try?  Your options are:"
print,"Number  Description                Number  Description"
print,"------  -------------------------  ------  -------------------------"
print,"   0    Default = Cartesian           1    Zenithal perspective"
print,"   2    Gnomic                        3    Orthographic"
print,"   4    Stereographic                 5    Zenithal Equidistant"
print,"   6    Zenithal polynomial (not implemented)"
print,"   7    Zenithal equal area           8    Airy"
print,"   9    Cylindrical perspective      10    Cartesian"
print,"  11    Mercator                     12    Cylindrical equal area"
print,"  13    Conical perspective          14    Conical equidistant"
print,"  15    Conical equal area           16    Conical orthomorphic"
print,"  17    Bonne's equal area           18    Polyconic"
print,"  19    Sanson-Flamsteed              20    Parabolic"
print,"  21    Hammer-Aitoff                22    Mollweide"
print,"  23    Cobe Quadrilateralized Spherical Cube"
print,"  24    Quadrilateralized Spherical Cube"
print,"  25    Tangential Spherical Cube"
print,""
print,$
"NOTE: This demo program does not support the map types: 1-4,8-9,11,13, or 16 "
print,$
"with coordinate system rotation (option 3 above).  These are allowed by"
print,$
"wcssph2xy.pro and wcsxy2sph.pro, but due to problems with the general case of"
print,$
"latitude and longitude restrictions, these map types were skipped here." 
print,""
repeat read,"Please enter a number from 0 to 25:",map $
until ((map ge 0) and (map le 25))

if (option eq 3) then begin
  if ((map le 4) or (map eq 8) or (map eq 9) or (map eq 11) or (map eq 13) $
      or (map eq 16)) then begin
    close,file_unit
    file_delete, file_name
    message,"The map type selected is not supported with coordinate rotations."
  endif else begin
    print,$
    "The idea behind the rotation of the coordinate systems is to relocate the"
    print,$
  "'special' point of the projection.  For instance, the azimuthal projections"
    print,$
    "project from the north pole.  So, the lines of longitude appear as rays"
    print,$
    "coming from the center of the projection and lines of latitude appear as"
    print,$
    "concentric rings around the center.  By rotating the coordinate system,"
    print,$
    "a different point can play the role of the north pole in this example."
    print,$
    "To perform the rotation, the latitude and longitude of the new 'special'"
    print,$
    "point must be given.  In addition, to specify a full rotation, a third"
    print,$
    "angle must be given.  This angle specifies the longitude of the north"
    print,$
    "pole in the transformed system and has a default of 180 degrees."
    print,""
    read,"Please enter the longitude of the 'special' point:",alpha 
    read,"Please enter the latitude of the 'special' point:",delta
    read,"Please enter the third angle (enter 180 for the default):",longpole
  endelse
endif
  
printf,file_unit,";SET-UP"
printf,file_unit,"; Set-up constants used later in this procedure"
printf,file_unit,"map = ",map
print,""

; Get parameters for map types that require them.
case map of
  1:begin
    read,$
    "AZP: Enter distance of source to projection (range = [0,10^14]):",param1
  end
  6:begin
    close,file_unit
    file_delete,file_name,/allow
    message,"ZPN: This map projection has not been implemented."
  end
  8:begin
    print,"AIR: Enter the angular distance from the tangent point in which the"
    read,"error is to be minimized (range = [0,90]):",param1
  end
  9:begin
    read,"CYP: Enter the radius of the cylinder (range = [0,10^14]):",param2
    print,"CYP: Enter the distance from the projection point to the center of"
    read,"the sphere (range = [-10^14,10^14], but not -radius):",param1
  end
  12:begin
    print,"CEA: Enter the square of the cosine of the latitude at which the"
    read,"map is conformal (range = [0,1]):",param1
  end
  13:begin
    read,$
    "COP: Lower angle at which cone intersects sphere (range = [-90,upper]):",$
    theta1
    read,$
    "COP: Upper angle at which cone intersects sphere (range = [lower,90]):",$
    theta2
    param1 = (theta2+theta1)/2.
    param2 = abs(theta2 - theta1)/2
  end
  14:begin
    read,$
    "COD: Lower angle at which cone intersects sphere (range = [-90,upper]):",$
    param1
    read,$
    "COD: Upper angle at which cone intersects sphere (range = [lower,90]):",$
    param2
  end
  15:begin
    read,$
    "COE: Lower angle at which cone intersects sphere (range = [-90,upper]):",$
    param1
    read,$
    "COE: Upper angle at which cone intersects sphere (range = [lower,90]):",$
    param2
  end
  16:begin
    read,$
    "COO: Lower angle at which cone intersects sphere (range = [-90,upper]):",$
    param1
    read,$
    "COO: Upper angle at which cone intersects sphere (range = [lower,90]):",$
    param2
  end
  17:begin
    read,"BON: Characteristic angle (range = [-90,90]):",param1
  end
  else:
endcase

if (n_elements(param1) ne 0) then printf,file_unit,"param1 = ",param1
if (n_elements(param2) ne 0) then printf,file_unit,"param2 = ",param2
if (n_elements(alpha) ne 0) then printf,file_unit,"alpha = ",alpha
if (n_elements(delta) ne 0) then printf,file_unit,"delta = ",delta
if (n_elements(longpole) ne 0) then printf,file_unit,"longpole = ",longpole

print,"Would you like to:"
print,"(1) Do a whole-sky map."
print,"(2) Select a (rectangular) region on the sky to map."
print,""
repeat read,"Enter '1' or '2':",choice until ((choice eq 1) or (choice eq 2))
print,""

; Set-up to do a full-sky map.
if (choice eq 1) then begin
  ; set-up the longitude range
  printf,file_unit,"min_lon = 0"
  printf,file_unit,"max_lon = 345"
  printf,file_unit,"lon_spacing = 15"

  ; set-up the latitude range (this differs from map to map because some maps
  ; diverge at particular latitudes)
  if ((map eq  1) or (map eq 3)) then begin
    printf,file_unit,"min_lat = 0"
    printf,file_unit,"max_lat = 90"
  endif else if (map eq 2) then begin
    printf,file_unit,"min_lat = 15"
    printf,file_unit,"max_lat = 90"
  endif else if (map eq 4) then begin
    printf,file_unit,"min_lat = -75"
    printf,file_unit,"max_lat = 90"
  endif else if (map eq 8) then begin
  ; For the Airy map projection, the minimum usable latitude depends on the
  ; input parameters, so it must be calculated now.
    xi = (findgen(90) + 1)/!radeg
    xi_b = (!pi/2.0 - param1/!radeg)/2.0
    radius=-!radeg*(alog(cos(xi))/tan(xi)+alog(cos(xi_b))/tan(xi_b)*tan(xi))
    i = 0
    repeat i = i + 1 $
    until ((radius[i + 1] lt radius[i]) or (i eq (n_elements(radius) - 2)))
    i = i - 1
    min_lat = 90 - 2*!radeg*xi[i]
    printf,file_unit,"min_lat = ",min_lat[0]
    printf,file_unit,"max_lat = 90"
  endif else if (map eq 9) then begin
 ; The CYP map projection diverges at the poles when param1 (mu) is equal to 0.
    if (param1 eq 0) then begin
      printf,file_unit,"min_lat = -75"
      printf,file_unit,"max_lat = 75"
    endif else begin
      printf,file_unit,"min_lat = -90"
      printf,file_unit,"max_lat = 90"
    endelse
  endif else if (map eq 11) then begin
    printf,file_unit,"min_lat = -75"
    printf,file_unit,"max_lat = 75"
  endif else if (map eq 13) then begin
    printf,file_unit,"min_lat = -90 > (param1 - 90 + 15)"
    printf,file_unit,"max_lat =  90 < (param1 + 90 - 15)"
  endif else if (map eq 16) then begin
    printf,file_unit,"min_lat = -75"
    printf,file_unit,"max_lat = 90"
  endif else begin
    printf,file_unit,"min_lat = -90"
    printf,file_unit,"max_lat = 90"
  endelse
  printf,file_unit,"lat_spacing = 15"
endif else if (choice eq 2) then begin
  print,"Please enter the following quantities in degrees.'
  read,"  minimum longitude:",min_lon
  printf,file_unit,"min_lon = ",min_lon
  read,"  maximum longitude:",max_lon
  printf,file_unit,"max_lon = ",max_lon
  read,"  longitude spacing:",lon_spacing
  printf,file_unit,"lon_spacing = ",lon_spacing
  read,"  minimum latitude:",min_lat
  printf,file_unit,"min_lat = ",min_lat
  read,"  maximum latitude:",max_lat
  printf,file_unit,"max_lat = ",max_lat
  read,"  latitude spacing:",lat_spacing
  printf,file_unit,"lat_spacing = ",lat_spacing
endif

printf,file_unit,""
printf,file_unit,$
"; Based on the ranges for latitude and longitude, as well as their spacing,"
printf,file_unit,$
"; generate the latitude and longitude arrays."
printf,file_unit,"num_lon = long((max_lon - min_lon)/lon_spacing) + 1"
printf,file_unit,"lon = dindgen(num_lon)*lon_spacing + min_lon"
printf,file_unit,"num_lat = long((max_lat - min_lat)/lat_spacing) + 1"
printf,file_unit,"lat = dindgen(num_lat)*lat_spacing + min_lat"
printf,file_unit,"longitude = dblarr(num_lon,num_lat)"
printf,file_unit,"for i = 0,num_lat - 1 do longitude[*,i] = lon"
printf,file_unit,"latitude = dblarr(num_lon,num_lat)"
printf,file_unit,"for i = 0,num_lon - 1 do latitude[i,*] = lat"

printf,file_unit,""
printf,file_unit,";CONVERSION"

printf,file_unit,$
"; Convert the spherical coordinates into x-y coordinates by using wcssph2xy."
if (map lt 23) then begin
  if (n_elements(param1) eq 0) then begin
    if (n_elements(alpha) ne 0) then begin
      printf,file_unit,$
        "wcssph2xy,longitude,latitude,x,y,map,crval=[alpha,delta],$"
      printf,file_unit,"          longpole=longpole"
    endif else begin
      printf,file_unit,"wcssph2xy,longitude,latitude,x,y,map"
    endelse
  endif else if (n_elements(param2) eq 0) then begin
    if (n_elements(alpha) ne 0) then begin
      printf,file_unit,$
        "wcssph2xy,longitude,latitude,x,y,map,pv2=param1, $"
      printf,file_unit,"          crval=[alpha,delta],longpole=longpole"
    endif else begin
       printf,file_unit,"wcssph2xy,longitude,latitude,x,y,map,pv2=param1"
    endelse
  endif else begin
    if (n_elements(alpha) ne 0) then begin
      printf,file_unit,$
        "wcssph2xy,longitude,latitude,x,y,map,pv2=[param1,param2],$
      printf,file_unit,"          crval=[alpha,delta],longpole=longpole"
    endif else begin
      printf,file_unit,$
        "wcssph2xy,longitude,latitude,x,y,map,pv2=[param1,param2]"
    endelse
  endelse
endif else begin
  printf,file_unit,$
 "; The variable face must be declared with the same structure as latitude and"
  printf,file_unit,"; longitude before calling wcssph2xy." 
  printf,file_unit,"face = longitude - longitude"
  if (n_elements(alpha) ne 0) then begin
    printf,file_unit,$
      "wcssph2xy,longitude,latitude,x,y,map,face=face,crval=[alpha,delta], $
    printf,file_unit,"          longpole=longpole"
  endif else begin
    printf,file_unit,"wcssph2xy,longitude,latitude,x,y,map,face=face"
  endelse
endelse
printf,file_unit,""

printf,file_unit,";PLOTTING"
printf,file_unit,$
"; all maps have x increasing to the left, so switch this"
printf,file_unit,"xx = -x"
printf,file_unit,""

printf,file_unit,";LABELS"
printf,file_unit,$
"; The arrays lon_index and lat_index contain the indices for the latitude"
printf,file_unit,$
"; and longitude labels.  Labels occur every 30 degrees unless 30 doesn't"
printf,file_unit,$
"; divide into any of the latitude and longitude values evenly.  In this case,"
printf,file_unit,$
"; all latitude and longitude lines are labeled."
printf,file_unit,$
  "lon_index = where(long(longitude[*,0])/30 eq longitude[*,0]/30.)"
printf,file_unit,$
  "lat_index = where(long(latitude[0,*])/30 eq latitude[0,*]/30.)"
printf,file_unit,$
  "if (lat_index[0] eq -1) then lat_index = indgen(n_elements(latitude[0,*]))"
printf,file_unit,$
  "if (lon_index[0] eq -1) then lon_index = indgen(n_elements(longitude[*,0]))"

printf,file_unit,""

if (option lt 3) then begin
 if (n_elements(param2) eq 1) then wcssph2xy_plot,file_unit,map,param1,param2 $
  else if (n_elements(param1) eq 1) then wcssph2xy_plot,file_unit,map,param1 $
  else wcssph2xy_plot,file_unit,map

  if (option eq 2) then begin
    printf,file_unit,"key = ''"
    printf,file_unit,"read,'Press return to continue',key"

    if (n_elements(param2) eq 1) then $
       inversion_error,file_unit,map,param1,param2 $
    else if (n_elements(param1) eq 1) then $
       inversion_error,file_unit,map,param1 $
    else inversion_error,file_unit,map
  endif
endif else begin
  if (n_elements(param2) eq 1) then wcs_rot,file_unit,map,param1,param2 $
  else if (n_elements(param1) eq 1) then wcs_rot,file_unit,map,param1 $
  else wcs_rot,file_unit,map
endelse

printf,file_unit,"end"
close,file_unit
print,$
"The commands needed to execute what you are about to see can be executed"
print,"interactively, by typing ",strmid(file_name,0,strlen(file_name)-3)
print,""
command = strmid(file_name,0,strlen(file_name) - 4)
r = execute(command)
endrep until (option eq 4)
end
