;+
; NAME:
;       AITOFF_GRID
;
; PURPOSE:
;       Produce an overlay of latitude and longitude lines over a plot or image
; EXPLANATION:
;       The grid is plotted on the current graphics device. AITOFF_GRID 
;       assumes that the ouput plot coordinates span the x-range of 
;       -180 to 180 and the y-range goes from -90 to 90.
;
; CALLING SEQUENCE:
;
;       AITOFF_GRID [,DLONG,DLAT, LABEL=, /NEW, CHARTHICK=, CHARSIZE=, 
;                     FONT=, _EXTRA=]
;
; OPTIONAL INPUTS:
;
;       DLONG   = Optional input longitude line spacing in degrees. If left
;                 out, defaults to 30.
;       DLAT    = Optional input latitude line spacing in degrees. If left
;                 out, defaults to 30.
;
; OPTIONAL INPUT KEYWORDS:
;
;       LABEL           = Optional keyword specifying that the latitude and
;                         longitude lines on the prime meridian and the
;                         equator should be labeled in degrees. If LABELS is
;                         given a value of 2, i.e. LABELS=2, then the longitude
;                         labels will be in hours instead of degrees.
;        CHARSIZE       = If /LABEL is set, then CHARSIZE specifies the size
;                         of the label characters (passed to XYOUTS)
;        CHARTHICK     =  If /LABEL is set, then CHARTHICK specifies the 
;                         thickness of the label characters (passed to XYOUTS)
;       FONT          =   scalar font graphics keyword (-1,0 or 1) for text
;       /NEW          =   If this keyword is set, then AITOFF_GRID will create
;                         a new plot grid, rather than overlay an existing plot.
;
;       Any valid keyword to OPLOT such as COLOR, LINESTYLE, THICK can be 
;       passed to AITOFF_GRID (though the _EXTRA facility) to to specify the
;       color, style, or thickness of the grid lines.
; OUTPUTS:
;       Draws grid lines on current graphics device.
;
; EXAMPLE:
;       Create a labeled Aitoff grid of the Galaxy, and overlay stars at 
;       specified Galactic longitudes, glong and latitudes, glat
;
;       IDL> aitoff_grid,/label,/new        ;Create labeled grid
;       IDL> aitoff, glong, glat, x,y      ;Convert to X,Y coordinates
;       IDL> plots,x,y,psym=2              ;Overlay "star" positions
;
; PROCEDURES USED:
;       AITOFF
; NOTES:
;       If labeling in hours (LABEL=2) then the longitude spacing should be
;       a multiple of 15 degrees
;
; AUTHOR AND MODIFICATIONS:
;
;       J. Bloch        1.2     6/2/91
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Create default plotting coords, if needed   W. Landsman  August 2000
;       Added _EXTRA, CHARTHICK, CHARSIZE keywords  W. Landsman  March 2001
;       Several tweaks, plot only hours not minutes W. Landsman January 2002
;       Allow FONT keyword to be passed to XYOUTS.  T. Robishaw Apr. 2006
;-
PRO AITOFF_GRID,DLONG,DLAT,LABEL=LABEL, NEW = new, _EXTRA= E, $
     CHARSIZE = charsize, CHARTHICK =charthick, FONT=font

        if  N_elements(dlong) EQ 0 then dlong = 30.0
        if  N_elements(dlat) EQ 0 then dlat = 30.0
        if  N_elements(font) EQ 0 then font = !p.font

; If no plotting axis has been defined, then create a default one

        new = keyword_set(new)
        if not new then new =  (!X.crange[0] EQ 0) and (!X.crange[1] EQ 0)
        if new then plot,[-180,180],[-90,90],/nodata,xsty=5,ysty=5
;
;       Do lines of constant longitude
;
        lat=findgen(181)-90
        lng=fltarr(181,/nozero)
        lngtot = long(180.0/dlong)

        for i=0,lngtot do begin
                replicate_inplace, lng, -180.0 + (i*dlong)
                aitoff,lng,lat,x,y
                oplot,x,y,_extra=e
                oplot,-x,y,_extra=e
        endfor
;
;       Do lines of constant latitude
;
        lng = findgen(361)-180.0
        lat = fltarr(361,/nozero)
        lattot=long(180.0/dlat)
        for i=1,lattot do begin
                replicate_inplace, lat, -90. + (i*dlat)
                aitoff,lng,lat,x,y
                oplot,x,y,_extra=e
         endfor
;
;       Do labeling if requested
;
        if keyword_set(label) then begin

;
;       Label equator
;
          if (!d.name eq 'PS') and (font eq 0) then hr = '!Uh!N' else hr='h'
             xoff = 2*dlong/30.
            for i=0,2*lngtot-1 do begin
                lng =  (180 + (i*dlong)) mod 360
                if (lng ne 0.0) and (lng ne 180.0) then begin
                    aitoff,lng,0.0,x,y
                    if label eq 1 then xyouts,x[0]+xoff,y[0]+1,$
                        strcompress(string(lng,format="(I4)"),/remove_all), $
                        charsize = charsize, charthick = charthick,font=font $
                    else begin
                         tmp = lng/15.
                         xyouts,round(x[0])+xoff,round(y[0])+1,string(tmp[0],$
                            format='(I2)') + hr, font=font,$
                            charsize = charsize, charthick = charthick
                    endelse
                endif
            endfor
;
;       Label prime meridian
;
            lat = -90 + (indgen(lattot-1)+1)*dlat
            aitoff,fltarr(lattot-1),lat,x,y
            slat = strtrim(round(lat),2)
            pos = where(lat GT 0, Npos)
            if Npos GT 0 then slat[pos] = '+' + slat[pos] 
            for i=0,lattot-2 do begin
                 xyouts,x[i]+2,y[i]+1, slat[i], font=font, $
                        charsize = charsize, charthick = charthick
            endfor
        endif

        return
end
