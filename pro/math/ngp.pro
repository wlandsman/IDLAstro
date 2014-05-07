FUNCTION ngp,value,posx,nx,posy,ny,posz,nz, $
             AVERAGE=average,WRAPAROUND=wraparound,NO_MESSAGE=no_message
;+
; NAME:
;       NGP
;
; PURPOSE:
;       Interpolate an irregularly sampled field using Nearest Grid Point
;
; EXPLANATION:
;       This function interpolates irregularly gridded points to a
;       regular grid using Nearest Grid Point.
;
; CATEGORY:
;       Mathematical functions, Interpolation
;
; CALLING SEQUENCE:
;       Result = NGP, VALUE, POSX, NX[, POSY, NY, POSZ, NZ, 
;                     /AVERAGE, /WRAPAROUND, /NO_MESSAGE]
;
; INPUTS:
;       VALUE: Array of sample weights (field values). For e.g. a
;              temperature field this would be the temperature and the
;              keyword AVERAGE should be set. For e.g. a density field
;              this could be either the particle mass (AVERAGE should
;              not be set) or the density (AVERAGE should be set).
;       POSX:  Array of X coordinates of field samples, unit indices: [0,NX>.
;       NX:    Desired number of grid points in X-direction.
;       
; OPTIONAL INPUTS:
;      POSY: Array of Y coordinates of field samples, unit indices: [0,NY>.
;      NY:   Desired number of grid points in Y-direction.
;      POSZ: Array of Z coordinates of field samples, unit indices: [0,NZ>.
;      NZ:   Desired number of grid points in Z-direction.
;
; KEYWORD PARAMETERS:
;       AVERAGE:    Set this keyword if the nodes contain field samples
;                   (e.g. a temperature field). The value at each grid
;                   point will then be the average of all the samples
;                   allocated to it. If this keyword is not set, the
;                   value at each grid point will be the sum of all the
;                   nodes allocated to it (e.g. for a density field from
;                   a distribution of particles). (D=0). 
;       WRAPAROUND: Set this keyword if the data is periodic and if you
;                   want the first grid point to contain samples of both
;                   sides of the volume (see below). (D=0).
;       NO_MESSAGE: Suppress informational messages.
;
; Example of default NGP allocation: n0=4, *=gridpoint.
;
;     0   1   2   3     Index of gridpoints
;     *   *   *   *     Grid points
;   |---|---|---|---|   Range allocated to gridpoints ([0.0,1.0> --> 0, etc.)
;   0   1   2   3   4   posx
;
; Example of NGP allocation for WRAPAROUND: n0=4, *=gridpoint.
;
;   0   1   2   3         Index of gridpoints
;   *   *   *   *         Grid points
; |---|---|---|---|--     Range allocated to gridpoints ([0.5,1.5> --> 1, etc.)
;   0   1   2   3   4=0   posx
;
;
; OUTPUTS:
;       Prints that a NGP interpolation is being performed of x
;       samples to y grid points, unless NO_MESSAGE is set. 
;
; RESTRICTIONS:
;       All input arrays must have the same dimensions.
;       Position coordinates should be in `index units' of the
;       desired grid: POSX=[0,NX>, etc.
;
; PROCEDURE:
;       Nearest grid point is determined for each sample.
;       Samples are allocated to nearest grid points.
;       Grid point values are computed (sum or average of samples).
;
; EXAMPLE:
;       nx = 20
;       ny = 10
;       posx = randomu(s,1000)
;       posy = randomu(s,1000)
;       value = posx^2+posy^2
;       field = ngp(value,posx*nx,nx,posy*ny,ny,/average)
;       surface,field,/lego
;
; NOTES:
;       Use tsc.pro or cic.pro for a higher order interpolation schemes.    A 
;       standard reference for these interpolation methods is:   R.W. Hockney 
;       and J.W. Eastwood, Computer Simulations Using Particles (New York: 
;       McGraw-Hill, 1981).
; MODIFICATION HISTORY:
;       Written by Joop Schaye, Feb 1999.
;       Check for LONG overflow  P. Riley/W. Landsman   December 1999
;-

nrsamples=n_elements(value)
nparams=n_params()
dim=(nparams-1)/2

IF dim LE 2 THEN BEGIN
    nz=1
    IF dim EQ 1 THEN ny=1
ENDIF
nxny = long(nx)*long(ny)


;---------------------
; Some error handling.
;---------------------

on_error,2  ; Return to caller if an error occurs.

IF NOT (nparams EQ 3 OR nparams EQ 5 OR nparams EQ 7) THEN BEGIN
    message,'Incorrect number of arguments!',/continue
    message,'Syntax: NGP, VALUE, POSX, NX[, POSY, NY, POSZ, NZ,' + $
      ' /AVERAGE, /WRAPAROUND, /NO_MESSAGE]'
ENDIF 

IF (nrsamples NE n_elements(posx)) OR $
  (dim GE 2 AND nrsamples NE n_elements(posy)) OR $
  (dim EQ 3 AND nrsamples NE n_elements(posz)) THEN $
  message,'Input arrays must have the same dimensions!'

IF NOT keyword_set(no_message) THEN $
  print,'Interpolating ' + strtrim(string(nrsamples,format='(i10)'),1) $
  + ' samples to ' + strtrim(string(nxny*nz,format='(i10)'),1) + $
  ' grid points using NGP...'


;-----------------------------
; Compute nearest grid points.
;-----------------------------

IF keyword_set(wraparound) THEN BEGIN
    ; Coordinates of nearest grid point (ngp).
    ngx=fix(posx+0.5)
    ; Periodic boundary conditions.
    bad=where(ngx EQ nx,count)
    IF count NE 0 THEN ngx[bad]=0
    IF dim GE 2 THEN BEGIN 
        ngy=fix(posy+0.5)
        bad=where(ngy EQ ny,count)
        IF count NE 0 THEN ngy[bad]=0
        IF dim EQ 3 THEN BEGIN
            ngz=fix(posz+0.5)
            bad=where(ngz EQ nz,count)
            IF count NE 0 THEN ngz[bad]=0
        ENDIF
    ENDIF 
    bad=0  ; Free memory.
ENDIF ELSE BEGIN
    ; Coordinates of nearest grid point (ngp).
    ngx=fix(posx)
    IF dim GE 2 THEN BEGIN  
        ngy=fix(posy)
        IF dim EQ 3 THEN ngz=fix(posz)
    ENDIF
ENDELSE

; Indices of grid points to which samples are assigned.
CASE dim OF
    1: index=temporary(ngx)
    2: index=temporary(ngx)+temporary(ngy)*nx
    3: index=temporary(ngx)+temporary(ngy)*nx+temporary(ngz)*nxny
ENDCASE


;-------------------------------
; Interpolate samples to grid.
;-------------------------------

field=fltarr(nx,ny,nz)

FOR i=0l,nrsamples-1l DO field[index[i]]=field[index[i]]+value[i]


;--------------------------
; Compute weighted average.
;--------------------------

IF keyword_set(average) THEN BEGIN
    ; Number of samples per grid point.
    frequency=histogram(temporary(index),min=0,max=nxny*nz-1l)
    
    ; Normalize.
    good=where(frequency NE 0,nrgood)
    field[good]=temporary(field[good])/temporary(frequency[good])
ENDIF

return,field

END  ; End of function ngp.








