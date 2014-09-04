FUNCTION tsc,value,posx,nx,posy,ny,posz,nz, $
             AVERAGE=average,WRAPAROUND=wraparound,NO_MESSAGE=no_message, $
             ISOLATED=isolated
;+
; NAME:
;       TSC
;
; PURPOSE:
;       Interpolate an irregularly sampled field using a Triangular Shaped Cloud
;
; EXPLANATION:
;       This function interpolates an irregularly sampled field to a
;       regular grid using Triangular Shaped Cloud (nearest grid point
;       gets weight 0.75-dx^2, points before and after nearest grid
;       points get weight 0.5*(1.5-dx)^2, where dx is the distance
;       from the sample to the grid point in units of the cell size).
;
; CATEGORY:
;       Mathematical functions, Interpolation
;
; CALLING SEQUENCE:
;       Result = TSC, VALUE, POSX, NX[, POSY, NY, POSZ, NZ, 
;                     AVERAGE = average, WRAPAROUND =  wraparound,
;                     ISOLATED = isolated, NO_MESSAGE = no_message]
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
;                   point will then be the weighted average of all the
;                   samples allocated to it. If this keyword is not
;                   set, the value at each grid point will be the
;                   weighted sum of all the nodes allocated to it
;                   (e.g. for a density field from a distribution of
;                   particles). (D=0). 
;       WRAPAROUND: Set this keyword if you want the first grid point
;                   to contain samples of both sides of the volume
;                   (see below).
;       ISOLATED:   Set this keyword if the data is isolated, i.e. not
;                   periodic. In that case total `mass' is not conserved.
;                   This keyword cannot be used in combination with the
;                   keyword WRAPAROUND.
;       NO_MESSAGE: Suppress informational messages.
;
; Example of default allocation of nearest grid points: n0=4, *=gridpoint.
;
;     0   1   2   3     Index of gridpoints
;     *   *   *   *     Grid points
;   |---|---|---|---|   Range allocated to gridpoints ([0.0,1.0> --> 0, etc.)
;   0   1   2   3   4   posx
;
; Example of ngp allocation for WRAPAROUND: n0=4, *=gridpoint.
;
;   0   1   2   3         Index of gridpoints
;   *   *   *   *         Grid points
; |---|---|---|---|--     Range allocated to gridpoints ([0.5,1.5> --> 1, etc.)
;   0   1   2   3   4=0   posx
;
;
; OUTPUTS:
;       Prints that a TSC interpolation is being performed of x
;       samples to y grid points, unless NO_MESSAGE is set.
;
; RESTRICTIONS:
;       Field data is assumed to be periodic with the sampled volume
;       the basic cell, unless ISOLATED is set.
;       All input arrays must have the same dimensions.
;       Position coordinates should be in `index units' of the
;       desired grid: POSX=[0,NX>, etc.
;       Keywords ISOLATED and WRAPAROUND cannot both be set.
;
; PROCEDURE:
;       Nearest grid point is determined for each sample.
;       TSC weights are computed for each sample.
;       Samples are interpolated to the grid.
;       Grid point values are computed (sum or average of samples).
;
; EXAMPLE:
;       nx=20
;       ny=10
;       posx=randomu(s,1000)
;       posy=randomu(s,1000)
;       value=posx^2+posy^2
;       field=tsc(value,posx*nx,nx,posy*ny,ny,/average)
;       surface,field,/lego
;
; NOTES:
;       Use csc.pro or ngp.pro for lower order interpolation schemes.    A 
;       standard reference for these interpolation methods is:   R.W. Hockney 
;       and J.W. Eastwood, Computer Simulations Using Particles (New York: 
;       McGraw-Hill, 1981).
;
; MODIFICATION HISTORY:
;       Written by Joop Schaye, Feb 1999.
;       Check for overflow for large dimensions  P. Riley/W. Landsman Dec. 1999
;-

nrsamples=n_elements(value)
nparams=n_params()
dim=(nparams-1)/2

IF dim LE 2 THEN BEGIN
    nz=1
    IF dim EQ 1 THEN ny=1
ENDIF
nxny=long(nx)*long(ny)


;---------------------
; Some error handling.
;---------------------

on_error,2  ; Return to caller if an error occurs.

IF NOT (nparams EQ 3 OR nparams EQ 5 OR nparams EQ 7) THEN BEGIN
    message,'Incorrect number of arguments!',/continue
    message,'Syntax: TSC, VALUE, POSX, NX[, POSY, NY, POSZ, NZ,' + $
      ' AVERAGE = average, WRAPAROUND =  wraparound]'
ENDIF 

IF (nrsamples NE n_elements(posx)) OR $
  (dim GE 2 AND nrsamples NE n_elements(posy)) OR $
  (dim EQ 3 AND nrsamples NE n_elements(posz)) THEN $
  message,'Input arrays must have the same dimensions!'

IF keyword_set(isolated) AND keyword_set(wraparound) THEN $
  message,'Keywords ISOLATED and WRAPAROUND cannot both be set!'

IF NOT keyword_set(no_message) THEN $
  print,'Interpolating ' + strtrim(string(nrsamples,format='(i10)'),1) $
  + ' samples to ' + strtrim(string(nxny*nz,format='(i10)'),1) + $
  ' grid points using TSC...'


;-----------------------
; Calculate TSC weights.
;-----------------------

; Compute weights per axis, in order to reduce memory (everything
; needs to be in memory if we compute all nearest grid points first).

;*************
; X-direction.
;*************

; Coordinates of nearest grid point (ngp).
IF keyword_set(wraparound) THEN ngx=fix(posx+0.5) $
ELSE ngx=fix(posx)+0.5

; Distance from sample to ngp.
dngx=ngx-posx

; Index of ngp.
IF keyword_set(wraparound) THEN kx2=temporary(ngx) $
ELSE kx2=temporary(ngx)-0.5
; Weight of ngp.
wx2=0.75-dngx*dngx

; Point before ngp.
kx1=kx2-1  ; Index.
dx=1.0-dngx  ; Distance to sample.
wx1=0.5*(1.5-temporary(dx))^2  ; TSC-weight.

; Point after ngp.
kx3=kx2+1  ; Index.
dx=1.0+temporary(dngx)  ; Distance to sample.
wx3=0.5*(1.5-temporary(dx))^2  ; TSC-weight.

; Periodic boundary conditions.
bad=where(kx2 EQ 0,count)
IF count NE 0 THEN BEGIN       ; Otherwise kx1=-1.
    kx1[bad]=nx-1
    IF keyword_set(isolated) THEN wx1[bad]=0.
ENDIF
bad=where(kx2 EQ nx-1,count)
IF count NE 0 THEN BEGIN       ; Otherwise kx3=nx.
    kx3[bad]=0
    IF keyword_set(isolated) THEN wx3[bad]=0.
ENDIF
IF keyword_set(wraparound) THEN BEGIN
    bad=where(kx2 EQ nx,count)
    IF count NE 0 THEN BEGIN
        kx2[bad]=0
        kx3[bad]=1
    ENDIF
ENDIF
bad=0  ; Free memory.


;*************
; Y-direction.
;*************

IF dim GE 2 THEN BEGIN 
    ; Coordinates of nearest grid point (ngp).
    IF keyword_set(wraparound) THEN ngy=fix(posy+0.5) $
    ELSE ngy=fix(posy)+0.5

    ; Distance from sample to ngp.
    dngy=ngy-posy

    ; Index of ngp.
    IF keyword_set(wraparound) THEN ky2=temporary(ngy) $
    ELSE ky2=temporary(ngy)-0.5
    ; Weight of ngp.
    wy2=0.75-dngy*dngy

    ; Point before ngp.
    ky1=ky2-1  ; Index.
    dy=1.0-dngy  ; Distance to sample.
    wy1=0.5*(1.5-temporary(dy))^2  ; TSC-weight.

    ; Point after ngp.
    ky3=ky2+1  ; Index.
    dy=1.0+temporary(dngy)  ; Distance to sample.
    wy3=0.5*(1.5-temporary(dy))^2  ; TSC-weight.

    ; Periodic boundary conditions.
    bad=where(ky2 EQ 0,count)
    IF count NE 0 THEN BEGIN       ; Otherwise ky1=-1.
        ky1[bad]=ny-1
        IF keyword_set(isolated) THEN wy1[bad]=0.
    ENDIF
    bad=where(ky2 EQ ny-1,count)
    IF count NE 0 THEN BEGIN       ; Otherwise ky3=ny.
        ky3[bad]=0
        IF keyword_set(isolated) THEN wy3[bad]=0.
    ENDIF
    IF keyword_set(wraparound) THEN BEGIN
        bad=where(ky2 EQ ny,count)
        IF count NE 0 THEN BEGIN
            ky2[bad]=0
            ky3[bad]=1
        ENDIF
    ENDIF
    bad=0  ; Free memory.
ENDIF ELSE BEGIN
    ky1=0
    ky2=0
    wy1=1
    wy2=1
ENDELSE


;*************
; Z-direction.
;*************

IF dim EQ 3 THEN BEGIN
    ; Coordinates of nearest grid point (ngp).
    IF keyword_set(wraparound) THEN ngz=fix(posz+0.5) $
    ELSE ngz=fix(posz)+0.5

    ; Distance from sample to ngp.
    dngz=ngz-posz

    ; Index of ngp.
    IF keyword_set(wraparound) THEN kz2=temporary(ngz) $
    ELSE kz2=temporary(ngz)-0.5
    ; Weight of ngp.
    wz2=0.75-dngz*dngz

    ; Point before ngp.
    kz1=kz2-1  ; Index.
    dz=1.0-dngz  ; Distance to sample.
    wz1=0.5*(1.5-temporary(dz))^2  ; TSC-weight.

    ; Point after ngp.
    kz3=kz2+1  ; Index.
    dz=1.0+temporary(dngz)  ; Distance to sample.
    wz3=0.5*(1.5-temporary(dz))^2  ; TSC-weight.

    ; Periodic boundary conditions.
    bad=where(kz2 EQ 0,count)
    IF count NE 0 THEN BEGIN       ; Otherwise kz1=-1.
        kz1[bad]=nz-1
        IF keyword_set(isolated) THEN wz1[bad]=0.
    ENDIF
    bad=where(kz2 EQ nz-1,count)
    IF count NE 0 THEN BEGIN       ; Otherwise kz3=nz.
        kz3[bad]=0
        IF keyword_set(isolated) THEN wz3[bad]=0.
    ENDIF
    IF keyword_set(wraparound) THEN BEGIN
        bad=where(kz2 EQ nz,count)
        IF count NE 0 THEN BEGIN
            kz2[bad]=0
            kz3[bad]=1
        ENDIF
    ENDIF
    bad=0  ; Free memory.
ENDIF ELSE BEGIN
    kz1=0
    kz2=0
    wz1=1
    wz2=1
ENDELSE
    

;-----------------------------
; Interpolate samples to grid.
;-----------------------------

field=fltarr(nx,ny,nz)
IF keyword_set(average) THEN tottscweight=fltarr(nx,ny,nz)

; tscweight adds up all tsc weights allocated to a grid point, we need
; to keep track of this in order to compute the temperature.
; Note that total(tscweight) is equal to nrsamples and that
; total(ifield)=n0^3 if sph.plot NE 'sph,temp' (not 1 because we use
; xpos=posx*n0 --> cube length different from EDFW paper).

index=kx1+ky1*nx+kz1*nxny
tscweight=wx1*wy1*wz1
IF keyword_set(average) THEN BEGIN
    FOR j=0l,nrsamples-1l DO BEGIN
        field[index[j]]=field[index[j]]+tscweight[j]*value[j]
        tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
    ENDFOR
ENDIF ELSE FOR j=0l,nrsamples-1l DO $
  field[index[j]]=field[index[j]]+tscweight[j]*value[j]
index=kx2+ky1*nx+kz1*nxny
tscweight=wx2*wy1*wz1
IF keyword_set(average) THEN BEGIN
    FOR j=0l,nrsamples-1l DO BEGIN
        field[index[j]]=field[index[j]]+tscweight[j]*value[j]
        tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
    ENDFOR
ENDIF ELSE FOR j=0l,nrsamples-1l DO $
  field[index[j]]=field[index[j]]+tscweight[j]*value[j]
index=kx3+ky1*nx+kz1*nxny
tscweight=wx3*wy1*wz1
IF keyword_set(average) THEN BEGIN
    FOR j=0l,nrsamples-1l DO BEGIN
        field[index[j]]=field[index[j]]+tscweight[j]*value[j]
        tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
    ENDFOR
ENDIF ELSE FOR j=0l,nrsamples-1l DO $
  field[index[j]]=field[index[j]]+tscweight[j]*value[j]

IF dim GE 2 THEN BEGIN
    index=kx1+ky2*nx+kz1*nxny
    tscweight=wx1*wy2*wz1
    IF keyword_set(average) THEN BEGIN
        FOR j=0l,nrsamples-1l DO BEGIN
            field[index[j]]=field[index[j]]+tscweight[j]*value[j]
            tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
        ENDFOR
    ENDIF ELSE FOR j=0l,nrsamples-1l DO $
      field[index[j]]=field[index[j]]+tscweight[j]*value[j]
    index=kx2+ky2*nx+kz1*nxny
    tscweight=wx2*wy2*wz1
    IF keyword_set(average) THEN BEGIN
        FOR j=0l,nrsamples-1l DO BEGIN
            field[index[j]]=field[index[j]]+tscweight[j]*value[j]
            tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
        ENDFOR
    ENDIF ELSE FOR j=0l,nrsamples-1l DO $
      field[index[j]]=field[index[j]]+tscweight[j]*value[j]
    index=kx3+ky2*nx+kz1*nxny
    tscweight=wx3*wy2*wz1
    IF keyword_set(average) THEN BEGIN
        FOR j=0l,nrsamples-1l DO BEGIN
            field[index[j]]=field[index[j]]+tscweight[j]*value[j]
            tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
        ENDFOR
    ENDIF ELSE FOR j=0l,nrsamples-1l DO $
      field[index[j]]=field[index[j]]+tscweight[j]*value[j]
    index=kx1+ky3*nx+kz1*nxny
    tscweight=wx1*wy3*wz1
    IF keyword_set(average) THEN BEGIN
        FOR j=0l,nrsamples-1l DO BEGIN
            field[index[j]]=field[index[j]]+tscweight[j]*value[j]
            tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
        ENDFOR
    ENDIF ELSE FOR j=0l,nrsamples-1l DO $
      field[index[j]]=field[index[j]]+tscweight[j]*value[j]
    index=kx2+ky3*nx+kz1*nxny
    tscweight=wx2*wy3*wz1
    IF keyword_set(average) THEN BEGIN
        FOR j=0l,nrsamples-1l DO BEGIN
            field[index[j]]=field[index[j]]+tscweight[j]*value[j]
            tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
        ENDFOR
    ENDIF ELSE FOR j=0l,nrsamples-1l DO $
      field[index[j]]=field[index[j]]+tscweight[j]*value[j]
    index=kx3+ky3*nx+kz1*nxny
    tscweight=wx3*wy3*wz1
    IF keyword_set(average) THEN BEGIN
        FOR j=0l,nrsamples-1l DO BEGIN
            field[index[j]]=field[index[j]]+tscweight[j]*value[j]
            tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
        ENDFOR
    ENDIF ELSE FOR j=0l,nrsamples-1l DO $
      field[index[j]]=field[index[j]]+tscweight[j]*value[j]

    IF dim EQ 3 THEN BEGIN
        index=kx1+ky1*nx+kz2*nxny
        tscweight=wx1*wy1*wz2
        IF keyword_set(average) THEN BEGIN
            FOR j=0l,nrsamples-1l DO BEGIN
                field[index[j]]=field[index[j]]+tscweight[j]*value[j]
                tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
            ENDFOR
        ENDIF ELSE FOR j=0l,nrsamples-1l DO $
          field[index[j]]=field[index[j]]+tscweight[j]*value[j]
        index=kx2+ky1*nx+kz2*nxny
        tscweight=wx2*wy1*wz2
        IF keyword_set(average) THEN BEGIN
            FOR j=0l,nrsamples-1l DO BEGIN
                field[index[j]]=field[index[j]]+tscweight[j]*value[j]
                tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
            ENDFOR
        ENDIF ELSE FOR j=0l,nrsamples-1l DO $
          field[index[j]]=field[index[j]]+tscweight[j]*value[j]
        index=kx3+ky1*nx+kz2*nxny
        tscweight=wx3*wy1*wz2
        IF keyword_set(average) THEN BEGIN
            FOR j=0l,nrsamples-1l DO BEGIN
                field[index[j]]=field[index[j]]+tscweight[j]*value[j]
                tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
            ENDFOR
        ENDIF ELSE FOR j=0l,nrsamples-1l DO $
          field[index[j]]=field[index[j]]+tscweight[j]*value[j]
        index=kx1+ky2*nx+kz2*nxny
        tscweight=wx1*wy2*wz2
        IF keyword_set(average) THEN BEGIN
            FOR j=0l,nrsamples-1l DO BEGIN
                field[index[j]]=field[index[j]]+tscweight[j]*value[j]
                tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
            ENDFOR
        ENDIF ELSE FOR j=0l,nrsamples-1l DO $
          field[index[j]]=field[index[j]]+tscweight[j]*value[j]
        index=kx2+ky2*nx+kz2*nxny
        tscweight=wx2*wy2*wz2
        IF keyword_set(average) THEN BEGIN
            FOR j=0l,nrsamples-1l DO BEGIN
                field[index[j]]=field[index[j]]+tscweight[j]*value[j]
                tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
            ENDFOR
        ENDIF ELSE FOR j=0l,nrsamples-1l DO $
          field[index[j]]=field[index[j]]+tscweight[j]*value[j]
        index=kx3+ky2*nx+kz2*nxny
        tscweight=wx3*wy2*wz2
        IF keyword_set(average) THEN BEGIN
            FOR j=0l,nrsamples-1l DO BEGIN
                field[index[j]]=field[index[j]]+tscweight[j]*value[j]
                tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
            ENDFOR
        ENDIF ELSE FOR j=0l,nrsamples-1l DO $
          field[index[j]]=field[index[j]]+tscweight[j]*value[j]
        index=kx1+ky3*nx+kz2*nxny
        tscweight=wx1*wy3*wz2
        IF keyword_set(average) THEN BEGIN
            FOR j=0l,nrsamples-1l DO BEGIN
                field[index[j]]=field[index[j]]+tscweight[j]*value[j]
                tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
            ENDFOR
        ENDIF ELSE FOR j=0l,nrsamples-1l DO $
          field[index[j]]=field[index[j]]+tscweight[j]*value[j]
        index=kx2+ky3*nx+kz2*nxny
        tscweight=wx2*wy3*wz2
        IF keyword_set(average) THEN BEGIN
            FOR j=0l,nrsamples-1l DO BEGIN
                field[index[j]]=field[index[j]]+tscweight[j]*value[j]
                tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
            ENDFOR
        ENDIF ELSE FOR j=0l,nrsamples-1l DO $
          field[index[j]]=field[index[j]]+tscweight[j]*value[j]
        index=kx3+ky3*nx+kz2*nxny
        tscweight=wx3*wy3*wz2
        IF keyword_set(average) THEN BEGIN
            FOR j=0l,nrsamples-1l DO BEGIN
                field[index[j]]=field[index[j]]+tscweight[j]*value[j]
                tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
            ENDFOR
        ENDIF ELSE FOR j=0l,nrsamples-1l DO $
          field[index[j]]=field[index[j]]+tscweight[j]*value[j]
        index=kx1+ky1*nx+kz3*nxny
        tscweight=wx1*wy1*wz3
        IF keyword_set(average) THEN BEGIN
            FOR j=0l,nrsamples-1l DO BEGIN
                field[index[j]]=field[index[j]]+tscweight[j]*value[j]
                tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
            ENDFOR
        ENDIF ELSE FOR j=0l,nrsamples-1l DO $
          field[index[j]]=field[index[j]]+tscweight[j]*value[j]
        index=kx2+ky1*nx+kz3*nxny
        tscweight=wx2*wy1*wz3
        IF keyword_set(average) THEN BEGIN
            FOR j=0l,nrsamples-1l DO BEGIN
                field[index[j]]=field[index[j]]+tscweight[j]*value[j]
                tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
            ENDFOR
        ENDIF ELSE FOR j=0l,nrsamples-1l DO $
          field[index[j]]=field[index[j]]+tscweight[j]*value[j]
        index=kx3+ky1*nx+kz3*nxny
        tscweight=wx3*wy1*wz3
        IF keyword_set(average) THEN BEGIN
            FOR j=0l,nrsamples-1l DO BEGIN
                field[index[j]]=field[index[j]]+tscweight[j]*value[j]
                tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
            ENDFOR
        ENDIF ELSE FOR j=0l,nrsamples-1l DO $
          field[index[j]]=field[index[j]]+tscweight[j]*value[j]
        index=kx1+ky2*nx+kz3*nxny
        tscweight=wx1*wy2*wz3
        IF keyword_set(average) THEN BEGIN
            FOR j=0l,nrsamples-1l DO BEGIN
                field[index[j]]=field[index[j]]+tscweight[j]*value[j]
                tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
            ENDFOR
        ENDIF ELSE FOR j=0l,nrsamples-1l DO $
          field[index[j]]=field[index[j]]+tscweight[j]*value[j]
        index=kx2+ky2*nx+kz3*nxny
        tscweight=wx2*wy2*wz3
        IF keyword_set(average) THEN BEGIN
            FOR j=0l,nrsamples-1l DO BEGIN
                field[index[j]]=field[index[j]]+tscweight[j]*value[j]
                tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
            ENDFOR
        ENDIF ELSE FOR j=0l,nrsamples-1l DO $
          field[index[j]]=field[index[j]]+tscweight[j]*value[j]
        index=kx3+ky2*nx+kz3*nxny
        tscweight=wx3*wy2*wz3
        IF keyword_set(average) THEN BEGIN
            FOR j=0l,nrsamples-1l DO BEGIN
                field[index[j]]=field[index[j]]+tscweight[j]*value[j]
                tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
            ENDFOR
        ENDIF ELSE FOR j=0l,nrsamples-1l DO $
          field[index[j]]=field[index[j]]+tscweight[j]*value[j]
        index=kx1+ky3*nx+kz3*nxny
        tscweight=wx1*wy3*wz3
        IF keyword_set(average) THEN BEGIN
            FOR j=0l,nrsamples-1l DO BEGIN
                field[index[j]]=field[index[j]]+tscweight[j]*value[j]
                tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
            ENDFOR
        ENDIF ELSE FOR j=0l,nrsamples-1l DO $
          field[index[j]]=field[index[j]]+tscweight[j]*value[j]
        index=kx2+ky3*nx+kz3*nxny
        tscweight=wx2*wy3*wz3
        IF keyword_set(average) THEN BEGIN
            FOR j=0l,nrsamples-1l DO BEGIN
                field[index[j]]=field[index[j]]+tscweight[j]*value[j]
                tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
            ENDFOR
        ENDIF ELSE FOR j=0l,nrsamples-1l DO $
          field[index[j]]=field[index[j]]+tscweight[j]*value[j]
        index=kx3+ky3*nx+kz3*nxny
        tscweight=wx3*wy3*wz3
        IF keyword_set(average) THEN BEGIN
            FOR j=0l,nrsamples-1l DO BEGIN
                field[index[j]]=field[index[j]]+tscweight[j]*value[j]
                tottscweight[index[j]]=tottscweight[index[j]]+tscweight[j]
            ENDFOR
        ENDIF ELSE FOR j=0l,nrsamples-1l DO $
          field[index[j]]=field[index[j]]+tscweight[j]*value[j]
    ENDIF

ENDIF

; Free memory (no need to free any more local arrays, will not lower
; maximum memory usage).
index=0
weight=0


;--------------------------
; Compute weighted average.
;--------------------------

IF keyword_set(average) THEN BEGIN
    good=where(tottscweight NE 0,nrgood)
    field[good]=temporary(field[good])/temporary(tottscweight[good])
ENDIF

return,field

END  ; End of procedure tsc.
