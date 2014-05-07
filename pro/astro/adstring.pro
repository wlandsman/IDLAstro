Function adstring,ra_dec,dec,precision, TRUNCATE = truncate,PRECISION=prec
;+
; NAME:
;       ADSTRING
; PURPOSE:
;       Return RA and Dec as character string(s) in sexagesimal format.
; EXPLANATION:
;       RA and Dec may be entered as either a 2 element vector or as
;       two separate vectors (or scalars).  One can also specify the precision 
;       of the declination in digits after the decimal point.
;
; CALLING SEQUENCE
;       result = ADSTRING( ra_dec, precision, /TRUNCATE )           
;               or
;       result = ADSTRING( ra,dec,[ precision, /TRUNCATE ] )
;               or
;       result = ADSTRING( dec, [ PRECISION= ]   
;
; INPUTS:
;       RA_DEC - 2 element vector giving the Right Ascension and declination
;               in decimal degrees.
;                     or
;       RA     - Right ascension in decimal degrees, numeric scalar or vector
;       DEC    - Declination in decimal degrees, numeric scalar or vector
;
;     If only one parameter is supplied then it must be either a scalar (which
;     is converted to sexagesimal) or a two element [RA, Dec] vector.
; OPTIONAL INPUT:
;       PRECISION  - Integer scalar (0-4) giving the number of digits after the 
;               decimal of DEClination.   The RA is automatically 1 digit more.
;               This parameter may either be the third parameter after RA,DEC 
;               or the second parameter after [RA,DEC].  If only DEC is supplied 
;               then precision must be supplied as a keyword parameter.   If no
;               PRECISION parameter or keyword is passed, a  precision of 1 for
;               both RA and DEC is returned to maintain  compatibility with past
;               ADSTRING versions.    Values of  precision larger than 4 will 
;               be truncated to 4.    If PRECISION is 3 or 4, then RA and Dec 
;               should be input as double precision.
; OPTIONAL INPUT KEYWORD:
;       /TRUNCATE - if set, then the last displayed digit in the output is 
;               truncated in precision rather than rounded.   This option is
;               useful if ADSTRING() is used to form an official IAU name 
;               (see http://vizier.u-strasbg.fr/Dic/iau-spec.htx) with 
;               coordinate specification.   The IAU name will typically be
;               be created by applying STRCOMPRESS/REMOVE) after the ADSTRING()
;               call, e.g. 
;              strcompress( adstring(ra,dec,0,/truncate), /remove)   ;IAU format
;        PRECISION = Alternate method of supplying the precision parameter, 
; OUTPUT:
;       RESULT - Character string(s) containing HR,MIN,SEC,DEC,MIN,SEC formatted
;               as ( 2I3,F5.(p+1),2I3,F4.p ) where p is the PRECISION 
;               parameter.    If only a single scalar is supplied it is 
;               converted to a sexagesimal string (2I3,F5.1).
;
; EXAMPLE:
;       (1) Display CRVAL coordinates in a FITS header, H
;
;       IDL> crval = sxpar(h,'CRVAL*')  ;Extract 2 element CRVAL vector (degs)
;       IDL> print, adstring(crval)     ;Print CRVAL vector sexagesimal format
;
;       (2)  print,adstring(30.42,-1.23,1)  ==>  ' 02 01 40.80  -01 13 48.0'
;            print,adstring(30.42,+0.23)    ==>  ' 02 01 40.8   +00 13 48.0'    
;            print,adstring(+0.23)          ==>  '+00 13 48.0'
;
;       (3) The first two calls in (2) can be combined in a single call using
;           vector input
;              print,adstring([30.42,30.42],[-1.23,0.23], 1)
; PROCEDURES CALLED:
;       RADEC, SIXTY()
;
; REVISION HISTORY:
;       Written   W. Landsman                      June 1988
;       Addition of variable precision and DEC seconds precision fix. 
;       ver.  Aug. 1990 [E. Deutsch]
;       Output formatting spiffed up       October 1991 [W. Landsman]
;       Remove ZPARCHECK call, accept 1 element vector  April 1992 [W. Landsman]
;       Call ROUND() instead of NINT()    February 1996  [W. Landsman]
;       Check roundoff past 60s           October 1997   [W. Landsman]
;       Work for Precision =4             November 1997  [W. Landsman]
;       Major rewrite to allow vector inputs   W. Landsman  February 2000
;       Fix possible error in seconds display when Precision=0 
;                               P. Broos/W. Landsman April 2002
;       Added /TRUNCATE keyword, put leading zeros in seconds display
;                               P. Broos/W. Landsman September 2002
;       Fix declination zero values under vector processing W.Landsman Feb 2004
;       Fix possible problem in leading zero display W. Landsman June 2004
;       Assume since V5.4, omit fstring() call  W. Landsman April 2006
;       Fix significant bug when round a declination with -1<dec<0 
;          Add PRECISION keyword    W.L. Aug 2008
;       Use formatting for "+" and "0"  W. L.    May 2009
;       Allow formatting of longitudes >99.99  W. L.  Sep 2012
;-
  On_error,2
  compile_opt idl2

  Npar = N_params()
 

  case N_elements(ra_dec) of 

     1: if ( Npar EQ 1 ) then dec = ra_dec else ra = ra_dec
     2: begin
        if (N_elements(dec) LT 2) then begin 
              ra = ra_dec[0] mod 360.
              if N_elements(dec) EQ 1 then begin 
              precision = dec & Npar=3 & endif
              dec = ra_dec[1]
        endif else ra = ra_dec
        end
   else: begin
        If (Npar Eq 1) then message, $
	'ERROR - first parameter must be either a scalar or 2 element vector'
        ra = ra_dec 
        end
   endcase

  if N_elements(prec) EQ 1 then precision = prec
  
  if ( Npar GE 2 ) then $
        if N_elements(dec) NE N_elements(ra) then message, $
      'ERROR - RA and Declination do not have equal number of elements'

  if N_elements(ra) EQ N_elements(dec) then begin

    badrange = where( (dec LT -90.) or (dec GT 90.), Nbad)
    if Nbad GT 0 then message, /INF, $
      'WARNING - Some declination values are out of valid range (-90 < dec <90)'
     radec, ra, dec, ihr, imin, xsec, ideg, imn, xsc
     if N_elements(precision) EQ 0 then precision = 0
     precision = precision > 0 < 4         ;No more than 4 decimal places
 if ~keyword_set(truncate) then begin
     roundsec = [59.5,59.95,59.995,59.9995,59.99995,59.999995]
     carry = where(xsec GT roundsec[precision+1], Ncarry)
     if Ncarry GT 0 then begin
        imin[carry] = imin[carry] + 1
        xsec[carry] = 0.0
        mcarry = where(imin[carry] EQ 60, Nmcarry)
        if Nmcarry GT 0 then begin
                ic = carry[mcarry]
                ihr[ic] = (ihr[ic] + 1) mod 24
                imin[ic] = 0
        endif
     endif
  endif else xsec = (long(xsec*10L^(precision+1)))/10.0d^(precision+1)

     secfmt = '(F0' + string( 3+precision+1,'(I1)' ) + '.' + $
                     string(   precision+1,'(I1)' ) + ')'
     result = string(ihr,'(I3.2)') + string(imin,'(I3.2)') + ' ' +$
              strtrim(string(xsec,secfmt),2) + '  ' 
    if N_elements(precision) EQ 0 then precision = 1

  endif else begin

     x = sixty(dec)
     if N_elements(precision) EQ 0 then precision = 1
     ideg = fix(x[0]) & imn = fix(x[1]) & xsc = x[2]
     result = ''

  endelse

   imn = abs(imn)  & xsc = abs(xsc)
   if ( precision EQ 0 ) then begin 
           secfmt = '(I03.2)' 
           if ~keyword_set(truncate) then begin
           xsc = round(xsc)
           carry = where(xsc EQ 60, Ncarry)
           if Ncarry GT 0 then begin                 ;Updated April 2002
                  xsc[carry] = 0
                  imn[carry] = imn[carry] + 1
           endif
           endif
   endif else begin

         secfmt = '(F0' + string( 3+precision,'(I1)') + '.' + $
                         string(   precision,'(I1)') + ')'
			 
         if ~keyword_set(truncate) then begin
         ixsc = fix(xsc + 0.5/10^precision)
         carry = where(ixsc GE 60, Ncarry)
         if Ncarry GT 0 then begin
             xsc[carry] = 0.
             imn[carry] = imn[carry] + 1
         endif
         endif else $
              xsc = (long(xsc*10^precision))/10.0d^precision
  endelse

   pos = dec GE 0 
   carry = where(imn EQ 60, Ncarry)
   if Ncarry GT 0  then begin
       ideg[carry] = ideg[carry] -1 + 2*pos[carry]
        imn[carry] = 0
   endif
 
   deg = string(ideg,'(I+3.2)')
   big = where(abs(ideg) ge 100, Nbig)
   if Nbig GT 0 then deg[big] = string(ideg[big],'(I+4.3)')
   zero = where(ideg EQ 0, Nzero)
   if Nzero GT 0 then begin
       negzero = where( dec[zero] LT 0, Nneg)
       if Nneg GT 0 then deg[zero[negzero]] = '-00' 
    endif

    
   return, result + deg + string(imn,'(I3.2)') + ' ' +  $
            strtrim(string(xsc,secfmt),2)

   end
