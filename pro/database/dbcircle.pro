function dbcircle, ra_cen, dec_cen, radius, dis, sublist,SILENT=silent, $
                TO_J2000 = to_J2000, TO_B1950 = to_B1950, GALACTIC= galactic, $
		COUNT = nfound
;+
; NAME:
;      DBCIRCLE
; PURPOSE:
;      Find sources in a database within specified radius of specified center
; EXPLANATION:
;      Database must include items named 'RA' (in hours) and 'DEC' (in degrees)
;      and must have previously been opened with DBOPEN
;
; CALLING SEQUENCE:
;     list = DBCIRCLE( ra_cen, dec_cen, [radius, dis, sublist, /SILENT, 
;                                /GALACTIC, TO_B1950, /TO_J2000, COUNT= ] )   
;
; INPUTS:
;       RA_CEN - Right ascension of the search center in decimal HOURS, scalar
;       DEC_CEN - Declination of the search center in decimal DEGREES, scalar
;               RA_CEN and DEC_CEN should be in the same equinox as the 
;               currently opened catalog.
;
; OPTIONAL INPUT:
;       RADIUS - Radius of the search field in arc minutes, scalar.
;               DBCIRCLE prompts for RADIUS if not supplied.
;       SUBLIST - Vector giving entry numbers in currently opened database
;               to be searched.  Default is to search all entries
;
; OUTPUTS:
;     LIST - Vector giving entry numbers in the currently opened catalog
;            which have positions within the specified search circle
;            LIST is set to -1 if no sources fall within the search circle
;
; OPTIONAL OUTPUT
;       DIS -  The distance in arcminutes of each entry specified by LIST
;               to the search center (given by RA_CEN and DEC_CEN)
;
; OPTIONAL KEYWORD INPUT:
;       /GALACTIC - if set, then the first two parameters are interpreted as
;                 Galactic coordinates in degrees, and is converted internally
;                 to J2000 celestial to search the database.   
;       /SILENT - If this keyword is set, then DBCIRCLE will not print the 
;               number of entries found at the terminal
;       /TO_J2000 - If this keyword is set, then the entered coordinates are
;               assumed to be in equinox B1950, and will be converted to
;               J2000 before searching the database
;       /TO_B1950 - If this keyword is set, then the entered coordinates are
;               assumed to be in equinox J2000, and will be converted to
;               B1950 before searching the database
;               NOTE: The user must determine on his own whether the database
;               is in B1950 or J2000 coordinates.
; OPTIONAL KEYWORD OUTPUT:
;       COUNT - - Integer scalar giving the number of valid matches
; METHOD:
;       A DBFIND search is first performed on a square area of given radius.
;       The list is the restricted to a circular area by using GCIRC to 
;       compute the distance of each object to the field center.
;
; RESTRICTIONS;
;       The database must have items 'RA' (in hours) and 'DEC' (in degrees).
;       Alternatively, the database could have items RA_OBJ and DEC_OBJ 
;      (both in degrees)
; EXAMPLE:
;       Find all Hipparcos stars within 40' of the nucleus of M33
;       (at J2000 1h 33m 50.9s 30d 39' 36.7'')
;
;       IDL> dbopen,'hipparcos'
;       IDL> list = dbcircle( ten(1,33,50.9), ten(3,39,36.7), 40)
;
; PROCEDURE CALLS:
;       BPRECESS, DBFIND(), DBEXT, DB_INFO(), GCIRC, GLACTC, JPRECESS
; REVISION HISTORY:
;      Written W. Landsman     STX           January 1990
;      Fixed search when crossing 0h         July 1990
;      Spiffed up code a bit     October, 1991
;      Leave DIS vector unchanged if no entries found W. Landsman July 1999
;      Use maximum declination, rather than declination at field center to
;      correct RA for latitude effect    W. Landsman   September 1999
;      Added COUNT, GALACTIC keywords  W. Landsman   December 2008
;      Fix problem when RA range exceeds 24h  W. Landsman   April 2009
;      Work as advertised for RA_OBJ field  W. Landsman June 2010
;      Fix occasional problem when crossing 0h  E. Donoso/W.Landsman Jan 2013
;      Check if database has been opened W. Landsman Aug 2013
;-                   
 On_error,2
 compile_opt idl2

 if N_params() LT 2 then begin
    print,'Syntax - list = ' + $
    'DBCIRCLE( ra[hours], dec[degrees], radius[arcmin], [ dis, sublist  '
    print,'               Count=, /GALACTIC, /SILENT, /TO_J2000, /TO_B1950 ] )'
    if N_elements(sublist) GT 0 then return, sublist else return,[-1L]
 endif

 if (N_elements(ra_cen) NE 1) || (N_elements(dec_cen) NE 1) then begin
    print, 'DBCIRCLE: ERROR - Expecting scalar RA and Dec parameters'
    if N_elements(sublist) GT 0 then return, sublist else return,[-1L]
 endif

 if N_params() LT 3 then read,'Enter search radius in arc minutes: ',radius

 nentries = db_info( 'ENTRIES',0 ) 
 if nentries EQ 0 then begin
    if ~keyword_set(SILENT) then message, $
        'ERROR - No entries in database ' + db_info("NAME",0),/INF
    if N_elements(sublist) GT 0 then return, sublist else return,[-1]
 endif   
 
 if keyword_set(TO_J2000) then begin
        jprecess,ra_cen*15.,dec_cen,racen,deccen 
        racen = racen[0]/15.    &       deccen = deccen[0]
 endif else  if keyword_set(TO_B1950) then begin
        bprecess,ra_cen*15.,dec_cen,racen,deccen 
        racen = racen[0]/15.    &       deccen = deccen[0]
 endif else if keyword_set(galactic) then begin 
         glactc,racen,deccen,2000,ra_cen*15,dec_cen,2   ;Convert from Galactic		
 endif else begin
        racen = ra_cen[0]    &  deccen = dec_cen[0]
 endelse

 size = radius/60.      ;Size of search field in degrees
 decmin = double(deccen-size) > (-90.)
 decmax = double(deccen+size) < 90.
 bigdec = max(abs([decmin, decmax]))
 items = strtrim(db_item_info('name'))
 g = where(items EQ 'RA', Ncount)
 if Ncount EQ 0 then begin 
      g = where(items EQ 'RA_OBJ', Ncount)
      if Ncount EQ 0 then message, $
               'ERROR - Database must have item named RA or RA_OBJ' else begin
	       sra = 'RA_OBJ' & sdec = 'DEC_OBJ'
	       endelse
 endif else begin 
      sra = 'RA' & sdec = 'DEC'
 endelse         	        
 
 if abs(bigdec) EQ 90 then rasize = 24 else $             ;Updated Sep 1999
       rasize = abs(size/(15.*cos(bigdec/!RADEG))) < 24.  ;Correct for latitude effect

 if 2*rasize gt 24. then begin         ;Only need search on Dec?
      st = string(decmin) + '<dec<' + string(decmax) 
      redo = 0
 endif else begin
 rmin = double(racen-rasize)
 rmax = double(racen+rasize)


;  If minimum RA is less than 0, or maximum RA is greater than 24
;  then we must break up into two searchs

 if rmax gt 24. then begin
        redo = 1
        newrmax = rmax - 24.
        newrmin = 0.
        rmax = 24.
 endif else if rmin lt 0 then begin
        redo = 1
        newrmin = 24. + rmin
        newrmax = 24.
        rmin = 0.
 endif else redo = 0
 if sra EQ 'RA_OBJ' then begin      ;Item RA_OBJ assumed to be in degrees
	       rmin = rmin*15.
	       rmax = rmax*15.
 endif 	       

 
 st = string(rmin) + '<' + sra + '<' + string(rmax) +',' + $
      string(decmin) + '<' + sdec + '<' + string(decmax) 
 endelse

 if N_params() LT 5 then list = dbfind( st, /SIL ) else $
                         list = dbfind( st, sublist, /SIL )

 if redo then begin
        st = string(newrmin) + '<' +sra + '<' + string(newrmax) + ',' + $
                string(decmin) + '<' + sdec + '< ' + string(decmax)
        if N_params() LT 5 then newlist = dbfind(st,/SIL) else $ 
                  newlist = dbfind(st,sublist,/SIL)
        if list[0] GT 0 then list = [ list, newlist ] else list = newlist
 endif

; Use GCIRC to compute angular distance of each source to the field center

 silent = keyword_set(SILENT)
 if ~silent then begin
      print,' ' & print,' '
  endif     

 if max(list) GT 0 then begin                         ;Any entries found?
        dbext, list, sra + ',' + sdec, ra_match, dec_match
	if sra EQ 'RA_OBJ' then ra_match = ra_match/15.
        gcirc,1, racen, deccen, ra_match, dec_match, ddis
        good = where( ddis/3600. LT size, Nfound )
        if Nfound GT 0 then begin
             dis = ddis[good]/60.
             if ~silent then $
                 print, Nfound, ' entries found in ',db_info('name',0)
             return, list[good] 
        endif 
 endif 

 if ~silent then $
       print,'No entries found by dbcircle in ', db_info( 'NAME',0 )
 Nfound  = 0      
 return,[-1L]
 
 end
