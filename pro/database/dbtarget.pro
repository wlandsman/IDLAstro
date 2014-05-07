function dbtarget, target, radius, sublist,SILENT=silent, $
                 TO_B1950 = to_B1950, DIS = dis
;+
; NAME:
;      DBTARGET
; PURPOSE:
;      Find sources in a database within specified radius of specified target
; EXPLANATION:
;      Uses QuerySimbad to translate target name to RA and Dec, and then uses
;      DBCIRCLE() to find any entries within specified radius.   Database must 
;      include items named 'RA' (in hours) and 'DEC' (in degrees) and must 
;      have previously been opened with DBOPEN
;
; CALLING SEQUENCE:
;     list = DBTARGET(target, [radius, sublist, /SILENT, DIS= ,/TO_B1950 ] )   
;
; INPUTS:
;      TARGET - A scalar string giving an astronomical target name, which 
;          will be  translated into J2000 celestial coordinates by QuerySimbad 
;
; OPTIONAL INPUT:
;       RADIUS - Radius of the search field in arc minutes, scalar.
;                Default is 5 arc minutes
;       SUBLIST - Vector giving entry numbers in currently opened database
;               to be searched.  Default is to search all entries
;
; OUTPUTS:
;     LIST - Vector giving entry numbers in the currently opened catalog
;            which have positions within the specified search circle
;            LIST is set to -1 if no sources fall within the search circle
;            !ERR is set to the number sources found.
;
; OPTIONAL OUTPUT
;       DIS -  The distance in arcminutes of each entry specified by LIST
;               to the search center specified by the target.
;
; OPTIONAL KEYWORD INPUT:
;       /SILENT - If this keyword is set, then DBTARGET will not print the 
;               number of entries found at the terminal
;       /TO_B1950 - If this keyword is set, then the SIMBAD J2000 coordinates 
;               are converted to B1950 before searching the database
;               NOTE: The user must determine on his own whether the database
;               is in B1950 or J2000 coordinates.
;
; RESTRICTIONS;
;       The database must have items 'RA' (in hours) and 'DEC' (in degrees).
;       Alternatively, the database could have items RA_OBJ and DEC_OBJ 
;      (both in degrees)
; EXAMPLE:
;       (1) Use the HST_CATALOG database to find all  HST observations within 
;           5' (the default) of M33
;
;       IDL> dbopen,'hst_catalog'
;       IDL> list = dbtarget('M33')
;
;      (2) As above but restrict targets within 2' of the nucleus using the
;          WFPC2 camara
;
;       IDL> dbopen,'hst_catalog'
;       IDL> sublist = dbfind('config=WFPC2')
;       IDL> list = dbtarget('M33',2,sublist)
;
;
; PROCEDURE CALLS:
;       QuerySimbad, DBCIRCLE()
; REVISION HISTORY:
;      Written W. Landsman     SSAI          September 2002
;      Propagate /SILENT keyword to QuerySimbad    W. Landsman Oct 2009
;      Make sure a database is open  W.L. Oct 2010
;-                   
 On_error,2

 if N_params() LT 1 then begin
    print,'Syntax - list = DBTARGET( targetname_or_coord, [radius, sublist  '
    print,'                           DIS =, /SILENT, /TO_B1950 ] )'
    if N_elements(sublist) GT 0 then return, sublist else return,lonarr(1)-1
 endif
 
  if ~db_info('open') then message,'ERROR - No database open'

  QuerySimbad, target, ra,dec, Found = Found,Silent=silent
  if found EQ 0 then message,'Target name ' + target + $
  	     ' could not be translated by SIMBAD'
  ra = ra/15.
 
 if N_elements(radius) EQ 0 then radius = 5
 if n_elements(sublist) EQ 0 then $
 return, dbcircle(ra, dec, radius, dis, SILENT=silent, $
                   TO_B1950 = to_b1950 )
 return, dbcircle(ra, dec, radius, dis, sublist, SILENT=silent, $
                   TO_B1950 = to_b1950 )
  
 end
