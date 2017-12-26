FUNCTION query_irsa_cat, targetname_OR_coords, catalog=catalog, radius=radius, radunits=radunits, outfile=outfile, change_null=change_null, table_col_info = table_col_info, table_hdr=table_hdr, DEBUG=debug

;+
; NAME: 
;    QUERY_IRSA_CAT
;
; PURPOSE: 
;    Query a catalog in the NASA/IPAC Infrared Science Archive (IRSA)
;    by position or resolvable target name.
; 
; EXPLANATION:
;    Uses the IDLnetURL object to do a spatial cone search of a
;    catalog in the IRSA (https://irsa.ipac.caltech.edu/) database.  
;    Returns the table data in an array of structures.  Allows an 
;    additional structure with table column information (required 
;    to write the table later), and also a string array of headers.  If 
;    outfile is set, it saves the table as an IPAC table file.  This 
;    can be slow for large query results, so write only if needed.    
;     
; CALLING SEQUENCE: 
;    info = query_irsa_cat(targetname_or_coords, [catalog=catalog,
;    radius=radius, radunits=radunits, outfile=outfile,
;    change_null=change_null, table_col_info=table_col_info, 
;    table_hdr=table_hdr, /DEBUG])
;
; REQUIRED INPUT: 
;
;    TARGETNAME_OR_COORDS - Either a string giving a resolvable target
;           name (with J2000 coordinates determined by NED or SIMBAD), or a 
;           2-element numeric vector giving the J2000 right ascension 
;           and declination, both in degrees.
;
; OPTIONAL INPUT:
;
;    CATALOG - string giving the identifier of the IRSA catalog to be
;           searched.  The complete list of catalogs and identifier
;           strings is available in XML format at:
;             https://irsa.ipac.caltech.edu/cgi-bin/Gator/nph-scan?mode=xml
;           or as an IPAC Table (ascii) at:
;             https://irsa.ipac.caltech.edu/cgi-bin/Gator/nph-scan?mode=ascii
;
;           The identifier string is under "catname".
;
;           If this keyword is omitted, the program will query the 2MASS Point
;           Source Catalog ('fp_psc').
;
;           Examples of current IRSA catalogs include:  
;              'wise_allwise_p3as_psd' - AllWISE Source Catalog
;              'fp_psc' - 2MASS Point Source Catalog
;              'iraspsc' - IRAS Point Source Catalog v2.1 (PSC)
;              'irasfsc' - IRAS Faint Source Catalog v2.0
;              'cosmos_ib_phot' - COSMOS Intermediate and Broad Band Photometry Catalog 2008
;
;    RADIUS - scalar input of the radius of the search with default of
;           60 (arcsec). IRSA catalogs have maximum allowable search 
;           radii.  These are listed on the corresponding web
;           interface page for the catalog search, or in the
;           nph-scan return table under "coneradius".
;    
;    RADUNITS - string giving the units of the radius (default is 'arcsec').
;    
;    OUTFILE - if present, the query result is written to this filename.
;
;    CHANGE_NULL - a numeric value (input as integer) to put in the
;           structure if the table uses a string for null
;           integer values (default is -9999).
;
;    TABLE_COL_INFO - An IDL structure with table column headers.
;           (required if table is written with write_ipac_table).
;
;    TABLE_HDR - An string array with header comments and keywords.
;
;    DEBUG - /DEBUG provides some additional output.
;
; OUTPUT: 
;    info - An IDL structure with the query result rows.  The structure
;           tags are the catalog column names, modified as needed for IDL.
;
;           If the query fails or is invalid, the return value is -1.  
;
; EXAMPLES: 
;           (1) Plot a histogram of the J magnitudes of all 2MASS
;               point sources within 10 arcminutes of the center of the
;               globular cluster M13.  Save the IPAC table. 
;
;             IDL> info = query_irsa_cat('m13',radius=10,radunits='arcmin',outfile='save.tbl')
;             IDL> help,/struct,info
;             IDL> plothist,info.j_m,xran=[10,20]
;
;           (2) Find the position of the faintest IRAS 60 micron
;               source within 1 degree of central position of the
;               COSMOS survey (10h00m28.6s +02d12m21.0s in J2000)
;  
;             IDL> info = query_irsa_cat([150.11917,2.205833], catalog='irasfsc', radius=1, radunits='deg')
;             IDL> help,/struct,info
;             IDL> idx = where(info.fnu_60 eq min(info.fnu_60))
;             IDL> print, (info.ra)[idx], (info.dec)[idx]
;
; PROCEDURES USED:
;    READ_IPAC_VAR  comes with query_irsa_cat.pro
;    VALID_NUM  from IDLastro
;
; NOTES:
;    The program writes an output IPAC table file only if the
;    OUTFILE keyword is set.
;
; MODIFICATION HISTORY:
;    Adapted from queryvizier.pro - H. Teplitz, IPAC  September 2010
;    Removed requirement of writing/reading IPAC table file -
;      T. Brooke, IPAC May 2011
;    Longer timeout for webget, added change_null - TYB June 2013
;    Added status message - TYB Jan 2016
;    Replace webget with IDLnetURL and redo structues - TYB Aug 2017
;-

;Copyright © 2013, California Institute of Technology
;All rights reserved. Based on Government Sponsored Research NAS7-03001 and NNN12AA01C.
;
;
;Redistribution and use in source and binary forms, with or without
;modification, are permitted provided that the following conditions
;are met:
;
; *  Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
;
; *  Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in
;    the documentation and/or other materials provided with the
;    distribution.
;
; *  Neither the name of the California Institute of Technology
;    (Caltech) nor the names of its contributors may be used to
;    endorse or promote products derived from this software without
;    specific prior written permission.
;
;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
;OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
;WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;POSSIBILITY OF SUCH DAMAGE.
;

on_error,2
compile_opt idl2

if N_params() lt 1 then begin
  print,'Syntax - info = query_irsa_cat(targetname_or_coords,'
  print,'           [catalog=catalog,radius=radius,radunits=radunits,'
  print,'            outfile=outfile,change_null=change_null,'
  print,'            table_col_info=table_col_info,table_hdr=table_hdr,'
  print,'            /DEBUG])'
endif

IF NOT(keyword_set(radius)) THEN radius = 60
IF NOT(keyword_set(radunits)) THEN radunits = 'arcsec'

IF (keyword_set(outfile)) THEN BEGIN
  writefile=outfile
  check = file_search(writefile)
  IF check NE '' THEN BEGIN
    print, 'OUTFILE exists.  Delete it [y/n]?  '
    c2 = get_kbrd(1)
    IF c2 EQ 'y' OR c2 EQ 'Y' THEN spawn, 'rm '+writefile $
    ELSE return, -1
  ENDIF
ENDIF 

IF ( keyword_set(change_null) ) THEN BEGIN 
  IF ( NOT(valid_num(change_null,/integer)) ) THEN BEGIN
    print, 'ERROR: change null value must be integer.'
    return, -1
  ENDIF
  null_num = change_null
ENDIF 

;;;;;;;;;;;;;;;;;;;  CONSTRUCT THE PARTS OF THE QUERY STRING

query_host = 'irsa.ipac.caltech.edu'
query_path = '/cgi-bin/Gator/nph-query'

;;;; CATALOG STRING

IF keyword_set(catalog) THEN catalog_name=catalog ELSE catalog_name='fp_psc'

catstr='&catalog='+catalog_name

;;;; OBJECT STRING

target = targetname_OR_coords

IF N_elements(target) EQ 2 THEN BEGIN 
   ra = double(target[0])
   dec = double(target[1])
   objstr = '&objstr='+strn(ra)+'+'+strn(dec)
ENDIF $
ELSE BEGIN 
   object = repstr(target,'+','%2B')
   object = repstr(strcompress(object),' ','+')
   objstr = '&objstr='+object
ENDELSE 

; No empty string
IF strlen(objstr) le 8 THEN BEGIN
  print, 'Empty object string not allowed.'
  return, -1
ENDIF

;;;;  SEARCH SHAPE AND SIZE

spatial_str='Cone' 
spatial_param_name=['radius','radunits']
spatial_param_value_str = [strn(radius), radunits]

nspat = n_elements(spatial_param_name)

spatstr = '&spatial='+spatial_str
spatparstr = ''

FOR i = 0l, nspat-1 DO $
   spatparstr=spatparstr+'&'+spatial_param_name[i]+'='+spatial_param_value_str[i]
      
;;;; USE IPAC FORMAT

out_fmt = 'outfmt=1'

;;;; combine into query string

url_q = out_fmt+objstr+spatstr+spatparstr+catstr
IF keyword_set(debug) THEN print, url_q

;;;;;  use IDLnetURL to do the GET

IF keyword_set(debug) THEN print, systime(0) 

print, 'Query ',catalog_name, ' for object ', target

oURL = obj_new('IDLnetURL')
oURL -> SetProperty, URL_Scheme='https', URL_hostname=query_host, $
                    URL_query=url_q, URL_Path=query_path
url_return = oURL -> GET(/STRING_ARRAY)      

IF keyword_set(debug) THEN print, systime(0) 

;;;;;  If requested, write the output to the outputfile

IF (keyword_set(outfile)) THEN BEGIN
  n = N_ELEMENTS(url_return)
  OPENW, wunit, writefile, /get_lun
  FOR i = 0l, n-1 DO PRINTF, wunit, url_return[i]
  FREE_LUN, wunit
ENDIF

;;;;; read the IPAC query into structures

IF (keyword_set(change_null)) THEN $
  info = read_ipac_var(url_return, table_col_info=table_col_info, table_hdr=table_hdr, change_null = null_num) $
ELSE $
  info = read_ipac_var(url_return, table_col_info=table_col_info, table_hdr=table_hdr)

IF (n_tags(info) eq 0) THEN BEGIN
  print,'ERROR: unable to read results into structure.'
  return, -1
ENDIF

return, info

END
