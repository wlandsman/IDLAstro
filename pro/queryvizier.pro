function Queryvizier, catalog, target, dis, VERBOSE=verbose, CANADA = canada, $
               CONSTRAINT = constraint, ALLCOLUMNS=allcolumns, SILENT=silent
;+
; NAME: 
;   QUERYVIZIER
;
; PURPOSE: 
;   Query any catalog in the Vizier database by position
; 
; EXPLANATION:
;   Uses the IDL SOCKET command to provide a positional query of any catalog 
;   in the the Vizier (http://vizier.u-strasbg.fr/) database over the Web and
;   return results in an IDL structure.    
; 
;    
; CALLING SEQUENCE: 
;     info = QueryVizier(catalog, targetname_or_coords, [ dis
;                        /ALLCOLUMNS, /CANADA, CONSTRAINT= ,/VERBOSE ])
;
; INPUTS: 
;      CATALOG - Scalar string giving the name of the VIZIER catalog to be
;            searched.    The complete list of catalog names is available at
;            http://vizier.u-strasbg.fr/vizier/cats/U.htx . 
;
;            Popular VIZIER catalogs include 
;            'II/328'- AllWISE Data Release (Cutri+ 2013)
;            'V/139' - Sloan SDSS photometric catalog Release 9 (2012)
;            '2MASS-PSC' - 2MASS point source catalog (2003)
;            'GSC2.3' - Version 2.3.2 of the HST Guide Star Catalog (2006)
;            'USNO-B1' - Verson B1 of the US Naval Observatory catalog (2003)
;            'UCAC4'  - 4th U.S. Naval Observatory CCD Astrograph Catalog (2012)
;            'B/DENIS/DENIS' - 2nd Deep Near Infrared Survey of southern Sky (2005)
;            'I/259/TYC2' - Tycho-2 main catalog (2000)
;            'I/311/HIP2' - Hipparcos main catalog, new reduction (2007)
;
;          Note that some names will prompt a search of multiple catalogs
;          and QUERYVIZIER will only return the result of the first search.
;          Thus, setting catalog to "HIP2" will search all catalogs 
;          associated with the Hipparcos mission, and return results for the
;          first catalog found.    To specifically search the Hipparcos or
;          Tycho main catalogs use the VIZIER catalog names listed above
;                             
;      TARGETNAME_OR_COORDS - Either a scalar string giving a target name, 
;          (with J2000 coordinates determined by SIMBAD), or a 2-element
;          numeric vector giving the J2000 right ascension in *degrees* and 
;          the target declination in degrees.
;          If the targetname is set to 'NONE' then QUERYVIZIER will perform
;          an all-sky search using the constraints given in the CONSTRAINT
;          keyword.   
; OPTIONAL INPUT:
;    dis - scalar or 2-element vector.   If one value is supplied then this
;          is the search radius in arcminutes.     If two values are supplied
;          then this is the width (i.e., in longitude direction) and height
;          of the search box.   Default is a radius search with radius of
;          5 arcminutes
;
; OUTPUTS: 
;   info - Anonymous IDL structure containing information on the catalog  
;          sources within the specified distance of the specified center.  The 
;          structure tag names are identical with the VIZIER  catalog column 
;          names, with the exception of an occasional underscore
;          addition, if necessary to convert the column name to a valid 
;          structure tag.    The VIZIER Web  page should consulted for the 
;          column names and their meaning for each particular catalog..
;           
;          If the tagname is numeric and the catalog field is blank then either
;          NaN  (if floating) or -1 (if integer) is placed in the tag.
;
;          If no sources are found within the specified radius, or an
;          error occurs in the query then -1 is returned. 
; OPTIONAL KEYWORDS:
;          /ALLCOLUMNS - if set, then all columns for the catalog are returned
;                 The default is to return a smaller VIZIER default set. 
;
;          /CANADA - By default, the query is sent to the main VIZIER site in
;            Strasbourg, France.   If /CANADA is set then the VIZIER site
;            at the Canadian Astronomical Data Center (CADC) is used instead.
;            Note that not all Vizier sites have the option to return
;            tab-separated values (TSV) which is required by this program.
;   
;          CONSTRAINT - string giving additional nonpositional numeric 
;            constraints on the entries to be selected.     For example, when 
;            in the GSC2.3  catalog, to only select sources with Rmag < 16 set 
;            Constraint = 'Rmag<16'.    Multiple constraints can be 
;            separated by commas.    Use '!=' for "not equal", '<=' for smaller
;            or equal, ">=" for greater than or equal.  See the complete list
;            of operators at  
;                 http://vizier.u-strasbg.fr/doc/asu.html#AnnexQual
;            For this keyword only, **THE COLUMN NAME IS CASE SENSITIVE** and 
;            must be written exactly as displayed on the VIZIER Web page.  
;            Thus for the GSC2.3 catalog one must use 'Rmag' and not 'rmag' or
;            'RMAG'.    In addition, *DO NOT INCLUDE ANY BLANK SPACE* unless it 
;            is a necessary part of the query.
;         
;           /SILENT - If set, then no message will be displayed if no sources
;                are found.    Error messages are still displayed.
;           /VERBOSE - If set then the query sent to the VIZIER site is
;               displayed, along with the returned title(s) of found catalog(s)
; EXAMPLES: 
;          (1) Plot a histogram of the J magnitudes of all 2MASS point sources 
;          stars within 10 arcminutes of the center of the globular cluster M13 
;
;          IDL>  info = queryvizier('2MASS-PSC','m13',10)
;          IDL> plothist,info.jmag,xran=[10,20]
;
;          (2)  Find the brightest J mag GSC2.3 source within 3' of the 
;               J2000 position ra = 10:12:34, dec = -23:34:35
;          
;          IDL> str = queryvizier('GSC2.3',[ten(10,12,34)*15,ten(-23,34,35)],3)
;          IDL> print,min(str.jmag,/NAN)
;
;          (3) Find sources with V < 19 in the Magellanic Clouds Photometric 
;              Survey (Zaritsky+, 2002) within 5 arc minutes of  the position 
;              00:47:34 -73:06:27
;
;              Checking the VIZIER Web page we find that this catalog is
;          IDL>  catname =  'J/AJ/123/855/table1'
;          IDL>  ra = ten(0,47,34)*15 & dec = ten(-73,6,27)
;          IDL> str = queryvizier(catname, [ra,dec], 5, constra='Vmag<19')
;
;          (4) Perform an all-sky search of the Tycho-2 catalog for stars with
;              BTmag = 13+/-0.1
;
;         IDL> str = queryvizier('I/259/TYC2','NONE',constrain='BTmag=13+/-0.1')
;
; PROCEDURES USED:
;          GETTOK(), REMCHAR, REPSTR(), STRCOMPRESS2(), WEBGET()
; TO DO:
;       (1) Allow specification of output sorting
; MODIFICATION HISTORY: 
;         Written by W. Landsman  SSAI  October 2003
;         Give structure name returned by VIZIER not that given by user
;                    W. Landsman   February 2004 
;         Don't assume same format for all found sources W. L. March 2004
;         Added CONSTRAINT keyword for non-positional constraints WL July 2004
;         Remove use of EXECUTE() statement WL June 2005
;         Make dis optional as advertised WL August 2005
;         Update for change in Vizier output format WL February 2006
;         Fix problem in Feb 2006 update when only 1 object found
;                     WL/D.Apai        March 2006
;         Accept 'E' format for floating point. M. Perrin April 2006
;         Added /ALLCOLUMNS option to return even more data.  M. Perrin, May 2006
;         Return anonymous structure W. Landsman  May 2006
;         Removed V6.0 notation to restore V5 compatibility W.Landsman July2006
;         Accept target='NONE' for all-sky search, allow '+/-' constraints
;                W. Landsman  October 2006
;         Use HTTP 1.0 protocol in call to webget.pro
;         Use vector form of IDL_VALIDNAME if V6.4 or later W.L. Dec 2007
;         Update Strasbourg Web address for target name W.L. 3 March 2008
;         Also update Web address for coordinate search W.L. 7 March 2008 
;         Allow for 'D' specification format  R. Gutermuth/W.L.  June 2008
;         Allow for possible lower-case returned formats  W.L. July 2008
;         Use STRCOMPRESS2()to remove blanks around operators in constraint
;              string  W.L.  August 2008
;         Added /SILENT keyword  W.L.  Jan 2009
;         Avoid error if output columns but not data returned W.L. Mar 2010
;         Ignore vector tags (e.g. SED spectra) W.L.   April 2011
;         Better checking when more than one catalog returned W.L. June 2012
;         Assume since IDL V6.4 W.L. Aug 2013
;         Update HTTP syntax for /CANADA    W. L.  Feb 2014
;-
  On_error,2
  compile_opt idl2
  if N_params() LT 2 then begin
       print,'Syntax - info = QueryVizier(catalog, targetname_or_coord, dis,'
       print,'         [/ALLCOLUMNS, /SILENT, /VERBOSE, /CANADA, CONSTRAINT= ]'
       print,'                       '
       print,'  Coordinates (if supplied) should be J2000 RA (degrees) and Dec'
       print,'  dis -- search radius or box in arcminutes'
       if N_elements(info) GT 0 then return,info else return, -1
  endif

 if keyword_set(canada) then root = "http://vizier.hia.nrc.ca/viz-bin/" $
                       else  root = "http://webviz.u-strasbg.fr/viz-bin/" 
 silent = keyword_set(silent)
 
  if N_elements(catalog) EQ 0 then $
            message,'ERROR - A catalog name must be supplied as a keyword'
  targname = 0b
 if N_elements(dis) EQ 0 then dis = 5
 if min(dis) LE 0 then $
     message,'ERROR - Search distances must be greater than zero'
 
 nopoint = 0b
 if N_elements(dis) EQ 2 then $
    search = "&-c.bm=" + strtrim(dis[0],2) + '/' + strtrim(dis[1],2) else $
    search = "&-c.rm=" + strtrim(dis,2) 
    if N_elements(target) EQ 2 then begin
      ra = float(target[0])
      dec = float(target[1])
   endif else begin
       nopoint = strupcase( strtrim(target,2) ) EQ 'NONE' 
       object = repstr(target,'+','%2B')
        object = repstr(strcompress(object),' ','+')
       targname = 1b 
  endelse

; Add any additional constraints to the search. Convert any URL special 
; special characters in the constraint string.

 if N_elements(constraint) EQ 0 then constraint = '' 
 if strlen(constraint) GT 0 then begin
     urlconstrain = strtrim(constraint,2)
     urlconstrain = strcompress2(constraint,['<','>','='])
      urlconstrain = repstr(urlconstrain, ',','&')
     urlconstrain = repstr(urlconstrain, '<','=%3C')
     urlconstrain = repstr(urlconstrain, '>','=%3E')
     urlconstrain = repstr(urlconstrain, '+','%2B')
     urlconstrain = repstr(urlconstrain, '/','%2F')
     urlconstrain = repstr(urlconstrain, '!','=!')
     if nopoint then search = urlconstrain else $
                     search = search + '&' + urlconstrain
 endif
 ;
 if nopoint then $
  QueryURL = root + "asu-tsv/?-source=" + catalog + '&' + $
              search + '&-out.max=unlimited' else $
 if targname then $
  QueryURL = $
   root + "asu-tsv/?-source=" + catalog + $
     "&-c=" + object + search + '&-out.max=unlimited' else $
  queryURL = $
   root + "asu-tsv/?-source=" + catalog + $
       "&-c.ra=" + strtrim(ra,2) + '&-c.dec=' + strtrim(dec,2) + $
       search + '&-out.max=unlimited'

  if keyword_set(allcolumns) then queryURL = queryURL + '&-out.all=1'
 if keyword_set(verbose) then message,queryurl,/inf

  Result = webget(QueryURL,/http10, silent=silent)
;
  t = strtrim(result.text,2)
  keyword = strtrim(strmid(t,0,7),2)

  linecon = where(keyword EQ '#---Lis', Ncon)
  if Ncon GT 0 then remove,linecon, t, keyword
 
; Check to see if more than one catalog has been searched
; Use only the first catalog found

  rcol = where(keyword Eq '#RESOUR', Nfound) 
  if N_elements(rcol) GT 1 then begin 
       t = t[0:rcol[1]-1 ]
       keyword = keyword[0:rcol[1]-1]
  endif     
  lcol = where(keyword EQ "#Column", Nfound)
  if Nfound EQ 0 then begin
       if max(strpos(strlowcase(t),'errors')) GE 0 then begin 
            message,'ERROR - Unsuccessful VIZIER query',/CON 
            print,t
       endif else if ~silent then $
            message,'No sources found within specified radius',/INF
       return,-1
  endif
  

  if keyword_set(verbose) then begin
    titcol = where(keyword EQ '#Title:', Ntit)
        if Ntit GT 0 then message,/inform, $
        strtrim(strmid(t[titcol[0]],8),2)
  endif
;Check if any Warnings or fatal errors in the VIZIER output
   badflag = strmid(keyword,0,5)
   warn = where(badflag EQ '#++++', Nwarn)
   if Nwarn GT 0 then for i=0,Nwarn-1 do $
        message,'Warning: ' + strtrim(t[warn[i]],2),/info
   
   fatal = where(badflag EQ '#****', Nfatal)
   if Nfatal GT 0 then for i=0,Nfatal-1 do $
        message,'Error: ' + strtrim(t[fatal[i]],2),/info


  trow = t[lcol]
  dum = gettok(trow,' ')
  colname = gettok(trow,' ')
  fmt = gettok(trow,' ')

  remchar,fmt,'('
  remchar,fmt,')' 
  remchar,colname,')'
  colname = IDL_VALIDNAME(colname,/convert_all)
 
; Find the vector tags (Format begins with a number) and remove them 

 bad = where(stregex(fmt,'^[0-9]') GE 0, Nbad)
 if Nbad GT 0 then remove,bad,fmt,colname 
 
 ntag = N_elements(colname)
 fmt = strupcase(fmt)
 val = fix(strmid(fmt,1,4))
 
 for i=0,Ntag-1 do begin

 case strmid(fmt[i],0,1) of 
 
  'A': cval = ' '
  'I': cval = (val[i] LE 4) ? 0 : 0L         ;16 bit integer if 4 chars or less
  'F': cval = (val[i] LE 7) ? 0. : 0.0d      ;floating point if 7 chars or less
   'E': cval = (val[i] LE 7) ? 0. : 0.0d 
  'D': cval = (val[i] LE 7) ? 0. : 0.0d 
   else: message,'ERROR - unrecognized format ' + fmt[i]
 
  endcase

   if i EQ 0 then   info = create_struct(colname[0], cval) else begin
	   ; If you set the /ALLCOLUMNS flag, in some cases (2MASS) you
	   ; get a duplicate column name. Check for this and avoid it by appending
	   ; an extra bit to the duplicate name
	   if where(tag_names(info) eq strupcase(colname[i])) ge 0 then $
	      colname[i] = colname[i] + '_2'
   info =  create_struct(temporary(info), colname[i],cval)
   endelse
 endfor
 
  i0 = max(lcol) + 4  
  if i0 GT (N_elements(t)-1) then begin 
       message,'No sources found within specified radius',/INF
       return,-1
  endif
  
  iend = where( t[i0:*] EQ '', Nend)
  if Nend EQ  0  then iend = N_elements(t) else iend = iend[0] + i0
  nstar = iend - i0 
  info = replicate(info, nstar)

; Find positions of tab characters 
  t = t[i0:iend-1]

  for j=0,Ntag-1 do begin
      x = strtrim( gettok(t,string(9b),/exact ),2)
       dtype = size(info[0].(j),/type)
       if dtype NE 7 then begin
       bad = where(strlen(x) EQ 0, Nbad)
      if (Nbad GT 0) then $
           if (dtype EQ 4) || (dtype EQ 5) then x[bad] = 'NaN' $
                                           else x[bad] = -1
      endif
      info.(j) = x 
   endfor
 return,info
END 
  

