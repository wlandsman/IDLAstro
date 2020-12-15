PRO QuerySimbad, name, ra, de, id, Found = found, NED = ned, ERRMSG = errmsg, $
    Verbose = verbose, CFA=cfa, Server=server, SILENT=silent, $
    Print = print,Vmag=Vmag,Jmag=Jmag,Hmag=Hmag,Kmag=Kmag,parallax=parallax
;+
; NAME: 
;   QUERYSIMBAD
;
; PURPOSE: 
;   Query the SIMBAD/NED/Vizier astronomical name resolver to obtain coordinates
;
; EXPLANATION: 
;   Uses the IDLnetURL object to query either the SIMBAD or NED nameserver 
;   over the Web to return J2000 coordinates.  By default, QuerySimbad 
;   first queries the Simbad database, then (if no match found) the NED 
;   database, and then the Vizier database.
;    
;   For details on the SIMBAD service, see http://simbad.u-strasbg.fr/Simbad 
;   and for the NED service, see http://ned.ipac.caltech.edu/
;
; CALLING SEQUENCE: 
;    QuerySimbad, name, ra, dec, [ id, Found=, /NED, /CFA, ERRMSG=, /VERBOSE]
;        /PRINT, Vmag=V, Jmag=J, Hmag=H, Kmag=Kmag, parallax=parallax
;
; INPUTS: 
;    name - a scalar string containing the target name in SIMBAD (or NED)
;           nomenclature. For SIMBAD details see
;           http://vizier.u-strasbg.fr/cgi-bin/Dic-Simbad .
;
; OUTPUTS: 
;     ra -  Right ascension of the target in J2000.0 in *degrees*, scalar 
;     dec - declination of the target in degrees, scalar
;
; OPTIONAL INPUT KEYWORD:
;     /CFA - if set, then use the Simbad server at the Center for Astrophysics
;             rather than the default server in Strasbourg, France.
;     ERRMSG   = If defined and passed, then any error messages will be
;                  returned to the user in this parameter rather than
;                  depending on the MESSAGE routine in IDL.  If no errors are
;                  encountered, then a null string is returned. 
;     /NED - if set, then only the nameserver of the NASA Extragalactic database
;            is used to resolve the name and return coordinates.   Note that
;           /NED cannot be used with Galactic objects
;     /VERBOSE - If set, then the HTTP-GET command is displayed
;     /PRINT - if set, then output coordinates are displayed at the terminal 
;            By default, the coordinates are displayed if no output parameters
;           are supplied to QUERYSIMBAD
;     /SILENT - If set, then don't print warnings if multiple SIMBAD objects
;             correspond to the supplied name.
; OPTIONAL OUTPUT: 
;     id - the primary SIMBAD (or NED) ID of the target, scalar string
;          As of June 2009, a more reliable ID seems to be found when using 
;          CFA (/CFA) server.
;
; OPTIONAL KEYWORD OUTPUTS:
;     found - set to 1 if the translation was successful, or to 0 if the
;           the object name could not be translated by SIMBAD or NED
;     Errmsg - if supplied, then any error messages are returned in this
;            keyword, rather than being printed at the terminal.   May be either
;            a scalar or array.
;     Server - Character indicating which server was actually used to resolve
;           the object, 'S'imbad, 'N'ed or 'V'izier
;     Vmag - supply to receive the SIMBAD V magnitude 
;     Jmag - supply to receive the SIMBAD J magntiude
;     Hmag - supply to receive the SIMBAD H magnitude 
;     Kmag - supply to receive the SIMBAD K magnitude 
;     Parallax - supply to receive the SIMBAD parallax in milliarcseconds
;            
; EXAMPLES:
;     (1) Display the J2000 coordinates for the ultracompact HII region
;         G45.45+0.06 
;
;      IDL> QuerySimbad,'GAL045.45+00.06'
;           ===>19 14 21.30  +11 09 13.0
; PROCEDURES USED:
;       REPSTR(), ADSTRING()
; NOTES:
;     The actual  query is made to the Sesame name resolver 
;     ( see http://cdsweb.u-strasbg.fr/doc/sesame.htx ).     The Sesame
;     resolver first searches the Simbad name resolver, then  NED and then
;     Vizier.   
; MODIFICATION HISTORY: 
;     Written by M. Feldt, Heidelberg, Oct 2001   <mfeldt@mpia.de>
;     Added option to use NED server, better parsing of SIMBAD names such as 
;          IRAS F10190+5349    W. Landsman  March 2003
;     Turn off extended name search for NED server, fix negative declination
;     with /NED    W. Landsman  April 2003
;     Use Simbad Sesame sever, add /Verbose, /CADC keywords 
;       B. Stecklum, TLS Tautenburg/ W. Landsman, Feb 2007
;    Update NED query to account for new IPAC format, A. Barth  March 2007
;    Update NED query to account for another new IPAC format, A. Barth  
;                                                   July 2007
;     Update message when NED does not find object  W.L.  October 2008
;     Remove CADC keyword, add CFA keyword, warning if more than two
;         matches  W.L. November 2008 
;     Make NED queries through the Sesame server, add Server output 
;          keyword  W.L.  June 2009
;     Don't get primary name if user didn't ask for it  W.L. Aug 2009
;     Added /SILENT keyword W.L. Oct 2009
;     Added /PRINT keyword W.L.   Oct 2011
;     Added ability to get V, J, H, and K magnitudes as well as
;     a parallax - jswift, Jan 2014
;     Use IDLnetURL instead of WebGet()  W.L.  Oct. 2014
;-

  compile_opt idl2
  if N_params() LT 1 then begin
       print,'Syntax - QuerySimbad, name, ra, dec, [ id, ]'
       print,'                 Found=, /CFA, /NED, ERRMSG=, /VERBOSE]'
       print,'   Input - object name, scalar string'
       print,'   Output -  Ra, dec of object (degrees)'
       return
  endif
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
      Catch,/CANCEL
      void = cgErrorMsg(/Quiet)
      RETURN
      ENDIF   
  ;;
  fluxes = arg_present(Vmag) || arg_present(Jmag) || arg_present(Hmag) $
    || arg_present(Kmag)
  printerr = ~arg_present(errmsg)
  if ~printerr  then errmsg = ''

 host =  keyword_set(cfa) ? 'vizier.cfa.harvard.edu' : $
                            'cdsweb.u-strasbg.fr'

   path = "/viz-bin/nph-sesame/-oI
   if fluxes then path+='F'
    if keyword_set(NED) then path+='/N' else path+='/SNV'
 
  queryURL = repstr(strcompress(name,/remove),'+','%2B')
  ;;
  if keyword_set(verbose) then message,/INF,'http://' + host + path + '?' + queryURL
  
  oURL = obj_new('IDLnetURL')
  oURL-> SetProperty, URL_Scheme = 'http',URL_Host=host,URL_Query=QueryURL, $
                      URL_PATH=path
  result = oURL-> GET(/STRING_ARRAY)
  found = 0
  
  ;;
   if arg_present(server) then $ 
        server = strmid(result[1],2,1)
; look for J2000 coords
      prefix = strmid(Result,0,5)         

  idx=where(strmid(prefix,0,3) EQ '%J ',cnt)

  if cnt GE 1 then begin
      if cnt GT 1 then begin 
          if ~keyword_set(SILENT) then $
            message,/INF,'Warning - More than one match found for name '  + name
          idx = idx[0]
      endif 	
      found=1   
      ra = 0.0d & de = 0.0d
      reads,strmid(Result[idx],2),ra,de

      if N_params() GT 3 then begin 
      
                idx2= where(strpos(Result, '%I.0 ') ne -1,cnt)
          if cnt GT 0 then id = strtrim(strmid(Result[idx2],4),2) else $
            if ~keyword_set(SILENT) then $
            message,'Warning - could not determine primary ID',/inf 
      endif	    

      ; Get V mag if present
      vi = where(strpos(Result, '%M.V ') ne -1,vcnt)
      if vcnt GE 1 then reads,strmid(Result[vi],4),vmag

      ; Get J mag if present
      ji = where(strpos(Result, '%M.J ') ne -1,jcnt)
      if jcnt GE 1 then reads,strmid(Result[ji],4),jmag

      ; Get H mag if present
      hi = where(strpos(Result, '%M.H ') ne -1,hcnt)
      if hcnt GE 1 then reads,strmid(Result[hi],4),hmag

      ; Get K mag if present
      ki = where(strpos(Result, '%M.K ') ne -1,kcnt)
      if kcnt GE 1 then reads,strmid(Result[ki],4),kmag

      ; Get parallax if present
      plxi = where(strpos(Result, '%X ') ne -1,plxcnt)
      if plxcnt GE 1 then reads,strmid(Result[plxi],2),parallax
      
  ENDIF ELSE BEGIN 
      errmsg = ['No objects returned found.   The server answered:' , $
                 strjoin(result)]
      if printerr then begin
         message, errmsg[0], /info	
	 message,strjoin(result),/info
      endif	 
  ENDELSE
  if found GT 0 && ((N_params() LT 2) || keyword_set(print)) then $
       print,adstring(ra,de,1)
        
  
  return 
END 
  
