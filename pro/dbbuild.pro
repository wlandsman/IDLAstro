pro dbbuild,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18, $
    v19,v20,v21,v22,v23,v24,v25,v26,v27,v28,v29,v30,v31,v32,v33,v34,v35,v36, $
    v37,v38,v39,v40,v41,v42,v43,v44,v45,v46,v47,v48,v49,v50, $
    NOINDEX = noindex, STATUS=STATUS, SILENT=SILENT
;+
; NAME:
;	DBBUILD
; PURPOSE:
;	Build a database by appending new values for every item.  
; EXPLANATION:
;	The database must be opened for update (with DBOPEN) before calling 
;	DBBUILD.   
;
; CALLING SEQUENCE:
;	DBBUILD, [ v1, v2, v3, v4......v50, /NOINDEX, /SILENT, STATUS =  ]
;
; INPUTS:
;	v1,v2....v50 - vectors containing values for all items in the database.
;         V1 contains values for the first item, V2 for the second, etc.
;         The number of vectors supplied must equal the number of items
;         (excluding entry number) in the database.  The number of elements 
;         in each vector should be the same.   A multiple valued item
;         should be dimensioned NVALUE by NENTRY, where NVALUE is the number
;         of values, and NENTRY is the number of entries.
;
; OPTIONAL INPUT KEYWORDS:
;	/NOINDEX - If this keyword is supplied and non-zero then DBBUILD will
;             *not* create an indexed file.    Useful to save time if
;             DBBUILD is to be called several times and the indexed file need
;             only be created on the last call
;
;	/SILENT  - If the keyword SILENT is set and non-zero, then DBBUILD
;	      will not print a message when the index files are generated
;
; OPTIONAL OUTPUT KEYWORD:
;	STATUS - Returns a status code denoting whether the operation was
;	      successful (1) or unsuccessful (0).  Useful when DBBUILD is
;	      called from within other applications.
;
; EXAMPLE:
;	Suppose a database named STARS contains the four items NAME,RA,DEC, and 
;	FLUX.   Assume that one already has the four vectors containing the
;	values, and that the database definition (.DBD) file already exists.
;
;	IDL> !PRIV=2                  ;Writing to database requires !PRIV=2
;	IDL> dbcreate,'stars',1,1   ;Create database (.dbf) & index (.dbx) file
;	IDL> dbopen,'stars',1         ;Open database for update
;	IDL> dbbuild,name,ra,dec,flux ;Write 4 vectors into the database
;
; NOTES:
;	Do not call DBCREATE before DBBUILD if you want to append entries to
;	an existing database
;
;	DBBUILD checks that each value vector matches the idl type given in the
;	database definition (..dbd) file, and that character strings are the 
;	proper length. 
;
;   The database is closed after DBBUILD exits.
; PROCEDURE CALLS:
;       DBCLOSE, DBINDEX, DBXPUT, DBWRT, IS_IEEE_BIG()
; REVISION HISTORY:
;	Written          W. Landsman           March, 1989
;	Added /NOINDEX keyword           W. Landsman        November, 1992
;	User no longer need supply all items   W. Landsman  December, 1992 
;	Added STATUS keyword, William Thompson, GSFC, 1 April 1994
;	Added /SILENT keyword, William Thompson, GSFC, October 1995
;	Allow up to 30 items, fix problem if first item was multiple value
;				  W. Landsman    GSFC, July 1996
;	Faster build of external databases on big endian machines 
;				  W. Landsman    GSFC, November 1997  
;       Use SIZE(/TNAME) for error mesage display  W.Landsman   July 2001
;       Fix message display error introduced July 2001  W. Landsman   Oct. 2001 
;       Make sure error message appears even if !QUIET is set W.L November 2006
;       Major rewrite to use SCOPE_VARFETCH, accept 50 input items
;                   W. Landsman    November 2006
;      Fix warning if parameters have different # of elements W.L.  May 2010
;      Fix warning if scalar parameter supplied W.L.  June 2010
;      Fix for when first parameter is multi-dimensioned W.L. July 2010
;      Check data type of first parameter W.L. Jan 2012
;-
  COMPILE_OPT IDL2
  On_error,2                            ;Return to caller
  npar = N_params()
  if npar LT 1 then begin
    print,'Syntax - DBBUILD, v1, [ v2, v3, v4, v5, ... v50,' 
    print,'         /NOINDEX, /SILENT, STATUS =  ]'
    return
  endif

 dtype = ['UNDEFINED','BYTE','INT','LONG','FLOAT','DOUBLE', $
        'COMPLEX','STRING','STRUCT','DCOMPLEX','POINTER','OBJREF', $ 
        'UINT', 'ULONG', 'LONG64','ULONG64']

 
;  Initialize STATUS as unsuccessful (0).  If the routine is successful, this
;  will be updated below.

  status = 0

  nitem = db_info( 'ITEMS' )
  if nitem LE npar  then message, 'ERROR - ' + strtrim(npar,2) + $ $
     ' variables supplied but only ' + strtrim(nitem-1,2) + ' items in database' 

   items = indgen(nitem)
   db_item, items, itnum, ivalnum, idltype, sbyte, numvals, nbyte
   nitems = ( npar < nitem)
   vv = 'v' + strtrim( indgen(nitems+1), 2)

;Create a pointer array to point at each of the supplied variables   
   tmp = ptrarr(nitems,/allocate_heap)
   for i=0,nitems-1 do *tmp[i] = SCOPE_VARFETCH(vv[i+1], LEVEL=0)

   ndata = N_elements(v1)/ numvals[1]   ;# of elements in last dimension

   for i = 1,npar do begin    ;Get the dimensions and type of each input vector

      sz = size( *tmp[i-1], /STRUCT)
       ndatai = sz.N_elements/numvals[i]
      if ndatai NE ndata then message, $
          'WARNING - Parameter ' + strtrim(i,2) + ' has dimension ' +  $
	  strjoin(strtrim( sz.dimensions[0:sz.n_dimensions-1 > 0],2),' ') ,/con
      if sz.type_name NE dtype[idltype[i]] then begin
        message, 'Item ' + strtrim( db_item_info('NAME',i),2) + $
           ' - parameter '+strtrim(i,2) + ' - has an incorrect data type',/CON
        message, 'Required data type is ' + dtype[idltype[i]], /INF
        message, 'Supplied data type is ' + sz.type_name, /INF
	ptr_free,tmp
        return
     endif

  endfor
  external = db_info('external',0)
  noconvert = external ? is_ieee_big() : 1b

  entry = make_array( DIMEN = db_info('LENGTH'),/BYTE ) ;Empty entry array
  nvalues = long( db_item_info( 'NVALUES' ) )       ;# of values per item
  nbyte = nbyte*nvalues                             ;Number of bytes per item
                    
  for i = 0l, Ndata - 1 do begin
       i1 = i*nvalues
       i2 = i1 + nvalues -1

        dbxput,0l,entry,idltype[0],sbyte[0],nbyte[0]
	for j = 1,nitems  do $
	dbxput, (*tmp[j-1])[ i1[j]:i2[j] ], $
	       entry,idltype[j], sbyte[j], nbyte[j] 
	       
      dbwrt,entry,noconvert=noconvert        ;Write the entry into the database

  endfor
  ptr_free,tmp

  if ~keyword_set( NOINDEX ) then begin

      indexed = db_item_info( 'INDEX' )      ;Need to create an indexed file?
      if ~array_equal(indexed,0)  then begin
	   if ~keyword_set(silent) then	$
	           message,'Now creating indexed files',/INF
           dbindex,items
       endif

  endif

  dbclose

;  Mark successful completion, and return.

  status = 1
  return
  end
