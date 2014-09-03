pro dbdelete, list, name, DEBUG = debug
;+
; NAME:
;       DBDELETE
; PURPOSE:
;       Deletes specified entries from data base
;
; CALLING SEQUENCE:
;       DBDELETE, list, [ name, /DEBUG ]   
;
; INPUTS:
;       list - list of entries to be deleted, scalar or vector
;       name - optional name of data base, scalar string.  If not specified
;               then the data base file must be previously opened for update 
;               by DBOPEN.
;
; OPERATIONAL NOTES:
;       !PRIV must be at least 3 to execute.
;
; SIDE EFFECTS:
;       The data base file (ZDBASE:name.dbf) is modified by removing the
;       specified entries and reordering the remaining entry numbers
;       accordingly (ie. if you delete entry 100, it will be replaced
;       by entry 101 and the database will contain 1 less entry.
;
; EXAMPLE:
;        Delete entries in a database STARS where RA=DEC = 0.0
;
;        IDL> !PRIV= 3                           ;Set privileges
;        IDL> dbopen,'STARS',1                   ;Open for update
;        IDL> list = dbfind('ra=0.0,dec=0.0')    ;Obtain LIST vector
;        IDL> dbdelete, list             ;Delete specified entries from db
;
; NOTES:
;       The procedure is rather slow because the entire database is re-
;       created with the specified entries deleted.
; OPTIONAL KEYWORD INPUT:
;        DEBUG - if this keyword is set and non-zero, then additional 
;               diagnostics will be printed as each entry is deleted.
; COMMON BLOCKS:
;       DBCOM
; PROCEDURE CALLS:
;       DBINDEX, DB_INFO(), DBOPEN, DBPUT, ZPARCHECK
; HISTORY
;       Version 2  D. Lindler  July, 1989
;       Updated documentation   W. Landsman    December 1992
;       William Thompson, GSFC, 28 February 1995
;                       Fixed bug when external representation used.
;       Fixed for case where second parameter supplied W. Landsman April 1996
;       Use keyword DEBUG rather than !DEBUG   W. Landsman    May 1997
;       Don't call DBINDEX if no indexed items  W. Landsman May 2006  
;       Use TRUNCATE_LUN if V5.6 or later W. Landsman   Sep 2006 
;       Fix problem when deleting last entry   W. Landsman Mar 2007
;       Assume since V5.6 so TRUNCATE_LUN is available   W. Landsman
;       
;-
;-------------------------------------------------------------------------------
  On_error,2
  compile_opt idl2

  if N_params() EQ 0 then begin
      print,'Syntax - DBDELETE, entry, [ dbname ]'
      return
  endif 

; data base common block

 common db_com,QDB,QITEMS,QDBREC

; Check parameters

 zparcheck, 'DBDELETE', list, 1, [1,2,3], [0,1], 'entry list'
 if N_params() GT 1 then $
        zparcheck, 'dbdelete', name, 2, 7, 0, 'data base name'
 
 if !PRIV lt 3 then $
        message,'!priv must be at least 3 to execute'

; Open data base if name supplied

  if N_params() GT 1 then dbopen,name,1 else begin    ;Open specified database

     if not db_info( 'OPEN') then $
        message,'No database open for update'
     if not db_info('update') then $
            message,'Database '+ db_info('NAME',0) + ' not open for update'
  
   endelse

; Determine whether or not the database uses external data representation.

 external = qdb[119] eq 1


; Create vector if list is a scalar

  outrec = 0L                           ; Create counter of output record
  len = db_info('length')
 
; loop on entries in data base

  qnentry = db_info('ENTRIES',0)
  
  for i = 1L, qnentry do begin

        ; Is it to be kept?

        found = where( list EQ i, Nfound)

        if keyword_set(debug) then print,i,nfound           ; allow diags.

        if ( Nfound LE 0 ) then begin
                outrec = outrec + 1                ; increment counter
                if ( outrec NE i ) then begin
                        entry = qdbrec[i]
                        tmp = outrec
                        if external then byteorder,tmp,/htonl
                        dbput, 0, tmp, entry   ; modify entry number
                        qdbrec[outrec] = entry
                endif
        endif
  endfor

; Update adjusted total number of entries.

  qdb[84] = byte( outrec,0,4 )

; Truncate the .dbf file at the current position.

  unit = db_info('unit_dbf')
  point_lun, unit, long64(outrec+1)*len
  truncate_lun, unit

; Update index file

  indextype = db_item_info( 'INDEX')
  if total(indextype) NE 0 then dbindex

  if N_params() GT 1 then dbclose

  return  ; dbdelete
  end  ; dbdelete
