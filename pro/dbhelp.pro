pro dbhelp,flag,TEXTOUT=textout,sort=sort
;+
; NAME:
;     DBHELP
; PURPOSE:
;     List available databases or items in the currently open database
; EXPLANATION:
;     Procedure to either list available databases (if no database is 
;     currently open) or the items in the currently open database.
;
; CALLING SEQUENCE:  
;     dbhelp, [ flag , TEXTOUT=, /SORT ]
;
; INPUT:
;     flag - (optional) if set to nonzero then item or database
;             descriptions are also printed
;             default=0
;             If flag is a string, then it is interpreted as the
;             name of a data base (if no data base is opened) or a name 
;             of an item in the opened data base.   In this case, help
;             is displayed only for the particular item or database
;
; OUTPUTS:
;      None
; OPTIONAL INPUT KEYWORDS:
;      TEXTOUT  - Used to determine output device.  If not present, the
;                value of !TEXTOUT system variable is used (see TEXTOPEN )
;
;               textout=0       Nowhere
;               textout=1       if a TTY then TERMINAL using /more option
;                                   otherwise standard (Unit=-1) output
;               textout=2       if a TTY then TERMINAL without /more option
;                                   otherwise standard (Unit=-1) output
;               textout=3       <program>.prt
;               textout=4       laser.tmp
;               textout=5      user must open file
;               textout=7      same as 3 but text is appended to <program>.prt
;                               file if it already exists.
;               textout = filename (default extension of .prt)
;
;        /SORT - If set and non-zero, then the help items will be displayed
;               sorted alphabetically.    If more than one database is open,
;               then this keyword does nothing.
; METHOD:
;       If no data base is opened then a list of data bases are
;       printed, otherwise the items in the open data base are printed.
;
;       If a string is supplied for flag and a data base is opened
;       flag is assumed to be an item name.  The information for that
;       item is printed along with contents in a optional file
;       zdbase:dbname_itemname.hlp
;       if a string is supplied for flag and no data base is opened,
;       then string is assumed to be the name of a data base file.
;       only information for that file is printed along with an
;       optional file zdbase:dbname.hlp.
; PROCEDURES USED:
;       DB_INFO(),DB_ITEM_INFO(),FIND_WITH_DEF(), TEXTOPEN, TEXTCLOSE, UNIQ()
; IDL VERSION:
;       V5.3 or later (uses vectorized FDECOMP)
; HISTORY:
;       Version 2  D. Lindler  Nov 1987 (new db format)
;       Faster printing of title desc. W. Landsman  May 1989 
;       Keyword textout added, J. Isensee, July, 1990
;       Modified to work on Unix, D. Neill, ACC, Feb 1991.
;       William Thompson, GSFC/CDS (ARC), 1 June 1994
;               Added support for external (IEEE) representation.
;       William Thompson, GSFC, 3 November 1994
;               Modified to allow ZDBASE to be a path string.
;       Remove duplicate database names  Wayne Landsman    December 1994
;       8/17/95 jkf/acc - force lowercase filenames for .hlp files.
;       Added /SORT keyword  J. Sandoval/W. Landsman     October 1998
;       V5.3 version use vectorized FDECOMP   W. Landsman   February 2001
;       Recognize 64 bit, unsigned integer datatypes W. Landsman September 2001
;       Fix display of number of bytes with /SORT W. Landsman February 2002
;       Assume since V5.2                 W. Landsman February 2002  
;       Assume since V5.5                 W. Landsman 
;       Define !TEXTOUT if not already defined W. Landsman  April 2016  
;-
;****************************************************************************

   defsysv,'!TEXTUNIT',exist=i
  if i EQ 0 then astrolib
  
;
; get flag value
;

  stn=''
  if N_params() GT 0 then begin
      if size(flag,/TNAME) EQ 'STRING' then $   ;item name or db name
             stn=strtrim(flag) 
  endif else flag = 0    ;flag not supplied
;
; Are any data bases opened?
;
opened = db_info('OPEN')
if opened then begin
        if stn EQ '' then xtype=1 $             ;all items
                     else xtype=2               ;single item
   end else begin
        if stn EQ '' then xtype=3 $             ;all db's
                     else xtype=4               ;single db
end
;
; determine where user wants output...default terminal.
;
if N_elements(textout) EQ 0 then textout = !textout  ;use default output dev.
;
textopen,'dbhelp',textout=textout
;
;--------------------------------------------------------------------
; if data base open then print info for it
;
if opened then begin                    ;data base opened?
;
; get list of items to print
;
        if xtype eq 1 then begin                ;all items?
                nitems=db_info('items') ;number of items
                itnums=indgen(nitems)
            end else begin
                nitems=1
                db_item,stn,itnums
        end
;
; get information on the items
;
     names = db_item_info('NAME',itnums)         ;item names
     idltype = db_item_info('IDLTYPE',itnums)    ;data type
     nbytes = db_item_info('NBYTES',itnums)      ;number of bytes
     desc = db_item_info('DESCRIPTION',itnums)   ;description
     pointer = db_item_info('POINTER',itnums)    ;file it points to
     index = db_item_info('INDEX',itnums)        ;index type
     pflag = db_item_info('PFLAG',itnums)        ;pointer item flag
     dbnumber = db_item_info('DBNUMBER',itnums)  ;opened data base number
     pnumber = db_item_info('PNUMBER',itnums)    ;opened data base it points to
     nvalues = db_item_info('NVALUES',itnums)    ;number of values for vector
     if keyword_set(sort) && (max(dbnumber) EQ 0) then begin 
          nsort = sort(names)
          names = names[nsort]
          idltype = idltype[nsort]
          desc = desc[nsort]
          nvalues = nvalues[nsort]
          nbytes = nbytes[nsort]
     endif
;
; get names and descriptions of opened db's
;
        
     if flag then begin         ;print descrip.?
             desc = strtrim(desc)
             printf,!textunit,' '
             printf,!textunit,'----- '+db_info('name',dbnumber[0]) +'  '+ $
                                 db_info('title',dbnumber[0])
             printf,!textunit,'   ITEM               TYPE            DESCRIPTION'
             for i=0,nitems-1 do begin
                 if i NE 0 then if dbnumber[i] ne dbnumber[i-1] then begin
                            printf,!textunit,' '
                            printf,!textunit,'----- '+db_info('name',dbnumber[i]) +'  '+ $
                                         db_info('title',dbnumber[i])
                            printf,!textunit,'   ITEM              TYPE            DESCRIPTION'
                 end
                  case idltype[i] of
                       1: type = 'byte'
                       2: type = 'int*2'
                       3: type = 'int*4'
                       4: type = 'real*4'
                       5: type = 'real*8'
                       7: type = 'char*'+strtrim(nbytes[i],2)
                       12: type = 'uint*2'
                       13: type = 'uint*4'
                       14: type = 'int*8'
                       15: type = 'uint*8'
                        end
                   while strlen(type) lt 8 do type=type+' '
                   qname = names[i]
                   if nvalues[i] GT 1 then begin
                           qname=strtrim(qname)
                           qname=qname+'('+strtrim(nvalues[i],2)+')'
                           while strlen(qname) lt 20 do qname=qname+' '
                  end
                  printf,!textunit,strmid(qname,0,18),' ',type,' ', desc[i]
                end
        end else begin                  ;just print item names
                printf,!textunit,form='(1x,7a11)',names
        end
;
; print index information -----------------------------------------
;
        if (xtype EQ 1) && (total(index) GT 0) then begin
                if xtype EQ 1 then begin
                        printf,!textunit,' '
                        printf,!textunit,'-------  Indexed Items ------'
                        indexed=where(index)
                        printf,!textunit,names[indexed]
                   end else begin
                        printf,!textunit,'The item is indexed'
                end
        end
;
; print pointer information ----------------------------------------
;
        if (total(pflag) GT 0) && (xtype EQ 1) then begin
                good = where( pflag, n)
                printf,!textunit,' '
                printf,!textunit,'----- Pointer Information ----'
                for i=0,n-1 do begin
                    pos=good[i]
                    if pnumber[pos] GT 0 then popen=' (presently opened)' $
                                         else popen=''
                    printf,!textunit,strtrim(db_info('name',dbnumber[pos]))+ $
                                '.'+strtrim(names[pos])+' ---> '+ $
                                strtrim(pointer[pos])+popen
                end
        end
;
; print information on data base size ----------------------------
;
        printf,!textunit,' '
        if xtype EQ 1 then printf,!textunit,'data base contains', $
                        db_info('ENTRIES',0),' entries'
;
; print data base information --------------------------------
;
  end else begin                        ;list data bases
        if stn EQ '' then begin
                names=list_with_path('*.dbh', 'ZDBASE', COUNT=n) ;get list
                if n EQ 0 then message,'No databases found in ZDBASE directory'
       endif else begin
                names=list_with_path(stn+'*.dbh', 'ZDBASE', COUNT=n) ;get list
                if n EQ 0 then message,'Unable to locate database '+stn
       endelse
       fdecomp,names,disk,dir,fnames
       fsort = uniq(fnames,sort(fnames))
        n = N_elements(fsort)
        if flag then  begin                ;print description from .DBH file
             get_lun,unit
             names = names[fsort]
             b=bytarr(79)              ;Database title is 79 bytes
             for i=0,n-1 do begin
                  openr,unit,names[i],error=err
                  if err NE 0 then message,/CON, 'Error opening ' + names[i]
                  readu,unit,b
                  printf,!TEXTUNIT,strtrim(b[0:78],2) 
                  close,unit
             endfor
             free_lun,unit
       endif else  $                            ;just print names
                printf,!textunit,form='(A,T20,A,T40,A,T60,A)',fnames[fsort]
endelse
;
; now print aux help file info if flag was a string ---------------------
;
if stn NE '' then begin
        if xtype EQ 4 then file=find_with_def(stn+'.hlp', 'ZDBASE') $
                      else file=find_with_def(strlowcase( $
                                strtrim(db_info( 'NAME', dbnumber[0]))+ $
                                '_' + strtrim(names[0]) + '.hlp'), 'ZDBASE')
        openr,unit,strlowcase(file),error=err,/get_lun
        if err EQ 0 then begin
          st=''
          while not eof(unit) do begin
                readf,unit,st
                printf,!textunit,st
          end; while
          free_lun,unit
        endif
end
;
; close unit opened by TEXTOPEN
;
textclose, TEXTOUT = textout

return
end
