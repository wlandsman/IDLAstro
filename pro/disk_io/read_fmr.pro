;+
; NAME:
;  READ_FMR
;
; PURPOSE:
;   Read a journal (ApJ, AJ) machine-readable table into IDL
;
; EXPLANATION:
;  Given a machine readable table name and optionally column
;  numbers, this FUNCTION reads the format information in the
;  meta-header and outputs a IDL function containing either the
;  complete table or only the requested columns.
;
; CALLING SEQUENCE:
;  data = read_fmr(filename)
;
; INPUTS:
;  filename [STRING]: the name of the file containing the machine
;  readable table. If filename is missing a dialog to select the
;  filename will be presented
;
; INPUT KEYWORD PARAMETERS:
;   /HELP  - if set show the help
;
;   COLUMNS -  [(array of) integers or strings] of column(s) to be returned.
;     If columns is of type integer they represent indices for which
;     column numbers to return, if they are strings the columns with the
;     corresponding names will be returned in the order as given.
;
;   MISSINGVALUE [float]: value with which to replace the missing values in the 
;        table, default is NaN.
;
;   /USE_COLNUM - If  specified and non-zero then column names will be generated
;        as 'C1, C2,  .... Cn'  for the number of columns in the table, rather
;        than using the table names.
;
; OUTPUTS:
;  The ouput data structure will look like:
;    TYPE            STRING    'mr_structure'
;    NAME            STRING    Array[X]
;    UNIT            STRING    Array[X]
;    DESCRIPTION     STRING    Array[X]
;    DATA            STRUCT    -> <Anonymous> Array[1]
;  where name contains the names of each columns
;  unit contains the given units
;  description contains the short descriptions and
;  data holds the values of the separate columns.   By default the tag names are
;  taken from the column names, with modifications necessary to make them a 
;  valid tag name.    For example, the column name 'B-V' will be converted to 
;  'B_V' to become a valid tag name.    If the /USE_COLNUM keyword is set, then
;  the column will be named  C0,  C1, ... , CX, where X stands for the total 
;  number of columns read.
;
; RESTRICTIONS:
;  (1) The file to be read should be formatted as a machine readable datafile.
;  (2) Use of the COLUMN keyword currently requires use of the EXECUTE function,
;      and so cannot be used with the IDL Virtual machine.
; EXAMPLE:
;  meas = read_fmr('smith.dat',col=[2,5,6], /Use_colnum)
;   plot,meas.data.c1,ytitle=meas.name[1]+' ('+meas.unit[1]+')'
;
;  and
;  data = read_fmr('smith.dat',col=['Name','Date'], /Use_colnum)
;   print,meas.data.c0
;   
; MODIFICATION HISTORY:
;  Version 1:
;  Written by Sacha Hony (ESA) Nov 14 2003
;   Based heavily on mrcolextract by Greg Schwarz (AAS Journals
;   staff scientist) on 8/16/00.
;
;  Version 1.1:
;    Fixed bug where column=[3,4] always returned the first few columns
;
;  VErsion 2.0 By default use column names as tag names W. Landsman Feb 2010
;  Version 3.0 Use long integers W. Landsman/T. Ellsworth-Bowers May 2013
;  Version 3.1 Assume since IDL V6.4  W.L. Aug 2013
;-

FUNCTION read_fmr,filename, $
                  columns=columns, $
                  missingvalue=missingvalue, $
                  help=help, $
		  use_colnum = use_colnum

  compile_opt idl2
  ;; Only print the usage info and return if asked for help
  IF keyword_set(help) THEN BEGIN
      doc_library,'read_fmr'
      return,0
  ENDIF
  
  ;; If no filename is given then pop-up the dialog_pickfile dialog
  IF N_elements(filename) EQ 0 THEN BEGIN
       filename =dialog_pickfile(filter=['*.dat;*.asc*;*.txt','*'], $
                                 /must_exist)
   ENDIF
  
  ;; Check that file exists and is readable otherwise bail-out
  IF ~FILE_TEST(filename) THEN BEGIN
      message,'The file: '+filename+' does cannot be found or read', $
              /informational
      return,0
  ENDIF
  
  IF N_elements(missingvalue) EQ 0 THEN missingvalue=!VALUES.F_NAN
  
;; Variables needed to read single lines of the file
  dumI=' '
  tmp=''
  irow=0L ;; Make sure it can hold a lot of lines
  startpos=' '
  endpos=' '

;; Variable in which the total information of the files is collected  
  names=''
  units=''
  descriptions=''
  startposs=0
  idltypes=0
  
  openr,lun,filename,/get_lun
  
;; Read the first few lines into a dummy variable
;; because this info is not needed.  However, keep
;; track of the number of lines.
  WHILE (strpos(dumI,'Bytes Format') EQ -1) DO BEGIN
      readf,lun,dumI
      irow++
  END 
  
  readf,lun,dumI
  irow++
  
;; Read until you reach a '------' line terminator
  WHILE (strpos(tmp,'-----------------') EQ -1) DO BEGIN
      irow++
      
;; Extract out the 6-8th positions.  
;; If there is a number you have a column
      readf,lun,f='(1X,A3,1X,A3,1X,A80)',startpos,endpos,tmp
      
;; If startpos is --- then you are at the end 
;; so set the 9999 flag so it isn't counted
      IF (startpos EQ '---') THEN startpos = '9999'

;; If starpos is blank then this is either a continuation
;; line or a column that is only one digit wide.  You can
;; tell by checking if endpos is also blank.  If it is a 
;; column then set startpos and endpos to the same value
      IF (startpos EQ '   ') THEN BEGIN
          startpos = endpos
          IF (endpos EQ '   ') THEN startpos = '9999'
      ENDIF
      IF (fix(startpos) GE 1 AND fix(startpos) LE 999) THEN BEGIN
          
;; Squeeze out the blanks.
          less_blanks = strcompress(tmp)
          
;; Separate the non-location info by sorting into an array that is 
;; delimited by blank spaces.  The first position is the format,
;; the second is the units, the third is the name, and the last
;; positions are the short description of the column
          
;;(SH Nov 18 2003) strsplit is not available in older versions of IDL
         components=strsplit(less_blanks,' ',/extract)
 
;; Determine the column type (A|I|F|E)
          vtype = strmid(components[0],0,1)
          CASE vtype OF
              'A': idltype = 7
              'I': idltype = 3
              'F': idltype = 5
              'E': idltype = 5
          ENDCASE
          
          ;; Add the collected data to the lists
          names=[names,components[2]]
          units=[units,components[1]]
          ;; Take the rest of the strings a description
          description=''
          FOR i=3,n_elements(components)-1 DO description=description+ $
            components[i]+' '
          descriptions=[descriptions,description]
          startposs=[startposs,startpos-1]
          idltypes=[idltypes,idltype]
      ENDIF 
  ENDWHILE

;; iskip is the end (maybe see below) of the meta-header 
  iskip=irow
  
;; Continue reading the file to get the number of lines
  lastdash=0L
  WHILE ~eof(lun) DO BEGIN
      readf,lun,dumI
      irow++
;; If you encounter another '--------' (e.g. the end of a
;; notes subsection) mark it because you don't want to 
;; read the previous information as data!
      IF (strmid(dumI,0,6) EQ '------') THEN BEGIN
          lastdash=irow
      ENDIF
  ENDWHILE
  
  ;; Make sure we close the file and free the lun
  free_lun,lun
  
;; If you found a '-------' line then set iskip to the last dash
;; line so not to read any extra headers
  IF (lastdash NE 0L) THEN BEGIN
      iskip=lastdash
  ENDIF

;; Clean the arrays from the first dummy element
  names=names[1:*]
  units=units[1:*]
  descriptions=descriptions[1:*]
  startposs=startposs[1:*]
  idltypes=idltypes[1:*]
  ncolumns = n_elements(startposs)
  if keyword_set(USE_COLNUM) then $
      fieldnames = 'C' + strtrim(indgen(ncolumns),2) else $
      fieldnames = IDL_VALIDNAME(names,/convert_all)
  
  ;; now fill the template stuff for read_ascii
  template = {VERSION:1.00000, $
              DATASTART:iskip, $
              DELIMITER:0B, $
              MISSINGVALUE:missingvalue, $
              COMMENTSYMBOL:'', $
              FIELDCOUNT:ncolumns, $
              FIELDTYPES:idltypes, $
              FIELDNAMES: fieldnames, $
              FIELDLOCATIONS:startposs, $
              FIELDGROUPS:indgen(ncolumns)}
  
  data = read_ascii(filename,template=template)
  

  ;; This is all if the columns keyword is given then
  ;; only certain columns are requested. So do the selections here
  IF keyword_set(columns) THEN BEGIN

      ncolumns = n_elements(columns)
   
      ;; are they strings?
      IF size(columns,/TNAME) EQ 'STRING' THEN BEGIN

          ;; first convert the columns and the output names to uppercase
          ;; to be able to compare them directly without strcmp
          names_up   = strupcase(names)
          columns_up = strupcase(columns)

          ;; create an array to hold the requested column numbers set
          ;; these to -1
          idx_columns = make_array(ncolumns,value=-1)

          ;; Now match each string with the names
          FOR i=0,ncolumns-1 DO BEGIN
              ;; take the first instance where the uppercase name and
              ;; uppercase column match
              idx_columns[i] = ( where(names_up EQ columns_up[i]) )[0]
          ENDFOR

          ;; Are there elements which did not find a match?
          idx_missing_columns = where(idx_columns EQ -1,cnt)

          ;; All the elements of idx_columns are -1
          IF (cnt EQ ncolumns) THEN BEGIN
              message,'None of the column names could be found in the table', $
                      /informational
              return,0
          ENDIF

          ;; Some elements are matched but some are missing
          IF (cnt NE 0) THEN BEGIN
              message,'The following columns are not present in the table:', $
                      /informational
              message,columns[idx_missing_columns], $
                      /informational
              ;; Only take the valid columns and still continue
              idx_columns =idx_columns[where(idx_columns NE -1)]
          ENDIF

      ENDIF ELSE BEGIN
          ;; Assume the columns are numbers which indicate the
          ;; requested column numbers

          max_column=n_tags(data)-1
          columns = fix(columns)
          ;; make sure they are not higher than the available number
          ;; of columns and not negative
          idx_columns = columns[where( (columns LE max_column) AND $
                                       (columns GE 0) ,cnt)]

          IF (cnt EQ 0) THEN BEGIN
              message,'The requested columns are not present in the file', $
                      /informational
              return,0
          ENDIF

          ;; Some elements are matched but some are too high
          IF cnt NE ncolumns THEN BEGIN
              message,'Some column numbers are out of range.'+ $
                      ' Valid range=[0,'+ $
                      strcompress(string(max_column),/remove_all)+']', $
                      /informational
          ENDIF
      ENDELSE

;; now take the requested columns
      names=names[idx_columns]
      units=units[idx_columns]
      if ~keyword_set(use_colnum) then fieldnames = fieldnames[idx_columns] $
         else fieldnames = 'C' + strtrim(indgen(ncolumns),2)
      descriptions=descriptions[idx_columns]
      ncolumns = n_elements(names)
      

      ;; We need this to restructure the data structure to hold only
      ;; the requested columns
      exec_string = 'data={' + fieldnames[0] + $
                    ':data.('+string(idx_columns[0])+')'
      FOR i=1,ncolumns-1 DO BEGIN
          exec_string = exec_string + ',' + fieldnames[i] + $
                        ':data.('+string(idx_columns[i])+')'
      ENDFOR
      exec_string=exec_string+'}'
      foo = execute(exec_string)
  ENDIF
  
  
  out = {type:'mr_structure', $
         name:names, $
         unit:units, $
         description:descriptions, $
         data:data}
  
  message,"Read "+strcompress(ncolumns)+" columns from "+ $
          filename,/informational
  
  return,out
  
END
