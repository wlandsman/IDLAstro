function db_info,request,dbname
;+
; NAME:
;       DB_INFO
; PURPOSE:
;       Function to obtain information on opened data base file(s)
;
; CALLING SEQUENCES:
;       1)  result = db_info(request)
;       2)  result = db_info(request,dbname)
; INPUTS (calling sequence 1):
;
;       request - string specifying requested value(s)
;               value of request          value returned in result
;                       'open'          Flag set to 1 if data base(s) are opened
;                       'number'        Number of data base files opened
;                       'items'         Total number of items (all db's opened)
;                       'update'        update flag (1 if opened for update)
;                       'unit_dbf'      Unit number of the .dbf files
;                       'unit_dbx'      Unit number of the .dbx files
;                       'entries'       Number of entries in the db's
;                       'length'        Record lengths for the db's
;                       'external'      True if the db's are in external format
;
; INPUTS (calling sequence 2):
;
;       request - string specifying requested value(s)
;                  value of request       value returned in result
;                       'name'          Name of the data base
;                       'number'        Sequential number of the db
;                       'items'         Number of items for this db
;                       'item1'         Position of item1 for this db
;                                       in item list for all db's
;                       'item2'         Position of last item for this db.
;                       'pointer'       Number of the item which points
;                                       to this db. 0 for first or primary
;                                       db.  -1 if link file pointers.
;                       'length'        Record length for this db.
;                       'title'         Title of the data base
;                       'unit_dbf'      Unit number of the .dbf file
;                       'unit_dbx'      Unit number of the .dbx file
;                       'entries'       Number of entries in the db
;                       'seqnum'        Last sequence number used
;                       'alloc'         Allocated space (# entries)
;                       'update'        1 if data base opened for update
;                       'external'      True if data base in external format
;                       'newdb'         True if new (post Oct 2010) format 
;                                       that allows entries > 32767 bytes
;
;       dbname - data base name or number
; OUTPUTS:
;       Requested value(s) are returned as the function value.
;
; HISTORY:
;       version 1  D. Lindler    Oct. 1987
;       changed type from 1 to 7 for IDLV2, J. Isensee, Nov., 1990
;       William Thompson, GSFC/CDS (ARC), 30 May 1994
;               Added EXTERNAL request type.
;       Support new DB format, add NEWDB request type W. Landsman Oct 2010
;-
;------------------------------------------------------------------------
on_error,2                       ;Return to caller
;
; data base common block
;
common db_com,QDB,QITEMS,QLINK
;
; QDB[*,i] contains the following for each data base opened
;
;       bytes
;         0-18   data base name character*19
;         19-79  data base title character*61
;         80-81  number of items (integer*2)
;         82-83  record length of DBF file (integer*2), old format
;         84-87  number of entries in file (integer*4)
;         88-89  position of first item for this file in QITEMS (I*2)
;         90-91  position of last item for this file (I*2)
;         92-95  Last Sequence number used (item=SEQNUM) (I*4)
;         96     Unit number of .DBF file
;         97     Unit number of .dbx file (0 if none exists)
;         98-99  Index number of item pointing to this file (0 for first db)
;         100-103 Number of entries with space allocated
;         104    Update flag (0 open for read only, 1 open for update)
;         105-108  record length of DBF file (integer*4), new format
;         119    True if database is in external (IEEE) format
;
;  QITEMS[*,i] contains deacription of item number i with following
;  byte assignments:
;
;       0-19    item name (character*20)
;       20-21   IDL data type (integet*2)
;       22-23   Number of values for item (1 for scalar) (integer*2)
;       24-25   Starting byte position in original DBF record (integer*2)
;       26-27   Number of bytes per data value (integer*2)
;       28      Index type
;       29-97   Item description
;       98-99   Print field length
;       100     Flag set to one if pointer item
;       101-119 Data base this item points to
;       120-125 Print format
;       126-170 Print headers
;       171-172 Starting byte in record returned by DBRD, old format
;       173-174 Data base number in QDB
;       175-176 Data base number this item points to
;       177-178 Item number within the specific data base
;       179-182 Number of values for item (1 for scalar) (integer*4)
;       183-186  Starting byte position in original DBF record (integer*4)
;       187-190 Starting byte in record returned by DBRD
;
;
; QLINK[i] contains the entry number in the second data base
;       corresponding to entry i in the first data base.
;-------------------------------------------------------------------------
;
req=strtrim(strupcase(request))         ;requested value
s=size(qdb)
if req eq 'OPEN' then begin
        if s[0] eq 0 then return,0 else return,1
end
if s[0] eq 0 then message,'No data base file(s) opened'
n=s[2]                                  ;number of data bases
;
; calling sequence 1  result=db_info(request)
;
newdb = qdb[118,0]
if N_params() lt 2 then begin
    case req of
        'NUMBER'  : return,n                    ;number of files opened
        'ITEMS'   : begin                       ;total number of items
                        s=size(qitems)
                        return,s[2]
                    end
        'LENGTH'  : begin
                    len = newdb ? long( qdb[105:108,*],0,n) : $
                                   fix(qdb[82:83,*],0,n)
                    return,len
                    end
                                                ;total record length
        'UPDATE'  : return,qdb[104,0]           ;update flag
        'UNIT_DBF'  : return,qdb[96,*]          ;.dbf unit number
        'UNIT_DBX'  : return,qdb[97,*]          ;.dbx unit number
        'ENTRIES'   : return,long(qdb[84:87,*],0,n)     ;number of entries
        'EXTERNAL'  : return,qdb[119,*] eq 1    ;external format?
        'NEWDB'     : return,  newdb         ;New db format?                  
        else :  message,'Invalid request for information'
    endcase
endif
;
; second calling sequence:  result=db_info(request,dbname) ----------
;
s=size(dbname)
ndim=s[0]
type=s[ndim+1]
if (ndim gt 0) || (type eq 0) then goto,abort
;
; convert name to number
;
if type eq 7 then begin
        db_name=strtrim(strupcase(dbname))
        for i=0,n-1 do $
                if db_name eq strtrim(string(qdb[0:18,i])) then goto,found
        goto,abort                                      ;not found
found:  dbnum=i
   end else begin                                       ;number supplied
        dbnum=fix(dbname)
        if (dbnum lt 0) || (dbnum ge n) then goto,abort
end
newdb = qdb[118,dbnum]

case req of
        'NAME'     : return,strtrim(string(qdb[0:18,dbnum]))  ;db name
        'NUMBER'   : return,dbnum                       ;data base number
        'ITEMS'    : begin                              ;number of items
                        x=fix(qdb[80:81,dbnum],0,1)
                        return,x[0]
                     end
        'ITEM1'    : begin                              ;starting item number
                        x=fix(qdb[88:89,dbnum],0,1)
                        return,x[0]
                     end
        'ITEM2'    : begin                              ;last item number
                        x=fix(qdb[90:91,dbnum],0,1)
                        return,x[0]
                     end
        'POINTER'   : begin                             ;item number pointer
                        x=fix(qdb[98:99,dbnum],0,1)
                        return,x[0]
                      end
        'LENGTH'    : begin 
                        x = newdb ? long(qdb[105:108,dbnum],0,1) : $                            ;record length
                                   fix(qdb[82:83,dbnum],0,1)
                      return,long(x[0])
                      end
        'TITLE'     : return,strtrim(string(qdb[19:79,dbnum])) ;data base title
        'UNIT_DBF'  : return,qdb[96,dbnum]              ;.dbf unit number
        'UNIT_DBX'  : return,qdb[97,dbnum]              ;.dbx unit number
        'ENTRIES'   : begin                             ;number of entries
                        x=long(qdb[84:87,dbnum],0,1)
                        return,x[0]
                      end
        'SEQNUM'    : begin                             ;last sequence number
                        x=long(qdb[92:95,dbnum],0,1)
                        return,x[0]
                      end
        'ALLOC'     : begin                             ;allocated size
                        x=long(qdb[100:103,dbnum],0,1)
                        return,x[0]
                      end
        'UPDATE'    : return,qdb[104,dbnum]             ;update flag
        'EXTERNAL'  : begin                             ;External format?
                        x=qdb[119,*] eq 1
                        return,x[0]
                      end
        'NEWDB'     :      return,  newdb         ;New db format?                  
        else: message,'Invalid information request'
endcase
abort:  message,'Invalid data base name or number supplied'
end
