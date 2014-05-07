function db_item_info,request,itnums
;+
; NAME:
;	DB_ITEM_INFO
; PURPOSE:
;	routine to return information on selected item(s) in the opened
;	data bases.
;
; CALLING SEQUENCE:
;	result = db_item_info( request, itnums)
; INPUTS:
;	request - string giving the requested information.
;		'name'		- item names
;		'idltype'	- IDL data type (integers)
;				  see documentation of intrinsic SIZE funtion
;		'nvalues'	- vector item length (1 for scalar)
;		'sbyte'		- starting byte in .dbf record (use bytepos
;				  to get starting byte in record returned by
;				  dbrd)
;		'nbytes'	- bytes per data value
;		'index'		- index types
;		'description'	- description of the item
;		'pflag'		- pointer item flags
;		'pointer'	- data bases the items point to
;		'format'	- print formats
;		'flen'		- print field length
;		'headers'	- print headers
;		'bytepos'	- starting byte in dbrd record for the items
;		'dbnumber'	- number of the opened data base
;		'pnumber'	- number of db it points to (if the db is
;					opened)
;		'itemnumber'	- item number in the file
;
;	itnums -(optional) Item numbers.  If not supplied info on all items
;		are returned.
; OUTPUT:
;	Requested information is returned as a vector.  Its type depends
;	on the item requested.
; HISTORY:
;	version 1  D. Lindler  Nov. 1987
;	Converted to IDL V5.0   W. Landsman   September 1997
;       Support new DB format which allows > 32767 bytes W.L. Oct 2010
;-
;------------------------------------------------------------------------
; data base common block
;               
common db_com,QDB,QITEMS,QLINK
;
; QDB[*,i] contains the following for each data base opened
;
;	bytes
;	  0-18   data base name character*19
;	  19-79  data base title character*61
;	  80-81  number of items (integer*2)
;	  82-83  record length of DBF file (integer*2)
;	  84-87  number of entries in file (integer*4)
;	  88-89  position of first item for this file in QITEMS (I*2)
;	  90-91  position of last item for this file (I*2)
;	  92-95  Last Sequence number used (item=SEQNUM) (I*4)
;	  96	 Unit number of .DBF file
;	  97	 Unit number of .IND file (0 if none exists)
;	  98-99  Index number of item pointing to this file (0 for first db)
;	  100-103 Number of entries with space allocated
;	  104	 Update flag (0 open for read only, 1 open for update)
;	  119	 Equals 1 if external data representation (IEEE) is used
;	
;  QITEMS[*,i] contains a description of item number i with following
;  byte assignments:
;
;	0-19	item name (character*20)
;	20-21   IDL data type (integet*2)
;	22-23 	Number of values for item (1 for scalar) (integer*2)
;	24-25	Starting byte position in original DBF record (integer*2)
;	26-27	Number of bytes per data value (integer*2)
;	28	Index type
;	29-97	Item description
;	98-99	Print format field length
;	100	Flag set to one if pointer item
;	101-119 Data base this item points to
;	120-125 Print format
;	126-170 Print headers
;	171-172 Starting byte in record returned by DBRD
;	173-174 Data base number in QDB
;	175-176 Data base number this item points to
;	177-178 item number within file
;       179-182 Number of values for item (1 for scalar) (integer*4)
;       183-186   Starting byte position in original DBF record (integer*4)
;       187-190 Starting byte in record returned by DBRD
;
; QLINK[i] contains the entry number in the second data base
;	corresponding to entry i in the first data base.
;-------------------------------------------------------------------------
s=size(qitems) & n=s[2]
newdb = qdb[118,0] EQ 1
case strupcase(strtrim(request)) of

	'NAME'		: x=string(qitems[0:19,*])
	'IDLTYPE'	: x=fix(qitems[20:21,*],0,n)
	'NVALUES'	: x = newdb? long(qitems[179:182,*],0,n) : $ 
			             fix(qitems[22:23,*],0,n)
	'SBYTE'		: x = newdb ? long(qitems[183:186,*],0,n) : $
			               fix(qitems[24:25,*],0,n) 
	'NBYTES'	: x=fix(qitems[26:27,*],0,n)
	'INDEX'		: x=qitems[28,*]
	'DESCRIPTION'	: x=string(qitems[29:99,*])
	'PFLAG'		: x=qitems[100,*]
	'POINTER'	: x=string(qitems[101:119,*])
	'FORMAT'	: x=string(qitems[120:125,*])
	'FLEN'		: x=fix(qitems[98:99,*],0,n)
	'HEADERS'	: x=string(qitems[126:170,*])
	'BYTEPOS'	: x = newdb ?  long(qitems[187:190,*],0,n) : $ 
	                                fix(qitems[171:172,*],0,n)
	'DBNUMBER'	: x=fix(qitems[173:174,*],0,n)
	'PNUMBER'	: x=fix(qitems[175:176,*],0,n)
	'ITEMNUMBER'	: x=fix(qitems[177:178,*],0,n)
	else: begin
		print,'DB_ITEM_INFO-- invalid information request'
		retall
	      end
endcase
if N_params() eq 1 then return,x else return,x[itnums]
end
