pro fits_info, filename, SILENT=silent,TEXTOUT=textout, N_ext=n_ext, extname=extname
;+
; NAME:
;     FITS_INFO
; PURPOSE:
;     Provide information about the contents of a FITS file
; EXPLANATION:
;     Information includes number of header records and size of data array.
;     Applies to primary header and all extensions.    Information can be 
;     printed at the terminal and/or stored in a common block
;
;     This routine is mostly obsolete, and better results can be usually be
;     performed with FITS_HELP (for display) or FITS_OPEN (to read FITS 
;     information into a structure)
;
; CALLING SEQUENCE:
;     FITS_INFO, Filename, [ /SILENT , TEXTOUT = , N_ext =, EXTNAME= ]
;
; INPUT:
;     Filename - Scalar string giving the name of the FITS file(s)
;               Can include wildcards such as '*.fits', or regular expressions 
;               allowed by the FILE_SEARCH() function.     One can also search 
;               gzip compressed  FITS files, but their extension must
;               end in .gz or .ftz.
; OPTIONAL INPUT KEYWORDS:
;     /SILENT - If set, then the display of the file description on the 
;                terminal will be suppressed
;
;      TEXTOUT - specifies output device.
;               textout=1        TERMINAL using /more option
;               textout=2        TERMINAL without /more option
;               textout=3        <program>.prt
;               textout=4        laser.tmp
;               textout=5        user must open file, see TEXTOPEN
;               textout=7       append to existing <program.prt> file
;               textout = filename (default extension of .prt)
;
;               If TEXTOUT is not supplied, then !TEXTOUT is used
; OPTIONAL OUTPUT KEYWORDS:
;       The following keyowrds are for use when only one file is processed
;
;       N_ext - Returns an integer scalar giving the number of extensions in
;               the FITS file
;       extname - returns a list containing the EXTNAME keywords for each
;       		extension.
;
; COMMON BLOCKS
;       DESCRIPTOR =  File descriptor string of the form N_hdrrec Naxis IDL_type
;               Naxis1 Naxis2 ... Naxisn [N_hdrrec table_type Naxis
;               IDL_type Naxis1 ... Naxisn] (repeated for each extension) 
;               For example, the following descriptor 
;                    167 2 4 3839 4 55 BINTABLE 2 1 89 5
; 
;               indicates that the  primary header containing 167 lines, and 
;               the primary (2D) floating point image (IDL type 4) 
;               is of size 3839 x 4.    The first extension header contains
;               55 lines, and the  byte (IDL type 1) table array is of size
;               89 x 5.
;
;               The DESCRIPTOR is *only* computed if /SILENT is set.
; EXAMPLE:
;       Display info about all FITS files of the form '*.fit' in the current
;               directory
;
;               IDL> fits_info, '*.fit'
;
;       Any time a *.fit file is found which is *not* in FITS format, an error 
;       message is displayed at the terminal and the program continues
;
; PROCEDURES USED:
;       GETTOK(), MRD_SKIP, STRN(), SXPAR(), TEXTOPEN, TEXTCLOSE 
;
; SYSTEM VARIABLES:
;       The non-standard system variables !TEXTOUT and !TEXTUNIT will be  
;       created by FITS_INFO if they are not previously defined.   
;
;       DEFSYSV,'!TEXTOUT',1
;       DEFSYSV,'!TEXTUNIT',0
;
;       See TEXTOPEN.PRO for more info
; MODIFICATION HISTORY:
;       Written, K. Venkatakrishna, Hughes STX, May 1992
;       Added N_ext keyword, and table_name info, G. Reichert
;       Work on *very* large FITS files   October 92
;       More checks to recognize corrupted FITS files     February, 1993
;       Proper check for END keyword    December 1994
;       Correctly size variable length binary tables  WBL December 1994
;       EXTNAME keyword can be anywhere in extension header WBL  January 1998
;       Correctly skip past extensions with no data   WBL   April 1998
;       Converted to IDL V5.0, W. Landsman, April 1998
;       No need for !TEXTOUT if /SILENT D.Finkbeiner   February 2002
;       Define !TEXTOUT if needed.  R. Sterner, 2002 Aug 27
;       Work on gzip compressed files for V5.3 or later  W. Landsman 2003 Jan
;       Improve speed by only reading first 36 lines of header 
;       Count headers with more than 32767 lines         W. Landsman Feb. 2003
;       Assume since V5.3 (OPENR,/COMPRESS)   W. Landsman Feb 2004
;       EXTNAME keyword can be anywhere in extension header again 
;                         WBL/S. Bansal Dec 2004
;       Read more than 200 extensions  WBL   March 2005
;       Work for FITS files with SIMPLE=F   WBL July 2005
;       Assume since V5.4, fstat.compress available WBL April 2006
;       Added EXTNAME as an IDL keyword to return values. M. Perrin Dec 2007
;       make Ndata a long64 to deal with large files. E. Hivon Mar 2008
;       For GDL compatibility, first check if file is compressed  before using
;          OPENR,/COMPRESS  B. Roukema/WL    Apr 2010
;       Increased nmax (max number of extensions) from 400 to 2000   Sept 2012
;       Correctly fills EXTNAME when SILENT is set    EH   Jan 2013
;       Turned ptr to long64 for very large files EH Dec 2013
;       Replace 2880L with 2880LL for very large files  EH  Mar 2015
;       Let TEXTOPEN test for !TEXTOUT  WL Sep 2016
;-
 On_error,2
 compile_opt idl2
 COMMON descriptor,fdescript

 if N_params() lt 1 then begin
     print,'Syntax - FITS_INFO, filename, [/SILENT, TEXTOUT=, N_ext=, EXTNAME=]'
     return
 endif

 fil = file_search( filename, COUNT = nfiles) 
 if nfiles EQ 0 then message,'No files found'
; File is gzip compressed if it ends in .gz or .ftz 
 len = strlen(fil)
 ext = strlowcase(strmid(fil,transpose(len-3),3))
 compress = (ext EQ '.gz') || (ext EQ 'ftz')

 silent = keyword_set( SILENT )
 if ~silent then textopen, 'FITS_INFO', TEXTOUT=textout

 for nf = 0, nfiles-1 do begin

     file = fil[nf]

     openr, lun1, file, /GET_LUN, COMPRESS = compress[nf]
     
     N_ext = -1
     fdescript = ''
     nmax = 2000                ; MDP was 100, then 400
     nbuf= nmax
     extname = strarr(nmax)

     ptr = 0LL
     START:  
     ON_IOerror, BAD_FILE
     descript = ''
;   Is this a proper FITS file?     
     test = bytarr(8)
     readu, lun1, test
     
     if N_ext EQ -1 then begin
         if string(test) NE 'SIMPLE  ' then goto, BAD_FILE
         simple = 1
     endif else begin
         if string(test) NE 'XTENSION' then goto, END_OF_FILE
         simple = 0
     endelse
     point_lun, lun1, ptr

;                               Read the header
     hdr = bytarr(80, 36, /NOZERO)
     N_hdrblock = 1
     readu, lun1, hdr
     ptr += 2880LL
     hd = string( hdr > 32b)
     
;                               Get values of BITPIX, NAXIS etc.
     bitpix = sxpar(hd, 'BITPIX', Count = N_BITPIX)
     if N_BITPIX EQ 0 then $ 
       message, 'WARNING - FITS header missing BITPIX keyword',/CON
     Naxis = sxpar( hd, 'NAXIS', Count = N_NAXIS)
     if N_NAXIS EQ 0 then message, $ 
       'WARNING - FITS header missing NAXIS keyword',/CON
     
     exten = sxpar( hd, 'XTENSION')
     Ext_type = strmid( strtrim( exten ,2), 0, 8) ;Use only first 8 char
     gcount = sxpar( hd, 'GCOUNT') > 1
     pcount = sxpar( hd, 'PCOUNT')
     
     if strn(Ext_type) NE '0' then begin
         if (gcount NE 1) or (pcount NE 0) then $
           ext_type = 'VAR_' + ext_type
         descript += ' ' + Ext_type
     endif

     descript += ' ' + strn(Naxis)
     
     case BITPIX of
           8:   IDL_type = 1      ;Byte
          16:   IDL_type = 2      ;16 bit signed integer
          32:   IDL_type = 3      ;32 bit signed integer
         -32:   IDL_type = 4      ;Float
         -64:   IDL_type = 5      ;Double
         ELSE: begin 
             message, ' Illegal value of BITPIX = ' + strn(bitpix) + $
                      ' in header',/CON
             goto, SKIP
         end
     endcase

     if Naxis GT 0 then begin
         descript += ' ' + strn(IDL_type)
         Nax = sxpar( hd, 'NAXIS*')
         if N_elements(Nax) LT Naxis then begin 
             message, $
               'ERROR - Missing required NAXISi keyword in FITS header',/CON
             goto, SKIP
         endif
         for i = 1, Naxis do descript += ' '+strn(Nax[i-1])
     endif
     
     end_rec = where( strtrim(strmid(hd,0,8),2) EQ  'END')
     
     exname = sxpar(hd, 'extname', Count = N_extname)
     if N_extname GT 0 then extname[N_ext+1] = exname
     get_extname =  (N_ext GE 0) && (N_extname EQ 0)  
  
;  Read header records, till end of header is reached

     hdr = bytarr(80, 36, /NOZERO)
     while (end_rec[0] EQ -1) && (~eof(lun1) ) do begin
         readu,lun1,hdr
         ptr += 2880LL
         hd1 = string( hdr > 32b)
         end_rec = where( strtrim(strmid(hd1,0,8),2) EQ  'END')
         n_hdrblock++ 
         if get_extname then begin
             exname = sxpar(hd1, 'extname', Count = N_extname)
             if N_extname GT 0 then begin
                 extname[N_ext+1] = exname
                 get_extname = 0
             endif
         endif 
     endwhile
     
     n_hdrec = 36L*(n_hdrblock-1) + end_rec[0] + 1L ; size of header
     descript = strn( n_hdrec ) + descript
     
;  If there is data associated with primary header, then find out the size
     
     if Naxis GT 0 then begin
         ndata = long64(Nax[0])
         if naxis GT 1 then for i = 2, naxis do ndata *= Nax[i-1]
     endif else ndata = 0
     
     nbytes = (abs(bitpix)/8) * gcount * (pcount + ndata)
     nrec = long(( nbytes +2879)/ 2880)
     

 
; Check if all headers have been read 

     if ( simple EQ 0 ) && ( strlen(strn(exten)) EQ 1) then goto, END_OF_FILE  

     N_ext++ 
     if N_ext GE (nmax-1) then begin 
         extname = [extname,strarr(nbuf)]
         nmax = N_elements(extname)
     endif	
     
; Append information concerning the current extension to descriptor
     
     fdescript += ' ' + descript
     
; Check for EOF
; Skip the headers and data records

     ptr += nrec*2880LL
     if compress[nf] then mrd_skip,lun1,nrec*2880LL else point_lun,lun1,ptr
     if ~eof(lun1) then goto, START
;
     END_OF_FILE:  
     
     extname = extname[0:N_ext] ;strip off bogus first value
                                  ;otherwise will end up with '' at end

     if ~SILENT then begin
         printf,!textunit,file,' has ',strn(N_ext),' extensions'
         printf,!textunit,'Primary header: ',gettok(fdescript,' '),' records'
 
         Naxis = gettok( fdescript,' ' ) 
         
         If Naxis NE '0' then begin
             
             case gettok(fdescript,' ') of
                 
                 '1': image_type = 'Byte'
                 '2': image_type = 'Integer*2'    
                 '3': image_type = 'Integer*4'
                 '4': image_type = 'Real*4'
                 '5': image_type = 'Real*8'
                 
             endcase
             
             image_desc = 'Image -- ' + image_type + ' array ('
             for i = 0,fix(Naxis)-1 do image_desc = image_desc + ' '+ gettok(fdescript,' ')
             image_desc = image_desc+' )'
             
         endif else image_desc = 'No data'
         printf,!textunit, format='(a)',image_desc
         
         if N_ext GT 0 then begin
             for i = 1,N_ext do begin
                 
                 printf, !TEXTUNIT, 'Extension ' + strn(i) + ' -- '+extname[i]
                 
                 header_desc = '               Header : '+gettok(fdescript,' ')+' records'
                 printf, !textunit, format = '(a)',header_desc
                 
                 table_type = gettok(fdescript,' ')
                 
                 case table_type of
                     'A3DTABLE' : table_desc = 'Binary Table'
                     'BINTABLE' : table_desc = 'Binary Table'
                     'VAR_BINTABLE': table_desc = 'Variable length Binary Table'
                     'TABLE':     table_desc = 'ASCII Table'
                     ELSE:       table_desc = table_type
                 endcase

                 table_desc = '               ' + table_desc + ' ( '
                 table_dim = fix( gettok( fdescript,' ') )
                 if table_dim GT 0 then begin
                     table_type = gettok(fdescript,' ')
                     for j = 0, table_dim-1 do $
                             table_desc += gettok(fdescript,' ') + ' '
                 endif
                 table_desc += ')'
                 
                 printf,!textunit, format='(a)',table_desc
             endfor
         endif
         
         printf, !TEXTUNIT, ' '
     endif 
     SKIP: free_lun, lun1
 endfor
 if ~silent then textclose, TEXTOUT=textout
 return
 
 BAD_FILE:
 message, 'Error reading FITS file ' + file, /CON
 goto,SKIP
end
