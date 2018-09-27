;+
; NAME:
;     MRDFITS
;
; PURPOSE:
;     Read all standard FITS data types into arrays or structures.
;
; EXPLANATION:
;      Further information on MRDFITS is available at
;      http://idlastro.gsfc.nasa.gov/mrdfits.html 
;
; CALLING SEQUENCE:
;      Result = MRDFITS( Filename/FileUnit,[Exten_no/Exten_name, Header],
;                       /FPACK, /NO_FPACK, /FSCALE , /DSCALE , /UNSIGNED,
;                       ALIAS=strarr[2,n], /USE_COLNUM,
;                       /NO_TDIM, ROWS = [a,b,...], $
;                       /POINTER_VAR, /FIXED_VAR, EXTNUM= 
;                       RANGE=[a,b], COLUMNS=[a,b,...]), ERROR_ACTION=x,
;                       COMPRESS=comp_prog, STATUS=status, /VERSION, 
;                       /EMPTYSTRING )
;
; INPUTS:
;      Filename = String containing the name of the file to be read or
;                 file number of an open unit.  If an empty string is supplied,
;                 then user will be prompted for the file name.    The user
;                 will also be prompted if a wild card is given in the file
;                 name, and there is more than one file name match.
;                 If the file name ends in .gz or .fz (or .Z on Unix systems)
;                 the file will be dynamically decompressed.
;                                    or
;      FiluUnit = An integer file unit which has already been
;                 opened for input.  Data will be read from this
;                 unit and the unit will be left pointing immediately
;                 after the HDU that is read.  Thus to read a compressed
;                 file with many HDU's a user might do something like:
;                      lun=fxposit(filename, 3)  ; Skip the first three HDU's
;                      repeat begin
;                          thisHDU = mrdfits(lun, 0, hdr, status=status)
;                          ... process the HDU ...
;                      endrep until status lt 0
;
;      Exten_no= Extension number to be read, 0 for primary array.
;                 Assumed 0 if not specified.
;                 If a unit rather than a filename
;                 is specified in the first argument, this is
;                 the number of HDU's to skip from the current position.
;      Exten_name - Name of the extension to read (as stored in the EXTNAME
;                 keyword).   This is a slightly slower method then specifying
;                 the extension number.
; OUTPUTS:
;      Result = FITS data array or structure constructed from
;               the designated extension.  The format of result depends
;               upon the type of FITS data read.
;             Non-group primary array or IMAGE extension:
;               A simple multidimensional array is returned with the
;               dimensions given in the NAXISn keywords.
;             Grouped image data with PCOUNT=0.
;               As above but with GCOUNT treated as NAXIS(n+1).
;             Grouped image data with PCOUNT>0.
;               The data is returned as an array of structures.  Each
;               structure has two elements.  The first is a one-dimensional
;               array of the group parameters, the second is a multidimensional
;               array as given by the NAXIS2-n keywords.
;             ASCII and BINARY tables.
;               The data is returned as a structure with one column for
;               each field in the table.  The names of the columns are
;               normally taken from the TTYPE keywords (but see USE_COLNUM).
;               Bit field columns
;               are stored in byte arrays of the minimum necessary
;               length.  Spaces and invalid characters are replaced by 
;               underscores, and other invalid tag names are converted using
;               the IDL_VALIDNAME(/CONVERT_ALL) function.
;               Columns specified as variable length columns are stored
;               with a dimension equal to the largest actual dimension
;               used.  Extra values in rows are filled with 0's or blanks.
;               If the size of the variable length column is not
;               a constant, then an additional column is created giving the 
;               size used in the current row.  This additional column will 
;               have a tag name of the form L#_"colname" where # is the column
;               number and colname is the column name of the variable length
;               column.   If the length of each element of a variable length 
;               column is 0 then the column is deleted.
;
;
; OPTIONAL OUTPUT:
;       Header = String array containing the header from the FITS extension.
;
; OPTIONAL INPUT KEYWORDS:
;       ALIAS    The keyword allows the user to specify the column names
;                to be created when reading FITS data.  The value of
;                this keyword should be a 2xn string array.  The first
;                value of each pair of strings should be the desired
;                tag name for the IDL column.  The second should be
;                the FITS TTYPE value.  Note that there are restrictions
;                on valid tag names.  The order of the ALIAS keyword
;                is compatible with MWRFITS.
;       COLUMNS - This keyword allows the user to specify that only a
;                subset of columns is to be returned.  The columns
;                may be specified either as number 1,... n or by
;                name or some combination of these two.
;                If /USE_COLNUM is specified names should be C1,...Cn.
;                The use of this keyword will not save time or internal
;                memory since the extraction of specified columns
;                is done after all columns have been retrieved from the
;                FITS file.      Structure columns are returned in the order
;                supplied in this keyword.
;       COMPRESS - This keyword allows the user to specify a
;                decompression program to use to decompress a file that
;                will not be automatically recognized based upon
;                the file name.
;       /DSCALE - As with FSCALE except that the resulting data is
;                stored in doubles.
;       /EMPTYSTRING - There was a bug in memory management for IDL versions 
;                 prior to V8.0, causing a memory leak when reading
;                 empty strings in a FITS table.   Setting /EMPTYSTRING will
;                 avoid this problem by first reading strings into bytes and
;                 then converting.   However, there is a performance penalty.                 
;       ERROR_ACTION - Set the on_error action to this value (defaults
;                to 2).
;       /FIXED_VAR- Translate variable length columns into fixed length columns
;                and provide a length column for truly varying columns.
;                This was the only behavior prior to V2.5 for MRDFITS and remains
;                the default (see /POINTER_VAR)
;       /FPACK - If set, then assume the FITS file uses FPACK compression 
;                (http://heasarc.gsfc.nasa.gov/fitsio/fpack/).     To read
;                an FPACK compressed file, either this must be set or the 
;                file name must end in ".fz"
;       /NO_FPACK - If present, then MRDFITS will not uncompress an extension
;                compressed with FPACK (i.e with a .fz extension), but will 
;                just read the compressed binary stream. 
;       /FSCALE - If present and non-zero then scale data to float
;                numbers for arrays and columns which have either
;                non-zero offset or non-unity scale.
;                If scaling parameters are applied, then the corresponding
;                FITS scaling keywords will be modified.
;       NO_TDIM  - Disable processing of TDIM keywords.  If NO_TDIM
;                is specified MRDFITS will ignore TDIM keywords in
;                binary tables.
;       /POINTER_VAR- Use pointer arrays for variable length columns.
;                The pointer tag must be dereferenced in the output structure
;                to access the variable length column.   Prior to IDL V8.0, the user 
;                was responsible for memory management when deleting or reassigning 
;                the structure (e.g. using HEAP_FREE), but memory management of pointer
;                arrays is now automatic.         
;       RANGE  - A scalar or two element vector giving the start
;                and end rows to be retrieved.  For ASCII and BINARY
;                tables this specifies the row number.  For GROUPed data
;                this will specify the groups.  For array images, this
;                refers to the last non-unity index in the array.  E.g.,
;                for a 3 D image with NAXIS* values = [100,100,1], the
;                range may be specified as 0:99, since the last axis
;                is suppressed.  Note that the range uses IDL indexing
;                So that the first row is row 0.
;                If only a single value, x, is given in the range,
;                the range is assumed to be [0,x-1].
;       ROWS -  A scalar or vector specifying a  specific row or rows to read 
;               (first row is 0).   For example to read rows 0,
;               12 and 23 only, set ROWS=[0,12,23].   Valid for images, ASCII 
;               and binary tables, but not GROUPed data.   For images
;               the row numbers refer to the last non-unity index in the array.
;               Note that the use of the ROWS will not improve the speed of
;               MRDFITS since the entire table will be read in, and then subset
;               to the specified rows.     Cannot be used at the same time as 
;               the RANGE keyword
;       /SILENT - Suppress informative messages.
;       STRUCTYP - The structyp keyword specifies the name to be used
;                for the structure defined when reading ASCII or binary
;                tables.  Generally users will not be able to conveniently
;                combine data from multiple files unless the STRUCTYP
;                parameter is specified.  An error will occur if the
;                user specifies the same value for the STRUCTYP keyword
;                in calls to MRDFITS in the same IDL session for extensions
;                which have different structures.
;       /UNSIGNED - For integer data with appropriate zero points and scales
;                read the data into unsigned integer arrays.
;       /USE_COLNUM - When creating column names for binary and ASCII tables
;                MRDFITS attempts to use the appropriate TTYPE keyword
;                values.  If USE_COLNUM is specified and non-zero then
;                column names will be generated as 'C1, C2, ... 'Cn'
;                for the number of columns in the table.
;       /VERSION Print the current version number
;
; OPTIONAL OUTPUT KEYWORDS:
;       EXTNUM - the number of the extension actually read.   Useful if the
;                 user specified the extension by name.
;       OUTALIAS - This is a 2xn string array where the first column gives the
;                actual structure tagname, and the second gives the
;                corresponding FITS keyword name (e.g. in the TTYPE keyword).   
;                This array can be passed directly to
;                the alias keyword of MWRFITS to recreate the file originally
;                read by MRDFITS.
;       STATUS - A integer status indicating success or failure of
;                the request.  A status of >=0 indicates a successful read.
;                Currently
;                    0 -> successful completion
;                   -1 -> error
;                   -2 -> end of file
;
; EXAMPLES:
;       (1) Read a FITS primary array:
;               a = mrdfits('TEST.FITS')    or
;               a = mrdfits('TEST.FITS', 0, header)
;       The second example also retrieves header information.
;
;       (2) Read rows 10-100 of the second extension of a FITS file.
;               a = mrdfits('TEST.FITS', 2, header, range=[10,100])
;
;       (3) Read a table and ask that any scalings be applied and the
;       scaled data be converted to doubles.  Use simple column names,
;       suppress outputs.
;               a = mrdfits('TEST.FITS', 1, /dscale, /use_colnum, /silent)
;
;       (4) Read rows 3, 34 and 52 of a binary table and request that 
;           variable length columns be stored as a pointer variable in the 
;           output structure
;              a = mrdfits('TEST.FITS',1,rows=[3,34,52],/POINTER)
  
; RESTRICTIONS:
;       (1)     Cannot handle data in non-standard FITS formats.
;       (2)     Doesn't do anything with BLANK or NULL values or
;               NaN's.  They are just read in.  They may be scaled
;               if scaling is applied.
;       (3)     Does not automatically detect a FPACK compressed file.  Either
;               the file name must end in .fz, or the /FPACK keyword must
;               be set
; NOTES:
;       This multiple format FITS reader is designed to provide a
;       single, simple interface to reading all common types of FITS data.
;       MRDFITS DOES NOT scale data by default.  The FSCALE or DSCALE
;       parameters must be used.
;
;       Null values in an FITS ASCII table are converted to NaN (floating data),
;       or -2147483647L (longwords) or '...' (strings).   
;
; PROCEDURES USED:
;       The following procedures are contained in the main MRDFITS program.
;           MRD_IMAGE           -- Generate array/structure for images.
;           MRD_READ_IMAGE      -- Read image data.
;           MRD_ASCII           -- Generate structure for ASCII tables.
;           MRD_READ_ASCII      -- Read an ASCII table.
;           MRD_TABLE           -- Generate structure for Binary tables.
;           MRD_READ_TABLE      -- Read binary table info.
;           MRD_READ_HEAP       -- Read variable length record info.
;           MRD_SCALE           -- Apply scaling to data.
;           MRD_COLUMNS         -- Extract columns.
;
;        Other ASTRON Library routines used
;           FXPAR(), FXADDPAR, FXPOSIT, FXMOVE(), MATCH, MRD_STRUCT(), MRD_SKIP 
;
; MODIfICATION HISTORY:
;       V1.0 November 9, 1994 ----  Initial release.
;          Creator: Thomas A. McGlynn
;       V1.1 January 20, 1995 T.A. McGlynn
;          Fixed bug in variable length records.
;          Added TDIM support -- new routine mrd_tdim in MRD_TABLE.
;       V1.2
;          Added support for dynamic decompression of files.
;          Fixed further bugs in variable length record handling.
;       V1.2a
;          Added NO_TDIM keyword to turn off TDIM processing for
;          those who don't want it.
;          Bug fixes: Handle one row tables correctly, use BZERO rather than
;               BOFFSET.     Fix error in scaling of images.  
;       V1.2b 
;          Changed MRD_HREAD to handle null characters in headers.
;       V2.0 April 1, 1996
;          -Handles FITS tables with an arbitrary number of columns.
;          -Substantial changes to MRD_STRUCT to allow the use of
;          substructures when more than 127 columns are desired.
;          -All references to table columns are now made through the
;          functions MRD_GETC and MRD_PUTC.  See description above.
;          -Use of SILENT will now eliminate compilation messages for
;          temporary functions.
;          -Bugs in handling of variable length columns with either
;          a single row in the table or a maximum of a single element
;          in the column fixed.
;          -Added support for DCOMPLEX numbers in binary tables (M formats) for
;          IDL versions above 4.0.  
;          -Created regression test procedure to check in new versions.
;          -Added error_action parameter to allow user to specify
;          on_error action.  This should allow better interaction with
;          new CHECK facility.  ON_ERROR statements deleted from
;          most called routines.
;          - Modified MRDFITS to read in headers containing null characters
;          with a warning message printed.
;       V2.0a April 16, 1996
;          - Added IS_IEEE_BIG() checks (and routine) so that we don't
;          worry about IEEE to host conversions if the machine's native
;          format is IEEE Big-endian.
;       V2.1 August 24, 1996
;          - Use resolve_routine for dynamically defined functions
;          for versions > 4.0
;          - Fix some processing in random groups format.
;          - Handle cases where the data segment is--legally--null.
;          In this case MRDFITS returns a scalar 0.
;          - Fix bugs with the values for BSCALE and BZERO (and PSCAL and
;          PZERO) parameters set by MRDFITS.
;       V2.1a April 24, 1997  Handle binary tables with zero length columns
;       V2.1b May 13,1997 Remove whitespace from replicate structure definition
;       V2.1c May 28,1997 Less strict parsing of XTENSION keyword
;       V2.1d June 16, 1997 Fixed problem for >32767 entries introduced 24-Apr
;       V2.1e Aug 12, 1997 Fixed problem handling double complex arrays
;       V2.1f Oct 22, 1997 IDL reserved words can't be structure tag names
;       V2.1g Nov 24, 1997 Handle XTENSION keywords with extra blanks.
;       V2.1h Jul 26, 1998 More flexible parsing of TFORM characters
;       V2.2 Dec 14, 1998 Allow fields with longer names for
;                        later versions of IDL.
;                        Fix handling of arrays in scaling routines.
;                        Allow >128 fields in structures for IDL >4.0
;                        Use more efficient structure copying for
;                        IDL>5.0
;       V2.2b June 17, 1999 Fix bug in handling case where
;                           all variable length columns are deleted
;                           because they are empty.
;       V2.3 March 7, 2000 Allow user to supply file handle rather
;                          than file name.
;                          Added status field.
;                          Now needs FXMOVE routine
;       V2.3b April 4, 2000
;                          Added compress option (from D. Palmer)
;       V2.4  July 4, 2000 Added STATUS=-1 for "File access error" (Zarro/GSFC)
;       V2.4a May 2, 2001  Trim binary format string   (W. Landsman)
;       V2.5 December 5, 2001 Add unsigned, alias, 64 bit integers. version, $
;                           /pointer_val, /fixed_var.
;       V2.5a Fix problem when both the first and the last character
;            in a TTYPEnn value are invalid structure tag characters
;       V2.6 February 15, 2002 Fix error in handling unsigned numbers, $
;                           and 64 bit unsigneds. (Thanks to Stephane Beland)
;       V2.6a September 2, 2002 Fix possible conflicting data structure for
;                          variable length arrays (W. Landsman)
;       V2.7 July, 2003  Added Rows keyword (W. Landsman)
;       V2.7a September  2003 Convert dimensions to long64 to handle huge files
;       V2.8 October 2003 Use IDL_VALIDNAME() function to ensure valid tag names
;                         Removed OLD_STRUCT and TEMPDIR keywords W. Landsman
;       V2.9 February 2004 Added internal MRD_FXPAR procedure for faster
;                     processing of binary table headers E. Sheldon
;       V2.9a March 2004 Restore ability to read empty binary table W. Landsman
;             Swallow binary tables with more columns than given in TFIELDS
;       V2.9b Fix to ensure order of TFORMn doesn't matter
;       V2.9c Check if extra degenerate NAXISn keyword are present W.L. Oct 2004 
;       V2.9d Propagate /SILENT to MRD_HREAD, more LONG64 casting W. L. Dec 2004
;       V2.9e Add typarr[good] to fix a problem reading zero-length columns
;             A.Csillaghy, csillag@ssl.berkeley.edu (RHESSI)
;       V2.9f Fix problem with string variable binary tables, possible math 
;             overflow on non-IEEE machines  WL Feb. 2005 
;       V2.9g Fix problem when setting /USE_COLNUM   WL Feb. 2005
;       V2.10 Use faster keywords to BYTEORDER  WL May 2006
;       V2.11  Add ON_IOERROR, CATCH, and STATUS keyword to MRD_READ_IMAGE to
;             trap EOF in compressed files DZ  Also fix handling of unsigned 
;             images when BSCALE not present  K Chu/WL   June 2006 
;       V2.12 Allow extension to be specified by name, added EXTNUM keyword
;                     WL    December 2006
;       V2.12a Convert ASCII table column to DOUBLE if single precision is
;                 insufficient  
;       V2.12b Fixed problem when both /fscale and /unsigned are set 
;                  C. Markwardt    Aug 2007
;       V2.13  Use SWAP_ENDIAN_INPLACE instead of IEEE_TO_HOST and IS_IEEE_BIG
;                W. Landsman Nov 2007
;       V2.13a One element vector allowed for file name W.L. Dec 2007
;       V2.13b More informative error message when EOF found W.L. Jun 2008
;       V2.14  Use vector form of VALID_NUM(), added OUTALIAS keyword
;                                       W.L. Aug 2008
;       V2.15  Use new FXPOSIT which uses on-the-fly byteswapping W.L. Mar 2009
;       V2.15a Small efficiency updates to MRD_SCALE W.L. Apr 2009
;       V2.15b Fixed typo introduced Apr 2009
;       V2.15c Fix bug introduced Mar 2009  when file unit used W.L. July 2009
;       V2.16  Handle FPACK compressed files    W. L. July 2009
;       V2.17  Use compile_opt hidden on all routines except mrdfits.pro W.L. July 2009
;       V2.18  Added /EMPTYSTRING keyword W. Landsman August 2009
;       V2.18a Fix Columns keyword output, A. Kimball/ W. Landsman Feb 2010
;       V2.18b Fix bug with /EMPTYSTRING and multidimensional strings
;                             S. Baldridge/W.L. August 2010
;       V2.18c Fix unsigned bug caused by compile_opt idl2 WL  Nov 2010
;       V2.19  Use V6.0 operators WL Nov 2010
;       V2.19a Fix complex data conversion in variable length tables WL Dec 2010 
;       V2.19b Fix bug with /FSCALE introduced Nov 2010 WL Jan 2011
;       V2.19c Fix bug with ROWS keyword introduced Nov 2010 WL Mar 2011
;       V2.20  Convert Nulls in ASCII tables, better check of duplicate keywords
;                                            WL May 2011
;       V2.20a Better error checking for FPACK files  WL October 2012
;       V2.20b Fix bug in MRD_SCALE introduced Nov 2010 (Sigh) WL Feb 2013
;       V2.21  Create unique structure tags when FITS column names differ 
;              only in having a different case   R. McMahon/WL   March 2013
;       V2.22  Handle 64 bit variable length binary tables WL   April 2014
;       V2.23  Use 64 bit for  very large files  WL  April 2014
;       V2.24  Binary table is allowed to have zero columns  WL  September 2018
;-
PRO mrd_fxpar, hdr, xten, nfld, nrow, rsize, fnames, fforms, scales, offsets
compile_opt idl2, hidden
;
;  Check for valid header.  Check header for proper attributes.
;
  S = SIZE(HDR)
  IF ( S[0] NE 1 ) || ( S[2] NE 7 ) THEN $
    MESSAGE,'FITS Header (first parameter) must be a string array'

  xten = fxpar(hdr, 'XTENSION')
  nfld = fxpar(hdr, 'TFIELDS')
  nrow = long64(fxpar(hdr, 'NAXIS2'))
  rsize = long64(fxpar(hdr, 'NAXIS1'))

  ;; will extract these for each
  names = ['TTYPE','TFORM', 'TSCAL', 'TZERO']
  nnames = n_elements(names)

;  Start by looking for the required TFORM keywords.    Then try to extract it 
;  along with names (TTYPE), scales (TSCAL), and offsets (TZERO)
   
 keyword = STRMID( hdr, 0, 8)

;
;  Find all instances of 'TFORM' followed by
;  a number.  Store the positions of the located keywords in mforms, and the
;  value of the number field in n_mforms
;

  mforms = WHERE(STRPOS(keyword,'TFORM') GE 0, n_mforms)
  if n_mforms GT nfld then begin
        message,/CON, $
        'WARNING - More columns found in binary table than specified in TFIELDS'
        n_mforms = nfld
        mforms = mforms[0:nfld-1]
  endif


  IF ( n_mforms GT 0 ) THEN BEGIN
      numst= STRMID(hdr[mforms], 5 ,3)      
 
      igood = WHERE(VALID_NUM(numst,/INTEGER), n_mforms)
      IF n_mforms GT 0 THEN BEGIN
          mforms = mforms[igood]
          number = fix( numst[igood])
          numst = numst[igood]
      ENDIF

  ENDIF ELSE RETURN              ;No fields in binary table

  ;; The others
  fnames = strarr(n_mforms)
  fforms = strarr(n_mforms) 
  scales = dblarr(n_mforms)
  offsets = dblarr(n_mforms)

  ;;comments = strarr(n_mnames)

  fnames_names  = 'TTYPE'+numst
  scales_names  = 'TSCAL'+numst
  offsets_names = 'TZERO'+numst
  number = number -1    ;Make zero-based


  match, keyword, fnames_names, mkey_names, mnames, count = N_mnames

  match, keyword, scales_names, mkey_scales, mscales, count = N_mscales

  match, keyword, offsets_names, mkey_offsets, moffsets,count = N_moffsets

  FOR in=0L, nnames-1 DO BEGIN 

      CASE names[in] OF
          'TTYPE': BEGIN 
              tmatches = mnames 
              matches = mkey_names
              nmatches = n_mnames
              result = fnames
          END 
          'TFORM': BEGIN 
              tmatches = lindgen(n_mforms)
              matches = mforms
              nmatches = n_mforms
              result = fforms
          END 
          'TSCAL': BEGIN 
              tmatches = mscales
              matches = mkey_scales
              nmatches = n_mscales
              result = scales
          END 
          'TZERO': BEGIN 
              tmatches = moffsets
              matches = mkey_offsets
              nmatches = n_moffsets
              result = offsets
          END 
          ELSE: message,'What?'
      ENDCASE 

      ;;help,matches,nmatches

;
;  Extract the parameter field from the specified header lines.  If one of the
;  special cases, then done.
;
      IF nmatches GT 0 THEN BEGIN 
          
          ;; "matches" is a subscript for hdr and keyword.
          ;; get just the matches in line
          
          line = hdr[matches]
          svalue = STRTRIM( STRMID(line,9,71),2)
          
          FOR i = 0, nmatches-1 DO BEGIN
              IF ( STRMID(svalue[i],0,1) EQ "'" ) THEN BEGIN

                  ;; Its a string
                  test = STRMID( svalue[i],1,STRLEN( svalue[i] )-1)
                  next_char = 0
                  off = 0
                  value = ''
;
;  Find the next apostrophe.
;
NEXT_APOST:
                  endap = STRPOS(test, "'", next_char)
                  IF endap LT 0 THEN MESSAGE,         $
                    'WARNING: Value of '+nam+' invalid in '+ " (no trailing ')", /info
                  value = value + STRMID( test, next_char, endap-next_char )
;
;  Test to see if the next character is also an apostrophe.  If so, then the
;  string isn't completed yet.  Apostrophes in the text string are signalled as
;  two apostrophes in a row.
;
                  IF STRMID( test, endap+1, 1) EQ "'" THEN BEGIN    
                      value = value + "'"
                      next_char = endap+2      
                      GOTO, NEXT_APOST
                  ENDIF

                 
;
;  If not a string, then separate the parameter field from the comment field.
;
              ENDIF ELSE BEGIN
                  ;; not a string
                  test = svalue[I]
                  slash = STRPOS(test, "/")
                  IF slash GT 0 THEN  test = STRMID(test, 0, slash)
                 
;
;  Find the first word in TEST.  Is it a logical value ('T' or 'F')?
;
                  test2 = test
                  value = GETTOK(test2,' ')
                  test2 = STRTRIM(test2,2)
                  IF ( value EQ 'T' ) THEN BEGIN
                      value = 1
                  END ELSE IF ( value EQ 'F' ) THEN BEGIN
                      value = 0
                  END ELSE BEGIN
;
;  Test to see if a complex number.  It's a complex number if the value and the
;  next word, if any, both are valid numbers.
;
                      IF STRLEN(test2) EQ 0 THEN GOTO, NOT_COMPLEX
                      test2 = GETTOK(test2,' ')
                      IF VALID_NUM(value,val1) && VALID_NUM(value2,val2) $
                        THEN BEGIN
                          value = COMPLEX(val1,val2)
                          GOTO, GOT_VALUE
                      ENDIF
;
;  Not a complex number.  Decide if it is a floating point, double precision,
;  or integer number.  If an error occurs, then a string value is returned.
;  If the integer is not within the range of a valid long value, then it will 
;  be converted to a double.  
;
NOT_COMPLEX:
                      ON_IOERROR, GOT_VALUE
                      value = test
                      IF ~VALID_NUM(value) THEN GOTO, GOT_VALUE

                      IF (STRPOS(value,'.') GE 0) || (STRPOS(value,'E') $
                                                      GE 0) || (STRPOS(value,'D') GE 0) THEN BEGIN
                          IF ( STRPOS(value,'D') GT 0 ) || $
                            ( STRLEN(value) GE 8 ) THEN BEGIN
                              value = DOUBLE(value)
                          END ELSE value = FLOAT(value)
                      ENDIF ELSE BEGIN
                          lmax = long64(2)^31 - 1
                          lmin = -long64(2)^31
                          value = long64(value)
                          if (value GE lmin) && (value LE lmax) THEN $
                            value = LONG(value)
                      ENDELSE
                      
;
GOT_VALUE:
                      ON_IOERROR, NULL
                  ENDELSE
              ENDELSE           ; if string
;
;  Add to vector if required.
;
              
              result[tmatches[i]] = value
             
          ENDFOR

          CASE names[in] OF
              'TTYPE': fnames[number] = strtrim(result, 2)
              'TFORM': fforms[number] = strtrim(result, 2)
              'TSCAL': scales[number] = result
              'TZERO': offsets[number] = result
              ELSE: message,'What?'
          ENDCASE 

;
;  Error point for keyword not found.
;
      ENDIF
;



  ENDFOR 
END
  

; Get a tag name give the column name and index
function  mrd_dofn, name, index, use_colnum, alias=alias
compile_opt idl2, hidden
    ; Check if the user has specified an alias.

    name = N_elements(name) EQ 0 ? 'C' + strtrim(index,2) : strtrim(name) 
    if keyword_set(alias) then begin
	sz = size(alias)
	
	if (sz[0] eq 1 || sz[0] eq 2) && (sz[1] eq 2) && (sz[sz[0]+1] eq 7) $
	   then begin
	    w = where( name eq alias[1,*], Nw)
	    if Nw GT 0 then name = alias[0,w[0]];
	endif
    endif
    ; Convert the string name to a valid variable name.  If name 
    ; is not defined generate the string Cnn when nn is the index 
    ; number.

    table = 0
     if ~use_colnum && (N_elements(name) GT 0)  then begin 
        if size(name,/type) eq 7 then begin 
            str = name[0] 
        endif else str = 'C'+strtrim(index,2) 
     endif else str = 'C'+strtrim(index,2) 
  
    return, IDL_VALIDNAME(str,/CONVERT_ALL) 
 
end 

;***************************************************************



; Parse the TFORM keyword and return the type and dimension of the 
; data. 
pro mrd_doff, form, dim, type 
compile_opt idl2, hidden 
    ; Find the first non-numeric character. 
 
    len = strlen(form) 
 
    if len le 0 then return 
 
    i = stregex( form, '[^0-9]')       ;Position of first non-numeric character
 
    if i lt 0 then return              ;Any non-numeric character found?
 
    if i gt 0 then begin 
        dim = long(strmid(form, 0, i)) 
        if dim EQ 0l then dim = -1l
    endif else dim = 0
 
    type = strmid(form, i, 1) 
end 



;*********************************************************************

;  Check that this name is unique with regard to other column names.

function mrd_chkfn, name, namelist, index, silent=silent
  compile_opt idl2, hidden
     ;
     ;

     maxlen = 127

     if strlen(name) gt maxlen then name = strmid(name, 0, maxlen)
     ; make case insensitive since structure tags are case insensitive
     ; (rgm 2013-03-03)
     ;if ~array_equal(namelist eq name,0b ) then begin
     if ~array_equal(strupcase(namelist) eq strupcase(name),0b ) then begin

         oldname=name
         name = 'gen$name_'+strcompress(string(index+1),/remove_all)

         ; report the column name conflict
         if ~keyword_set(silent) then print, 'Column name conflict: ', $
           index, ': ', oldname, ' -> ', name

     endif

     return, name
end     

; Find the appropriate offset for a given unsigned type.
; The type may be given as the bitpix value or the IDL
; variable type.

function mrd_unsigned_offset, type
compile_opt idl2, hidden
    	    
    if (type eq 12) || (type eq 16) then begin
	return, uint(32768)
    endif else if (type eq 13) || (type eq 32) then begin
	return, ulong('2147483648')
    endif else if (type eq 15) || (type eq 64) then begin
	return, ulong64('9223372036854775808');
    endif
    return, 0
end



; Can we treat this data as unsigned?

function mrd_chkunsigned, bitpix, scale, zero, unsigned=unsigned
compile_opt idl2, hidden
    if ~keyword_set(unsigned) then return, 0

    if scale eq 1 then begin
	if (bitpix eq 16 && zero eq 32768L) ||                   $
	   (bitpix eq 32 && zero eq 2147483648UL) ||        $
	   (bitpix eq 64 && zero eq 9223372036854775808ULL) then return,1
    endif
    
    return, 0
end

; Is this one of the IDL unsigned types?
function mrd_unsignedtype, data
 compile_opt idl2, hidden      
 type = size(data,/type) 
 
    if (type eq 12) || (type eq 13) || (type eq 15) then return, type $
                                                    else return, 0
    
end
    
; Return the currrent version string for MRDFITS
function mrd_version
compile_opt idl2, hidden
    return, '2.23 '
end
;=====================================================================
; END OF GENERAL UTILITY FUNCTIONS ===================================
;=====================================================================


; Parse the TFORM keyword and return the type and dimension of the
; data.
pro mrd_atype, form, type, slen
compile_opt idl2, hidden

    ; Find the first non-numeric character.


    ; Get rid of blanks.
    form = strcompress(form,/remove_all)
    len = strlen(form)
    if len le 0 then return

    type = strmid(form, 0,1)
    length = strmid(form,1,len-1)
    ;
    ; Ignore the number of decimal places.  We assume that there
    ; is a decimal point.
    ;
    p = strpos(length, '.')
    if p gt 0 then length = strmid(length,0,p)

   if strlen(length) gt 0 then slen = fix(length) else slen = 1
   if (type EQ 'F') || (type EQ 'E') then $     ;Updated April 2007
        if (slen GE 8) then type = 'D'

end


; Read in the table information.
pro mrd_read_ascii, unit, range, nbytes, nrows, nfld, typarr, posarr,   $
     lenarr, nullarr, table, old_struct=old_struct, rows=rows
compile_opt idl2, hidden
    ;
    ; Unit          Unit to read data from.
    ; Range         Range of  to be read
    ; Nbytes        Number of bytes per row.
    ; Nrows         Number of rows.
    ; Nfld          Number of fields in structure.
    ; Typarr        Array indicating type of variable.
    ; Posarr        Starting position of fields (first char at 0)
    ; Lenarr        Length of fields
    ; Nullarr       Array of null values
    ; Table         Table to read information into.
    ; Old_struct    Should recursive structure format be used?

    bigstr = bytarr(nbytes, range[1]-range[0]+1)

    if range[0] gt 0 then mrd_skip, unit, nbytes*range[0]
    readu,unit, bigstr
    if N_elements(rows) GT 0 then bigstr = bigstr[*,rows-range[0]] 

    ; Skip to the end of the data area.

    nSkipRow = nrows - range[1] - 1
    nskipB = 2880 - (nbytes*nrows) mod 2880
    if nskipB eq 2880 then nskipB = 0

    mrd_skip, unit, nskipRow*nbytes+nskipB

    s1 = posarr-1
    s2 = s1 + lenarr - 1
    for i=0, nfld-1 do begin
        flds = strtrim(bigstr[s1[i]:s2[i],* ]) 
       if nullarr[i] ne '' then begin
	    
		curr_col = table.(i)
           w = where(flds NE strtrim(nullarr[i]), Ngood)
	 
            if Ngood GT 0 then begin
                if N_elements(w) EQ 1 then w = w[0]
                if typarr[i] eq 'I' then begin
                    curr_col[w] = long(flds[w])
                endif else if typarr[i] eq 'E' || typarr[i] eq 'F' then begin
                    curr_col[w] = float(flds[w])
                endif else if typarr[i] eq 'D' then begin
                    curr_col[w] = double(flds[w])
                endif else if typarr[i] eq 'A' then begin
                    curr_col[w] = flds[w]
                endif
            endif

                table.(i) = curr_col
                
        endif else begin
                
   

           if typarr[i] eq 'I' then begin
                    table.(i) =  long(flds)
            endif else if typarr[i] eq 'E' || typarr[i] eq 'F' then begin
                    table.(i) = float(flds)
            endif else if typarr[i] eq 'D' then begin
                    table.(i) = double(flds)
             endif else if typarr[i] eq 'A' then begin
                    table.(i) = flds
            endif
        endelse
    endfor

end


; Define a structure to hold a FITS ASCII table.
pro mrd_ascii, header, structyp, use_colnum,   $
    range, table, $
    nbytes, nrows, nfld, typarr, posarr, lenarr, nullarr, $
    fnames, fvalues, scales, offsets, scaling, status, rows = rows, $
    silent=silent, columns=columns, alias=alias, outalias=outalias
compile_opt idl2, hidden
    ;
    ; Header                FITS header for table.
    ; Structyp              IDL structure type to be used for
    ;                       structure.
    ; Use_colnum            Use column numbers not names.
    ; Range                 Range of rows of interest
    ; Table                 Structure to be defined.
    ; Nbytes                Bytes per row
    ; Nrows                 Number of rows in table
    ; Nfld                  Number of fields
    ; Typarr                Array of field types
    ; Posarr                Array of field offsets
    ; Lenarr                Array of field lengths
    ; Nullarr               Array of field null values
    ; Fname                 Column names
    ; Fvalues               Formats for columns
    ; Scales/offsets        Scaling factors for columns
    ; Scaling               Do we need to scale?
    ; Status                Return status.

    table = 0

    types  = ['I', 'E', 'F', 'D', 'A']
; Set default 'null' values   
    sclstr = ['-2147483647L', '!VALUES.f_nan', '!VALUES.f_nan', '!VALUES.d_nan', '...']
    status = 0

    if strmid(fxpar(header, 'XTENSION'),0,8) ne 'TABLE   ' then begin
        message, 'ERROR - Header is not from ASCII table.',/CON
        status = -1;
        return
    endif

    nfld = fxpar(header, 'TFIELDS')
    nrows = long64( fxpar(header, 'NAXIS2'))
    nbytes = long64( fxpar(header, 'NAXIS1'))
 
    if range[0] ge 0 then begin
        range[0] = range[0] < (nrows-1)
        range[1] = range[1] < (nrows-1)
    endif else begin
        range[0] = 0
        range[1] = nrows-1
    endelse

    if N_elements(rows) EQ 0 then nrows = range[1] - range[0] + 1 else begin
          bad = where(rows GT nrows, Nbad)
          if Nbad GT 0  then begin 
             message,/CON,'ERROR: Row numbers must be between 0 and ' + $
                      strtrim(nrows-1,2)
             status = -1
             return
          endif      
          nrows = N_elements(rows)
     endelse

    if nrows le 0 then begin
        if ~keyword_set(silent) then begin
            print,'MRDFITS: ASCII table.  ',strcompress(string(nfld)),  $
                  ' columns, no rows'
        endif
        return
    endif

    ;
    ;  Loop over the columns

    typarr  = strarr(nfld)
    lenarr  = intarr(nfld)
    posarr  = intarr(nfld)
    nullarr = strarr(nfld)
    fnames  = strarr(nfld)
    fvalues = strarr(nfld)
    scales  = dblarr(nfld)
    offsets = dblarr(nfld)
    tname  =  strarr(nfld)

    for i=0, nfld-1 do begin
        suffix = strcompress(string(i+1), /remove_all)
        fname = fxpar(header, 'TTYPE' + suffix, count=cnt)
	tname[i] = fname
	if cnt eq 0 then xx = temporary(fname)
        fform = fxpar(header, 'TFORM' + suffix)
        fpos = fxpar(header, 'TBCOL' + suffix)
        fnull = fxpar(header, 'TNULL' + suffix, count=cnt)
	if cnt eq 0 then fnull = ''
        scales[i] = fxpar(header, 'TSCAL' + suffix)
        if scales[i] eq 0.0d0 then scales[i] = 1.0d0
        offsets[i] = fxpar(header, 'TZERO'+suffix)
        
        fname = strupcase( mrd_dofn(fname,i+1, use_colnum, alias=alias))

        if i GT 0 then fname = mrd_chkfn(fname, fnames, i, SILENT=silent) ;Check for duplicates
	fnames[i] = fname
        
        mrd_atype, fform, ftype, flen
        typarr[i] = ftype
        lenarr[i] = flen
        posarr[i] = fpos
        nullarr[i] = fnull
        
 
       j = where(types EQ ftype, Nj) 
       if Nj EQ 0 then begin 
                message, 'Invalid format code:'+ ftype + ' for column ' + $
		    strtrim(i+1,2),/CON
                status = -1
                return
       endif	       
       fvalues[i] = ftype NE 'A' ? sclstr[j] : $
	                  'string(replicate(32b,'+strtrim(flen,2)+'))'
                                               
         
    endfor
    
    if scaling then $
        scaling = ~array_equal(scales,1.0d0) || ~array_equal(offsets,0.0)
   
    if ~scaling && ~keyword_set(columns) then begin
        table = mrd_struct(fnames, fvalues, nrows, structyp=structyp, $
           silent=silent)
    endif else begin
        table = mrd_struct(fnames, fvalues, nrows, silent=silent)
    endelse

    if ~keyword_set(silent) then begin
        print,'MRDFITS: ASCII table.  ',strcompress(string(nfld)),  $
         ' columns by ',strcompress(string(nrows)), ' rows.'
    endif
    
    outalias = transpose([ [tag_names(table)],[tname] ] )
    status = 0
    return

end


; Eliminate columns from the table that do not match the
; user specification.
pro  mrd_columns, table, columns, fnames, fvalues, $
    vcls, vtpes, scales,  offsets, scaling,        $
    structyp=structyp, silent=silent
compile_opt idl2, hidden



    type = size(columns,/type)
    nele = N_elements(columns)
    if type eq 8 || type eq 6 || type eq 0 then return  ; Can't use structs
                                                    ; or complex.

    if type eq 4 || type eq 5 then tcols = fix(columns)
    if type eq 1 || type eq 2 || type eq 3 then tcols = columns

    ; Convert strings to uppercase and compare with column names.

    if type eq 7 then begin
       match, strupcase(columns), strupcase(fnames), tmp, tcols,count=nmatch 
       if Nmatch GT 0 then begin 
              s = sort(tmp)             ;Sort order of supplied column name
              tcols = tcols[s] + 1
       endif     
     endif

    ; Subtract one from column indices and check that all indices >= 0.
    if n_elements(tcols) gt 0 then begin
        tcols = tcols-1
        w = where(tcols ge 0, Nw)
        if Nw EQ 0 then dummy = temporary(tcols)
    endif

    if n_elements(tcols) le 0 then begin
        print, 'MRDFITS:  No columns match'
        
        ; Undefine variables.  First ensure they are defined, then
        ; use temporary() to undefine them.
        table = 0
        fnames = 0
        fvalues = 0
        vcls = 0
        vtpes = 0
        scales = 0
        offsets = 0
        dummy = temporary(fnames)
        dummy = temporary(fvalues)
        dummy = temporary(vcls)
        dummy = temporary(vtpes)
        dummy = temporary(scales)
        dummy = temporary(offsets)
        scaling = 0
        
    endif else begin

        ; Replace arrays with only desired columns.
        
        fnames = fnames[tcols]
        fvalues = fvalues[tcols]
        
        ; Check if there are still variable length columns.
        if n_elements(vcls) gt 0 then begin
            vcls = vcls[tcols]
            vtpes = vtpes[tcols]
            w = where(vcls eq 1, Nw)
            if Nw EQ 0 then begin
                dummy = temporary(vcls)
                dummy = temporary(vtpes)
            endif
        endif
        
        ; Check if there are still columns that need scaling.
        if n_elements(scales) gt 0 then begin
            scales = scales[tcols]
            offsets = offsets[tcols]
	    scaling = ~array_equal(scales,1.d0) || ~array_equal(offsets,0.0)
         endif
        

        ndim = n_elements(table)
        
        if scaling || n_elements(vcls) gt 0 then begin
            tabx = mrd_struct(fnames, fvalues, ndim, silent=silent )
        endif else begin
            tabx = mrd_struct(fnames, fvalues, ndim, structyp=structyp, silent=silent )
        endelse
        
        for i=0, n_elements(tcols)-1 do $
                tabx.(i) = table.(tcols[i]);
 
        table = temporary(tabx)
    endelse
    
end


; Read in the image information. 
pro mrd_read_image, unit, range, maxd, rsize, table, rows = rows,status=status, $
     unixpipe = unixpipe
 compile_opt idl2, hidden
    ; 
    ; Unit          Unit to read data from. 
    ; Table         Table/array to read information into. 
    ; 

    error=0
    catch,error
    if error ne 0 then begin
     catch,/cancel
     status=-2
     return
    endif

    ; If necessary skip to beginning of desired data. 

    if range[0] gt 0 then mrd_skip, unit, range[0]*rsize

    status=-2
    if rsize eq 0 then return 

    on_ioerror,done
    readu, unit, table

    if N_elements(rows) GT 0 then begin
    row1 = rows- range[0]
    case size(table,/n_dimen) of 
    1: table = table[row1]
    2: table = table[*,row1]
    3: table = table[*,*,row1]
    4: table = table[*,*,*,row1]
    5: table = table[*,*,*,*,row1]
    6: table = table[*,*,*,*,*,row1]
    7: table = table[*,*,*,*,*,*,row1]
    8: table = table[*,*,*,*,*,*,*,row1]
    else: begin 
          print,'MRDFITS: Subscripted image must be between 1 and 8 dimensions'
          status = -1
          return
          end
    endcase
    endif

    ; Skip to the end of the data

    skipB = 2880 - (maxd*rsize) mod 2880
    if skipB eq 2880 then skipB = 0

    if range[1] lt maxd-1 then $
        skipB += (maxd-range[1]-1)*rsize
 
    mrd_skip, unit, skipB
    if unixpipe then swap_endian_inplace, table,/swap_if_little

    ; Fix offset for unsigned data
    type = mrd_unsignedtype(table)
    if type gt 0 then $
	table -= mrd_unsigned_offset(type)
    
    status=0
    done:

;-- probably an EOF 

    if status ne 0 then begin 
          message,!ERROR_STATE.MSG,/CON
         free_lun,unit
    endif

    return
end 

; Truncate superfluous axes.

pro mrd_axes_trunc,naxis, dims, silent
compile_opt idl2, hidden
    mysilent = silent
    for i=naxis-1,1,-1 do begin 

        if dims[i] eq 1 then begin
            if ~mysilent then begin
                print, 'MRDFITS: Truncating unused dimensions'
                mysilent = 1
            endif
            dims = dims[0:i-1] 
            naxis = naxis - 1 
        
        endif else return
     
    endfor 
 
    return
end

; Define structure/array to hold a FITS image. 
pro mrd_image, header, range, maxd, rsize, table, scales, offsets, scaling, $
  status, silent=silent, unsigned=unsigned, rows = rows
 compile_opt idl2, hidden
    ; 
    ; Header                FITS header for table. 
    ; Range                 Range of data to be retrieved. 
    ; Rsize                 Size of a row or group. 
    ; Table                 Structure to be defined. 
    ; Status                Return status
    ; Silent=silent         Suppress info messages?
 
    table = 0

    ; type    0         1           2         3         4         5  6  7  8  9 10 11        12         13          14          15
    lens =  [ 0,        1,          2,        4,        4,        8, 0, 0, 0, 0, 0, 0,        2,         4,          8,          8] 
    typstrs=['',   'Byte',    'Int*2',  'Int*4', 'Real*4', 'Real*8','','','','','','', 'UInt*2',  'Uint*4',    'Int*8',    'Uint*8']
    typarr= ['', 'bytarr',   'intarr', 'lonarr', 'fltarr', 'dblarr','','','','','','','uintarr', 'ulonarr', 'lon64arr', 'ulon64arr'] 
 
    status = 0 
 
 
    naxis = fxpar(header, 'NAXIS') 
    bitpix= fxpar(header, 'BITPIX') 
    if naxis gt 0 then begin 
          dims = long64(fxpar(header, 'NAXIS*', Count = N_axis)) 
          if N_axis GT naxis then begin
; Check if extra NAXISn keywords are present (though this is not legal FITS)
                   nextra = N_axis - naxis
                   dim_extra = dims[naxis:N_axis-1]
                   if total(dim_extra) EQ nextra then $
                        dims = dims[0:naxis-1] else $
                   message,'ERROR - NAXIS = ' + strtrim(naxis,2) +  $
                          ' but NAXIS' + strtrim(N_axis,2) + ' keyword present'
          endif
   endif else dims = 0
    
    gcount = fxpar(header, 'GCOUNT') 
    pcount = fxpar(header, 'PCOUNT')
    isgroup = fxpar(header, 'GROUPS')
    gcount = long(gcount)

    xscale = fxpar(header, 'BSCALE', count=cnt)
    if cnt eq 0 then xscale = 1      ;Corrected 06/29/06
    
    xunsigned = mrd_chkunsigned(bitpix,  xscale, $
				fxpar(header, 'BZERO'), unsigned=unsigned)
    ; Note that type is one less than the type signifier returned in the size call.
    type = -1
    
    if ~xunsigned then begin 
 
        if bitpix eq 8        then type = 1     $ 
        else if bitpix eq  16 then type = 2     $ 
        else if bitpix eq  32 then type = 3     $ 
        else if bitpix eq -32 then type = 4     $ 
        else if bitpix eq -64 then type = 5     $
        else if bitpix eq  64 then type = 14

    endif else begin

	if bitpix eq 16       then type = 12     $
	else if bitpix eq  32 then type = 13     $
	else if bitpix eq  64 then type = 15
	
    endelse

    if type eq -1 then begin
	print,'MRDFITS: Error: Invalid BITPIX: '+strtrim(bitpix)
	table = 0
	return
    endif

    ; Note that for random groups data we must ignore the first NAXISn keyword. 
    if isgroup GT 0  then begin 


        range[0] = range[0] > 0
        if (range[1] eq -1) then begin
            range[1] = gcount-1
        endif else begin
            range[1] = range[1] < gcount - 1
        endelse
	
	maxd = gcount
        
        if (n_elements(dims) gt 1) then begin
            dims = dims[1:*]
            naxis = naxis-1
        endif else begin
            print, 'MRDFITS: Warning: No data specified for group data.'
            dims = [0]
            naxis = 0
        endelse
        
        ; The last entry is the scaling for the sample data.
        
        if (pcount gt 0) then begin
            scales  = dblarr(pcount+1)
            offsets = dblarr(pcount+1)
        endif
        
        values = strarr(2)
        
        
        mrd_axes_trunc, naxis, dims, keyword_set(silent)
        
        values[0] = typarr[type] + "("+string(pcount)+")" 
        rsize = dims[0] 
        sarr = "(" + strcompress(string(dims[0]), /remo )
         
        for i=1, naxis-1 do begin
	    
            sarr = sarr + "," + strcompress(string(dims[i]),/remo)
            rsize = rsize*dims[i]
	    
        endfor 
         
        sarr = sarr + ")"

        if ~keyword_set(silent) then print,'MRDFITS--Image with groups:', $
          ' Ngroup=',strcompress(string(gcount)),' Npar=',                   $
          strcompress(string(pcount),/remo), ' Group=', sarr, '  Type=',typstrs[type]

        sarr = typarr[type] + sarr
        values[1] = sarr 
        rsize = (rsize + pcount)*lens[type] 
         
        table = mrd_struct(['params','array'], values, range[1]-range[0]+1, $
                           silent=silent)

	if xunsigned then begin
	    fxaddpar,header, 'BZERO', 0, 'Reset by MRDFITS v'+mrd_version()
	endif
           

        for i=0, pcount-1 do begin
	    
            istr = strcompress(string(i+1),/remo)
	    
            scales[i] = fxpar(header, 'PSCAL'+istr)
            if scales[i] eq 0.0d0 then scales[i] =1.0d0
	    
            offsets[i] = fxpar(header, 'PZERO'+istr)
	    
            scales[pcount] = fxpar(header, 'BSCALE')
            if scales[pcount] eq 0.0d0 then scales[pcount] = 1.0d0
            offsets[pcount] = fxpar(header, 'BZERO')
	    
        endfor
  
     if scaling then $
        scaling = ~array_equal(scales,1.0d0) || ~array_equal(offsets,0.0)
         
    endif else begin 
 
        if naxis eq 0 then begin
	
            rsize = 0 
            table = 0
            if ~keyword_set(silent) then $
                print, 'MRDFITS: Null image, NAXIS=0'
            return
	    
        endif 
         
        if gcount gt 1 then begin 
            dims = [dims, gcount] 
            naxis = naxis + 1 
        endif 
         
        mrd_axes_trunc, naxis, dims, keyword_set(silent)

                
        maxd = dims[naxis-1] 
         
        if range[0] ne -1 then begin 
            range[0] = range[0]<(maxd-1) 
            range[1] = range[1]<(maxd-1) 
        endif else begin 
            range[0] = 0 
            range[1] = maxd - 1 
        endelse 

        Nlast = dims[naxis-1]   
        dims[naxis-1] = range[1]-range[0]+1
        pdims = dims
        if N_elements(rows) GT 0 then begin
             if max(rows) GE Nlast then begin 
               print, 'MRDFITS: Row numbers must be between 0 and ' + $
                      strtrim(Nlast-1,2)
               status = -1 & rsize = 0
               return
             endif
             pdims[naxis-1] = N_elements(rows)
        endif 
 
        if ~keyword_set(silent) then begin
            str = '('
            for i=0, naxis-1 do begin
                if i ne 0 then str = str + ','
                str = str + strcompress(string(pdims[i]),/remo)
            endfor
            str = str+')'
            print, 'MRDFITS: Image array ',str, '  Type=', typstrs[type]
        endif
         
        rsize = 1
	
        if naxis gt 1 then for i=0, naxis - 2 do rsize=rsize*dims[i] 
        rsize = rsize*lens[type] 
        sz = lonarr(naxis+3) 
        sz[0] = naxis 
        sz[1:naxis] = dims 

	nele = product(dims,/integer)
         
        sz[naxis+1] = type   
        sz[naxis+2] = nele 
  
        table = nele GT 0 ? make_array(size=sz) : 0
	
        scales = dblarr(1)
        offsets = dblarr(1)

	if xunsigned then begin
	    fxaddpar,header, 'BZERO', 0, 'Updated by MRDFITS v'+mrd_version()
	endif
	
        scales[0] = fxpar(header, 'BSCALE')
        offsets[0] = fxpar(header, 'BZERO')
	
        if scales[0] eq 0.0d0 then scales[0] = 1.0d0
        if scaling && (scales[0] eq 1.0d0) && (offsets[0] eq 0.0d0) then  $
	          scaling = 0
    endelse 
         
    status = 0 
    return 
 
end

; Scale an array of pointers
pro mrd_ptrscale, array, scale, offset
compile_opt idl2, hidden
    for i=0, n_elements(array)-1 do begin
        if ptr_valid(array[i]) then begin
	    array[i] = ptr_new(*array[i] * scale + offset)
	endif
    endfor
end

; Scale a FITS array or table.
pro mrd_string, table, header, typarr, $
               fnames, fvalues, nrec, structyp=structyp, silent=silent
compile_opt idl2, hidden
    ;
    ; Type:         FITS file type, 0=image/primary array
    ;                               1=ASCII table
    ;                               2=Binary table
    ;
    ; scales:       An array of scaling info
    ; offsets:      An array of offset information
    ; table:        The FITS data.
    ; header:       The FITS header.
    ; dscale:       Should data be scaled to R*8?
    ; fnames:       Names of table columns.
    ; fvalues:      Values of table columns.
    ; nrec:         Number of records used.
    ; structyp:     Structure name.
 
    w = where( typarr EQ 'A', Nw, $
                complement=ww, Ncomplement = Nww)
		
    if Nw EQ 0 then return    ;No tags require string conversion? 

; First do ASCII and Binary tables.    We need to create a new structure 
; because scaling will change the tag data types.

          sclr = "' '"
          vc = 'strarr'
                
           for i=0, Nw-1 do begin
                col = w[i]
                sz = size(table[0].(col),/str)

		; Handle pointer columns
		if sz.type eq 10 then begin
		    fvalues[col] = 'ptr_new()'

		; Scalar columns
		endif else if sz.N_dimensions eq 0 then begin
                    fvalues[col] = sclr

		; Vectors
                endif else begin
		    dim = sz.dimensions[0:sz.N_dimensions-1]
                    fvalues[col] = vc + $
		      '(' + strjoin(strtrim(dim,2),',') + ')'
		    
                endelse
            endfor
        tabx = mrd_struct(fnames, fvalues, nrec, structyp=structyp, silent=silent )

; First copy the unscaled columns indexed by ww.     This is actually more 
; efficient than using STRUCT_ASSIGN since the tag names are all identical,
; so STRUCT_ASSIGN would copy everything (scaled and unscaled).
 	    
       for i=0, Nww - 1 do tabx.(ww[i]) = table.(ww[i])
       
; Now copy the string items indexed by w after converting the byte array        
       
        for i=0, Nw - 1 do begin	    
 		
		str = size(tabx.(w[i]),/str)
		dim = [1,str.dimensions[0:str.N_dimensions-1]]
                if str.n_dimensions GT 1 then $
                tabx.(w[i]) = string(reform(table.(w[i]),dim)) else $
		tabx.(w[i]) = string(table.(w[i]))
			    
        endfor

        table = temporary(tabx)   ;Remove original structure from memory
  
end


; Scale a FITS array or table.
pro mrd_scale, type, scales, offsets, table, header,  $
               fnames, fvalues, nrec, dscale = dscale, structyp=structyp, silent=silent
compile_opt idl2, hidden
    ;
    ; Type:         FITS file type, 0=image/primary array
    ;                               1=ASCII table
    ;                               2=Binary table
    ;
    ; scales:       An array of scaling info
    ; offsets:      An array of offset information
    ; table:        The FITS data.
    ; header:       The FITS header.
    ; dscale:       Should data be scaled to R*8?
    ; fnames:       Names of table columns.
    ; fvalues:      Values of table columns.
    ; nrec:         Number of records used.
    ; structyp:     Structure name.
 
    w = where( (scales ne 1.d0)  or (offsets ne 0.d0), Nw, $ 
                complement=ww, Ncomplement = Nww)
		
    if Nw EQ 0 then return    ;No tags require scaling? 

; First do ASCII and Binary tables.    We need to create a new structure 
; because scaling will change the tag data types.

    if type ne 0 then begin
        
        if type eq 1 then begin
	    fvalues[w] = keyword_set(dscale) ? '0.0d0' : '0.0 
        endif else if type eq 2 then begin

            if keyword_set(dscale) then begin
                sclr = '0.d0'
                vc = 'dblarr'
            endif else begin
                sclr = '0.0'
                vc = 'fltarr'
            endelse
                
           for i=0, Nw-1 do begin
                col = w[i]
                sz = size(table[0].(col),/str)

		; Handle pointer columns
		if sz.type eq 10 then begin
		    fvalues[col] = 'ptr_new()'

		; Scalar columns
		endif else if sz.N_dimensions eq 0 then begin
                    fvalues[col] = sclr

		; Vectors
                endif else begin
		    dim = sz.dimensions[0:sz.N_dimensions-1]
                    fvalues[col] = vc + $
		      '(' + strjoin(strtrim(dim,2),',') + ')'
		    
                endelse
            endfor
        endif

        tabx = mrd_struct(fnames, fvalues, nrec, structyp=structyp, silent=silent )

; First copy the unscaled columns indexed by ww.     This is actually more 
; efficient than using STRUCT_ASSIGN since the tag names are all identical,
; so STRUCT_ASSIGN would copy everything (scaled and unscaled).
 	    
       for i=0, Nww - 1 do tabx.(ww[i]) = table.(ww[i])
       
; Now copy the scaled items indexed by w after applying the scaling.        
       
        for i=0, Nw - 1 do begin	    
 		
		dtype = size(tabx.(w[i]),/type)
		if dtype eq 10 then $
		    mrd_ptrscale, table.(w[i]), scales[w[i]], offsets[w[i]]
		
                tabx.(w[i]) = table.(w[i])*scales[w[i]] + offsets[w[i]]
		
            istr = strtrim(w[i]+1,2)
            fxaddpar, header, 'TSCAL'+istr, 1.0, ' Set by MRD_SCALE'
            fxaddpar, header, 'TZERO'+istr, 0.0, ' Set by MRD_SCALE'
	    
        endfor

        table = temporary(tabx)   ;Remove original structure from memory
    endif else begin
    ; Now process images and random groups.

        sz = size(table[0])
        if sz[sz[0]+1] ne 8 then begin
            ; Not a structure so we just have an array of data.
            if keyword_set(dscale) then begin
                table = temporary(table)*scales[0]+offsets[0]
            endif else begin
                table = temporary(table)*float(scales[0]) + float(offsets[0])
            endelse
            fxaddpar, header, 'BSCALE', 1.0, 'Set by MRD_SCALE'
            fxaddpar, header, 'BZERO', 0.0, 'Set by MRD_SCALE'

        endif else begin
            ; Random groups.  Get the number of parameters by looking
            ; at the first element in the table.
            nparam = n_elements(table[0].(0))
            if keyword_set(dscale) then typ = 'dbl' else typ='flt'
            s1 = typ+'arr('+string(nparam)+')'
            ngr = n_elements(table)
            sz = size(table[0].(1))
            if sz[0] eq 0 then dims = [1] else dims=sz[1:sz[0]]
            s2 = typ + 'arr('
            for i=0, n_elements(dims)-1 do begin 
                if i ne 0 then s2 = s2+ ','
                s2 = s2+string(dims[i])
            endfor
            s2 = s2+')'
            tabx = mrd_struct(['params', 'array'],[s1,s2],ngr, silent=silent)

            for i=0, nparam-1 do begin
                istr = strcompress(string(i+1),/remo)
                fxaddpar, header, 'PSCAL'+istr, 1.0, 'Added by MRD_SCALE'
                fxaddpar, header, 'PZERO'+istr, 0.0, 'Added by MRD_SCALE'
                tabx.(0)[i] = table.(0)[i]*scales[i]+offsets[i]
            endfor
	    
            tabx.(1) = table.(1)*scales[nparam] + offsets[nparam]
            fxaddpar, header, 'BSCALE', 1.0, 'Added by MRD_SCALE'
            fxaddpar, header, 'BZERO', 0.0, 'Added by MRD_SCALE'
            table = temporary(tabx)
        endelse
    endelse

end

; Read a variable length column into a pointer array.
pro mrd_varcolumn, vtype, array, heap, off, siz
compile_opt idl2, hidden

    ; Guaranteed to have at least one non-zero length column
    w   = where(siz gt 0)
    nw  = n_elements(w)
    
    if vtype eq 'X' then siz = 1 + (siz-1)/8
    
    siz = siz[w]
    off = off[w]

    unsigned = 0
    if vtype eq '1' then begin
	unsigned = 12
    endif else if vtype eq '2' then begin
	unsigned = 13
    endif else if vtype eq '3' then begin
	unsigned = 15;
    endif
    unsigned = mrd_unsigned_offset(unsigned)
    

    for j=0, nw-1 do begin

        case vtype of

            'L': array[w[j]] = ptr_new(  byte(heap,off[j],siz[j]) )
            'X': array[w[j]] = ptr_new(  byte(heap,off[j],siz[j]) )
            'B': array[w[j]] = ptr_new(  byte(heap,off[j],siz[j]) )
	
            'I': array[w[j]] = ptr_new(  fix(heap, off[j], siz[j]) )
            'J': array[w[j]] = ptr_new(  long(heap, off[j], siz[j]) )
            'K': array[w[j]] = ptr_new(  long64(heap, off[j], siz[j]) )
			  
            'E': array[w[j]] = ptr_new(  float(heap, off[j], siz[j]) )
            'D': array[w[j]] = ptr_new(  double(heap, off[j], siz[j]) )
			  
            'C': array[w[j]] = ptr_new(  complex(heap, off[j], siz[j]) )
            'M': array[w[j]] = ptr_new(  dcomplex(heap, off[j], siz[j]) )
			   
            '1': array[w[j]] = ptr_new(  uint(heap, off[j], siz[j]) )
            '2': array[w[j]] = ptr_new(  ulong(heap, off[j], siz[j]) )
            '3': array[w[j]] = ptr_new(  ulong64(heap, off[j], siz[j]) )
      
        endcase

	; Fix endianness.
        if (vtype ne 'B') && (vtype ne 'X') && (vtype ne 'L') then begin
	    swap_endian_inplace, *array[w[j]],/swap_if_little
        endif

	; Scale unsigneds.
	if unsigned gt 0 then *array[w[j]] = *array[w[j]] - unsigned
	
    endfor
end

; Read a variable length column into a fixed length array.
pro mrd_fixcolumn, vtype, array, heap, off, siz
compile_opt idl2, hidden

    w   = where(siz gt 0, nw)
    if nw EQ 0 then return
    
    if vtype eq 'X' then siz = 1 + (siz-1)/8
    
    siz = siz[w]
    off = off[w]

    for j=0, nw-1 do begin
        case vtype of
            'L': array[0:siz[j]-1,w[j]] = byte(heap,off[j],siz[j])  
            'X': array[0:siz[j]-1,w[j]] = byte(heap,off[j],siz[j])
            'B': array[0:siz[j]-1,w[j]] = byte(heap,off[j],siz[j]) 
	
            'I': array[0:siz[j]-1,w[j]] = fix(heap, off[j], siz[j]) 
            'J': array[0:siz[j]-1,w[j]] = long(heap, off[j], siz[j]) 
            'K': array[0:siz[j]-1,w[j]] = long64(heap, off[j], siz[j]) 
			  
            'E': begin                  ;Delay conversion until after byteswapping to avoid possible math overflow   Feb 2005
	         temp = heap[off[j]: off[j] + 4*siz[j]-1 ]
		 byteorder, temp, /LSWAP, /SWAP_IF_LITTLE	 
	         array[0:siz[j]-1,w[j]] = float(temp,0,siz[j]) 
                 end			  
           'D': begin 
	         temp = heap[off[j]: off[j] + 8*siz[j]-1 ]
		 byteorder, temp, /L64SWAP, /SWAP_IF_LITTLE		 
	         array[0:siz[j]-1,w[j]] = double(temp,0,siz[j]) 
                 end			  
            'C': array[0:siz[j]-1,w[j]] = complex(heap, off[j], siz[j]) 
            'M': array[0:siz[j]-1,w[j]] = dcomplex(heap, off[j], siz[j]) 
			   
            'A': array[w[j]] = string(byte(heap,off[j],siz[j])) 

            '1': array[0:siz[j]-1,w[j]] = uint(heap, off[j], siz[j]) 
            '2': array[0:siz[j]-1,w[j]] = ulong(heap, off[j], siz[j])
            '3': array[0:siz[j]-1,w[j]] = ulong64(heap, off[j], siz[j])
      
        endcase

    endfor

    ; Fix endianness for datatypes with more than 1 byte
    if  ~stregex(vtype,'[^ABXLDE]') then $ 
	swap_endian_inplace, array, /swap_if_little
 
    ; Scale unsigned data
    case vtype of
    '1': unsigned = 12
    '2': unsigned = 13
    '3': unsigned = 15
    else: unsigned = 0
    endcase
   
    if unsigned gt 0 then $
        unsigned = mrd_unsigned_offset(unsigned)
   
    if unsigned gt 0 then begin
        for j=0, nw-1 do begin
            array[0:siz[j]-1,w[j]] = array[0:siz[j]-1,w[j]] - unsigned
	endfor
    endif


end
		
; Read the heap area to get the actual values of variable 
; length arrays. 
pro mrd_read_heap, unit, header, range, fnames, fvalues, vcls, vtpes, table, $ 
   structyp, scaling, scales, offsets, status, silent=silent,                $
   columns=columns, rows = rows, pointer_var=pointer_var, fixed_var=fixed_var
compile_opt idl2, hidden
    ; 
    ; Unit:         FITS unit number. 
    ; header:       FITS header. 
    ; fnames:       Column names. 
    ; fvalues:      Column values. 
    ; vcols:        Column numbers of variable length columns. 
    ; vtypes:       Actual types of variable length columns 
    ; table:        Table of data from standard data area, on output 
    ;               contains the variable length data. 
    ; structyp:     Structure name. 
    ; scaling:      Is there going to be scaling of the data?
    ; status:       Set to -1 if an error occurs.
    ;
    typstr = 'LXBIJKAEDCM123' 
    prefix = ['bytarr(', 'bytarr(', 'bytarr(', 'intarr(',     $ 
              'lonarr(', 'lon64arr(', 'string(bytarr(', 'fltarr(',         $ 
              'dblarr(', 'complexarr(', 'dcomplexarr(',            $
	      'uintarr(', 'ulonarr(', 'ulon64arr(']
    
    status = 0 

    ; Convert from a list of indicators of whether a column is variable
    ; length to pointers to only the variable columns.

    vcols = where(vcls eq 1)
    vtypes = vtpes[vcols]

    nv = n_elements(vcols) 
 
    ; Find the beginning of the heap area. 
 
    heapoff = long64(fxpar(header, 'THEAP')) 
    sz = fxpar(header, 'NAXIS1')*fxpar(header, 'NAXIS2')
    
    if (heapoff ne 0) && (heapoff lt sz) then begin 
        print, 'MRDFITS: ERROR Heap begins within data area' 
        status = -1 
        return 
    endif

    ; Skip to beginning.
    if (heapoff > sz) then begin
        mrd_skip, unit, heapoff-sz
    endif
 
    ; Get the size of the heap. 
    pc = long64(fxpar(header, 'PCOUNT')) 
    if heapoff eq 0 then heapoff = sz 
    hpsiz = pc - (heapoff-sz) 
 
    if (hpsiz gt 0) then heap = bytarr(hpsiz) 
 
 
    ; Read in the heap 
    readu, unit, heap

    ; Skip to the end of the data area.
    skipB = 2880 - (sz+pc) mod 2880
    if skipB ne 2880 then begin
        mrd_skip, unit, skipB
    endif
 
    ; Find the maximum dimensions of the arrays. 
    ; 
    ; Note that the variable length column currently has fields which 
    ; are I*4 2-element arrays where the first element is the 
    ; length of the field on the current row and the second is the 
    ; offset into the heap. 

    vdims = lonarr(nv)
    for i=0, nv-1 do begin 
        col = vcols[i]
        curr_col = table.(col)
        vdims[i] = max(curr_col[0,*])
        w = where(curr_col[0,*] ne vdims[i])
        if w[0] ne -1 then begin
            if n_elements(lencols) eq 0 then begin
                lencols = [col]
            endif else begin
                lencols=[lencols,col]
            endelse
        endif

        if vtypes[i] eq 'X' then vdims[i]=(vdims[i]+7)/8 
        ind = strpos(typstr, vtypes[i])
    
        ; Note in the following that we ensure that the array is
        ; at least one element long.

        fvalues[col] = prefix[ind] + string((vdims[i] > 1)) + ')'
        if vtypes[i] eq 'A' then fvalues[col] = fvalues[col] + ')' 
    
    endfor 
 
    nfld = n_elements(fnames) 

    ; Get rid of columns which have no actual data. 
    w= intarr(nfld) 
    w[*] = 1
    corres = indgen(nfld)

    
    ; Should we get rid of empty columns?
    delete = 1
    if keyword_set(pointer_var) then delete = pointer_var eq 1

    if delete then begin
	
        ww = where(vdims eq 0, N_ww) 
        if N_ww GT 0 then  begin
            w[vcols[ww]] = 0
            if ~keyword_set(silent) then $
                print, 'MRDFITS: ', strcompress(string(n_elements(ww))),  $
                  ' unused variable length columns deleted'
        endif

        ; Check if all columns have been deleted...
        wx = where(w gt 0, N_wx)
        if N_wx EQ 0 then begin
            if ~keyword_set(silent) then $
                print, 'MRDFITS: All columns have been deleted'
 	    table = 0
	    return
        endif
    

        ; Get rid of unused columns.
        corres = corres[wx]
        fnames = fnames[wx] 
        fvalues = fvalues[wx]
        scales = scales[wx]
        offsets = offsets[wx]

        wx = where(vdims gt 0)
    
        if (wx[0] eq -1) then begin
            vcols=[-9999]
	    x=temporary(vtypes)
	    x=temporary(vdims)
        endif else begin 
            vcols = vcols[wx]
            vtypes = vtypes[wx]
            vdims = vdims[wx]
        endelse
    endif

    if ~keyword_set(pointer_var) then begin
        ; Now add columns for lengths of truly variable length records.
        if n_elements(lencols) gt 0 then begin
            if ~keyword_set(silent) then $
                print, 'MRDFITS: ', strcompress(string(n_elements(lencols))), $
                  ' length column[s] added'
         

            for i=0, n_elements(lencols)-1 do begin
                col = lencols[i]
                w = where(col eq corres)
                ww = where(col eq vcols)
                w = w[0]
                ww = ww[0]
                fvstr = '0L' ; <-- Originally, '0l'; breaks under the virtual machine!
                fnstr = 'L'+strcompress(string(col),/remo)+'_'+fnames[w]
                nf = n_elements(fnames)
                
                ; Note that lencols and col refer to the index of the
                ; column before we started adding in the length
                ; columns.
                
                if w eq nf-1 then begin
                    ; Subtract -1 for the length columns so 0 -> -1 and
                    ; we can distinguish this column.

                    corres = [corres, -col-1 ]
                    fnames = [fnames, fnstr ]
                    fvalues = [fvalues, fvstr ]
                    scales = [scales, 1.0d0 ]
                    offsets = [offsets, 0.0d0 ]
		    
                endif else begin
		    
                    corres = [corres[0:w],-col-1,corres[w+1:nf-1] ]
                    fnames = [fnames[0:w],fnstr,fnames[w+1:nf-1] ]
                    fvalues = [fvalues[0:w],fvstr,fvalues[w+1:nf-1] ]
                    scales = [scales[0:w], 1.0d0, scales[w+1:nf-1] ]
                    offsets = [offsets[0:w],0.0d0, offsets[w+1:nf-1] ]
                endelse
            endfor
        endif
	
    endif else begin
	
        ; We'll just read data into pointer arrays.
	for i=0,n_elements(lencols)-1 do begin
	    col = lencols[i]
	    if vtpes[col] eq 'A' then begin
	        fvalues[col] = '" "'
	    endif else begin
	        fvalues[col] = 'ptr_new()'
	    endelse
	endfor
	
    endelse
	


    ; Generate a new table with the appropriate structure definitions 
    if ~scaling && ~keyword_set(columns) then begin
        tablex = mrd_struct(fnames, fvalues, n_elements(table), structyp=structyp, $
                            silent=silent)
    endif else begin
        tablex = mrd_struct(fnames, fvalues, n_elements(table), silent=silent)
    endelse


    if N_elements(rows) EQ 0 then nrow = range[1]-range[0]+1 $
                             else nrow = N_elements(rows)
    
    ; I loops over the new table columns, col loops over the old table.
    ; When col is negative, it is a length column.
    for i=0, n_elements(fnames)-1 do begin
        
        col = corres[i]
                
        if col ge 0 then begin
	    
            w = where(vcols eq col)
                
            ; First handle the case of a column that is not
            ; variable length -- just copy the column.
                
            if w[0] eq -1 then begin
		    
                     tablex.(i) = table.(col)
 		    
            endif else begin
		
                vc = w[0]
                ; Now handle the variable length columns
                        
                ; If only one row in table, then
                ; IDL will return curr_col as one-dimensional.
                ; Since this is a variable length pointer column we
                ; know that the dimension of the column is 2.
                    curr_col = table.(col)
		
                if (nrow eq 1) then curr_col = reform(curr_col,2,1)
                siz = curr_col[0,*] 
                off = curr_col[1,*] 
                    
                ; Now process each type.
                    curr_colx = tablex.(i)
                    sz = size(curr_colx)
                    if (sz[0] lt 2) then begin
                        curr_colx = reform(curr_colx, 1, n_elements(curr_colx), /overwrite)
                    endif
                           
                    
                ; As above we have to worry about IDL truncating
                ; dimensions.  This can happen if either
                ; nrow=1 or the max dimension of the column is 1.
                    

                    sz = size(tablex.(i))
 
                nel = sz[sz[0]+2]
                if (nrow eq 1) && (nel eq 1) then begin
                    curr_colx = make_array(1,1,value=curr_colx)
                endif else if nrow eq 1 then begin
                    curr_colx = reform(curr_colx,[nel, 1], /overwrite)
                endif else if nel eq 1 then begin
                    curr_colx = reform(curr_colx,[1, nrow], /overwrite)
                endif

		vtype = vtypes[vc]
		varying = 0
		if n_elements(lencols) gt 0 then begin
		    varying = where(lencols eq col)
		    if varying[0] eq -1 then varying=0 else varying=1
		endif
		
		if varying && keyword_set(pointer_var) && (vtype ne 'A') then begin
		    mrd_varcolumn, vtype, curr_colx, heap, off, siz
		endif else begin
		    mrd_fixcolumn, vtype, curr_colx, heap, off, siz
		endelse


                
                if nel eq 1 and nrow eq 1 then begin
                    curr_colx = curr_colx[0]
                endif else if nrow eq 1 then begin
                    curr_colx = reform(curr_colx, nel, /overwrite)
                endif else if nel eq 1 then begin
                    curr_colx = reform(curr_colx, nrow, /overwrite)
                endif

                    sz = size(curr_colx)
                    if sz[1] eq 1 then begin
                         sz_tablex = size(tablex.(i))
                         sdimen = sz_tablex[1:sz_tablex[0]]
                         tablex.(i) = reform(curr_colx,sdimen)
                    endif else begin
                        tablex.(i) = curr_colx
                    endelse
                 
            endelse
                
        endif else begin
            ; Now handle the added columns which hold the lengths
            ; of the variable length columns.
                
            ncol = -col - 1 ; Remember we subtracted an extra one.
                xx = table.(ncol)
                tablex.(i) = reform(xx[0,*])
       endelse
    endfor 
 
    ; Finally get rid of the initial table and return the table with the 
    ; variable arrays read in. 
    ; 
    table = temporary(tablex) 
    return 
end 

; Read in the binary table information. 
pro mrd_read_table, unit, range, rsize, structyp, nrows, nfld, typarr, table, rows = rows, $
     unixpipe = unixpipe
compile_opt idl2, hidden 
    ; 
    ; 
    ; Unit          Unit to read data from. 
    ; Range         Desired range 
    ; Rsize         Size of row. 
    ; structyp      Structure type. 
    ; Nfld          Number of fields in structure. 
    ; Typarr        Field types 
    ; Table         Table to read information into.
    ; 

    if range[0] gt 0 then mrd_skip, unit, rsize*range[0]
    readu,unit, table
    if N_elements(rows) GT 0 then table = table[rows- range[0]]

    ; Move to the beginning of the heap -- we may have only read some rows of
    ; the data.
    if range[1] lt nrows-1 then begin
        skip_dist = (nrows-range[1]-1)*rsize
        mrd_skip, unit, skip_dist
    endif

    

    ; If necessary then convert to native format.
    if unixpipe then swap_endian_inplace,table,/swap_if_little
	

    ; Handle unsigned fields.
    for i=0, nfld-1 do begin

	    type = mrd_unsignedtype(table.(i))

	    if type gt 0 then begin	    
	        table.(i) = table.(i) - mrd_unsigned_offset(type)
	    endif
	    
	
    endfor
 end


; Check the values of TDIM keywords to see that they have valid
; dimensionalities.  If the TDIM keyword is not present or valid
; then the a one-dimensional array with a size given in the TFORM
; keyword is used.

pro mrd_tdim, header, index, flen, arrstr, no_tdim=no_tdim
compile_opt idl2, hidden
    ; HEADER        Current header array.
    ; Index         Index of current parameter
    ; flen          Len given in TFORM keyword
    ; arrstr        String returned to be included within paren's in definition.
    ; no_tdim       Disable TDIM processing

    arrstr = strcompress(string(flen),/remo)

    if keyword_set(no_tdim) then return

    tdstr = fxpar(header, 'TDIM'+strcompress(string(index),/remo))
    if tdstr eq '' then return

    ;
    ; Parse the string.  It should be of the form '(n1,n2,...nx)' where
    ; all of the n's are positive integers and the product equals flen.
    ;
    tdstr = strcompress(tdstr,/remo)
    len = strlen(tdstr)
    if strmid(tdstr,0,1) ne '(' && strmid(tdstr,len-1,1) ne ')' || len lt 3 then begin
        print, 'MRDFITS: Error: invalid TDIM for column', index
        return
    endif

    ; Get rid of parens.
    tdstr = strmid(tdstr,1,len-2)
    len = len-2

    nind = 0
    cnum = 0

    for nchr=0, len-1 do begin
        c = strmid(tdstr,nchr, 1)
        
        if c ge '0' &&  c le '9' then begin
            cnum = 10*cnum + long(c)
                
        endif else if c eq ',' then begin
        
            if cnum le 0 then begin
                print,'MRDFITS: Error: invalid TDIM for column', index
                return
            endif
                
            if n_elements(numbs) eq 0 then  $
                 numbs = cnum $
            else    numbs = [numbs,cnum]
                
            cnum = 0
                
       endif else begin
       
            print,'MRDFITS: Error: invalid TDIM for column', index
            return
                
       endelse

    endfor

    ; Handle the last number.
    if cnum le 0 then begin
        print,'MRDFITS: Error: invalid TDIM for column', index
        return
    endif

    if n_elements(numbs) eq 0 then numbs = cnum else numbs = [numbs,cnum]

    prod = 1

    for i=0, n_elements(numbs)-1 do prod = prod*numbs[i]
 
    if prod ne flen then begin
        print,'MRDFITS: Error: TDIM/TFORM dimension mismatch'
        return
    endif

    arrstr = tdstr
end
 
; Define a structure to hold a FITS binary table. 
pro mrd_table, header, structyp, use_colnum,           $ 
    range, rsize, table, nrows, nfld, typarr, fnames, fvalues,   $ 
    vcls, vtpes, scales, offsets, scaling, status, rows = rows, $
    silent=silent, columns=columns, no_tdim=no_tdim, $
    alias=alias, unsigned=unsigned, outalias=outalias,emptystring=emptystring
 compile_opt idl2, hidden
    ; 
    ; Header                FITS header for table. 
    ; Structyp              IDL structure type to be used for 
    ;                       structure. 
    ; Table                 Structure to be defined. 
    ; Status                Return status.
    ; No_tdim               Disable TDIM processing.

    table = 0

    types =  ['L', 'X', 'B', 'I', 'J', 'K', 'A', 'E', 'D', 'C', 'M', 'P','Q']
    arrstr = ['bytarr(', 'bytarr(', 'bytarr(', 'intarr(', 'lonarr(', 'lon64arr(',      $ 
              'string(replicate(32b,', 'fltarr(', 'dblarr(', 'complexarr(',            $ 
              'dcomplexarr(', 'lonarr(2*','lon64arr(2*']
    bitpix = [  0,   0,   0,  16,  32,  64,   0,  0,   0,   0,   0,   0, 0]

    sclstr = ["'T'", '0B', '0B', '0', '0L', '0LL', '" "', '0.', '0.d0', 'complex(0.,0.)', $ 
              'dcomplex(0.d0,0.d0)', 'lonarr(2)','lon64arr(2)']
    if keyword_set(emptystring) then begin 
        sclstr[6] = '0B'
        arrstr[6] = 'bytarr(' 
    endif 	
    unsarr = ['', '', '', 'uintarr(', 'ulonarr(', 'ulon64arr('];
    unsscl = ['', '', '', '0US',        '0UL',      '0ULL']
 

    status = 0 

; NEW WAY: E.S.S.

    ;; get info from header. Using vectors is much faster
    ;; when there are many columns

    mrd_fxpar, header, xten, nfld, nrow, rsize, fnames, fforms, scales, offsets
    nnames = n_elements(fnames)
    if nnames EQ 0 then begin
          if ~keyword_set(silent) then $
            print, 'MRDFITS: Binary table.  0 columns ', strtrim(nfld,2),' rows'
          return  
    endif       

    tname = fnames
    ;; nrow will change later
    nrows = nrow

    ;; Use scale=1 if not found
    if nnames GT 0 then begin
      wsc=where(scales EQ 0.0d,nwsc)
      IF nwsc NE 0 THEN scales[wsc] = 1.0d
    endif

    xten = strtrim(xten,2)
    if xten ne 'BINTABLE' and xten ne 'A3DTABLE' then begin 
        print, 'MRDFITS: ERROR - Header is not from binary table.' 
        nfld = 0 & status = -1 
        return 
    endif 
 
    if range[0] ge 0 then begin 
        range[0] = range[0] < (nrow-1) 
        range[1] = range[1] < (nrow-1) 
    endif else begin 
        range[0] = 0 
        range[1] = nrow - 1 
    endelse
    
    nrow = range[1] - range[0] + 1 
    if nrow le 0 then begin
        if ~keyword_set(silent) then $
            print, 'MRDFITS: Binary table. ', $
             strcompress(string(nfld)), ' columns, no rows.'
        return
    endif

    if N_elements(rows) EQ 0 then nrowp  = nrow else begin 
          bad = where((rows LT range[0]) or (rows GT range[1]), Nbad)
          if Nbad GT 0 then begin 
             print,'MRDFITS: Row numbers must be between 0 and ' + $
                    strtrim(nrow-1,2)      
             status = -1
             return
           endif
           nrowp = N_elements(rows)
    endelse
;    rsize = fxpar(header, 'NAXIS1') 
 
    ; 
    ;  Loop over the columns           
 
    typarr   = strarr(nfld) 
    
    fvalues  = strarr(nfld) 
    dimfld   = strarr(nfld)
    
    vcls     = intarr(nfld)
    vtpes    = strarr(nfld)
 
    fnames2 = strarr(nfld)

    for i=0, nfld-1 do begin
	
        istr = strcompress(string(i+1), /remo)

        fname = fnames[i]

        ;; check for a name conflict
        fname = mrd_dofn(fname, i+1, use_colnum, alias=alias)
	
        ;; check for a name conflict
        fname = mrd_chkfn(fname, fnames2, i, SILENT=silent)

        ;; copy in the valid name
        fnames[i] = fname
        ;; for checking conflicts
        fnames2[i] = fname
	
        fform = fforms[i]

        mrd_doff, fform, dim, ftype
        
        ; Treat arrays of length 1 as scalars.
        if dim eq 1 then begin
            dim = 0
        endif else if dim EQ -1 then begin 
            dimfld[i] = -1
        endif else begin
            mrd_tdim, header, i+1, dim, str, no_tdim=no_tdim
            dimfld[i] = str
        endelse
                
        typarr[i] = ftype 
        
        
        ; Find the number of bytes in a bit array. 
 
        if ftype eq 'X' && (dim gt 0) then begin
            dim = (dim+7)/8 
            dimfld[i] = strtrim(string(dim),2)
        endif
         
        ; Add in the structure label. 
        ; 
         
        ; Handle variable length columns. 
        
        if (ftype eq 'P') || (ftype eq 'Q') then begin 
 
            if (dim ne 0)  && (dim ne 1) then begin 
                print, 'MRDFITS: Invalid dimension for variable array column '+string(i+1) 
                status = -1 
                return 
            endif
	    
            ppos = ftype eq 'P' ? strpos(fform, 'P') : strpos(fform, 'Q')
            vf = strmid(fform, ppos+1, 1); 
            if strpos('LXBIJKAEDCM', vf) eq -1 then begin 
                print, 'MRDFITS: Invalid type for variable array column '+string(i+1) 
                status = -1 
                return 
            endif 

            vcls[i] = 1
	    
	    
	    xunsigned = mrd_chkunsigned(bitpix[ppos], scales[i],       $
				       offsets[i], $
				       unsigned=unsigned)

	    if (xunsigned) then begin
		
		if      vf eq 'I' then vf = '1' $
		else if vf eq 'J' then vf = '2' $
		else if vf eq 'K' then vf = '3'
		
	    endif
							   
            vtpes[i] = vf
            dim = 0
                         
        endif 
         

        for j=0, n_elements(types) - 1 do begin
	    
            if ftype eq types[j] then begin

                xunsigned = mrd_chkunsigned(bitpix[j], scales[i], $
                                            offsets[i], $
                                            unsigned=unsigned)

		if xunsigned then begin		     
		    fxaddpar, header, 'TZERO'+istr, 0, 'Modified by MRDFITS V'+mrd_version()
                    offsets[i] = 0 ;; C. Markwardt Aug 2007 - reset to zero so offset is not applied twice'
	        endif
                if dim eq 0 then begin

                   fvalues[i] = xunsigned ? unsscl[j] : sclstr[j]
		    
                endif else begin

		    line = xunsigned ?  unsarr[j] : arrstr[j]
		    
                    line += dimfld[i] + ')'
                    if ~keyword_set(emptystring) then $
		         if ftype eq 'A' then line += ')' 
                    fvalues[i] = line
		    
                endelse
		
                goto, next_col
		
            endif
	    
        endfor 
         
        print, 'MRDFITS: Invalid format code:',ftype, ' for column ', i+1 
        status = -1 
        return 
  next_col: 
    endfor 

    ; Check if there are any variable length columns.  If not then
    ; undefine vcls and vtpes
    w = where(vcls eq 1, N_w)
    if N_w eq 0 then begin
        dummy = temporary(vcls)
        dummy = temporary(vtpes)
        dummy = 0
    endif

    if scaling then begin 
        w = where( (scales ne 1.0d0) or (offsets ne 0.0d0), Nw)
        scaling = Nw GT 0
    endif

    zero = where(long(dimfld) LT 0L, N_zero)
    if N_zero GT 0 then begin
	
        if N_zero Eq nfld then begin
            print,'MRDFITS: Error - All fields have zero length'
            return
        endif
	
        for i=0, N_zero-1 do begin
	    print,'MRDFITS: Table column ' + fnames[zero[i]] + ' has zero length'
	endfor
	
        nfld    = nfld - N_zero
        good    = where(dimfld GE 0)
        fnames  = fnames[good]
        fvalues = fvalues[good]
        typarr = typarr[good]      ;Added 2005-1-6   (A.Csillaghy)
        tname = tname[good]        
	
    endif

    if n_elements(vcls) eq 0  &&  (~scaling) && ~keyword_set(columns) then begin
	
        table = mrd_struct(fnames, fvalues, nrow, structyp=structyp,  silent=silent )
	
    endif else begin
	
        table = mrd_struct(fnames, fvalues, nrow, silent=silent )
	
    endelse

    if ~keyword_set(silent) then begin
        print, 'MRDFITS: Binary table. ',strcompress(string(nfld)), ' columns by ',  $
          strcompress(string(nrowp)), ' rows.'
        if n_elements(vcls) gt 0 then begin
                print, 'MRDFITS: Uses variable length arrays'
        endif
    endif

    outalias = transpose([[tag_names(table)],[tname] ])
    status = 0 
    return 
 
end 

function mrdfits, file, extension, header,      $
        structyp = structyp,                    $
        use_colnum = use_colnum,                $
        range = range,                          $
        dscale = dscale, fscale=fscale,         $
        fpack = fpack, no_fpack = no_fpack,     $
        silent = silent,                        $
        columns = columns,                      $
        no_tdim = no_tdim,                      $
        error_action = error_action,            $
 	compress=compress,                      $
	alias=alias,                            $
        rows = rows,                        $
	unsigned=unsigned,                      $
	version=version,                        $
	pointer_var=pointer_var,                $
	fixed_var=fixed_var,                    $
	outalias = outalias,                     $
	emptystring = emptystring,               $
        status=status, extnum = extnum

    compile_opt idl2    
    ;   Let user know version if MRDFITS being used.
    if keyword_set(version) then $
        print,'MRDFITS: Version '+mrd_version() + 'April 24, 2014'
        
      
    if N_elements(error_action) EQ 0 then error_action = 2
    On_error, error_action
   
    ; Check positional arguments.

    if n_params() le 0  || n_params() gt 3 then begin
	if keyword_set(version) then return, 0
        print, 'MRDFITS: Usage'
        print, '   a=mrdfits(file/unit, [exten_no/exten_name, header], /version $'
        print, '       /fscale, /dscale, /unsigned, /use_colnum, /silent    $'
        print, '       range=, rows= , structyp=, columns=, $'
	print, '       /pointer_var, /fixed_var, error_action=, status= )'
        return, 0
    endif
   
    if n_params() eq 1 then extension = 0
   
    ; Check optional arguments.
    ;
    ;  *** Structure name ***

    if keyword_set(structyp) then begin
        sz = size(structyp)
        if sz[0] ne 0 then begin
            ; Use first element of array
            structyp = structyp[0]
            sz = size(structyp[0])
        endif
	
        if sz[1] ne 7 then begin
            print, 'MRDFITS: stucture type must be a string'
            return, 0
        endif
    endif

    ;  *** Use column numbers not names?
    use_colnum = keyword_set(use_colnum)

    ;  *** Get only a part of the FITS file.
    if N_elements(rows) GT 0 then begin
        range1 = min(rows,max=range2)
        range = [range1,range2]
    endif
    if keyword_set(range) then begin
        if n_elements(range) eq 2 then arange = range $
        else if n_elements(range) eq 1 then arange = [0,range[0]-1] $
        else if n_elements(range) gt 2 then arange = range[0:1] $
        else if n_elements(range) eq 0 then arange = [-1,-1]
	
    endif else begin
	arange = [-1,-1]
    endelse

    arange = long64(arange)

    ; Open the file and position to the appropriate extension then read
    ; the header.

    if (N_elements(file) GT 1 ) then begin
        print, 'MRDFITS: Vector input not supported'
        return, 0
    endif

    inputUnit = 0
   
    dtype = size(file,/type)
    if (dtype gt 0) && (dtype lt 4) then begin    ;File unit number specified
	
        inputUnit = 1
        unit = file
        unixpipe =  (fstat(unit)).size EQ 0     ;Unix pipes have no files size    
        if fxmove(unit,extension) lt 0 then return, -1
    
    endif else begin                         ;File name specified

        unit = fxposit(file, extension, compress=compress, unixpipe=unixpipe, $
	               /readonly,extnum=extnum, errmsg= errmsg, fpack=fpack)

        if unit lt 0 then begin
            message, 'File access error',/CON
	    if errmsg NE '' then message,errmsg,/CON
	    if scope_level() GT 2 then help,/trace
            status = -1
            return, 0
        endif
    endelse

    if eof(unit) then begin
        message,'ERROR - Extension past EOF',/CON
	if inputUnit eq 0 then free_lun,unit 
	status = -2
	return, 0
    endif

    mrd_hread, unit, header, status, SILENT = silent, ERRMSG = errmsg
    
    if status lt 0 then begin
	message,'ERROR - ' +errmsg,/CON
        message, 'ERROR - FITS file may be invalid or corrupted',/CON
 	if inputUnit eq 0 then free_lun,unit
        return, 0
    endif

;	     
    ; If this is primary array then XTENSION will have value
    ; 0 which will be converted by strtrim to '0'

    xten = strtrim( fxpar(header,'XTENSION'), 2)
    if xten eq '0' || xten eq 'IMAGE' then type = 0 $
    else if xten eq 'TABLE' then type = 1 $
    else if xten eq 'BINTABLE' || xten eq 'A3DTABLE' then type = 2 $
    else begin 
        message, 'Unable to process extension type:' + strtrim(xten,2),/CON
	if inputUnit eq 0 then free_lun,unit
	status = -1
        return, 0
    endelse

    scaling = keyword_set(fscale) || keyword_set(dscale)

    if type eq 0 then begin

        ;*** Images/arrays
        
        mrd_image, header, arange, maxd, rsize, table, scales, offsets, $
          scaling, status, silent=silent, unsigned=unsigned, $
           rows= rows
       if (status ge 0) && (rsize gt 0) then begin
           mrd_read_image, unit, arange, maxd, rsize, table, rows = rows,$
            status=status, unixpipe=unixpipe
        endif
       size = rsize
    endif else if type eq 1 then begin

        ;*** ASCII tables.
        
        mrd_ascii, header, structyp, use_colnum,                              $
            arange, table, nbytes, nrows, nfld, rows=rows,                    $
            typarr, posarr, lenarr, nullarr, fnames, fvalues,                 $
            scales, offsets, scaling, status, silent=silent,                  $
            columns=columns, alias=alias, outalias=outalias
        size = nbytes*nrows
        
        if (status ge 0)   &&  (size gt 0)  then begin
        
            ;*** Read data.
            mrd_read_ascii, unit,  arange, nbytes, nrows,   $
              nfld, typarr, posarr, lenarr, nullarr, table,  rows= rows
              
            ;*** Extract desired columns.
            if (status ge 0) && keyword_set(columns) then                  $
                mrd_columns, table, columns, fnames, fvalues, vcls, vtps, $
                  scales, offsets, scaling, structyp=structyp, silent=silent
        endif
        
    endif else begin

        ; *** Binary tables.

        mrd_table, header, structyp, use_colnum,                            $
          arange, rsize, table, nrows, nfld, typarr,                        $ 
          fnames, fvalues, vcls, vtpes, scales, offsets, scaling, status,   $
          silent=silent, columns=columns, no_tdim=no_tdim, $
          alias=alias, unsigned=unsigned, rows = rows, outalias = outalias, $
	  emptystring=emptystring

        size = nfld*(arange[1] - arange[0] + 1)
        if (status ge 0)  &&  (size gt 0)  then begin

            ;*** Read data.
            mrd_read_table, unit, arange, rsize,  rows = rows, $
              structyp, nrows, nfld, typarr, table, unixpipe=unixpipe

            if (status ge 0) && keyword_set(columns) then begin
        
                ;*** Extract desired columns.
                mrd_columns, table, columns, fnames, fvalues,                  $
                  vcls, vtpes, scales, offsets, scaling, structyp=structyp,    $
                  silent=silent
	    
	    endif
         
             if keyword_set(emptystring) then $
	      mrd_string, table, header, typarr, $
               fnames, fvalues,  1+arange[1]-arange[0], structyp=structyp, silent=silent

            if (status ge 0) && n_elements(vcls) gt 0 then begin 
          
                ;*** Get variable length columns
                mrd_read_heap, unit, header, arange, fnames, fvalues,             $
                  vcls, vtpes, table, structyp, scaling, scales, offsets, status, $
                  silent=silent, pointer_var=pointer_var, fixed_var=fixed_var, rows= rows
		
	    endif else begin

	        ; Skip remainder of last data block
	        sz = long64(fxpar(header, 'NAXIS1'))* $
                     long64(fxpar(header,'NAXIS2')) +  $
		       long64(fxpar(header, 'PCOUNT'))
	        skipB = 2880 - sz mod 2880
	        if (skipB ne 2880) then mrd_skip, unit, skipB
            endelse
		     
        endif

    endelse


    ; Don't tie up a unit number that we allocated in this routine.
    if (unit gt 0) && (inputUnit eq 0) then free_lun, unit

; If any of the scales are non-unity, or any of the offsets are nonzero then 
; apply scalings.

    if  (status ge 0)  &&  scaling  &&  (size gt 0)  then begin
	noscale = array_equal(scales,1.d0) &&  array_equal(offsets,0.0) 
        
        if ~noscale then mrd_scale, type, scales, offsets, table, header,  $
            fnames, fvalues, 1+arange[1]-arange[0], structyp=structyp,       $
            dscale=dscale, silent=silent
    endif

    ; All done. Check the status to see if we ran into problems on the way.
    
    if status ge 0 then return, table else return,0
 
end
