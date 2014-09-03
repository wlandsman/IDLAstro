pro create_struct, struct, strname, tagnames, tag_descript, DIMEN = dimen, $
              CHATTER = chatter, NODELETE = nodelete
;+
; NAME:
;       CREATE_STRUCT
; PURPOSE:
;       Create an IDL structure from a list of tag names and dimensions
; EXPLANATION:
;       Dynamically create an IDL structure variable from list of tag names 
;       and data types of arbitrary dimensions.   Useful when the type of
;       structure needed is not known until run time.
;
;       Unlike the intrinsic function CREATE_STRUCT(), this procedure does not
;       require the user to know the number of tags before run time.   (Note
;       there is no name conflict since the intrinsic CREATE_STRUCT() is a 
;       function, and this file contains a procedure.)
; CALLING SEQUENCE:
;       CREATE_STRUCT, STRUCT, strname, tagnames, tag_descript, 
;                             [ DIMEN = , /CHATTER, /NODELETE ]
;
; INPUTS:
;       STRNAME -   name to be associated with structure (string)
;               Must be unique for each structure created.   Set
;               STRNAME = '' to create an anonymous structure
;
;       TAGNAMES -  tag names for structure elements (string or string array)
;                Any strings that are not valid IDL tag names (e.g. 'a\2')
;                will be converted by IDL_VALIDNAME to a valid tagname by 
;                replacing with underscores as necessary (e.g. 'a_2')
;
;       TAG_DESCRIPT -  String descriptor for the structure, containing the
;               tag type and dimensions.  For example, 'A(2),F(3),I', would
;               be the descriptor for a structure with 3 tags, strarr(2), 
;               fltarr(3) and Integer scalar, respectively.
;               Allowed types are 'A' for strings, 'B' or 'L' for unsigned byte 
;               integers, 'I' for integers, 'J' for longword integers, 
;               'K' for 64bit integers, 'F' or 'E' for floating point, 
;               'D' for double precision  'C' for complex, and 'M' for double 
;               complex.   Uninterpretable characters in a format field are 
;               ignored.
;
;               For vectors, the tag description can also be specified by
;               a repeat count.  For example, '16E,2J' would specify a 
;               structure with two tags, fltarr(16), and lonarr(2)
;
; OPTIONAL KEYWORD INPUTS:
;       DIMEN -    number of dimensions of structure array (default is 1)
;
;       CHATTER -  If set, then CREATE_STRUCT() will display
;                  the dimensions of the structure to be created, and prompt
;                  the user whether to continue.  Default is no prompt.
;
;       /NODELETE - If set, then the temporary file created
;                  CREATE_STRUCT will not be deleted upon exiting.   See below
;
; OUTPUTS:
;       STRUCT -   IDL structure, created according to specifications 
;
; EXAMPLES: 
;
;       IDL> create_struct, new, 'name',['tag1','tag2','tag3'], 'D(2),F,A(1)'
;
;       will create a structure variable new, with structure name NAME
;
;       To see the structure of new:
;
;       IDL> help,new,/struc
;       ** Structure NAME, 3 tags, 20 length:
;          TAG1            DOUBLE         Array[2]
;          TAG2            FLOAT          0.0
;          TAG3            STRING         Array[1]
;
; PROCEDURE:
;       Generates a temporary procedure file using input information with
;       the desired structure data types and dimensions hard-coded.
;       This file is then executed with CALL_PROCEDURE.
;
; NOTES:
;       If CREATE_STRUCT cannot write a temporary .pro file in the current 
;       directory, then it will write the temporary file in the getenv('HOME')
;       directory.
;
;       Note that 'L' now specifies a LOGICAL (byte) data type and not a
;       a LONG data type for consistency with FITS binary tables
;
; RESTRICTIONS:
;       The name of the structure must be unique, for each structure created.
;       Otherwise, the new variable will have the same structure as the 
;       previous definition (because the temporary procedure will not be
;       recompiled).  ** No error message will be generated  ***
;
; SUBROUTINES CALLED:
;       REPCHR() 
;
; MODIFICATION HISTORY:
;       Version 1.0 RAS January 1992
;       Modified 26 Feb 1992 for Rosat IDL Library (GAR)
;       Modified Jun 1992 to accept arrays for tag elements -- KLV, Hughes STX
;       Accept anonymous structures W. Landsman  HSTX    Sep. 92
;       Accept 'E' and 'J' format specifications   W. Landsman Jan 93
;       'L' format now stands for logical and not long array
;       Accept repeat format for vectors        W. Landsman Feb 93
;       Accept complex and double complex (for V4.0)   W. Landsman Jul 95
;       Work for long structure definitions  W. Landsman Aug 97
;       Write temporary file in HOME directory if necessary  W. Landsman Jul 98
;       Use OPENR,/DELETE for OS-independent file removal W. Landsman Jan 99
;       Use STRSPLIT() instead of GETTOK() W. Landsman  July 2002
;       Assume since V5.3 W. Landsman  Feb 2004
;       Added RESOLVE_ROUTINE to ensure recompilation W. Landsman Sep. 2004
;       Delete temporary with FILE_DELETE   W. Landsman Sep 2006
;       Assume since V5.5, delete VMS reference  W.Landsman Sep 2006
;       Added 'K' format for 64 bit integers, IDL_VALIDNAME check on tags
;                       W. Landsman  Feb 2007
;       Use vector form of IDL_VALIDNAME() if V6.4 or later W.L. Dec 2007
;       Suppress compilation mesage of temporary file A. Conley/W.L. May 2009
;       Remove FDECOMP, some cleaner coding  W.L. July 2009
;       Do not limit string length to 1000 chars   P. Broos,  Feb 2011
;       Assume since IDL V6.4 W. Landsman Aug 2013
;-
;-------------------------------------------------------------------------------

 compile_opt idl2
 if N_params() LT 4 then begin
   print,'Syntax - CREATE_STRUCT, STRUCT, strname, tagnames, tag_descript,' 
   print,'                  [ DIMEN = , /CHATTER, /NODELETE ]'
   return
 endif

 if ~keyword_set( chatter) then chatter = 0        ;default is 0
 if (N_elements(dimen) eq 0) then dimen = 1            ;default is 1

 if (dimen lt 1) then begin
  print,' Number of dimensions must be >= 1. Returning.'
  return
 endif

; For anonymous structure, strname = ''
  anonymous = 0b
  if (strlen( strtrim(strname,2)) EQ 0 ) then anonymous = 1b

 good_fmts = [ 'A', 'B', 'I', 'L', 'F', 'E', 'D', 'J','C','M', 'K' ]
 fmts = ["' '",'0B','0','0B','0.0','0.0','0.0D0','0L','complex(0)', $
           'dcomplex(0)', '0LL']
 arrs = [ 'strarr', 'bytarr', 'intarr', 'bytarr', 'fltarr', 'fltarr', $
          'dblarr', 'lonarr','complexarr','dcomplexarr','lon64arr']
 ngoodf = N_elements( good_fmts )

; If tagname is a scalar string separated by commas, convert to a string array

 if size(tagnames,/N_dimensions) EQ 0 then begin
            tagname = strsplit(tagnames,',',/EXTRACT) 
 endif else tagname = tagnames

 Ntags = N_elements(tagname)

; Make sure supplied tag names are valid.
 
 tagname = idl_validname( tagname, /convert_all )

;  If user supplied a scalar string descriptor then we want to break it up
;  into individual items.    This is somewhat complicated because the string
;  delimiter is not always a comma, e.g. if 'F,F(2,2),I(2)', so we need
;  to check positions of parenthesis also.

 sz = size(tag_descript)
 if sz[0] EQ 0 then begin
      tagvar = strarr( Ntags)
      temptag = tag_descript
      for i = 0, Ntags - 1 do begin
         comma = strpos( temptag, ',' )
         lparen = strpos( temptag, '(' )
         rparen = strpos( temptag, ')' )
            if ( comma GT lparen ) and (comma LT Rparen) then pos = Rparen+1 $
                                                         else pos = comma 
             if pos EQ -1 then begin
                 if i NE Ntags-1 then message, $
         'WARNING - could only parse ' + strtrim(i+1,2) + ' string descriptors'
                 tagvar[i] = temptag 
                 goto, DONE
             endif else begin
                    tagvar[i] = strmid( temptag, 0, pos )
                    temptag = strmid( temptag, pos+1)
              endelse
             endfor
             DONE:
            
 endif else tagvar = tag_descript

; create string array for IDL statements, to be written into 
; 'temp_'+strname+'.pro'

 pro_string = strarr (ntags + 2) 

 if (dimen EQ 1) then begin

   pro_string[0] = "struct =  { " + strname + " $"
   pro_string[ntags+1] = " } "

 endif else begin

   dimen = long(dimen)                ;Changed to LONG from FIX Mar 95
   pro_string[0] = "struct "   + " = replicate ( { " + strname + " $"
   pro_string[ntags+1] = " } , " + string(dimen) + ")"

 endelse

 tagvar = strupcase(tagvar) 

 for i = 0, ntags-1 do begin

   goodpos = -1
   for j = 0,ngoodf-1 do begin
         fmt_pos = strpos( tagvar[i], good_fmts[j] )
         if ( fmt_pos GE 0 ) then begin
              goodpos = j
              break
         endif
   endfor

  if goodpos EQ -1 then begin 
      print,' Format not recognized: ' + tagvar[i]
      print,' Allowed formats are :',good_fmts
      stop,' Redefine tag format (' + string(i) + ' ) or quit now'
  endif 


    if fmt_pos GT 0 then begin

           repeat_count = strmid( tagvar[i], 0, fmt_pos )
           if strnumber( repeat_count, value ) then begin
                fmt = arrs[ goodpos ] + '(' + strtrim(fix(value), 2) + ')'
           endif else begin 
                print,' Format not recognized: ' + tagvar[i]
                stop,' Redefine tag format (' + string(i) + ' ) or quit now'
           endelse

    endif else  begin

; Break up the tag descriptor into a format and a dimension
    tagfmts = strmid( tagvar[i], 0, 1)
    tagdim = strtrim( strmid( tagvar[i], 1, 80),2)
    if strmid(tagdim,0,1) NE '(' then tagdim = ''
    fmt = (tagdim EQ '') ? fmts[goodpos] : arrs[goodpos] + tagdim 
    endelse

  if anonymous and ( i EQ 0 ) then comma = '' else comma = " , "

      pro_string[i+1] = comma + tagname[i] + ": " + fmt + " $"      

 endfor

; Check that this structure definition is OK (if chatter set to 1)
 
 if keyword_set ( Chatter )  then begin
   ans = ''
   print,' Structure ',strname,' will be defined according to the following:'
   temp = repchr( pro_string, '$', '')
   print, temp
   read,' OK to continue? (Y or N)  ',ans
   if strmid(strupcase(ans),0,1) eq 'N' then begin
      print,' Returning at user request.'
     return
   endif
 endif 

; --- Determine if a file already exists with same name as temporary file

 tempfile = 'temp_' + strlowcase( strname )
 while file_test( tempfile + '.pro' ) do tempfile = tempfile + 'x'
 
; ---- open temp file and create procedure
; ---- If problems writing into the current directory, try the HOME directory

 cd,current= prodir 
 cdhome = 0
 openw, unit, tempfile +'.pro', /get_lun, ERROR = err
 if (err LT 0)  then begin
      prodir = getenv('HOME')
      tempfile = prodir + path_sep() + tempfile
      while file_test( tempfile + '.pro' ) do tempfile = tempfile + 'x'
      openw, unit, tempfile +'.pro', /get_lun, ERROR = err
      if err LT 0 then message,'Unable to create a temporary .pro file'
      cdhome = 1
  endif
 name = file_basename(tempfile)
 printf, unit, 'pro ' +  name + ', struct'
 printf,unit,'compile_opt hidden'
 for j = 0,N_elements(pro_string)-1 do $
        printf, unit, strtrim( pro_string[j] )
 printf, unit, 'return'
 printf, unit, 'end'
 free_lun, unit

; If using the HOME directory, it needs to be included in the IDL !PATH

 if cdhome then cd,getenv('HOME'),curr=curr
  resolve_routine, name
  Call_procedure, name, struct
 if cdhome then cd,curr

 if keyword_set( NODELETE ) then begin
    message,'Created temporary file ' + tempfile + '.pro',/INF
    return
 endif else file_delete, tempfile + '.pro'
  
  return
  end         ;pro create_struct


