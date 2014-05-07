;+
; NAME:
;       COMPARE_STRUCT  
; PURPOSE:
;       Compare all matching tag names and return differences
;
; EXPLANATION:
;       Compare all matching Tags names (except for "except_Tags")
;       between two structure arrays (may have different struct.definitions),
;       and return a structured List of fields found different.
;
;       The Exelis contrib library has a faster but less powerful procedure
;       struct_equal.pro, see 
;       http://www.exelisvis.com/Default.aspx?tabid=1540&id=1175
;
; CALLING SEQUENCE:
;       diff_List = compare_struct( struct_A, struct_B [ EXCEPT=, /BRIEF,
;                                    /FULL, /NaN, /RECUR_A, /RECUR_B )
; INPUTS:
;       struct_A, struct_B : the two structure arrays to compare.
;       Struct_Name : for internal recursion use only.
; OPTIONAL INPUT KEYWORDS:
;               EXCEPT = string array of Tag names to ignore (NOT to compare).
;               /BRIEF = number of differences found for each matching field
;                                               of two structures is printed.
;               /FULL = option to print even if zero differences found.
;               /NaN = if set, then tag values are considered equal if they
;                      are both set to NaN 
;               /RECUR_A = option to search for Tag names
;                               in sub-structures of struct_A,
;                               and then call compare_struct recursively
;                               for those nested sub-structures.
;               /RECUR_B = search for sub-structures of struct_B,
;                               and then call compare_struct recursively
;                               for those nested sub-structures.
;       Note:
;               compare_struct is automatically called recursively
;               for those nested sub-structures in both struct_A and struct_B
;               (otherwise cannot take difference)
; OUTPUT:
;       Returns a structure array describing differences found.   
;       which can be examined using print,diff_List or help,/st,diff_List.
;       The tags are
;       TAG_NUM_A - the tag number in structure A
;       TAG_NUM_B - the tag number in structure B
;       FIELD - the tag name
;       NDIFF - number of differences (always 1 for a scalar tag).
; PROCEDURE:
;       Match Tag names and then use where function on tags.
; EXAMPLE:
;       Find the tags in the !X system variable which are changed after a 
;       simple plot.
;       IDL> x = !X              ;Save original values
;       IDL> plot, indgen(25)    ;Make a simple plot
;       IDL> help,/str,compare_struct(x,!X)    ;See how structure has changed
;
;            and one will see that the tags  !X.crange and !X.S are changed
;            by the plot.
; MODIFICATION HISTORY:
;       written 1990 Frank Varosi STX @ NASA/GSFC (using copy_struct)
;       modif Aug.90 by F.V. to check and compare same # of elements only.
;       Added /NaN keyword W. Landsman  March 2004
;       Don't test string for NaN values W. Landsman March 2008
;-

function compare_struct, struct_A, struct_B, EXCEPT=except_Tags, Struct_Name, $
                                        FULL=full, BRIEF=brief, NaN = NaN, $
                                        RECUR_A = recur_A, RECUR_B = recur_B

   compile_opt idl2
   common compare_struct, defined
   if N_params() LT 2 then begin
       print,'Syntax - diff_List = compare_struct(struct_A, struct_B '
       print,'         [EXCEPT=, /BRIEF, /FULL, /NaN, /RECUR_A, /RECUR_B ]'
       if N_elements(diff_List) GT 0 then return, diff_List else return, -1
   endif

        if N_elements( defined ) NE 1 then begin

                diff_List = { DIFF_LIST, Tag_Num_A:0, Tag_Num_B:0, $
                                                Field:"",  Ndiff:0L }
                defined = N_tags( diff_List )
          endif else diff_List = replicate( {DIFF_LIST}, 1 )

        Ntag_A = N_tags( struct_A )
        if (Ntag_A LE 0) then begin
                message," 1st argument must be a structure variable",/CONTIN
                return,diff_List 
           endif
        Ntag_B = N_tags( struct_B )
        if (Ntag_B LE 0) then begin
                message," 2nd argument must be a structure variable",/CONTIN
                return,diff_List 
           endif

        N_A = N_elements( struct_A )
        N_B = N_elements( struct_B )

        if (N_A LT N_B) then begin

                message,"comparing "+strtrim(N_A,2)+" of first structure",/CON
                message,"to first "+strtrim(N_A,2)+" of "+strtrim(N_B,2)+ $
                        " in second structure",/CONTIN

                diff_List = compare_struct( struct_A, struct_B[0:N_A-1], $
                                                EXCEPT=except_Tags, $
                                                RECUR_A = recur_A, $
                                                RECUR_B = recur_B, $
                                                FULL=full, BRIEF=brief )
                return,diff_List 

          endif else if (N_A GT N_B) then begin

                message,"comparing first "+strtrim(N_B,2)+" of "+ $
                        strtrim(N_A,2)+" in first structure",/CON
                message,"to "+strtrim(N_B,2)+" in second structure",/CONTIN

                diff_List = compare_struct( struct_A[0:N_B-1], struct_B, $
                                                EXCEPT=except_Tags, $
                                                RECUR_A = recur_A, $
                                                RECUR_B = recur_B, $
                                                FULL=full, BRIEF=brief )
                return,diff_List 
           endif

        Tags_A = tag_names( struct_A )
        Tags_B = tag_names( struct_B )
        wB = indgen( N_elements( Tags_B ) )
        Nextag = N_elements( except_Tags )

        if (Nextag GT 0) then begin

                except_Tags = [strupcase( except_Tags )]

                for t=0,Nextag-1 do begin

                        w = where( Tags_B NE except_Tags[t], Ntag_B )
                        Tags_B = Tags_B[w]
                        wB = wB[w]
                  endfor
           endif

        if N_elements( struct_name ) NE 1 then sname = "." $
                                          else sname = struct_name + "." 

        for t = 0, Ntag_B-1 do begin

                wA = where( Tags_A EQ Tags_B[t] , nf )

                if (nf GT 0) then begin

                     tA = wA[0]
                     tB = wB[t]

                     NtA = N_tags( struct_A.(tA) )
                     NtB = N_tags( struct_B.(tB) )

                     if (NtA GT 0 ) AND (NtB GT 0) then begin

                        if keyword_set( full ) OR keyword_set( brief ) then $
                                                print, sname + Tags_A[tA], " :"

                        diffs = compare_struct( struct_A.(tA), struct_B.(tB), $
                                                sname + Tags_A[tA], $
                                                EXCEPT=except_Tags, $
                                                FULL=full, BRIEF=brief )
                        diff_List = [ diff_List, diffs ]

                      endif else if (NtA LE 0) AND (NtB LE 0) then begin

                           if keyword_set(NaN) then begin
                                  x1 = struct_b.(tB)
                                  x2 = struct_a.(tA)
				  if (size(x1,/tname) NE 'STRING') and $
				     (size(x2,/tname) NE 'STRING') then begin
                                  g = where( finite(x1) or finite(x2), Ndiff )
                                  if Ndiff GT 0 then $
                                    w = where( x1[g] NE x2[g], Ndiff ) 
				    endif
                           endif else $ 
                            w = where( struct_B.(tB) NE struct_A.(tA) , Ndiff )

                                if (Ndiff GT 0) then begin
                                        diff = replicate( {DIFF_LIST}, 1 )
                                        diff.Tag_Num_A = tA
                                        diff.Tag_Num_B = tB
                                        diff.Field = sname + Tags_A[tA] 
                                        diff.Ndiff = Ndiff
                                        diff_List = [ diff_List, diff ]
                                   endif

                                if keyword_set( full ) OR $
                                  (keyword_set( brief ) AND (Ndiff GT 0)) then $
                                   print, Tags_A[tA], Ndiff, FORM="(15X,A15,I9)"

                        endif else print, Tags_A[ta], " not compared"

                 endif
          endfor

        if keyword_set( recur_A ) then begin

                for tA = 0, Ntag_A-1 do begin

                   if N_tags( struct_A.(tA) ) GT 0 then begin

                        diffs = compare_struct( struct_A.(tA), struct_B, $
                                                sname + Tags_A[tA], $
                                                EXCEPT=except_Tags, $
                                                RECUR_A = recur_A, $
                                                RECUR_B = recur_B, $
                                                FULL=full, BRIEF=brief )
                        diff_List = [ diff_List, diffs ]
                     endif
                  endfor
          endif

        if keyword_set( recur_B ) then begin

                for tB = 0, Ntag_B-1 do begin

                   if N_tags( struct_B.(tB) ) GT 0 then begin

                        diffs = compare_struct( struct_A, struct_B.(tB), $
                                                sname + Tags_B[tB], $
                                                EXCEPT=except_Tags, $
                                                RECUR_A = recur_A, $
                                                RECUR_B = recur_B, $
                                                FULL=full, BRIEF=brief )
                        diff_List = [ diff_List, diffs ]
                     endif
                  endfor
          endif

        w = where( [diff_List.Ndiff] GT 0, np )
        if (np LE 0) then w = [0]

return, diff_List[w]
end
