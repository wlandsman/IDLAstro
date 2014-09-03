;+
; NAME:
;       PRINT_STRUCT
;
; PURPOSE:
;       Print the tag values of an array of structures in nice column format.
; EXPLANATION:
;       The tag names are displayed in a header line.
;
; CALLING SEQUENCE:
;       print_struct, structure, Tags_to_print [ , title, string_matrix 
;                FILE=, LUN_OUT=, TNUMS= , TRANGE= , FRANGE=, WHICH=
;                FORM_FLOAT =, MAX_ELEMENTS
; INPUTS:
;       structure = array of structured variables
;
;       Tags_to_print = string array specifying the names of tags to print.
;                       Default is to print all tags which are not arrays.
; OPTIONAL INPUT KEYWORDS:
;       FILE = string, optional file name to which output will then be written.
;       LUN_OUT = Logical unit number for output to an open file,
;               default is to print to standard output.
;       TNUMS = tag numbers to print (alternative to specifying tag names).
;       TRANGE = [beg,end] tag number range to print.
;       FRANGE = same as TRANGE.
;       WHICH = optional array of subscripts to select
;               which structure elements to print.
;       FORM_FLOAT = string array of three elements specifying
;               floating point format, ex: FORM=['f','9','2'] means "(F9.2)",
;               (default float format is G12.4).
;       MAX_ELEMENTS = positive integer, print only tags that have less than
;                       this number of elements (default is no screening).
;       /NO_TITLE - If set, then the header line of tag names is not printed
;       /STRINGS : instead of printing, return the array of strings in
;               fourth argument of procedure: string_matrix.
; OUTPUTS:
;       title = optional string, list of tags printed/processed.
;       string_matrix = optional output of string matrix of tag values,
;                       instead of printing to terminal or file, if /STRINGS.
; PROCEDURE:
;       Check the types and lengths of fields to decide formats,
;       then loop and form text string from requested fields, then print.
; HISTORY:
;       Written: Frank Varosi NASA/GSFC 1991.
;       F.V.1993, fixed up the print formats.
;       F.V.1994, added more keyword options.
;       F.V.1997, added WHICH and MAX_ELEM keyword options.
;       WBL 1997, Use UNIQ() rather than UNIQUE function
;       Remove call to N_STRUCT()   W. Landsman  March 2004
;       Avoid overflow with more than 10000 elements  W. Landsman Nov 2005
;       Really remove call to N_STRUCT() W. Landsman July 2009
;-

pro print_struct, structure, Tags_to_print, title, string_matrix, TNUMS=tagi, $
                        FRANGE=fran, TRANGE=tran, FILE=filout, LUN_OUT=Lun, $
                        STRINGS=strings, FORM_FLOAT=formf, NO_TITLE=no_tit, $
                        WHICH_TO_PRINT=which, MAX_ELEMENTS=max_elements
                        
        compile_opt idl2
        if N_params() LT 1 then begin
        print, $
       'Syntax - PRINT_STRUCT, structure, Tags_to_print [ ,title, string_matrix' 
        print,'        FILE=, LUN_OUT=, TNUMS= , TRANGE= , FRANGE=, WHICH= '
        print,'        FORM_FLOAT =, MAX_ELEMENTS, /NO_TITLE'
        return
        end
   

        if size(structure,/TNAME) NE 'STRUCT' then begin
                message,"ERROR - expecting a structure",/INFO
                return
         endif
 ;Use size(/N_Elements) instead of N_elements() so it can work with assoc 
 ;variables	 
         Nstruct = size(structure,/N_elements)
	 Ntag = N_tags(structure)

        if Nstruct EQ 1 then structure = [structure]

        tags = [tag_names( structure )]
        Npr = N_elements( Tags_to_print )
        if N_elements( tran ) EQ 2 then fran = tran

        if N_elements( tagi ) GT 0 then begin

                tagi = ( tagi > 0 ) < (Ntag-1)
                tagi = tagi[ uniq( sort(tagi) ) ]

         endif else if N_elements( fran ) EQ 2 then begin

                fran = ( fran > 0 ) < (Ntag-1)
                nf = abs( fran[1] - fran[0] )+1
                tagi = indgen( nf ) + min( fran )

          endif else if (Npr LE 0) then begin

                for i=0,Ntag-1 do begin

                    if  (N_elements( structure[0].(i) ) LE 1) AND $
                        (N_tags( structure[0].(i) ) LE 0) then begin

                          if N_elements( tagi ) LE 0 then tagi = [i] $
                                                     else tagi = [ tagi, i ]
                      endif
                  endfor

           endif else begin

                ptags = [strupcase( Tags_to_print )]

                for i=0,Npr-1 do begin

                    w = where( tags EQ ptags[i], nf )

                    if (nf GT 0) then begin

                          if N_elements( tagi ) LE 0 then tagi = [w[0]] $
                                                else tagi = [ tagi, w[0] ]

                      endif else message,"Tag <"+ptags[i]+"> not found",/INFO
                  endfor
            endelse

        if N_elements( tagi ) LE 0 then begin
                message,"requested Tags are not in structure",/INFO
                return
           endif

        if keyword_set( max_elements ) then begin

                Ntag = N_elements( tagi )
                Ntel = Lonarr( Ntag )
                Ntst = intarr( Ntag )

                for i=0,Ntag-1 do begin
                        Ntel[i] = N_elements( structure[0].(tagi[i]) )
                        Ntst[i] = N_tags( structure[0].(tagi[i]) )
                  endfor

                w = where( (Ntel LE max_elements) and (Ntst LE 0), nw )

                if (nw GT 0) then  tagi = tagi[w]  else begin
                        message,"requested Tags have too many elements",/INFO
                        return
                   endelse
           endif

        ndigit = ceil(alog10(Nstruct))      ;Number of digits in index
        iform = "(I" + strtrim(ndigit,2) + ")"
        if ndigit GT 1 then $
             title = string(replicate(32b,ndigit-1)) else title=''
        title = title + '#'

        Tags_to_print = tags[tagi]
        Npr = N_elements( tagi )
        vtypes = intarr( Npr )
        sLens = intarr( Npr )
        formats = strarr( Npr )
        ncht = strlen( Tags_to_print ) + 2
        minch = [ 0, 5, 8, 12, 12, 12, 12, 0 ]

        for i=0,Npr-1 do begin
                st = size( structure[0].(tagi[i]) )
                vtypes[i] = st[st[0]+1]
                CASE vtypes[i] OF
                1:      formats[i] = "I" + strtrim( ncht[i]>5, 2 ) + ")"
                2:      formats[i] = "I" + strtrim( ncht[i]>8, 2 ) + ")"
                3:      formats[i] = "I" + strtrim( ncht[i]>12, 2 ) + ")"
                7: BEGIN
                        sLens[i] = $
                         ( max( strlen( structure.(tagi[i]) ) ) + 2 ) > ncht[i]
                        formats[i] = "A" + strtrim( sLens[i], 2 ) + ")"
                     END
                else: BEGIN
                        if N_elements( formf ) EQ 3 then begin
                                formf = strtrim( formf, 2 )
                                ndig = fix( formf[1] )
                                minch[4] = ndig
                                formats[i] = formf[0] + $
                                        strtrim( ncht[i] > ndig, 2 ) + $
                                        "." + formf[2] + ")"
                         endif else $
                           formats[i] = "G" + strtrim( ncht[i]>12, 2 ) + ".4)"
                        END
                ENDCASE
                nelem = st[st[0]+2]
                formats[i] = "(" + strtrim( nelem, 2 ) + formats[i]
                minch[7] = sLens[i]
                nb = nelem * ( ncht[i] > minch[vtypes[i]] ) - ncht[i] + 2
                title = title + string( replicate( 32b,nb ) ) + Tags_to_print[i]
          endfor

        if N_elements( which ) GT 0 then begin
                w = where( (which GE 0) AND (which LT Nstruct), nw )
                if (nw LE 0) then begin
                        message,"keyword WHICH subscripts out of range",/INFO
                        return
                   endif
                which = which[w]
                Nprint = nw
         endif else begin
                which = lindgen( Nstruct )
                Nprint = Nstruct
          endelse

        pr_tit = keyword_set( no_tit ) EQ 0

        if keyword_set( strings ) then begin
                string_matrix = strarr( Npr, Nprint )
                title = strmid( title, 3, 999 )
          endif else begin
                if keyword_set( filout ) then openw, Lun, filout,/GET_LUN
                if (pr_tit) then begin
                        if (Nstruct LE 3) then title = strmid( title, 3, 999 )
                        if N_elements( Lun ) EQ 1 then printf,Lun,title $
                                                else print,title
                   endif
           endelse

        for n=0,Nprint-1 do begin

            wp = which[n]

            if keyword_set( strings ) then begin

                for i=0,Npr-1 do string_matrix[i,n] = $
                        string( structure[wp].(tagi[i]), FORM=formats[i] )

             endif else begin

                if (pr_tit) AND (Nstruct GT 3) then $
                        text = string( wp,FORM=iform )  else text=""

                for i=0,Npr-1 do text = text + $
                        string( structure[wp].(tagi[i]), FORM=formats[i] )

                if N_elements( Lun ) EQ 1 then printf,Lun,text else print,text
              endelse
          endfor

        if keyword_set( filout ) then begin
                free_Lun, Lun
                message,"structure printed into file: " + filout,/INFO
           endif
end
