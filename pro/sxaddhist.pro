pro sxaddhist,history,header,blank = blank,comment= comment, location=key, $
                            pdu=pdu
;+
; NAME:
;       SXADDHIST                           
; PURPOSE:
;       Procedure to add HISTORY (or COMMENT) line(s) to a FITS header
;
; EXPLANATION:
;       The advantage of using SXADDHIST instead of SXADDPAR is that with 
;       SXADDHIST many HISTORY or COMMENT records can be added in a single call.
;
; CALLING SEQUENCE
;       sxaddhist, history, header, [ /PDU, /COMMENT ]
;
; INPUTS:
;        history - string or string array containing history or comment line(s)
;               to add to the FITS header
; INPUT/OUTPUT
;       header - FITS header (string array).   Upon output, it will contain the
;               specified HISTORY records added to the end
;
; OPTIONAL KEYWORD INPUTS:
;       /BLANK - If specified then blank ('       ') keywords will be written
;              rather than 'HISTORY ' keywords.
;       /COMMENT - If specified, then 'COMMENT ' keyword will be written rather
;              than 'HISTORY ' keywords.    
;              Note that according to the FITS definition, any number of 
;              'COMMENT' and 'HISTORY' or blank keywords may appear in a header,
;              whereas all other keywords may appear only once.   
;       LOCATION=key - If present, the history will be added before this
;              keyword.  Otherwise put it at the end.
;       /PDU - if specified, the history will be added to the primary
;              data unit header, (before the line beginning BEGIN EXTENSION...)               
;              Otherwise, it will be added to the end of the header.
;              This has meaning only for extension headers using the STScI
;              inheritance convention. 
; OUTPUTS:
;       header - updated FITS header
;
; EXAMPLES:
;       sxaddhist, 'I DID THIS', header      ;Add one history record
;
;       hist = strarr(3)
;       hist[0] = 'history line number 1'
;       hist[1[ = 'the next history line'
;       hist[2] = 'the last history line'
;       sxaddhist, hist, header              ;Add three history records
;
; SIDE EFFECTS:
;       Header array is truncated to the final END statement
;       LOCATION overrides PDU.
; HISTORY:
;       D. Lindler  Feb. 87
;       April 90  Converted to new idl  D. Lindler
;       Put only a single space after HISTORY   W. Landsman  November 1992
;       Aug. 95   Added PDU keyword parameters
;       LOCATION added.  M. Greason, 28 September 2004.
;       Missing minus sign (1 -> -1) in testing for WHERE output when 
;        looking for location to insert a comment  M. Haffner Oct 2012
;-
;--------------------------------------------------------------------
        On_error,2

        if N_params() LT 2 then begin
           print, ' Syntax - SXADDHIST, hist, header, '
           print,  '       /PDU, /BLANK, /COMMENT, LOCATION= ] '
           return
        endif

; Check input parameters

        if (n_elements(key) LE 0) then keynam = ''                      $
                                  else keynam = strupcase(strtrim(key, 2))

        s = size(history) & ndim = s[0] & type = s[ndim+1]
        if type NE 7 then message, $
            'Invalid history lines specified; must be a string or string array'
 
        if keyword_set(COMMENT) then keyword = 'COMMENT ' else $
        if keyword_set(BLANK) then keyword = ' ' else $
                                   keyword = 'HISTORY '
        nadd = N_elements(history)            ;Number of lines to add

        s = size(header) & ndim2 = s[0] & type = s[ndim2+1]
        if (ndim2 NE 1) || (type NE 7) then message, $
                'Invalid FITS header supplied; header must be a string array'

        nlines = N_elements(header)           ;Number of lines in header

; Find END statement of FITS header
        
        endline = where( strtrim(strmid(header,0,8),2) EQ 'END' )
        n = endline[0]
        if n LT 0 then message, $
                    'Invalid FITS header array, END keyword not found'

        blank = string( replicate(32b,80) )
        n1 = n          ;position to insert
;
; if LOCATION was specified and found, make room before it.
;
        locfnd = 0
        if (strlen(keynam) gt 0) then begin
            extline = where( strupcase(strtrim(strmid(header,0,8),2)) EQ keynam )
            n_ext = extline[0]
            if (n_ext gt -1) then begin
                n1 = n_ext
                locfnd = 1
            endif
        endif
;
; if /PDU find beginning of the extension header and make room for the
; history
;
        if (keyword_set(PDU) && (locfnd EQ 0)) then begin
            extline = where( strupcase(strtrim(strmid(header,0,8),2)) EQ 'BEGIN EX' )
            n_ext = extline[0]
            if n_ext gt 1 then n1 = n_ext
        end
;
; make room in the header
;
        if n1 eq 0 then header = [replicate(blank,nadd),header[n1:n]] else $
                header = [header[0:n1-1],replicate(blank,nadd),header[n1:n]]

; Add history records to header starting at position N1

        for i = 0, nadd-1 do begin
        
                newline = blank
                strput, newline, keyword + history[i]
                header[n1+i] = newline

        endfor
 return
 end
