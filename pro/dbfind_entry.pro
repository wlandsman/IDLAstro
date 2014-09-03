pro dbfind_entry,type,svals,nentries,values,Count = count
;+
; NAME:
;       DBFIND_ENTRY
; PURPOSE:
;       Subroutine of DBFIND to perform an entry number search 
; EXPLANATION:
;       This is a subroutine of dbfind and is not a standalone procedure
;       It performs a entry number search.
;
; CALLING SEQUENCE:
;       dbfind_entry, type, svals, nentries, values, [COUNT = ]
;
; INPUTS: 
;       type - type of search (output from dbfparse)
;       svals - search values (output from dbfparse)
;       values - array of values to search
; OUTPUT:
;       good - indices of good values
; OPTIONAL OUTPUT KEYWORD:
;       Count - integer scalar giving the number of valid matches
; SIDE EFFECTS"
;       The obsolete system variable !err is set to number of good values
;
; REVISION HISTORY:
;       D. Lindler  July,1987
;       Fixed test for final entry number  W. Landsman    Sept. 95       
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Added COUNT keyword, deprecate !ERR  W. Landsman   March 2000
;       Better checking of out of range values  W. Landsman February 2002
;-
sv0=long(strtrim(svals[0],2)) & sv1=long(strtrim(svals[1],2))

if values[0] eq -1 then begin           ;start with all entries
    case type of

         0:  begin
                if (sv0 gt 0) and (sv0 le nentries) then begin  ;Update Sep 95
                        values=lonarr(1)+sv0
                        count=1
                   end else count= 0
             end
        -1: begin
                 if nentries LT sv0 then count = 0 else begin
                    values=lindgen(nentries-sv0+1) + sv0   ;value>sv0
                    count=nentries-sv0+1
                 endelse
            end
        -2: begin
                values= lindgen(sv1>1<nentries)+1       ;value<sv1
                count=sv1>1<nentries
            end
        -3: begin                                       ;sv0<value<sv1
            if sv1 lt sv0 then begin
                temp=sv0
                sv0=sv1
                sv1=temp
            end
            if (sv1 LT 1) or (sv0 GT nentries) then count = 0 else begin
               sv0=sv0>1
               sv1=sv1<nentries
               values=lindgen(sv1-sv0+1)+sv0
               count=sv1-sv0+1
            endelse 
            end         
        -5: begin                               ;sv1 is tolerance
            minv=(sv0-abs(sv1))>1
            maxv=(sv0+abs(sv1))<nentries
            values=lindgen(maxv-minv+1)+minv
            count=maxv-minv+1
            end
        -4:                                     ;non-zero
        else: begin                             ;set of values
              sv=lonarr(type)
              for i=0L,type-1 do sv[i]=long(strtrim(svals[i],2))
              good=where((sv gt 0) and (sv le nentries), count)
              if count gt 0 then values=sv[good]
              end
    endcase
    if count GT 0 then !ERR = count else !ERR = -1
  end else begin                                        ;input list supplied
    case type of
 
        0:  good=where(values eq sv0, count)            ;value=sv0
        -1: good=where(values ge sv0, count)            ;value>sv0
        -2: good=where(values le sv1, count)            ;value<sv1
        -3: begin                               ;sv0<value<sv1
            if sv1 lt sv0 then begin
                temp=sv0
                sv0=sv1
                sv1=temp
            end
            good=where((values ge sv0) and (values le sv1), count)
            end         
        -5: begin                               ;sv1 is tolerance
            minv=sv0-abs(sv1)
            maxv=sv0+abs(sv1)
            good=where((values ge minv) and (values le maxv), count)
            end
        -4: good=where(values, count)                   ;non-zero
        else: begin                             ;set of values  
              count=0                              ;number found
              for i=0L,type-1 do begin          ;loop on possible values    
                g=where(values eq long(strtrim(svals[i],2)), nfound)
                if nfound gt 0 then begin
                        if nf eq 0 then good=g else good=[good,g]
                        count = count +nfound
                end
              end
              !err=count
              end
    endcase
    if count le 0 then return
    values=values[good]
end
return
end
