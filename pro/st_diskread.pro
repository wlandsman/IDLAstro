pro st_diskread, infiles, DUMP = dump
;+
; NAME: 
;       ST_DISKREAD
;
; PURPOSE:  
;       Read HST FITS formatted disk files and reconstruct GEIS (STSDAS) files.
;
; CALLING SEQUENCE:  
;       ST_DISKREAD, infiles
;
; INPUT PARAMETER:
;       infiles - (scalar string) input disk files to be converted into GEIS
;                       files. Wildcards are allowed.
; FILES CREATED:
;
;   GEIS files:
;         The GEIS file is reconstructed from each input Fits file. The 
;       output filename is composed from the rootname of the observation
;       and the appropriate GEIS file extension (i.e. d0h/d, c0h/d, etc.).
;   Tables:
;         If input file is a fits table, the output is an SDAS table.
;
; EXAMPLES:
;       a) Reconstruct the GEIS file for disk FITS file z29i020ct*.fits.
;               st_diskread,'z29i020ct*.fits'
;
; PROCEDURES CALLED:
;       ST_DISK_DATA, ST_DISK_TABLE, ST_DISK_GEIS
;       FTSIZE,SXPAR(),TAB_CREATE, TAB_WRITE
; HISTORY: 
;       10/17/94        JKF/ACC - taken from ST_TAPEREAD.
;       11/02/94        JKF/ACC - added /block on open statement to
;                                 handle files with 512 bytes/record.
;       12/6/95         JKF/ACC - include new jitter files...replaces
;                                               st_read_jitter.pro.
;       03/5/96         W. Landsman, change FORRD to READU, remove Version 1
;                               type codes, add message facility
;       05/20/00        W. Landsman, remove obsolete !ERR calls, new calling
;                               sequence to FTINFO
;       09/2006        W. Landsman, remove obsolete keywords to OPEN
;
;****************************************************************************
;       Converted to IDL V5.0   W. Landsman   September 1997
;-

 On_error,2

 if n_params() lt 1 then begin
        print,'Syntax - ST_DISKREAD, infiles'
        return
 endif
 !ERROR = 0
 if not keyword_set(DUMP) then dump = 0
;
; Search for names of input disk FITS files.
;
   file_list = file_search(infiles,count=count)
   if count le 0 then $                                              
        message,' No files found: '+ infiles $
   else message,/INF, $
        'Number of files to process: ' + strtrim(count,2)
;
; Loop on files
;
   for file = 0,count-1 do begin
        openr,unit,file_list[file],/get_lun
;
; read data header and data
;
        st_disk_data,unit,h,data,fname,gcount,dimen,opsize,nbytes,itype
        if !ERROR NE 0 then return
;
; read optional table extension
;
        st_disk_table,unit,htab,tab,table_available
        if !ERROR NE 0 then return
;
; Finished reading the input dataset at this point. Now process the information
; and create the output datasets.
;
;       GEIS file or trailer text file
;

        if sxpar(h,'naxis') gt 0 then begin
                st_disk_geis,h,data,htab,tab,table_available, $
                        fname,gcount,dimen,opsize,nbytes,itype  ;GEIS file
                if !ERROR NE 0 then return
                if dump gt 0 then $
                        print,format='(t5,i4,t15,a)',file+1,strlowcase(fname)
        end else begin                  ;either a text trailer or jitter table

           outname = strtrim(sxpar(htab,'extname'),2)
           if outname eq strtrim(0,2) then $
                outname= strtrim(sxpar(h,'filename')) 

           if  table_available then begin               

                outname = strtrim(sxpar(htab,'extname'))
                s=size(tab) & nl=s[2]                           
                name=strtrim(sxpar(htab,'extname'))             ;file name
                ;
                ;  What type of table?
                ;     - trailer file - ascii table
                ;     - jitter data  - sdas table
                ;
                if strpos(strlowcase(name),'jit') eq -1 then begin; text trailer
                  ;
                  ;     Special case NAME: PODPS/IRAF uses j7 as special 
                  ;     character, so that a file with z0j7<...> will be 
                  ;     created as z0.<...> ( . is substituted for j7 ).
                  ;     To avoid: Check file name for ., if found replace
                  ;     with j7.
                  ;
                  invalid_char = strpos(name,'.')
                  if invalid_char lt 5 then begin
                        message,' Warning: Invalid filename found: '+name ,/cont
                        name = strmid(name,0,invalid_char) + 'j7' + $
                                 strmid(name,invalid_char+1,strlen(name)) 
                        message,'   Filename will be changed to: '+ name,/cont
                  end       
                 
                  openw,ounit,name,/get_lun
                  for i = 0,nl-1 do printf,ounit,strtrim(string(tab[*,i]))
                  free_lun,ounit
                  if dump gt 0 then $
                        print,format='(t5,i4,t15,a)',file+1,strlowcase(name)
                end else begin                                  ; jitter table
                  ;
                  ; Convert from FITS to SDAS table
                  ;
                  ftsize,htab,tab,ncols,nrows,tfields
                  tab_create,tcb,otab,tfields,nrows,ncols/2
                  ftinfo,htab,ft_str
                  fname = ft_str.ttype
                  for j= 0, tfields-1 do begin
                        val=ftget(ft_str,tab,j+1)     ; extract column
                        tab_put,strtrim(fname[i]),val,tcb,otab
                  end
                  tab_write,outname,tcb,otab,htab
                  if dump gt 0 then $
                        print,format='(t5,i4,t15,a,a)',file+1, $
                                strlowcase(outname)," jitter table "
                end
           end else $
                if dump gt 0 then $
                        print,format='(t5,i4,t15,a,a)',file+1, $
                                strlowcase(outname)," (No data found)
        end     
        free_lun,unit
   endfor
return
end
;
pro st_disk_data,unit,h,data,name,gcount,dimen,opsize,nbytes,itype
;**************************************************************************
;+
; NAME:
;       ST_DISK_DATA 
;
; PURPOSE:
;       Routine to read next header and data array from an HST FITS disk file.
;       This is a subroutine of ST_DISKREAD and not intended for stand alone 
;       use.
;
;CALLING SEQUENCE:
;       st_disk_data,unit,h,data,name,gcount,dimen,opsize,nbytes,itype
;
;INPUTS:
;       unit - logical unit number.
;
;OUTPUTS:
;       h - FITS header
;       data - data array
;       name - file name
;       gcount - number of groups
;       dimen - data dimensions
;       opsize - parameter blocks size
;       nbytes - bytes per data group
;       itype - idl data type
;
; Notes:
;       This is not a standalone program. Use ST_DISKREAD.
;
; PROCEDURES CALLED:
;       GETTOK(), SXPAR()
; HISTORY:
;       10/17/94        JKF/ACC         - taken from ST_TAPE_DATA.
;
;***************************************************************************
;-
        On_error,2
;
; read fits header
;
        h = strarr(500)
        nhead = 0
        while 1 do begin
            buf=bytarr(2880)
            readu,unit,buf
        
            for i=0,35 do begin
                st = string(buf[i*80:i*80+79])
                h[nhead]=st
                if strtrim(strmid(st,0,8)) eq 'END' then goto,fini
                nhead=nhead+1
            endfor
        endwhile
fini:
;
; get keywords from header needed to read data
;
        bitpix = sxpar(h,'bitpix', Count = N_bitpix)

        if N_bitpix EQ 0 then begin
            message,/CON,'ERROR - BITPIX missing from FITS header'
            return
        endif

        naxis = sxpar(h,'naxis', Count = N_naxis)
        if N_naxis EQ 0 then begin
            message,/CON,'ERROR- NAXIS missing from FITS header'
            return
        endif
        if naxis eq 0 then return               ;NO data to read
;
; get scale factors
;
        bscale = sxpar(h,'bscale', Count = N_bscale)
        if N_bscale EQ 0 then bscale=1.
        bzero = sxpar(h,'bzero', Count = N_bzero)
        if N_bzero EQ 0 then bzero=0.
        iraf_bp = sxpar(h,'IRAF-B/P')           ;Geis file bitpix
        if iraf_bp ne 64 then begin
                bscale = float(bscale)
                bzero = float(bzero)
            end else begin
                bscale = double(bscale)
                bzero = double(bzero)
        end
;
; determine output bitpix
;
        obitpix = abs(bitpix)
        if (bscale ne 1.0) or (bzero ne 0.0) then obitpix = 32
        if iraf_bp eq 64 then obitpix = 64 
;
; get dimensions
;
        dimen = lonarr(naxis)
        npoints = 1L
        for i=0,naxis-1 do begin
            dimen[i]=sxpar(h,'naxis'+strtrim(i+1,2))
            if dimen[i] le 0 then begin
                message,/CON,'ERROR- Invalid data dimension'
                return
            endif
            npoints = npoints*dimen[i]
        endfor
;
; determine group count
;
        gcount = sxpar(h,'sdasmgnu')>1
        if gcount gt 1 then begin
                naxis = naxis-1
                dimen = dimen[0:naxis-1]     
                if n_elements(dimen) eq 1 then dimen = lonarr(1)+dimen
                npoints = npoints/gcount
        endif
;
; determine orignal psize in bytes
;
        opsize = sxpar(h,'opsize', Count = N_opsize)
        if N_opsize EQ 0 then opsize = 0
        opsize = opsize/8
;
; set up data array
;
        case bitpix of
           8: data = make_array(dimen=dimen,/byte)
          16: data = make_array(dimen=dimen,/int)
          32: data = make_array(dimen=dimen,/long)
          64: data = make_array(dimen=dimen,/double)
         -32: data = make_array(dimen=dimen,/float)
         -64: data = make_array(dimen=dimen,/double)

          else: begin
                message,/CON,'ERROR - Invalid BITPIX value'
                return
                end
        endcase
;
; determine file name
;
        ;
        ; Keyword IRAFNAME has been changed to FILENAME in new style 
        ;       PODPS keywords (JHB 11-2-91)
        ;
        name = sxpar(h,'FILENAME', Count = N_filename)
        if N_filename EQ 0 then begin
                name = sxpar(h,'IRAFNAME', Count = N_irafname)
                if N_irafname EQ 0 then $
                        message,' Keyword(IRAFNAME) missing from data header'+ $
                        '...ABORTING '
        endif

        ;
        ; Special case NAME: PODPS/IRAF uses j7 as special
        ; character, so that a file with z0j7<...> will be
        ; created as z0.<...> ( . is substituted for j7 ).
        ; To avoid: Check file name for ., if found replace
        ; with j7.
        ; Special case code added by JKF/ACC 12/30/91
        ;
        invalid_char = strpos(name,'.')
        if invalid_char lt 5 then begin
            message,' Warning: Invalid filename found: '+name ,/cont
            name = strmid(name,0,invalid_char) + 'j7' + $
                    strmid(name,invalid_char+1,strlen(name))
            message,'   Filename will be changed to: '+ name,/cont
            end

        name = strtrim(gettok(name,'.') +'.'+ gettok(name,'.'),2)
        pos = strpos(name,'_cvt')               ;take out _cvt
        if pos gt 4 then name = strmid(name,0,pos) + $
                                strmid(name,pos+4,strlen(name)-pos-4)
        dname = name
        strput,dname,'d',strlen(name)-1 ;change last character to a d
;
; determine number of blocks in the file
;
        bytes_per_point = obitpix/8
        in_bytes_per_point = abs(bitpix)/8
        nbytes = bytes_per_point * npoints
        nblocks = ((nbytes + opsize)*gcount + 511)/512
;
; open output data file
;
        close,1
        openw,1,dname
;
; create output assoc variable
;
        if (bzero eq 0) and (bscale eq 1) and (bitpix gt 0) then begin
                s = size(data) & itype = s[s[0]+1] ; idl data type
                tmp_data = make_array( dimen=dimen, type= itype )

           end else begin   

                if obitpix eq 32 then begin
                        tmp_data =  make_array(dimen=dimen,/float)
                        itype = 4
                   end else begin
                        tmp_data =  make_array(dimen=dimen,/double)
                        itype = 5
                end
        end 
;
; read data
;

        pointer = 2880          ;byte pointer in current 2880 byte disk record
              
        for group=0,gcount-1 do begin           ;loop on groups
            pos = 0                             ;current pointer in data array
            while pos lt npoints do begin
                if pointer ge 2880 then begin
                   readu,unit,buf
                   case bitpix of
                        16: byteorder,buf,/NtoHS
                        32: byteorder,buf,/NtoHL
                        -32: byteorder,buf,/XDRTOF
                        -64: byteorder,buf,/XDRTOD
                        ELSE:
                   endcase
                   pointer = 0
                endif
                words_needed = (npoints-pos)
                bytes_needed = words_needed*in_bytes_per_point
                bytes_to_take = (2880-pointer) < bytes_needed
                words_to_take = bytes_to_take/in_bytes_per_point

                case bitpix of
                        8: data[pos]=buf[pointer:bytes_to_take-1]
                        16: data[pos]=fix(buf,pointer,words_to_take)
                        32: data[pos]=long(buf,pointer,words_to_take)
                        64: data[pos]=double(buf,pointer,words_to_take)
                       -32: data[pos]=float(buf,pointer,words_to_take)   ;IEEE
                       -64: data[pos]=double(buf,pointer,words_to_take)  ;IEEE
                endcase
                pos = pos + words_to_take
                pointer = pointer + bytes_to_take
            endwhile
;
; write data
;
            if (bscale ne 1.0) or (bzero ne 0.0) then begin
        
                    out_rec = assoc(1,tmp_data,(nbytes+opsize)*group)
                    out_rec[0] = data * bscale + bzero  
                end else begin
                    out_rec = assoc(1,tmp_data,(nbytes+opsize)*group)
                    out_rec[0] = data
            end
        endfor
return               
end
;
pro st_disk_table,unit,h,data,table_available
;+
;NAME:
;       ST_DISK_TABLE 
;
; PURPOSE:
;       Routine to read FITS table from an ST fits on disk.
;       This is a subroutine of st_diskread and not intended for stand alone 
;       use.
;
; CALLING SEQUENCE:
;       st_disk_table,unit,h,data
;
; INPUTS PARAMETER:
;       unit - disk unit number
;
;
; OUTPUTS:
;       h - FITS header
;       data - table array
;
; NOTES:
;       This is not a standalone program. Use ST_DISKREAD.
;          
; HISTORY:
;       10/17/94        JKF/ACC - taken from ST_TAPE_TABLE.
;       12/7/95         JKF/ACC - handle tables for jitter data.
;                                            
;****************************************************************************
;-
;
; read fits header
;
   h = strarr(500)
   nhead = 0
   while 1 do begin

        buf  = bytarr(2880)
           
on_ioerror, no_table_found
        readu,unit,buf
        
        for i=0,35 do begin
                st = string(buf[i*80:i*80+79])
                h[nhead]=st
                if strtrim(strmid(st,0,8)) eq 'END' then goto,fini
                nhead=nhead+1
        endfor
   endwhile
fini:

;
; get keywords from header needed to read data
;
   bitpix = sxpar(h,'bitpix', Count = N_bitpix)
   if N_bitpix EQ 0 then begin
        message,/CON,'ERROR- BITPIX missing from FITS header'
        return
   endif
   if bitpix ne 8 then begin
        message,/CON,'Invalid BITPIX for FITS table'
        return
    endif
    naxis = sxpar(h,'naxis', Count = N_naxis)
    if N_naxis EQ 0 then begin
            message,/CON,'ERROR- NAXIS missing from FITS table header'
            return
    endif
    if naxis ne 2 then begin
        message,/CON,'Invalid NAXIS for FITS table '
        return
    endif

    dimen = lonarr(2)
    npoints = 1L
    for i=0,1 do begin
            dimen[i]=sxpar(h,'naxis'+strtrim(i+1,2))
            if dimen[i] le 0 then begin
                if dump gt 1 then message,/cont,"No data found in table"
                goto, no_table_found
            endif
            npoints = npoints*dimen[i]
    endfor
    data = make_array(dimen=dimen,/byte)
;
; read data array
;
    nrecs = (npoints + 2879)/2880
    nleft = npoints      

    for i=0L,nrecs-1 do begin
                readu,unit,buf
                case bitpix of
                        16: byteorder,buf,/NtoHS
                        32: byteorder,buf,/NtoHL
                        -32: byteorder,buf,/XDRTOF
                        -64: byteorder,buf,/XDRTOD
                        ELSE:
                endcase

                if nleft lt 2880 then max_nleft = nleft-1 $
                         else max_nleft= 2880L-1
                data[i*2880L] = buf[0 : max_nleft ]
                nleft   = (npoints-1) - ((i+1)*2880L)
    endfor

table_available=1
return

no_table_found:
table_available=0

return
end

pro st_disk_geis,h,data,htab,tab,table_available,name,gcount,dimen,opsize, $
                nbytes_g,itype
;+
; NAME:
;       ST_DISK_GEIS 
;
; PURPOSE:
;        Routine to construct GEIS files from ST FITS disk files.
;
; CALLING SEQUENCE:
;       ST_DISK_GEIS, h, data, htab, tab, table_available, name, gcount, 
;               dimen,opsize, nbytes_g,itype
;
; INPUT PARAMETERS:
;       h - header for data
;       data - data array
;       htab - header for the table
;       tab - fits table
;       table_available - logical variable (1 if table was found)
;       name - data set name
;       gcount - number of groups
;       dimen - data dimensions
;       opsize - original parameter block size
;       nbytes_g - number of bytes per group
;       itype - idl integer data type value for the output data groups
;
; SIDE EFFECTS:
;
;       GEIS file updated with group parameters in unit 1 (already open)
;       and header file created
;
; NOTES:
;       This is not a standalone program. Use st_diskread.
;
;       During the creation of the header, this routine performs the 
;       following steps:
;       1) create a basic fits header (7 keywords)
;       2) adjust basic fits header for the number of axis present (i.e. >1)
;       3) adjust basic fits header for parameter keywords (i.e. ptype,etc)
;       4) from this point, sequentially copies keywords until it hits one of
;               the following keywords 'INSTRUME','INSTRUID', or 'CONFG'.
;       5) append 'END' statement
;
; PROCEDURES CALLED:
;       FTSIZE, SXADDPAR, SXHWRITE
; HISTORY:
;       10/17/94        JKF/ACC         - taken from ST_DISK_GEIS
;
;****************************************************************************
;-
;
; convert table to parameter block 
;
        hpar = strarr(200)              ;parameter header
        hpar[0]='END'
        sxaddpar,hpar,'PCOUNT',0
        sxaddpar,hpar,'PSIZE',opsize*8
        npar = 0
        if table_available then begin
                ftsize,htab,tab,ncols,ngroups,npar
                if ngroups ne gcount then begin
                    print,'ST_DISK_GEIS - number of rows in table does '+ $
                        'not match GCOUNT'
                    retall
                endif
                sxaddpar,hpar,'PCOUNT',npar
;
; get parameter descriptions
;

                ptype = sxpar(htab,'ttype*')    ;parameter name
                tform = sxpar(htab,'tform*')    ;formats in table
                tbcol = sxpar(htab,'tbcol*')-1  ;starting byte in table
                twidth = intarr(npar)           ;width of table columns
                pdtype = strarr(16,npar)        ;data type
                nbytes = intarr(npar)           ;size in bytes of the par.
                sbyte = intarr(npar)            ;starting byte in par. block
                idltypes = intarr(npar)         ;idl data type
                for i=0,npar-1 do begin
                    type=strmid(tform[i],0,1)
                    case strupcase(type) of
                                'A' : idltype = 1
                                'I' : idltype = 16
                                'E' : idltype = 8
                                'F' : idltype = 8
                                'D' : idltype = 32
                    endcase
                    idltypes[i]=idltype
;
; get field width in characters
;
                    twidth[i]=fix(strtrim(gettok( $
                                strmid(tform[i],1,strlen(tform[i])-1),'.'),2))

                    case idltype of
                        1: begin                        ;string
                                if ((twidth[i] mod 4) gt 0) then $
                                        twidth[i]= (fix(twidth[i]/4)*4 + 4) 
                                nbytes[i] = twidth[i]
                                pdtype[i] = 'CHARACTER*'+strtrim(twidth[i],2)
                           end
                        8: begin
                                nbytes[i] = 4
                                pdtype[i] = 'REAL*4'
                           end
                        16: begin
                                nbytes[i] = 4          
                                pdtype[i] = 'INTEGER*4'
                            end
                        32: begin
                                nbytes[i] = 8
                                pdtype[i] = 'REAL*8'
                            end
                    endcase

                    if i gt 0 then sbyte[i] = nbytes[i-1]+sbyte[i-1]

                endfor
;
; complete parameter block portion of the header
;
                if total(nbytes) ne opsize then begin
                    print,'ST_DISK_GEIS - mismatch of computed and ' + $
                          'original group par. block sizes'
                    retall
                endif
                blank = string(replicate(32b,80))
                strput,blank,'=',8
                nhpar = 2
                for i=0,npar-1 do begin
                        st=strtrim(i+1,2)

                        line=blank                      ;PTYPEn
                        strput,line,'PTYPE'+st
                        strput,line,"'"+ptype[i]+"'",10
;
;       Add comments to group parameters (PTYPEn field)...JKF/ACC 1/22/92
;               
                        strput,line,'/',31
                        strput,line, strtrim(sxpar(htab,ptype[i]),2), 33
                        hpar[nhpar]=line

                        line=blank                      ;PDTYPEn
                        strput,line,'PDTYPE'+st
                        strput,line,"'"+pdtype[i]+"'",10
                        strput,line,'/',31
                        hpar[nhpar+1]=line

                        line=blank                      ;PSIZEn
                        strput,line,'PSIZE'+st
                        strput,line,string(nbytes[i]*8,'(I5)'),25
                        strput,line,'/',31
                        hpar[nhpar+2]=line
                        nhpar=nhpar+3
                endfor
                hpar[nhpar]='END'
;
; read table columns and insert into 2-d parameter block
;
                pblock=bytarr(total(nbytes),ngroups)
                for i=0,npar-1 do begin
                        width = twidth[i]
                        width1 = width-1
                        column = tab[tbcol[i]:tbcol[i]+width1,*]
                        if idltypes[i] ne 1 then begin
                                case idltypes[i] of
                                        8: val = fltarr(ngroups)
                                        16: val = lonarr(ngroups)
                                        32: val = dblarr(ngroups)
                                endcase
                                for j=0L,ngroups-1 do begin
                                    start = width*j
                                    ;
                                    ; If the field is blank, force atleast
                                    ;  a character 0. (DJL 10/92)
                                    ;
                                    tmp = string(column[start:start+width1])
                                    if strtrim(tmp) eq '' then tmp ='0'
                                    val[j]=tmp
                                endfor
                                column = byte(val,0,nbytes[i],ngroups)
                        endif
                        pblock[sbyte[i],0]=column
                endfor
        endif
;
; Create output header        ---------------------------------------------
;
; determine type and size of data
;
        case itype of
                1:  begin & datatype='BYTE'      & bitpix=8  & end
                2:  begin & datatype='INTEGER*2' & bitpix=16 & end
                3: begin & datatype='INTEGER*4' & bitpix=32 & end
                4:  begin & datatype='REAL*4'    & bitpix=32 & end
                5: begin & datatype='REAL*8'    & bitpix=64 & end
        endcase
;
; create output header for GEIS file
;

        hout = strarr(500) & hout[0]='END'      ;standard keywords
        sxaddpar,hout,'SIMPLE','F'              ;not standard fits
        sxaddpar,hout,'BITPIX',bitpix
        sxaddpar,hout,'DATATYPE',datatype
        sxaddpar,hout,'NAXIS',n_elements(dimen)
        ndim = n_elements(dimen)
        for i=1,ndim do sxaddpar,hout,'NAXIS'+strtrim(i,2),dimen[i-1]
        sxaddpar,hout,'GROUPS','T'              ;group format data
        sxaddpar,hout,'GCOUNT',gcount
;
; combine information from hpar, hs and h headers to form output header
;
        nout = 7
        while strtrim(strmid(hout[nout],0,8)) ne 'END' do nout=nout+1
;
; add parameter block information
;
        pos = 0
        while strtrim(strmid(hpar[pos],0,8)) ne 'END' do begin
                hout[nout]=hpar[pos]
                nout=nout+1
                pos=pos+1
        endwhile
;
; skip junk at first part of h header
;
        pos = 0
        while (strmid(h[pos],0,8) ne 'INSTRUME') and $
              (strmid(h[pos],0,8) ne 'INSTRUID') and $
              (strtrim(strmid(h[pos],0,8),2) ne 'CONFIG') do begin
            pos = pos + 1
            if strtrim(strmid(h[pos],0,8)) eq 'END' then begin
                print,'ST_DISK_GEIS- INSTRUME keyword missing from header'
                retall
            endif
        endwhile
;
; copy rest of header to hout
;
        while strtrim(strmid(h[pos],0,8)) ne 'END' do begin
                hout[nout] = h[pos]
                nout=nout+1
                pos=pos+1
        endwhile
        hout[nout]='END'
;
; Create output GEIS file --------------------------------------------------
;
        sxhwrite,name,hout                      ;output header file
        if npar gt 0 then begin
                out_rec = assoc(1,bytarr(1))    ;put in group parameters
                for i=0,gcount-1 do $
                        out_rec[i*(nbytes_g+opsize)+nbytes_g] = pblock[*,i]
        end
close,1
return
end
