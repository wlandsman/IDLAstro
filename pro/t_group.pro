pro t_group,fitsfile,rmax,xpar=xpar,ypar=ypar, NEWTABLE = newtable
;+
; NAME:
;	T_GROUP
; PURPOSE:
;	Driver procedure (for GROUP) to place stars in non-overlapping groups.
; EXPLANATION:
;	This procedure is part of the DAOPHOT sequence that places star
;	positions with non-overlapping PSFs into distinct groups   
;	Input and output are to FITS ASCII tables
;
; CALLING SEQUENCE:
;	T_GROUP, fitsfile, [ rmax, XPAR = , YPAR = , NEWTABLE = ]
;
; INPUTS:
;	FITSFILE -  Name of disk FITS ASCII table containing the X,Y positions
;		in FITS (FORTRAN) convention (first pixel is 1,1)
;
; OPTIONAL INPUTS:
;	rmax - maximum allowable distance between stars in a single group
;
; OPTIONAL INPUT KEYWORDS:
;	XPAR, YPAR - scalar strings giving the field name in the output table
;		containing the X and Y coordinates.   If not supplied,
;		then the fields 'X' and 'Y' are read.
;	NEWTABLE - scalar giving name of output disk FITS ASCII table.   If not
;		supplied, 
;
; PROCEDURES:
;	FTADDCOL, FTGET(), FTINFO, FTPUT, GROUP, READFITS(), SXADDHIST, 
;	SXADDHIST, WRITEFITS
; REVISION HISTORY:
;	Written, W. Landsman        STX Co.      May, 1996
;	Converted to IDL V5.0   W. Landsman   September 1997
;       Updated for new FTINFO call    W. Landsman    May 2000
;-
 On_error,2    

 if N_params() LT 1 then begin
	print,'Syntax - T_GROUP, fitsfile, [rmax, XPAR = , YPAR =, NEWTABLE = ]'
        return
 endif

 if not keyword_set(XPAR) then xpar = 'X'
 if not keyword_set(YPAR) then ypar = 'Y'
 if not keyword_set(NEWTABLE) then newtable = fitsfile

 dummy = readfits( fitsfile, hprimary, /SILENT )
 tab = readfits(fitsfile, h, /ext)

 ftinfo,h,ft_str
 ttype = strtrim(ft_str.ttype,2)
 x = ftget( ft_str, tab, xpar) - 1.
 y = ftget( ft_str, tab, ypar) - 1.

 if N_elements(rmax) EQ 0 then $
	read,'Enter maximum distance between stars in a group: ',rmax

 group, x, y, rmax, ngroup

 sxaddpar, h, 'RMAX', rmax, 'Maximum Distance in Group', 'TTYPE1'
 sxaddpar, h, 'EXTNAME', 'IDL DAOPHOT: Group', 'DAOPHOT Stage'

 gid = where(ttype EQ 'GROUP_ID', Nid)
 if Nid EQ 0 then ftaddcol, h, tab, 'GROUP_ID', 4, 'I4'
 ftput, h, tab, 'GROUP_ID', 0, ngroup
 sxaddhist, 'T_GROUP: ' + systime(),h

 writefits, newtable, 0, hprimary
 writefits, newtable, tab,h,/append
 return

 end
