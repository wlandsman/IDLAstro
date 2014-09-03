FUNCTION dbindex_blk, unit, nb, bsz, ofb, dtype
;+
; NAME:
;       DBINDEX_BLK
; PURPOSE:
;       Subroutine of DBINDEX to create associated variable of correct datatype
; EXPLANATION:
;       DBINDEX_BLK will offset into the file by a specified amount in 
;       preparation for writing to the file.   V5.2 or later
;
; CALLING SEQUENCE:
;       res = dbindex_blk(unit, nb, bsz, ofb, dtype)
;
; INPUTS:
;       unit   The unit number assigned to the file.
;       nb     The number of blocks to offset into the file.
;       bsz    The size of each block, in bytes, to offset into the file.
;       ofb    The offset into the block, in bytes.
;       dtype  The IDL datatype as defined in the SIZE function
;
; OUTPUTS:
;       res    The returned variable.  This is an associated variable.
;
; RESTRICTIONS:
;       The file must have been previously opened.
;
; MODIFICATION HISTORY:
;       Written by Michael R. Greason, STX, 14 June 1990.
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Use 64 bit integer for very large databases  W. Landsman February 2001
;       Added new unsigned & 64bit integer datatypes    W. Landsman July 2001
;-
offset = long64(nb) * long64(bsz) + long64(ofb)
case dtype of
        7: datarec=assoc(unit,bytarr(1),offset)         ; string
        1: datarec=assoc(unit,bytarr(1),offset)         ; byte
        2: datarec=assoc(unit,intarr(1),offset)         ; integer
        4: datarec=assoc(unit,fltarr(1),offset)         ; floating point
        3: datarec=assoc(unit,lonarr(1),offset)         ; longword
        5: datarec=assoc(unit,dblarr(1),offset)         ; double
        6: datarec=assoc(unit,complexarr(1),offset)     ; complex
       12: datarec=assoc(unit,uintarr(1),offset)        ; unsigned integer
       13: datarec=assoc(unit,ulonarr(1),offset)        ; unsigned longword
       14: datarec=assoc(unit,lon64arr(1),offset)       ; 64 bit longword
       15: datarec=assoc(unit,ulon64arr(1),offset)   ; unsigned 64bit longword
endcase
;
RETURN, datarec
END
