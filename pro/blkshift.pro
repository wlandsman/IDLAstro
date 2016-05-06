;+
; NAME:
;   BLKSHIFT
;
; PURPOSE:
;   Shift a block of data to a new position in a file (possibly overlapping)
;
; CALLING SEQUENCE:
;
;   BLKSHIFT, UNIT, POS, [ DELTA, TO=TO, /NOZERO, ERRMSG=ERRMSG, 
;             BUFFERSIZE=BUFFERSIZE ]
;
; DESCRIPTION:
;
;  BLKSHIFT moves a block of data forward or backward, to a new
;  position in a data file.  The old and new positions of the block
;  can overlap safely.
;
;  The new position can be specified with either the DELTA parameter,
;  which gives the number of bytes to move forward (positive delta) or
;  backward (negative delta); or the TO keyword, which give the new
;  absolute starting position of the block.
;
;  The block can be moved beyond the current end of file point, in
;  which case the intervening gap is filled with zeros (optionally).
;  The gap left at the old position of the block is also optionally
;  zero-filled.    If a set of data up to the end of the file is being
;  moved forward (thus making the file smaller) then
;  the file is truncated at the new end.using TRUNCATE_LUN.
;
; INPUTS:
;
;   UNIT - a logical unit number, opened for reading and writing.
;
;   POS - POS[0] is the position of the block in the file, in bytes,
;         before moving.  POS[1], if present, is the size of the block
;         in bytes.  If POS[1] is not given, then the block is from
;         POS[0] to the end of the file.
;
;   DELTA - the (optional) offset in bytes between the old and new
;           positions, from the start of the block.  Positive values
;           indicate moving the data forward (toward the end of file),
;           and negative values indicate moving the data backward
;           (toward the beginning of the file).  One of DELTA and TO
;           must be specified; DELTA overrides the TO keyword.
;
;           Attempts to move the block beyond the end of the file will
;           succeed.  A block can never be moved beyond the beginning
;           of the file; it will be moved to the beginning instead.
;
; KEYWORD PARAMETERS:
;
;   TO - the absolute file offset in bytes for the new start of the
;        block.  One of DELTA and TO must be specified; DELTA
;        overrides the TO keyword.
;
;   /NOZERO - if set, then newly created gaps will not be explicitly
;            zeroed.   Note that in same systems (e.g. MacOS) the gaps will
;            always be zeroed whether or not /NOZERO is set.
;
;   ERRMSG - If defined and passed, then any error messages will be
;            returned to the user in this parameter rather than
;            depending on the MESSAGE routine in IDL.  If no errors
;            are encountered, then a null string is returned.  
;
;			BLKSHIFT, UNIT, POS, DElTA, ERRMSG=ERRMSG, ...
;			IF ERRMSG NE '' THEN ...
;
;   BUFFERSIZE - the maximum buffer size for transfers, in bytes.
;                Larger values of this keyword impose larger memory
;                requirements on the application; smaller values will
;                lead to more transfer operations.
;                Default: 32768 (bytes)
;
; ORIGINAL AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craig.markwardt@nasa.gov
;
; MODIFICATION HISTORY:
;
;   Written, CM, Apr 2000
;   Documented and re-written, CM, 20 Jul 2000
;   Renamed from FXSHIFT to BLKSHIFT, CM, 21 Jul 2000
;   Documentation, CM, 12 Dec 2002
;   Truncate if moving data block forward from  the end of file 
;             using TRUNCATE_LUN   W. Landsman Feb. 2005 
;   Assume since V5.5, remove VMS support  W. Landsman  Sep 2006
;   Assume since V5.6, TRUNCATE_LUN available  W. Landsman Sep 2006
;   MacOS can point beyond EOF    W. Landsman   Aug 2009
;   Use V6.0 notation  W. Landsman Aprl 2014
;-
PRO BLKSHIFT, UNIT, POS0, DELTA0, NOZERO=NOZERO0, ERRMSG=ERRMSG, $
              BUFFERSIZE=BUFFERSIZE0, TO=TO0

  ;; Default error handling
  compile_opt idl2
  on_error, 2
  on_ioerror, IO_FINISH
  if n_params() LT 3 then begin
      message = 'BLKSHIFT, UNIT, POS, DELTA'
      goto, ERRMSG_OUT
  endif

  ;; Make sure file is open for writing, and begin parameter
  ;; processing
  fs = fstat(unit)
  if fs.open EQ 0 OR fs.write EQ 0 then begin
      message = 'File '+fs.name+' is not open for writing'
      goto, ERRMSG_OUT
  endif
  nozero = keyword_set(nozero0)
  pos_beg = floor(pos0[0])
  if n_elements(pos0) GT 1 then pos_fin = floor(pos0[1])
  if n_elements(pos_fin) EQ 0 then pos_fin = fs.size - 1L

  if pos_beg GE fs.size then goto, GOOD_FINISH
  if n_elements(to0) EQ 0 AND n_elements(delta0) EQ 0 then begin
      message = 'Must specify DELTA or TO'
      goto, ERRMSG_OUT
  endif

  ;; Parse the delta value, and enforce the file positioning
  if n_elements(delta0) GT 0 then begin
      delta = floor(delta0[0])
      ;; Can't move beyond beginning of file
      delta = ((pos_beg + delta) > 0L) - pos_beg 
  endif else begin
      delta = (floor(to0[0]) > 0L) - pos_beg
  endelse
      
  if delta EQ 0 then goto, GOOD_FINISH
  if pos_fin GE fs.size then pos_fin = fs.size - 1L
  if pos_fin LT pos_beg then goto, GOOD_FINISH

  if n_elements(buffersize0) EQ 0 then buffersize0 = 32768L
  buffersize = long(buffersize0[0])
  if buffersize LE 0 then buffersize = 32768L

  ;; Seek to end of file and add zeroes (if needed)
  pos_fin +=  1L

  ;; Unless /Nozero set, the zeroes will be explicitly written
  if (delta GT 0) && (nozero EQ 0) && (pos_fin+delta GT fs.size) then begin
      point_lun, unit, fs.size
      nleft = (pos_fin-fs.size) + delta
      while nleft GT 0 do begin
          ntrans = nleft < buffersize
          if n_elements(bb0) NE ntrans then bb0 = bytarr(ntrans)
          writeu, unit, bb0, transfer_count=cc
          if cc EQ 0 then goto, IO_FINISH
          nleft -= cc
      endwhile
  endif

  ;; Now shift the data forward or backward
  if delta GT 0 then begin

      ;; Shift forward (toward end of file)
      edat = pos_fin    ;; End of to-be-copied data segment
      while edat GT pos_beg do begin
          ntrans = (edat - pos_beg) < buffersize
          if n_elements(bb0) NE ntrans then bb0 = bytarr(ntrans)
          point_lun, unit, edat - ntrans
          readu, unit, bb0, transfer_count=cc
          if cc NE ntrans then goto, IO_FINISH
          point_lun, unit, edat - ntrans + delta
          writeu, unit, bb0, transfer_count=cc
          if cc NE ntrans then goto, IO_FINISH
          edat -= ntrans
      endwhile
  endif else begin

      ;; Shift backward (toward beginning of file)
      bdat = pos_beg   ;; Beginning of to-be-copied data segment
      while bdat LT pos_fin do begin
          ntrans = (pos_fin - bdat) < buffersize
          if n_elements(bb0) NE ntrans then bb0 = bytarr(ntrans)
          point_lun, unit, bdat
          readu, unit, bb0, transfer_count=cc
          if cc NE ntrans then goto, IO_FINISH
          point_lun, unit, bdat - abs(delta)
          writeu, unit, bb0, transfer_count=cc
          if cc NE ntrans then goto, IO_FINISH
          bdat += ntrans
      endwhile
      if pos_fin EQ fs.size  then begin 
                  Truncate_Lun, unit
                  goto, GOOD_FINISH
      endif
  endelse
  bb0 = [0b] & dummy = temporary(bb0)

  ;; Finally, zero out the gap we created
  if nozero EQ 0 then begin
      if delta GT 0 then begin
          point_lun, unit, pos_beg  ;; also, to be sure data is flushed
          z_fin = pos_fin < (pos_beg + delta)
          nleft = (z_fin - pos_beg)
      endif else begin
          z_beg = (pos_fin - abs(delta)) > pos_beg
          nleft = (pos_fin - z_beg)
          point_lun, unit, z_beg
      endelse
      while nleft GT 0 do begin
          i = nleft < buffersize
          if n_elements(bb0) NE i then bb0 = bytarr(i)
          writeu, unit, bb0, transfer_count=cc
          if cc EQ 0 then goto, IO_FINISH
          nleft -= cc
      endwhile
  endif
  point_lun, unit, pos_beg  ;; again, to be sure data is flushed

  GOOD_FINISH:
  if arg_present(errmsg) then errmsg = ''
  return

  IO_FINISH:
  on_ioerror, NULL
  message = 'ERROR: BLKSHIFT operation failed because of an I/O error'
  ;; fallthrough...

  ;; Error message processing.  Control does not pass through here.
  ERRMSG_OUT:
  if arg_present(errmsg) then begin
      errmsg = message
      return
  endif
  message, message
END

