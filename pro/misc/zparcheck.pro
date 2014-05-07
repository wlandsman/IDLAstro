pro zparcheck,progname,parameter,parnum,types,dimens,message
;+
; NAME:
;       ZPARCHECK
; PURPOSE:
;       Routine to check user parameters to a procedure
;
; CALLING SEQUENCE:
;       zparcheck, progname, parameter, parnum, types, dimens, [ message ]
;
; INPUTS:
;       progname  - scalar string name of calling procedure
;       parameter - parameter passed to the routine
;       parnum    - integer parameter number
;       types     - integer scalar or vector of valid types
;                1 - byte        2 - integer   3 - int*4
;                4 - real*4      5 - real*8    6 - complex
;                7 - string      8 - structure 9 - double complex
;               10 - pointer    11 - object ref 12 - Unsigned integer
;               13 - unsigned int*4 
;               14 - int*8  
;               15 - Unsigned int*8
;       dimens   - integer scalar or vector giving number
;                     of allowed dimensions.
; OPTIONAL INPUT:
;       message - string message describing the parameter to be printed if an 
;               error is found
;
; OUTPUTS:
;       none
;
; EXAMPLE:
;       IDL> zparcheck, 'HREBIN', hdr, 2, 7, 1, 'FITS Image Header'
;
;       This example checks whether the parameter 'hdr' is of type string (=7)
;       and is a vector (1 dimension).   If either of these tests fail, a 
;       message will be printed
;               "Parameter 2 (FITS Image Header) is undefined"
;               "Valid dimensions are 1"
;               "Valid types are string"        
;
; SIDE EFFECTS:
;       If an error in the parameter is a message is printed
;       a RETALL issued
;
; HISTORY
;       version 1  D. Lindler  Dec. 86
;       documentation updated.  M. Greason, May 1990.
;       Recognize double complex datatype    W. Landsman   September 1995
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Check for new data types (e.g. unsigned) W. Landsman February 2000
;       Print a traceback if an error occurs  W. Landsman  Aug 2011
;-
;----------------------------------------------------------
  compile_opt idl2
  if N_params() LT 4 then begin
        print, $
   'Syntax -  ZPARCHECK, progname, parameter, parnum, types, dimens, [message ]
        return
  endif

; get type and size of parameter

  s = size(parameter)
  ndim = s[0]
  type = s[ndim+1]

; check if parameter defined.

  if type EQ 0 then begin
        err = ' is undefined.'
        goto, ABORT 
  endif

; check for valid dimensions

  valid = where( ndim EQ dimens, Nvalid)
  if Nvalid LT 1 then begin
        err = 'has wrong number of dimensions'
        goto, ABORT   
  endif

; check for valid type

  valid = where(type EQ types, Ngood)
  if ngood lt 1 then begin
        err = 'is an invalid data type'
        goto, ABORT   
  endif

  return

; bad parameter

ABORT:
  mess = ' '
  if N_params() lt 6 then message = ''
  if message NE '' then mess = ' ('+message+') '
  print,string(7b) + 'Parameter '+strtrim(parnum,2) + mess,$
        ' of routine ', strupcase(progname) + ' ', err
  sdim = ' '
  for i = 0,N_elements(dimens)-1 do begin
        if dimens[i] eq 0 then sdim = sdim + 'scalar' $
                          else sdim = sdim + string(dimens[i],'(i3)')
  end
  print,'Valid dimensions are:'+sdim

  stype = ' '
  for i = 0, N_elements( types )-1 do begin
        case types[i] of
                1: stype = stype + ' byte'
                2: stype = stype + ' int*2'
                3: stype = stype + ' int*4'
                4: stype = stype + ' real*4'
                5: stype = stype + ' real*8'
                6: stype = stype + ' complex'
                7: stype = stype + ' string'
                8: stype = stype + ' structure'
                9: stype = stype + ' dcomplex'
               10: stype = stype + ' pointer'
               11: stype = stype + ' Object'
               12: stype = stype + ' Unsigned(i*2)'
               13: stype = stype + ' Unsigned(i*4)'
               14: stype = stype + ' int*8'
               15: stype = stype + ' Unsigned(i*8)'
        endcase
  endfor
  print,'Valid types are:' + stype
  if scope_level() GT 3 then help,/trace
  ;if !debug then stop
  retall  ; zparcheck
  end
