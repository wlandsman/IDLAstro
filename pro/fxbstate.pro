        FUNCTION FXBSTATE, UNIT
;+
; NAME: 
;      FXBSTATE()
;
; PURPOSE:
;       Returns the state of a FITS binary table.
;
; Explanation : This procedure returns the state of a FITS binary table that
;               was either opened for read with the command FXBOPEN, or for
;               write with the command FXBCREATE.
;
; Use         : Result = FXBSTATE(UNIT)
;
; Inputs      : UNIT    = Logical unit number returned by FXBOPEN routine.
;                         Must be a scalar integer.
;
; Opt. Inputs : None.
;
; Outputs     : The result of the function is the state of the FITS binary
;               table that UNIT points to.  This can be one of three values:
;
;                       0 = Closed
;                       1 = Open for read
;                       2 = Open for write
;
; Opt. Outputs: None.
;
; Keywords    : None.
;
; Calls       : FXBFINDLUN
;
; Common      : Uses common block FXBINTABLE--see "fxbintable.pro" for more
;               information.
;
; Restrictions: None.
;
; Side effects: If UNIT is an undefined variable, then 0 (closed) is returned.
;
; Category    : Data Handling, I/O, FITS, Generic.
;
; Prev. Hist. : None.
;
; Written     : William Thompson, GSFC, 1 July 1993.
;
; Modified    : Version 1, William Thompson, GSFC, 1 July 1993.
;
; Version     : Version 1, 1 July 1993.
;       Converted to IDL V5.0   W. Landsman   September 1997
;-
;
@fxbintable
        ON_ERROR, 2
;
;  Check the number of parameters.
;
        IF N_PARAMS() NE 1 THEN MESSAGE,'Syntax:  Result = FXBSTATE(UNIT)'
;
;  If UNIT is undefined, then return False.
;
        IF N_ELEMENTS(UNIT) EQ 0 THEN RETURN, 0
;
;  Check the validity of UNIT.
;
        IF N_ELEMENTS(UNIT) GT 1 THEN MESSAGE,'UNIT must be a scalar'
        SZ = SIZE(UNIT)
        IF SZ[SZ[0]+1] GT 3 THEN MESSAGE,'UNIT must be an integer'
;
;  Get the state associated with UNIT.
;
        ILUN = FXBFINDLUN(UNIT)
        RETURN, STATE[ILUN]
;
        END
