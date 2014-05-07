        FUNCTION FXBISOPEN,UNIT
;+
; NAME: 
;       FXBISOPEN()
;
; PURPOSE: 
;       Returns true if UNIT points to an open FITS binary table.
;
; Explanation : This procedure checks to see if the logical unit number given
;               by the variable UNIT corresponds to a FITS binary table opened
;               for read with the command FXBOPEN, and which has not yet been
;               closed with FXBCLOSE.
;
; Use         : Result = FXBISOPEN(UNIT)
;
;               If FXBISOPEN(UNIT) THEN ...
;
; Inputs      : UNIT    = Logical unit number returned by FXBOPEN routine.
;                         Must be a scalar integer.
;
; Opt. Inputs : None.
;
; Outputs     : The result of the function is either True (1) or False (0),
;               depending on whether UNIT points to an open binary table or
;               not.
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
; Side effects: If UNIT is an undefined variable, then False (0) is returned.
;
;               If UNIT points to a FITS binary table file that is opened for
;               write, then False (0) is returned.
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
        IF N_PARAMS() NE 1 THEN MESSAGE,'Syntax:  Result = FXBISOPEN(UNIT)'
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
        RETURN, STATE[ILUN] EQ 1
;
        END
