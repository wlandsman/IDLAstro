        FUNCTION FXBHEADER, UNIT
;+
; NAME: 
;       FXBHEADER()
;
; PURPOSE: 
;       Returns the header of an open FITS binary table.
;
; EXPLANATION:
;      This procedure returns the FITS extension header of a FITS
;         binary table opened for read with the command FXBOPEN.
;
; Use         : Result = FXBHEADER(UNIT)
;
; Inputs      : UNIT    = Logical unit number returned by FXBOPEN routine.
;                         Must be a scalar integer.
;
; Opt. Inputs : None.
;
; Outputs     : The result of the function is a string array containing the
;               header for the FITS binary table that UNIT points to.
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
; Side effects: The string array returned always has as many elements as the
;               largest header read by FXBOPEN.  Any extra elements beyond the
;               true header are blank or null strings.
;
;               The header will be returned whether or not the table is still
;               open or not.
;
;               If UNIT does not point to a binary table, then a string array
;               of nulls is returned.
;
;               If UNIT is an undefined variable, then the null string is
;               returned.
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
        IF N_PARAMS() NE 1 THEN MESSAGE,'Syntax:  Result = FXBHEADER(UNIT)'
;
;  If UNIT is undefined, then return the null string.
;
        IF N_ELEMENTS(UNIT) EQ 0 THEN RETURN, ''
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
        RETURN, HEAD[*,ILUN]
;
        END
