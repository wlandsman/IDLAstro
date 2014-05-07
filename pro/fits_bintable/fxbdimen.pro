        FUNCTION FXBDIMEN, UNIT, COL, ERRMSG=ERRMSG
;+
; NAME:
;     FXBDIMEN()
;
; PURPOSE:      
;      Returns the dimensions for a column in a FITS binary table.
;
; Explanation : This procedure returns the dimensions associated with a column
;               in a binary table opened for read with the command FXBOPEN.
;
; Use         : Result = FXBDIMEN(UNIT,COL)
;
; Inputs      : UNIT    = Logical unit number returned by FXBOPEN routine.
;                         Must be a scalar integer.
;
;               COL     = Column in the binary table to read data from, either
;                         as a character string containing a column label
;                         (TTYPE), or as a numerical column index starting from
;                         column one.
;
; Opt. Inputs : None.
;
; Outputs     : The result of the function is an array containing the
;               dimensions for the specified column in the FITS binary table
;               that UNIT points to.
;
; Opt. Outputs: None.
;
; Keywords :    ERRMSG  = If defined and passed, then any error messages will
;                         be returned to the user in this parameter rather than
;                         depending on the MESSAGE routine in IDL.  If no
;                         errors are encountered, then a null string is
;                         returned.  In order to use this feature, ERRMSG must
;                         be defined first, e.g.
;
;                               ERRMSG = ''
;                               Result = FXBDIMEN( ERRMSG=ERRMSG, ... )
;                               IF ERRMSG NE '' THEN ...
;
; Calls       : FXBCOLNUM, FXBFINDLUN
;
; Common      : Uses common block FXBINTABLE--see "fxbintable.pro" for more
;               information.
;
; Restrictions: None.
;
; Side effects: The dimensions will be returned whether or not the table is
;               still open or not.
;
;               If UNIT does not point to a binary table, then 0 is returned.
;
;               If UNIT is an undefined variable, then 0 is returned.
;
; Category    : Data Handling, I/O, FITS, Generic.
;
; Prev. Hist. : None.
;
; Written     : William Thompson, GSFC, 4 March 1994.
;
; Modified    : Version 1, William Thompson, GSFC, 4 March 1994.
;               Version 2, William Thompson, GSFC, 21 June 1994
;                       Added ERRMSG keyword.
;               Version 3, William Thompson, GSFC, 23 June 1994
;                       Modified so that ERRMSG is not touched if not defined.
;
; Version     : Version 3, 23 June 1994
;       Converted to IDL V5.0   W. Landsman   September 1997
;-
;
@fxbintable
        ON_ERROR, 2
;
;  Check the number of parameters.
;
        IF N_PARAMS() NE 2 THEN BEGIN
                MESSAGE = 'Syntax:  Result = FXBDIMEN(UNIT,COL)'
                IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                        ERRMSG = MESSAGE
                        RETURN, 0
                END ELSE MESSAGE, MESSAGE
        ENDIF
;
;  If UNIT is undefined, then return zero.
;
        IF N_ELEMENTS(UNIT) EQ 0 THEN RETURN, 0
;
;  Check the validity of UNIT.
;
        IF N_ELEMENTS(UNIT) GT 1 THEN BEGIN
                MESSAGE = 'UNIT must be a scalar'
                IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                        ERRMSG = MESSAGE
                        RETURN, 0
                END ELSE MESSAGE, MESSAGE
        ENDIF
        SZ = SIZE(UNIT)
        IF SZ[SZ[0]+1] GT 3 THEN BEGIN
                MESSAGE = 'UNIT must be an integer'
                IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                        ERRMSG = MESSAGE
                        RETURN, 0
                END ELSE MESSAGE, MESSAGE
        ENDIF
;
;  Find the column number for the requested column.
;
        IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                ICOL = FXBCOLNUM(UNIT,COL,ERRMSG=ERRMSG)
                IF MESSAGE NE '' THEN RETURN, 0
        END ELSE ICOL = FXBCOLNUM(UNIT,COL)
        IF ICOL EQ 0 THEN BEGIN
                MESSAGE = 'No such column'
                IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                        ERRMSG = MESSAGE
                        RETURN, 0
                END ELSE MESSAGE, MESSAGE
        ENDIF
;
;  Get the dimensions associated with UNIT and COL.
;
        ILUN = FXBFINDLUN(UNIT)
        DIMS = N_DIMS[*,ICOL-1,ILUN]
        IF N_ELEMENTS(ERRMSG) NE 0 THEN ERRMSG = ''
        RETURN, DIMS[1:DIMS[0]]
;
        END
