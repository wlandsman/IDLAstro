        FUNCTION FIND_ALL_DIR, PATH, PATH_FORMAT=PATH_FORMAT,   $
                PLUS_REQUIRED=PLUS_REQUIRED, RESET=RESET
;+
; NAME:
;       FIND_ALL_DIR()
; PURPOSE:
;       Finds all directories under a specified directory.
; EXPLANATION:
;       This routine finds all the directories in a directory tree when the
;       root of the tree is specified.  This provides the same functionality as
;       having a directory with a plus in front of it in the environment
;       variable IDL_PATH.
;
; CALLING SEQUENCE:
;       Result = FIND_ALL_DIR( PATH )
;
;               PATHS = FIND_ALL_DIR('+mypath', /PATH_FORMAT)
;               PATHS = FIND_ALL_DIR('+mypath1:+mypath2')
;
; INPUTS:
;       PATH    = The path specification for the top directory in the tree.
;               Optionally this may begin with the '+' character but the action
;               is the same unless the PLUS_REQUIRED keyword is set.
;
;               One can also path a series of directories separated
;               by the correct character (";" for Windows ":" for Unix)
;
; OUTPUTS:
;       The result of the function is a list of directories starting from the
;       top directory passed and working downward from there.   Normally, this
;       will be a string array with one directory per array element, but if
;       the PATH_FORMAT keyword is set, then a single string will be returned,
;       in the correct format to be incorporated into !PATH.
;
; OPTIONAL INPUT KEYWORDS:
;       PATH_FORMAT     = If set, then a single string is returned, in
;                                 the format of !PATH.
;
;       PLUS_REQUIRED   = If set, then a leading plus sign is required
;                       in order to expand out a directory tree.
;                       This is especially useful if the input is a
;                       series of directories, where some components
;                       should be expanded, but others shouldn't.
;
;       RESET   = Often FIND_ALL_DIR is used with logical names.  It
;               can be rather slow to search through these subdirectories.
;               The /RESET keyword can be used to redefine an environment
;               variable so that subsequent calls don't need to look for the
;               subdirectories.
;
;               To use /RESET, the PATH parameter must contain the name of a
;               *single* environment variable.  For example
;
;                               setenv,'FITS_DATA=+/datadisk/fits'
;                               dir = find_all_dir('FITS_DATA',/reset,/plus)
;
;               The /RESET keyword is usually combined with /PLUS_REQUIRED.
;
; PROCEDURE CALLS:
;       DEF_DIRLIST, FIND_WITH_DEF(), BREAK_PATH()
;
; RESTRICTIONS:
;       PATH must point to a directory that actually exists.
;
; REVISION HISTORY:
;               Version 11, Zarro (SM&A/GSFC), 23-March-00
;                       Removed all calls to IS_DIR
;               Version 12, William Thompson, GSFC, 02-Feb-2001
;                       In Windows, use built-in expand_path if able.
;               Version 13, William Thompson, GSFC, 23-Apr-2002
;                       Follow logical links in Unix
;                       (Suggested by Pascal Saint-Hilaire)
;               Version 14, Zarro (EER/GSFC), 26-Oct-2002
;                       Saved/restored current directory to protect against
;                       often mysterious directory changes caused by 
;                       spawning FIND in Unix
;               Version 15, William Thompson, GSFC, 9-Feb-2004
;                       Resolve environment variables in Windows.
;
; Version     : Version 16 W. Landsman GSFC Sep 2006
;                        Remove VMS support
;-
;
        ON_ERROR, 2
        compile_opt idl2
;
        IF N_PARAMS() NE 1 THEN MESSAGE,        $
                'Syntax:  Result = FIND_ALL_DIR( PATH )'

;-- save current directory

   cd,current=current

;
;  If more than one directory was passed, then call this routine reiteratively.
;  Then skip directly to the test for the PATH_FORMAT keyword.
;
        PATHS = BREAK_PATH(PATH, /NOCURRENT)
        IF N_ELEMENTS(PATHS) GT 1 THEN BEGIN
                DIRECTORIES = FIND_ALL_DIR(PATHS[0],    $
                        PLUS_REQUIRED=PLUS_REQUIRED)
                FOR I = 1,N_ELEMENTS(PATHS)-1 DO DIRECTORIES =  $
                        [DIRECTORIES, FIND_ALL_DIR(PATHS[I],    $
                                PLUS_REQUIRED=PLUS_REQUIRED)]
                GOTO, TEST_FORMAT
        ENDIF
;
;  Test to see if the first character is a plus sign.  If it is, then remove
;  it.  If it isn't, and PLUS_REQUIRED is set, then remove any trailing '/'
;  character and skip to the end.
;
        DIR = PATHS[0]
        IF STRMID(DIR,0,1) EQ '+' THEN BEGIN
                DIR = STRMID(DIR,1,STRLEN(DIR)-1)
        END ELSE IF KEYWORD_SET(PLUS_REQUIRED) THEN BEGIN
                DIRECTORIES = PATH
                IF STRMID(PATH,STRLEN(PATH)-1,1) EQ '/' THEN    $
                        DIRECTORIES = STRMID(PATH,0,STRLEN(PATH)-1)
                GOTO, TEST_FORMAT
        ENDIF
;
;  For windows,  use the built-in EXPAND_PATH program.   However, first 
;  resolve any environment variables.
;
        IF !VERSION.OS_FAMILY EQ 'Windows' THEN BEGIN
                WHILE STRMID(DIR,0,1) EQ '$' DO BEGIN
                    FSLASH = STRPOS(DIR,'/')
                    IF FSLASH LT 1 THEN FSLASH = STRLEN(DIR)
                    BSLASH = STRPOS(DIR,'/')
                    IF BSLASH LT 1 THEN BSLASH = STRLEN(DIR)
                    SLASH = FSLASH < BSLASH
                    TEST = STRMID(DIR,1,SLASH-1)
                    DIR = GETENV(TEST) + STRMID(DIR,SLASH,STRLEN(DIR)-SLASH)
                ENDWHILE
                TEMP = DIR
                TEST = STRMID(TEMP, STRLEN(TEMP)-1, 1)
                IF (TEST EQ '/') OR (TEST EQ '\') THEN  $
                      TEMP = STRMID(TEMP,0,STRLEN(TEMP)-1)
                DIRECTORIES = EXPAND_PATH('+' + TEMP, /ALL, /ARRAY)
;
;  On Unix machines spawn the Bourne shell command 'find'.  First, if the
;  directory name starts with a dollar sign, then try to interpret the
;  following environment variable.  If the result is the null string, then
;  signal an error.
;
        END ELSE BEGIN
                IF STRMID(DIR,0,1) EQ '$' THEN BEGIN
                    SLASH = STRPOS(DIR,'/')
                    IF SLASH LT 0 THEN SLASH = STRLEN(DIR)
                    EVAR = GETENV(STRMID(DIR,1,SLASH-1))
                    IF SLASH EQ STRLEN(DIR) THEN DIR = EVAR ELSE        $
                            DIR = EVAR + STRMID(DIR,SLASH,STRLEN(DIR)-SLASH)
                ENDIF
;               IF IS_DIR(DIR) NE 1 THEN MESSAGE,       $
;                       'A valid directory must be passed'
                IF STRMID(DIR,STRLEN(DIR)-1,1) NE '/' THEN DIR = DIR + '/'
                SPAWN,'find ' + DIR + ' -follow -type d -print | sort -', $
                        DIRECTORIES, /SH
;
;  Remove any trailing slash character from the first directory.
;
                TEMP = DIRECTORIES[0]
                IF STRMID(TEMP,STRLEN(TEMP)-1,1) EQ '/' THEN    $
                        DIRECTORIES[0] = STRMID(TEMP,0,STRLEN(TEMP)-1)
        ENDELSE
;
;  Reformat the string array into a single string, with the correct separator.
;  If the PATH_FORMAT keyword was set, then this string will be used.  Also use
;  it when the RESET keyword was passed.
;
TEST_FORMAT:
        DIR = DIRECTORIES[0]
        CASE !VERSION.OS_FAMILY OF
                'Windows':  SEP = ';'
                'MacOS': Sep = ','
                ELSE:  SEP = ':'
        ENDCASE
        FOR I = 1,N_ELEMENTS(DIRECTORIES)-1 DO DIR = DIR + SEP + DIRECTORIES[I]
;
;  If the RESET keyword is set, and the PATH variable contains a *single*
;  environment variable, then call SETENV to redefine the environment variable.
;  If the string starts with a $, then try it both with and without the $.
;
        IF KEYWORD_SET(RESET) THEN BEGIN
                EVAR = PATH
                TEST = GETENV(EVAR)
                IF TEST EQ '' THEN IF STRMID(EVAR,0,1) EQ '$' THEN BEGIN
                        EVAR = STRMID(EVAR,1,STRLEN(EVAR)-1)
                        TEST = GETENV(EVAR)
                ENDIF
                IF (TEST NE '') AND (TEST NE PATH) AND (DIR NE PATH) THEN $
                        SETENV, STRTRIM(EVAR,2) + '=' + $
			STRTRIM(STRJOIN(DIR,':'),2)
        ENDIF
;
;-- restore current directory

        cd,current

        IF KEYWORD_SET(PATH_FORMAT) THEN RETURN, DIR ELSE RETURN, DIRECTORIES
;
        END
