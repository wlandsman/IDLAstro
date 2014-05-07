PRO JULDATE, DATE, JD, PROMPT = prompt
;+                                                                  
; NAME:
;     JULDATE
; PURPOSE:                                   
;     Convert from calendar to Reduced Julian Date
;
; EXPLANATION:
;     Julian Day Number is a count of days elapsed since Greenwich mean noon 
;     on 1 January 4713 B.C.  The Julian Date is the Julian day number
;     followed by the fraction of the day elapsed since the preceding noon. 
;
;     This procedure duplicates the functionality of the JULDAY() function in
;     in the standard IDL distribution, but also allows interactive input and
;     gives output as Reduced Julian date (=JD - 2400000.)  

; CALLING SEQUENCE:
;     JULDATE, /PROMPT           ;Prompt for calendar Date, print Julian Date
;               or
;     JULDATE, date, jd      
;
; INPUT:
;     DATE -  3 to 6-element vector containing year,month (1-12),day, and 
;              optionally hour, minute, and second all specified as numbers
;              (Universal Time).   Year should be supplied with all digits.
;              Years B.C should be entered as negative numbers (and note that
;              Year 0 did not exist).  If Hour, minute or seconds are not 
;              supplied, they will default to 0. 
;
;  OUTPUT:
;       JD - Reduced Julian date, double precision scalar.  To convert to
;               Julian Date, add 2400000.   JULDATE will print the value of
;               JD at the terminal if less than 2 parameters are supplied, or 
;               if the /PROMPT keyword is set
;      
;  OPTIONAL INPUT KEYWORD:
;       /PROMPT - If this keyword is set and non-zero, then JULDATE will prompt
;               for the calendar date at the terminal.
;
;  RESTRICTIONS:
;       The procedure HELIO_JD can be used after JULDATE, if a heliocentric
;       Julian date is required.
;
;  EXAMPLE:
;       A date of 25-DEC-2006 06:25 UT may be expressed as either
;
;       IDL> juldate, [2006, 12, 25, 6, 25], jd       
;       IDL> juldate, [2006, 12, 25.2673611d], jd 
;
;       In either case, one should obtain a Reduced Julian date of 
;       JD = 54094.7673611
;
;  PROCEDURE USED:
;       GETOPT()
;  REVISION HISTORY
;       Adapted from IUE RDAF (S. Parsons)                      8-31-87
;       Algorithm from Sky and Telescope April 1981   
;       Added /PROMPT keyword, W. Landsman    September 1992
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Make negative years correspond to B.C. (no year 0), work for year 1582
;       Disallow 2 digit years.    W. Landsman    March 2000
;-
 On_error,2 

 if ( N_params() EQ 0 ) and ( ~keyword_set( PROMPT ) ) then begin
     print,'Syntax - JULDATE, date, jd          or JULDATE, /PROMPT'
     print, $
     '  date - 3-6 element vector containing [year,month,day,hour,minute,sec]'
     print,'  jd - output reduced julian date (double precision)'
     return
 endif

 if ( N_elements(date) EQ 0 ) then begin   

    opt = ''                                                          
    rd: read,' Enter Year,Month,Day,Hour, Minute, Seconds (All Numeric): ',opt
    date = getopt( opt, 'F' )

 endif

 case N_elements(date) of      

    6: 
    5: date = [ date, 0.0d]
    4: date = [ date, 0.0d,0.0d]    
    3: date = [ date, 0.0d, 0.0d,0.0d]
    else: message,'Illegal DATE Vector - must have a least 3 elements'

  endcase   

 iy = floor( date[0] ) 
 if iy lt 0 then iy++  else $
    if iy EQ 0 then message,'ERROR - There is no year 0'                   
 im = fix( date[1] )
 date = double(date)
 day = date[2] + ( date[3] + date[4]/60.0d + date[5]/3600.0d) / 24.0d
;
 if ( im LT 3 ) then begin   ;If month is Jan or Feb, don't include leap day

     iy-- & im = im+12 

 end

 a = long(iy/100)
 ry = float(iy)

 jd = floor(ry*0.25d) + 365.0d*(ry -1860.d) + fix(30.6001d*(im+1.)) + $
      day  - 105.5d

;Gregorian Calendar starts on Oct. 15, 1582 (= RJD -100830.5)
 if jd GT -100830.5 then jd = jd + 2 - a + floor(a/4)

 if N_params() LT 2 || keyword_set( PROMPT) then begin      
    yr = fix( date[0] )
    print, FORM='(A,I4,A,I3,A,F9.5)',$ 
       ' Year ',yr,'    Month', fix(date[1] ),'    Day', day 
    print, FORM='(A,F15.5)',' Reduced Julian Date:',JD                       
 endif
 
 return                               
 end                                  ; juldate
