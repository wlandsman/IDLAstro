function date_conv,date,type, BAD_DATE = bad_date
;+
; NAME:
;     DATE_CONV
; PURPOSE:
;     Procedure to perform conversion of dates to one of three possible formats.
;
; EXPLANATION:
;     The following date formats are allowed
;
;       format 1: real*8 scalar encoded as:
;               year*1000 + day + hour/24. + min/24./60 + sec/24./60/60
;               where day is the day of year (1 to 366)
;       format 2: Vector encoded as:
;               date[0] = year (eg. 2005)
;               date[1] = day of year (1 to 366)
;               date[2] = hour
;               date[3] = minute
;               date[4] = second
;               To indicate a date only, set a negative hour.
;       format 3: string (ascii text) encoded as
;               DD-MON-YEAR HH:MM:SS.SS
;               (eg.  14-JUL-2005 15:25:44.23)
;            OR
;               YYYY-MM-DD HH:MM:SS.SS  (ISO standard)
;               (eg.  1987-07-14 15:25:44.23 or 1987-07-14T15:25:44.23)
;
;            OR 
;               DD/MM/YY (pre-2000 option for FITS DATE keywords)
;            Time of day segment is optional in all of these.
;       
;       format 4: three element vector giving spacecraft time words
;       from a Hubble Space Telescope (HST) telemetry packet.   Based on
;       total number of secs since midnight, JAN. 1, 1979
;
;       format 5: Julian day. As this is also a scalar, like format 1, 
;       	the distinction between the two on input is made based on their
;       	value. Numbers > 2300000 are interpreted as Julian days.
;
; CALLING SEQUENCE
;       results = DATE_CONV( DATE, TYPE )
;
; INPUTS:
;       DATE - input date in one of the possible formats. Must be scalar.
;       TYPE - type of output format desired.  If not supplied then
;               format 3 (real*8 scalar) is used.
;                       valid values:
;                       'REAL'  - format 1
;                       'VECTOR' - format 2
;                       'STRING' - format 3
;                       'FITS' - YYYY-MM-DDTHH:MM:SS.SS'
;                       'JULIAN' - Julian date
;                       'MODIFIED' - Modified Julian date (JD-2400000.5)
;               TYPE can be abbreviated to the single character strings 'R',
;               'V', 'S', 'F', 'J', and 'M'.
;               Nobody wants to convert TO spacecraft time (I hope!)
; OUTPUTS:
;       The converted date is returned as the function value.
;       Output is -1 if date is unrecognisable. 
;
;       If the time of day is omitted from the input, it will also
;       be omitted from any output string (format STRING or FITS). 
;       Note that date-only strings are allowed by the FITS standard. 
;       For other output formats any missing time of day is set to 
;       00:00:00.0
;
; KEYWORD OUTPUTS
;
;        BAD_DATE set to 1B if date is unrecognisable
;
; EXAMPLES:
;       IDL> print,date_conv('2006-03-13 19:58:00.00'),f='(f15.5)' 
;             2006072.83194 
;       IDL> print,date_conv( 2006072.8319444d,'F')
;             2006-03-13T19:58:00.00
;       IDL> print,date_conv( 2006072.8319444d,'V')
;             2006.00      72.0000      19.0000      57.0000      59.9962
;       IDL> print,date_conv( 2006072.8319444d,'J'), f='(f15.5)'
;             2453808.33194
;
;
; HISTORY:
;      version 1  D. Lindler  July, 1987
;      adapted for IDL version 2  J. Isensee  May, 1990
;      Made year 2000 compliant; allow ISO format input  jls/acc Oct 1998
;      DJL/ACC Jan 1998, Modified to work with dates such as 6-JAN-1996 where
;               day of month has only one digit.
;      DJL, Nov. 2000, Added input/output format YYYY-MM-DDTHH:MM:SS.SS
;      Replace spaces with '0' in output FITS format  W.Landsman April 2006
;      Added Julian date capabilities on input and output.  M.Perrin, July 2007
;      Removed spurious /WARN keyword to MESSAGE W.L. Feb 2012
;      ...and another /WARN; added BAD_DATE, drop spurious time-of-day
;      output from strings. J. P. Leahy July 2013
;      changed all /CONTINUE warning messages to /INFO: can be suppressed 
;      by setting !QUIET = 1.  J. P. Leahy July 2013
;-
;-------------------------------------------------------------
;
compile_opt idl2
; data declaration
;
days = [0,31,28,31,30,31,30,31,31,30,31,30,31]
months = ['   ','JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT',$
        'NOV','DEC']
;
; set default type if not supplied
;
if N_params() lt 2 then type = 'REAL'
;
; Determine type of input supplied
;
s = size(date) & ndim = s[0] & datatype = s[ndim+1]
if ndim gt 0 then begin                 ;vector?
        if ndim gt 1 then goto,notvalid
        if (s[1] ne 5) && (s[1] ne 3) then goto,notvalid
        if (s[1] eq 5) then form = 2 else form = 4
   end else begin                       ;scalar input
        if datatype eq 0 then goto,notvalid
        if datatype eq 7 then form = 3 $        ;string
                         else form = 1  ;numeric scalar
end
;
;      -----------------------------------
;
;*** convert input to year,day,hour,minute,second
;
;      -----------------------------------
case form of

        1: begin                                        ;real scalar
			; The 'real' input format may be interpreted EITHER
			; a) if < 2300000
			;    as the traditional 'real*8 encoded' format used by date_conv
			; b) if > 2300000
			;    as a Julian Day Number
                idate = long(date)
                year = long(idate/1000)

				if year lt 2300 then begin
					
					; if year is only 2 digits, assume 1900
	                if year lt 100 then begin
	                   message,/INF, $
	                     'Warning: Year specified is only 2 digits, assuming 19xx'
	                   year=1900+year
	                   idate=1900000+idate
	                   date=1900000.+date
	                end
	                day = idate - year*1000
	                fdate = date-idate
	                fdate = fdate*24.
	                hour = fix(fdate)
	                fdate = (fdate-hour)*60.0
	                minute = fix(fdate)
	                sec = float((fdate-minute)*60.0)

				endif else begin
					daycnv, date, year, mn, mndy, hr
					; convert from month/day to day of year
					; how many days PRECEED the start of each month?
					YDAYS = [0,31,59,90,120,151,181,212,243,273,304,334,366] 
					LEAP =  (((YeaR MOD 4) EQ 0) AND ((YeaR MOD 100) NE 0)) OR $
					                 ((YeaR MOD 400) EQ 0)
			        IF LEAP THEN YDAYS[2:*] = YDAYS[2:*] + 1
					day = ydays[mn-1]+mndy
					
					hour = fix(hr)
					fmin = (hr-hour)*60
					minute = fix(fmin)
					sec = float((fmin-minute)*60)
				endelse
           end

        2: begin                                        ;vector
                year = fix(date[0])
;
; if year is only 2 digits, assume 1900
;
                if year lt 100 then begin
                   message,/INF, $
                    'Warning: Year specified is only 2 digits, assuming 19xx'
                   year=1900+year
                end
;
                day = fix(date[1])
                hour = fix(date[2])
                minute = fix(date[3])
                sec = float(date[4])
           end

        3: begin                                        ;string
                temp = date
;
; check for old type of date, DD-MMM-YYYY
;
                test = STRPOS(temp,'-')            
                if test ge 0 && test le 2 then begin
                  day_of_month = fix(gettok(temp,'-'))
                  month_name = gettok(temp,'-')
                  year = fix(gettok(temp,' '))
;
; determine month number from month name
;
                  month_name = strupcase(month_name)
                  for mon = 1,12 do begin
                        if month_name eq months[mon] then goto,found
                  end
                  message,/INFORMATIONAL, 'Invalid month name specified'
                  goto, notvalid
;
; check for new type of date, ISO: YYYY-MM-DD
;
                end else if strpos(temp,'-') eq 4 then begin
                  year = fix(gettok(temp,'-'))
                  month_name = gettok(temp,'-')
                  mon= FIX(month_name)
                  day_of_month=gettok(temp,' ')
                  if strlen(temp) eq 0 then begin
                        dtmp=gettok(day_of_month,'T')
                        temp=day_of_month
                        day_of_month=dtmp
                  end
                  day_of_month=fix(day_of_month)
;
; check for DD/MM/YY
;
                end else if STRPOS(temp,'/') eq 2 then begin
                  day_of_month = FIX(gettok(temp,'/'))
                  mon = FIX(gettok(temp,'/'))
                  year = 1900 + FIX(STRMID(temp,0,2))
                end else goto, notvalid
                
    found:
                hour = gettok(temp,':')
                hour =  hour NE '' ? FIX(hour) : -1
                minute = fix(gettok(temp,':'))
                sec = float(strtrim(strmid(temp,0,5)))
             
                IF (mon LT 1 || mon GT 12) THEN BEGIN
                    MESSAGE, /INFORMATIONAL, 'Invalid month specified'
                    goto, notvalid
                ENDIF
;
; if year is only 2 digits, assume 1900
;
                if year lt 100 then begin
                   message,/INFORMATIONAL, $ 
                     'Warning: Year specified is only 2 digits, assuming 19xx'
                   year=1900+year
                end
;
;
;            convert to day of year from month/day_of_month
;
;            correction for leap years
;
;               if (fix(year) mod 4) eq 0 then days(2) = 29     ;add one to february
                lpyr = ((year mod 4) eq 0) and ((year mod 100) ne 0) $
                        or ((year mod 400) eq 0)
                if lpyr eq 1 then days[2] = 29 ; if leap year, add day to Feb.
;
;
;            compute day of year
;
                  day = fix(total(days[0:mon-1])+day_of_month)
           end

        4 : begin                       ;spacecraft time
                SC = DOUBLE(date)
                SC = SC + (SC LT 0.0)*65536.    ;Get rid of neg. numbers 
;
;            Determine total number of secs since midnight, JAN. 1, 1979
;
                SECS = SC[2]/64 + SC[1]*1024 + SC[0]*1024*65536.
                SECS = SECS/8192.0D0            ;Convert from spacecraft units 
;
;            Determine number of years 
;
                MINS = SECS/60.
                HOURS = MINS/60.
                TOTDAYS = HOURS/24.
                YEARS = TOTDAYS/365.
                YEARS = FIX(YEARS)
;
;            Compute number of leap years past 
;
                LEAPYEARS = (YEARS+2)/4
;
;           Compute day of year 
;
                DAY = FIX(TOTDAYS-YEARS*365.-LEAPYEARS)
;
;           Correct for case of being right at end of leapyear
;
                IF DAY LT 0 THEN BEGIN
                  DAY = DAY+366
                  LEAPYEARS = LEAPYEARS-1
                  YEARS = YEARS-1
                END
;
;            COMPUTE HOUR OF DAY
;
                TOTDAYS = YEARS*365.+DAY+LEAPYEARS
                HOUR = FIX(HOURS - 24*TOTDAYS)
                TOTHOURS = TOTDAYS*24+HOUR
;
;            COMPUTE MINUTE
;
                MINUTE = FIX(MINS-TOTHOURS*60)
                TOTMIN = TOTHOURS*60+MINUTE
;
;            COMPUTE SEC
;
                SEC = SECS-TOTMIN*60
;
;            COMPUTE ACTUAL YEAR
;
                YEAR = YEARS+79
;
; if year is only 2 digits, assume 1900
;
                if year lt 100 then begin
                   message, /INF, $ 
                     'Warning: Year specified is only 2 digits, assuming 19xx'
                   year=1900+year
                end
;
;
;            START DAY AT ONE AND NOT ZERO
;
                DAY++
           END
ENDCASE
;
;            correction for leap years
;
        if form ne 3 then begin         ;Was it already done?
           lpyr = ((year mod 4) eq 0) && ((year mod 100) ne 0) $
                || ((year mod 400) eq 0)
           if lpyr eq 1 then days[2] = 29 ; if leap year, add day to Feb.
        end
;
;            check for valid day
;
        if (day lt 1) || (day gt total(days)) then begin
            message, /INFORMATIONAL, $
                'ERROR -- There are only ' + strtrim(fix(total(days)),2) + $
	              ' days  in year '+strtrim(year,2)
            goto, notvalid
        endif    
;
;            find month which day occurs
;
        day_of_month = day
        month_num = 1
        while day_of_month gt days[month_num] do begin
               day_of_month = day_of_month - days[month_num]
               month_num = month_num+1
        end
;           ---------------------------------------
;
;   *****       Now convert to output format
;
;           ---------------------------------------
;
; is type a string
;
s = size(type)
if (s[0] ne 0) or (s[1] ne 7) then $
        message,'ERROR - Output type specification must be a string'
;
outcode = STRMID(STRUPCASE(type),0,1)
IF (outcode EQ 'S' || outcode EQ 'F') && hour GE 0 THEN BEGIN
    xsec = strmid(string(sec+100,'(f6.2)'),1,5)
    if xsec EQ '60.00' then begin
        minute = minute+1
        xsec = '00.00'
    endif
    xminute =   string(minute,'(i2.2)')
    if xminute EQ '60' then begin
        hour = hour+1
        xminute = '00'                  
    endif          
    tod = string(hour,'(i2.2)') +  ':' +xminute + ':'+ xsec
ENDIF
 
case outcode of

        'V' : begin                             ;vector output
                out = fltarr(5)
                out[0] = year
                out[1] = day
                out[2] = hour > 0
                out[3] = minute
                out[4] = sec
             end
 
        'R' : begin                             ;floating point scalar
;               if year gt 1900 then year = year-1900
                out = sec/24.0d0/60./60. + minute/24.0d0/60. $
                + (hour > 0)/24.0d0  +  day + year*1000d0
              end

        'S' : begin                             ;string output 

                month_name = months[month_num]
;
;            encode into ascii_date
;
                out = string(day_of_month,'(i2)') +'-'+ month_name +'-' + $
                        string(year,'(i4)')
                        
  ; Omit time of day from output string if not specified on input
                IF hour GE 0 THEN out += ' '+tod
           end
        'F' : begin
                out = string(year,'(i4)')+'-'+string(month_num,'(I2.2)') $
                      + '-' +  string(day_of_month,'(i2.2)')
                IF hour GE 0 THEN out += 'T' + tod             
           end

		'J' : begin	; Julian Date
				ydn2md, year, day, mn, dy
				juldate, [year, mn, dy, hour, minute, sec], rjd
				out = rjd+2400000   ; convert from reduced to regular JD
			  end
		'M' : begin ; Modified Julian Date = JD - 2400000.5
				ydn2md, year, day, mn, dy
				juldate, [year, mn, dy, hour, minute, sec], rjd
				out = rjd-0.5   ; convert from reduced to modified JD
			  end

        else: begin                     ;invalid type specified
                print,'DATE_CONV-- Invalid output type specified'
                print,' It must be ''REAL'', ''STRING'', ''VECTOR'', ''JULIAN'', ''MODIFIED'', or ''FITS''.'
                return,-1
              end
endcase

bad_date = 0B
return,out
;
; invalid input date error section
;
NOTVALID:
bad_date = 1B
message, 'Invalid input date specified', /INFORMATIONAL
return, -1
end
