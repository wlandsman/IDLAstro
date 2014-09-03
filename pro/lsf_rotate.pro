 function lsf_rotate, deltav, vsini, EPSILON = epsilon, VELGRID = velgrid
;+
; NAME:
;     LSF_ROTATE:
;
; PURPOSE:
;     Create a 1-d convolution kernel to broaden a spectrum from a rotating star
;
; EXPLANATION:
;     Can be used to derive the broadening effect (line spread function; LSF) 
;     due to  rotation on a synthetic stellar spectrum.     Assumes constant 
;     limb darkening across the disk.
;
; CALLING SEQUENCE
;     lsf = LSF_ROTATE(deltav, vsini, EPSILON=, VELGRID=)
;
; INPUT PARAMETERS:
;    deltaV - numeric scalar giving the step increment (in km/s) in the output 
;             rotation kernel.  
;    Vsini - the rotational velocity projected  along the line of sight (km/s)
;
; OUTPUT PARAMETERS:
;    LSF - The convolution kernel vector for the specified rotational velocity.
;          The  number of points in LSF will be always be odd (the kernel is
;          symmetric) and equal to  either ceil(2*Vsini/deltav) or 
;          ceil(2*Vsini/deltav) +1 (whichever number is odd).    LSF will 
;          always be of type FLOAT.
;
;          To actually compute the broadening. the spectrum should be convolved
;          with the rotational LSF. 
; OPTIONAL INPUT PARAMETERS:
;    Epsilon - numeric scalar giving the limb-darkening coefficient, 
;          default = 0.6 which is typical for  photospheric lines.    The
;          specific intensity I at any angle theta from the specific intensity
;          Icen at the center of the disk is given by:
;  
;          I = Icen*(1-epsilon*(1-cos(theta))
;                    
; OPTIONAL OUTPUT PARAMETER:
;     Velgrid - Vector with the same number of elements as LSF 
; EXAMPLE:
;    (1) Plot the LSF for a star rotating at 90 km/s in both velocity space and
;        for a central wavelength of 4300 A.    Compute the LSF every 3 km/s
;
;       IDL> lsf = lsf_rotate(3,90,velgrid=vel)      ;LSF will contain 61 pts
;       IDL> plot,vel,lsf                    ;Plot the LSF in velocity space
;       IDL> wgrid = 4300*(1+vel/3e5)       ;Speed of light = 3e5 km/s
;       IDL> oplot,wgrid,lsf                ;Plot in wavelength space
;
; NOTES:
;    Adapted from rotin3.f in the SYNSPEC software of Hubeny & Lanz 
;        .http://nova.astro.umd.edu/index.html    Also see Eq. 17.12 in 
;    "The Observation and Analysis of Stellar Photospheres" by D. Gray (1992)
; REVISION HISTORY:
;    Written,   W. Landsman                November 2001
;-
    On_error,2
    compile_opt idl2
    if N_params() LT 1 then begin
         print,'Syntax - rkernel = lsf_rotate(deltav, vsini)'
         print,'      Input Keyword: Epsilon'
         print,'      Output Keyword: Velgrid'
         return,-1
    endif

    if N_elements(epsilon) EQ 0 then epsilon = 0.6
    e1 = 2.0d*(1.0d - epsilon)
    e2 = !dpi*epsilon/2.0d
    e3 = !dpi*(1.0d - epsilon/3.0d)

    npts = ceil(2*vsini/deltav)
    if npts mod 2 EQ 0 then npts = npts +1
    nwid = npts/2
    x = (dindgen(npts)- nwid)
    x = x*deltav/vsini  
    if arg_present(velgrid) then velgrid = x*vsini
    x1 = abs(1.0d - x^2)
    return, float((e1*sqrt(x1) + e2*x1)/e3)
   
    end   
