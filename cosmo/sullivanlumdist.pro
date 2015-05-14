;+
; NAME: 
;    SULLIVANLUMDIST
;       
; PURPOSE: 
;    Calculate luminosity distance (in Mpc) of an object given its redshift 
; EXPLANATION:
;    The luminosity distance in the Friedmann-Robertson-Walker model is 
;    taken from  Caroll, Press, and Turner (1992, ARAA, 30, 499), p. 511
;    Uses a closed form (Mattig equation) to compute the distance when the 
;    cosmological constant is zero.   Otherwise integrates the function using
;    QSIMP.	
; CALLING SEQUENCE: 
;    result = sullivanlumdist(z, [H0 = , OMEGA_K = , OMEGA_M =, OMEGA_L/OMEGA_DE = ,
;    q0 = w= ,/SILENT])
;      
; INPUTS:
;    z = redshift, positive scalar or vector
;
; OPTIONAL KEYWORD INPUTS: 
;    /SILENT - If set, the program will not display adopted cosmological
;        parameters at the terminal.
;    H0: Hubble parameter  in km/s/Mpc, default is 70
;
;        No more than two of the following four parameters should be
;        specified.   None of them need be specified -- the adopted defaults
;        are given.
;    OMEGA_K - curvature constant, normalized to the closure density.   Default is
;        0, indicating a flat universe
;    w - eqn. of state parameter for the dark energy
;    OMEGA_M -  Matter density, normalized to the closure density, default
;        is 0.3.   Must be non-negative
;    OMEGA_L - Cosmological constant, normalized to the closure density,
;        default is 0.7
;    OMEGA_DE - Same as OMEGA_L but more generally named
;    q0 - Deceleration parameter, numeric scalar = -R*(R'')/(R')^2, default
;        is -0.5
;       
; OUTPUTS:
;    The result of the function is the luminosity distance (in Mpc) for each 
;    input value of z.
;
; COMMENTS:
;    (1) Integrates using the IDL Astronomy Version procedure QSIMP.    (The 
;    intrinsic IDL QSIMP function is not called because of its ridiculous
;    restriction that only scalar arguments can be passed to the integrating
;    function.)
;    (2) Can fail to converge at high redshift for closed universes with
;    non-zero lambda.   This can presumably be fixed by replacing QSIMP with
;    an integrator that can handle a singularity 
; PROCEDURES CALLED:
;    COSMO_PARAM, QSIMP   
; REVISION HISTORY:
;    Written   W. Landsman        Raytheon ITSS       April 2000
;    Avoid integer overflow for more than 32767 redshifts  July 2001
;    "w" added by m sullivan Sep 2005
;-                                   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION sullivanldist, z, OMEGA_DE = omega_de, W=w, OMEGA_M=omega_m, OMEGA_K=omega_k

denom= omega_k*(1.d0+z)^2 + omega_m*(1.d0+z)^3 + omega_de*(1.d0+z)^(3.d0*(1.d0+w))

out = z*0.d0  
good = WHERE(denom GT 0.0d0, Ngood)
IF (Ngood GT 0) THEN out[good] = 1.d0/SQRT(denom[good])

RETURN, out
END 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION  sullivanlumdist, z, H0=h0, OMEGA_K = omega_k, OMEGA_L = omega_l, OMEGA_M = omega_m, $
  Q0 = q0, W=w, SILENT = silent, OMEGA_DE=omega_de

IF (N_params() EQ 0) THEN BEGIN
   print,'Syntax: result = SULLIVANLUMDIST(z, H0=, Q0=, OMEGA_M=, OMEGA_K=, OMEGA_L/OMEGA_DE=, w=])'
   print,'Returns luminosity distance in Mpc.'
   RETURN, !Values.D_NAN
ENDIF

IF(N_ELEMENTS(w) EQ 0)THEN w=-1.d0
IF N_ELEMENTS(H0) EQ 0 THEN H0 = 70.d0
IF(N_ELEMENTS(omega_de) EQ 0 AND N_ELEMENTS(omega_l) NE 0)THEN omega_de=omega_l

n = N_ELEMENTS(z)
cosmo_param,Omega_m,Omega_de, Omega_k, q0 


c = 2.99792458d5                ;;  speed of light in km/s

IF NOT KEYWORD_SET(silent) THEN $
  print,'SULLIVANLUMDIST: H0:', h0, ' Omega_m:', omega_m, ' Omega_de',omega_de, $
        ' q0: ',q0, ' Omega_k: ', omega_k, ' w: ',w, f='(A,I3,A,f5.2,A,f5.2,A,f5.2,A,F5.2,A,F5.2)' 

; For the case of Lambda = 0, we use the closed form from equation 5.238 of
; Astrophysical Formulae (Lang 1998).   This avoids terms that almost cancel
; at small q0*z better than the more familiar Mattig formula.
;
IF (omega_de EQ  0.d0) THEN BEGIN
   denom = SQRT(1.d0+2.d0*q0*z) + 1.d0 + q0*z 
   dlum = (c*z/h0)*(1.d0 + z*(1.d0-q0)/denom)
   RETURN,dlum
   
; For non-zero lambda 
ENDIF ELSE BEGIN 
   dlum = z*0.0d0
   FOR  i=0L,N-1 DO BEGIN
      IF z[i] LE 0.0d0 THEN dlum[i] = 0.0d0 ELSE BEGIN
         QSIMP,'SULLIVANLDIST',0.d0,z[i], lz,OMEGA_DE = omega_de, W=w, OMEGA_M=omega_m,OMEGA_K=omega_k,EPS=1d-6
         dlum[i] = lz
         ENDELSE
   ENDFOR
   IF (omega_k GT 0d0) THEN $
     dlum = SINH(SQRT(omega_k)*dlum)/SQRT(omega_k) $
   ELSE IF (omega_k LT 0d0) THEN $
     dlum = SIN(SQRT(-omega_k)*dlum)/SQRT(-omega_k) > 0d0 
   RETURN, c*(1d0+z)*dlum/h0
ENDELSE   

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
