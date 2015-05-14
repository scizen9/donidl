FUNCTION  lorentzian, x, parameter
;+
; NAME:
;           lorentzian
;
; PURPOSE:
;           Define Lorentzian fit model y =  1/pi * gamma/2 * 1/((x-x0)^2 + (gamma/2)^2)
;
; SUBCATEGORY:
;           fitmodel
;  
; INPUTS:
;           x         - Input x value array. Should be double precision.
;           parameter - Parameter to compute model. Must contain 2
;                       values.  
; 
; PARAMETERS:
;           lorentz:x0    - Center of the Lorentzian curve (mean value)
;           lorentz:gamma - Half-width factor of Lorentzian curve. 
;
; OUTPUT:
;           y =  1/pi * gamma/2 * 1/((x-x0)^2 + (gamma/2)^2)
;           
;
; SIDE EFFECTS:
;           None
;
;
; HISTORY:
;           $Id: cafe_model_lorentz.pro,v 1.1 2004/02/19 10:32:04 goehler Exp $
;-
;
; $Log: cafe_model_lorentz.pro,v $
; Revision 1.1  2004/02/19 10:32:04  goehler
; model to describe lorentzian curve
;
;
;
;



    name="lorentzian"

  ;; ------------------------------------------------------------
  ;; COMPUTE VALUE:
  ;; ------------------------------------------------------------

  ;; actual computation:
  return,  1.D0/!DPI * parameter[1]/2.D0 / SQRT((x-parameter[0])^2 + (parameter[1]/2.D0)^2)

END  
