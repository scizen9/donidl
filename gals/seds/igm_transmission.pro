function igm_transmission,z_source,lambda_obs


A=dblarr(4)
lambda=dblarr(5)
red_lambda=dblarr(5)

;    ;define constants from Madau et al 1996 :
A(0) = 0.0036                   ;Ly_alpha coefficient
A(1) = 1.7e-3                   ;Ly_beta ...
A(2) = 1.2e-3                   ;Ly_gamma ...
A(3) = 9.3e-4                   ;Ly_delta ...

lambda(0) = 1216.               ;Ly_alpha in angstroms
lambda(1) = 1026.               ;Ly_beta
lambda(2) = 973.                ;Ly_gamma
lambda(3) = 950.                ;Ly_delta
lambda(4) = 912.                ;Lyman limit

red_lambda(0:4) = (1.+z_source) * lambda ;redshifted lambdas

;variables for continuum absorption :
xc = lambda_obs/lambda(4)
xem = 1. + z_source

;We consider that between 912A and 950A, there is only Ly_delta
;absorption, because of lacking coefficients in Madau et al 1996.

;initialisation : if no absorption, igm_abs = 0.0 :
igm_absorption = 0.0 * lambda_obs

;Ly_alpha only :
iok=where(lambda_obs le red_lambda(0) and lambda_obs gt red_lambda(1),nok)
if (nok ne 0) then begin
    igm_absorption(iok) = A(0) * (lambda_obs(iok)/lambda(0))^(3.46)
endif

                                ;Ly_alpha and Ly_beta :
iok=where(lambda_obs le red_lambda(1) and lambda_obs gt red_lambda(2),nok)
if (nok ne 0) then begin
    igm_absorption(iok) = A(0) * (lambda_obs(iok)/lambda(0))^(3.46)+$
      A(1) * (lambda_obs(iok)/lambda(1))^(3.46)
endif

                                ;Ly_alpha, Ly_beta, and Ly_gamma :
iok=where(lambda_obs le red_lambda(2) and lambda_obs gt red_lambda(3),nok)
if (nok ne 0) then begin
    igm_absorption(iok) = A(0) * (lambda_obs(iok)/lambda(0))^(3.46)+$ 
      A(1) * (lambda_obs(iok)/lambda(1))^(3.46)+$
      A(2) * (lambda_obs(iok)/lambda(2))^(3.46)
endif

                                ;Ly_alpha, Ly_beta, Ly_gamma, and Ly_delta :
iok=where(lambda_obs le red_lambda(3) and lambda_obs gt red_lambda(4),nok)
if (nok ne 0) then begin
    igm_absorption(iok) = A(0) * (lambda_obs(iok)/lambda(0))^(3.46)+$
      A(1) * (lambda_obs(iok)/lambda(1))^(3.46)+$
      A(2) * (lambda_obs(iok)/lambda(2))^(3.46)+$
      A(3) * (lambda_obs(iok)/lambda(3))^(3.46)
endif

                                ;All these lines plus the photoelectric effect (shortwards 912A)
iok=where(lambda_obs le red_lambda(4),nok)
if (nok ne 0) then begin
    igm_absorption(iok) = A(0) * (lambda_obs(iok)/lambda(0))^(3.46)+$
      A(1) * (lambda_obs(iok)/lambda(1))^(3.46)+$
      A(2) * (lambda_obs(iok)/lambda(2))^(3.46)+$
      A(3) * (lambda_obs(iok)/lambda(3))^(3.46)+$
      0.25 * xc(iok)^3 * (xem^0.46 - xc(iok)^0.46)+$
      9.4 * xc(iok)^1.5 * (xem^0.18 - xc(iok)^0.18) -$
      0.7 * xc(iok)^3 * (xc(iok)^(-1.32) - xem^(-1.32)) -$
      0.023 * (xem^1.68 - xc(iok)^1.68)
endif

igm_transmission = exp(-igm_absorption)

return,igm_transmission

end
