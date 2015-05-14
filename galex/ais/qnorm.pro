function qnorm,q

;
;  NAME:
;         QNORM
;
;  PURPOSE:
;         Normalize quaternion
;         
;
;  INPUTS:
;         Q - quaternion 4-vector
;
;  OUTPUTS:
;         RESULT - quaternion 4-vector
;                  leaves q[3] with positive sign
;
; METHOD:
;
; REVISION HISTORY:
;       Written by D. Schiminovich, December 2001
;

if ( N_params() lt 1 ) then begin  
     print,'Syntax - result = qnorm(q)'
     print,'q      - quaternion 4-vector'       
     print,'RESULT - quaternion 4-vector'          
     return, -1
endif 


mag=sqrt(total(q*q))
q=q/mag
if q[3] lt 0 then q[0:3]=(-1.0)*q[0:3]
return,q
end