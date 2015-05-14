function qconj,q

;
;  NAME:
;         QCONJ
;
;  PURPOSE:
;         Returns conjugate quaternion
;         
;
;  INPUTS:
;         Q - quaternion 4-vector
;
;  OUTPUTS:
;         RESULT - quaternion 4-vector
;
; METHOD:
;
; REVISION HISTORY:
;       Written by D. Schiminovich, December 2001
;

if ( N_params() lt 1 ) then begin  
     print,'Syntax - result = qconj(q)'
     print,'q      - quaternion 4-vector'       
     print,'RESULT - quaternion 4-vector'          
     return, -1
endif 

return,[(-1.0)*q[0:2],q[3]]
end