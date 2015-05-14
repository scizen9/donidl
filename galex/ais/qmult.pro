function qmult,q1,q2

;
;  NAME:
;         QMULT
;
;  PURPOSE:
;         Multiply two quaternions
;         
;
;  INPUTS:
;         Q1 - quaternion 4-vector
;         Q2 - quaternion 4-vector
;		
;  OUTPUTS:
;         Q3 - quaternion 4-vector
;
; METHOD:
;
; REVISION HISTORY:
;       Written by D. Schiminovich, December 2001
;

if ( N_params() lt 1 ) then begin  
     print,'Syntax - result = qmult(q1,q2)'
     print,'q1     - quaternion 4-vector'       
     print,'q2     - quaternion 4-vector'       
     print,'RESULT - quaternion 4-vector'          
     return, -1
endif 

q3=dblarr(4)

q3[0]=q1[0]*q2[3]+q1[1]*q2[2]+q1[3]*q2[0]-(q1[2]*q2[1])
q3[1]=q1[1]*q2[3]+q1[2]*q2[0]+q1[3]*q2[1]-(q1[0]*q2[2])
q3[2]=q1[0]*q2[1]+q1[2]*q2[3]+q1[3]*q2[2]-(q1[1]*q2[0])
q3[3]=q1[3]*q2[3]-(q1[0]*q2[0]+q1[1]*q2[1]+q1[2]*q2[2])

return,q3
end


