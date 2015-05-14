function q2m,q

; using IDL's row major (row,column) format
;
;  NAME:
;         Q2M
;
;  PURPOSE:
;         Transform direction quaternion to cosine matrix
;
;  INPUTS:
;         quaternion 4 vector
;
;  OUTPUTS:
;         3 x 3 double matrix
;
; METHOD:
;
; REVISION HISTORY:
;       Written by D. Schiminovich, December 2001
;

if ( N_params() lt 1 ) then begin  
     print,'Syntax - result = q2m(q)'
     print,'Q      - quaternion 4-vector'          
     print,'RESULT - 3 x 3 direction cosines matrix
     return, -1
endif 

q=double(q)
m=dblarr(3,3)
m[0,0]=q[0]^2+q[3]^2-(q[1]^2+q[2]^2)
m[0,1]=2*(q[0]*q[1]+q[2]*q[3])
m[0,2]=2*(q[0]*q[2]-q[1]*q[3])
m[1,0]=2*(q[0]*q[1]-q[2]*q[3])
m[1,1]=q[1]^2+q[3]^2-(q[0]^2+q[2]^2)
m[1,2]=2*(q[1]*q[2]+q[0]*q[3])
m[2,0]=2*(q[0]*q[2]+q[1]*q[3])
m[2,1]=2*(q[1]*q[2]-q[0]*q[3])
m[2,2]=q[2]^2+q[3]^2-(q[0]^2+q[1]^2)
return,m
end

