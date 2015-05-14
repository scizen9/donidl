function m2q,m
;
;  NAME:
;         M2Q
;
;  PURPOSE:
;         Transform direction cosine matrix to quaternion
;
;  INPUTS:
;         3 x 3 direction cosines matrix
;
;  OUTPUTS:
;         quaternion 4 vector
;
;  METHOD:
;
;  REVISION HISTORY:
;       Written by D. Schiminovich, December 2001
;

if ( N_params() lt 1 ) then begin  
     print,'Syntax - result = m2q(m)'
     print,'m      - 3x3 direction cosines matrix'       
     print,'RESULT - quaternion 4-vector'          
     return, -1
endif 

  q=[0.d,0.d,0.d,0.d]

  m=double(m)

  tr=m[0,0]+m[1,1]+m[2,2]
  
  maxndx=3
  maxval=tr
  
  for i=0,2 do begin
   	if (m[i,i] GT maxval) then begin
     		maxndx=i
     		maxval=m[i,i]
   	endif
  endfor
     
  case maxndx of 
   	3: begin
             q[3]= sqrt((tr+1.0)/4.0)
             q[0]= (m[1,2]-m[2,1])/(4*q[3])
             q[1]= (m[2,0]-m[0,2])/(4*q[3])
             q[2]= (m[0,1]-m[1,0])/(4*q[3])
           end
        0: begin            

	     q[0]= sqrt((2*m[0,0]+1-tr)/4)
             q[1]= (m[0,1]+m[1,0])/(4*q[0])
             q[2]= (m[0,2]+m[2,0])/(4*q[0])
             q[3]= (m[1,2]-m[2,1])/(4*q[0])
           end
        1: begin            
             q[1]= sqrt((2*m[1,1]+1-tr)/4);
             q[0]= (m[0,1]+m[1,0])/(4*q[1]);
             q[2]= (m[2,1]+m[1,2])/(4*q[1]);
             q[3]= (m[2,0]-m[0,2])/(4*q[1]);
           end
        2: begin            
             q[2]= sqrt((2*m[2,2]+1-tr)/4);
             q[0]= (m[0,2]+m[2,0])/(4*q[2])
             q[1]= (m[2,1]+m[1,2])/(4*q[2])
             q[3]= (m[0,1]-m[1,0])/(4*q[2])
           end
	endcase
return,q
end





