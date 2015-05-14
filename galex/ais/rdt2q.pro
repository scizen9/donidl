function rdt2q,rdt

;
;  NAME:
;         RDT2Q
;
;  PURPOSE:
;         Transform RA, dec, twist of GALEX satellite into a
;         quaternion 4-vector
;
;  INPUTS:
;         RDT - three vector containing ra, dec, twist in degrees
;		
;  OUTPUTS:
;         quaternion 4-vector
;
; METHOD:
;
; REVISION HISTORY:
;       Written by D. Schiminovich, December 2001
;

if ( N_params() lt 1 ) then begin  
     print,'Syntax - result = rdt2q(rdt)'
     print,'RDT    - three vector containing ra, dec, twist in degrees'       
     print,'       - ra, dec J2000'
     print,'       - twist rotation of +X about +Z boresite, deg E of N'    
     print,'RESULT - quaternion 4-vector'          
     return, -1
endif 

; RDT = three vector containing ra, dec, twist in degrees

phi=double(rdt[0])
theta=double((90-rdt[1]))
psi=double((180-rdt[2]))

return,m2q(euler2m([phi,theta,psi]/!RADEG,'323'))

end







