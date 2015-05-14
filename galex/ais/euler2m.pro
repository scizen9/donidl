function euler2m,euler,typestr

;
;  NAME:
;         EULER2M
;
;  PURPOSE:
;         Transform RA, dec, twist of GALEX satellite into a
;         quaternion 4-vector
;
;  INPUTS:
;         EULER  - 3-vector containing phi, theta, psi in radians
;         TYPESTR- string containing Euler rotation type (e.g. '323')
;              each number indicates which body axis (x=1, y=2, z=3) is
;              used for each rotation (phi, theta, psi)
;		
;  OUTPUTS:
;         M     - 3 x 3 direction cosines matrix
;
; METHOD:
;
; REVISION HISTORY:
;       Written by D. Schiminovich, December 2001
;

if ( N_params() lt 2 ) then begin  
     print,'Syntax - result = euler2m(euler,typestr)'
     print,'euler  - 3-vector containing phi, theta, psi in radians'       
     print,'typestr- string containing Euler rotation type (e.g. "323")'
     print,'RESULT - 3 x 3 direction cosines matrix'          
     return, -1
endif 

phi=double(euler[0])
theta=double(euler[1])
psi=double(euler[2])

; 1 = x axis 
; 2 = y axis 
; 3 = z axis 

; phi (i axis - first rotation)
; theta (j axis - second rotation)
; psi (k axis - third rotation)

; Type 1 euler angle representation
a=dblarr(3,3)
case typestr of
	'123': begin
		a[0,0]=cos(psi)*cos(theta)
		a[0,1]=cos(psi)*sin(theta)*sin(phi)+sin(psi)*cos(phi)
		a[0,2]=(-1.0)*cos(psi)*sin(theta)*cos(phi)+sin(psi)*sin(phi)
                a[1,0]=(-1.0)*sin(psi)*cos(theta)
                a[1,1]=(-1.0)*sin(psi)*sin(theta)*sin(phi)+cos(psi)*cos(phi)
                a[1,2]=sin(psi)*sin(theta)*cos(phi)+cos(psi)*sin(phi)
                a[2,0]=sin(theta)
                a[2,1]=(-1.0)*cos(theta)*sin(phi)
                a[2,2]=cos(theta)*cos(phi)
	end
	'132': begin
		a[0,0]=cos(psi)*cos(theta)
		a[0,1]=cos(psi)*sin(theta)*cos(phi)+sin(psi)*sin(phi)
		a[0,2]=cos(psi)*sin(theta)*sin(phi)-sin(psi)*cos(phi)
                a[1,0]=(-1.0)*sin(theta)
                a[1,1]=cos(theta)*cos(phi)
                a[1,2]=cos(theta)*sin(phi)
                a[2,0]=sin(psi)*cos(theta)
                a[2,1]=sin(psi)*sin(theta)*cos(phi)-cos(psi)*sin(phi)
                a[2,2]=sin(psi)*sin(theta)*sin(phi)+cos(psi)*cos(phi)
	end
	'231': begin
                a[0,0]=cos(theta)*cos(phi)
                a[0,1]=sin(theta)
                a[0,2]=(-1.0)*cos(theta)*sin(phi)
		a[1,0]=(-1.0)*cos(psi)*sin(theta)*cos(phi)+sin(psi)*sin(phi)
		a[1,1]=cos(psi)*cos(theta)
		a[1,2]=cos(psi)*sin(theta)*sin(phi)+sin(psi)*cos(phi)
                a[2,0]=sin(psi)*sin(theta)*cos(phi)+cos(psi)*sin(phi)
                a[2,1]=(-1.0)*sin(psi)*cos(theta)
                a[2,2]=(-1.0)*sin(psi)*sin(theta)*sin(phi)+cos(psi)*cos(phi)
	end
	'213': begin
                a[0,0]=sin(psi)*sin(theta)*sin(phi)+cos(psi)*cos(phi)
                a[0,1]=sin(psi)*cos(theta)
                a[0,2]=sin(psi)*sin(theta)*cos(phi)-cos(psi)*sin(phi)
		a[1,0]=cos(psi)*sin(theta)*sin(phi)-sin(psi)*cos(phi)
		a[1,1]=cos(psi)*cos(theta)
		a[1,2]=cos(psi)*sin(theta)*cos(phi)+sin(psi)*sin(phi)
                a[2,0]=cos(theta)*sin(phi)
                a[2,1]=(-1.0)*sin(theta)
                a[2,2]=cos(theta)*cos(phi)
	end
	'312': begin
                a[0,0]=(-1.0)*sin(psi)*sin(theta)*sin(phi)+cos(psi)*cos(phi)
                a[0,1]=sin(psi)*sin(theta)*cos(phi)+cos(psi)*sin(phi)
                a[0,2]=(-1.0)*sin(psi)*cos(theta)
                a[1,0]=(-1.0)*cos(theta)*sin(phi)
                a[1,1]=cos(theta)*cos(phi)
                a[1,2]=sin(theta)
		a[2,0]=cos(psi)*sin(theta)*sin(phi)+sin(psi)*cos(phi)
		a[2,1]=(-1.0)*cos(psi)*sin(theta)*cos(phi)+sin(psi)*sin(phi)
		a[2,2]=cos(psi)*cos(theta)
	end
	'321': begin
                a[0,0]=cos(theta)*cos(phi)
                a[0,1]=cos(theta)*sin(phi)
                a[0,2]=(-1.0)*sin(theta)
                a[1,0]=sin(psi)*sin(theta)*cos(phi)-cos(psi)*sin(phi)
                a[1,1]=sin(psi)*sin(theta)*sin(phi)+cos(psi)*cos(phi)
                a[1,2]=sin(psi)*cos(theta)
		a[2,0]=cos(psi)*sin(theta)*cos(phi)+sin(psi)*sin(phi)
		a[2,1]=cos(psi)*sin(theta)*sin(phi)-sin(psi)*cos(phi)
		a[2,2]=cos(psi)*cos(theta)
	end
; Type 2 Euler angle rotation
	'121': begin
                a[0,0]=cos(theta)
                a[0,1]=sin(theta)*sin(phi)
                a[0,2]=(-1.0)*sin(theta)*cos(phi)
                a[1,0]=sin(psi)*sin(theta)
                a[1,1]=(-1.0)*sin(psi)*cos(theta)*sin(phi)+cos(psi)*cos(phi)
                a[1,2]=sin(psi)*cos(theta)*cos(phi)+cos(psi)*sin(phi)
		a[2,0]=cos(psi)*sin(theta)
		a[2,1]=(-1.0)*cos(psi)*cos(theta)*sin(phi)-sin(psi)*cos(phi)
		a[2,2]=cos(psi)*cos(theta)*cos(phi)-sin(psi)*sin(phi)
	end
	'131': begin
                a[0,0]=cos(theta)
                a[0,1]=sin(theta)*cos(phi)
                a[0,2]=sin(theta)*sin(phi)
		a[1,0]=(-1.0)*cos(psi)*sin(theta)
		a[1,1]=cos(psi)*cos(theta)*cos(phi)-sin(psi)*sin(phi)
		a[1,2]=cos(psi)*cos(theta)*sin(phi)+sin(psi)*cos(phi)
                a[2,0]=sin(psi)*sin(theta)
                a[2,1]=(-1.0)*sin(psi)*cos(theta)*cos(phi)-cos(psi)*sin(phi)
                a[2,2]=(-1.0)*sin(psi)*cos(theta)*sin(phi)+cos(psi)*cos(phi)
	end
	'212': begin
                a[0,0]=cos(psi)*cos(phi)-sin(psi)*cos(theta)*sin(phi)
                a[0,1]=sin(psi)*sin(theta)
                a[0,2]=(-1.0)*cos(psi)*sin(phi)-sin(psi)*cos(theta)*cos(phi)
		a[1,0]=sin(theta)*sin(phi)
		a[1,1]=cos(theta)
		a[1,2]=sin(theta)*cos(phi)
                a[2,0]=sin(psi)*cos(phi)+cos(psi)*cos(theta)*sin(phi)
                a[2,1]=(-1.0)*cos(psi)*sin(theta)
                a[2,2]=(-1.0)*sin(psi)*sin(phi)+cos(psi)*cos(theta)*cos(phi)
	end
	'232': begin
                a[0,0]=cos(psi)*cos(theta)*cos(phi)-sin(psi)*sin(phi)
                a[0,1]=cos(psi)*sin(theta)
                a[0,2]=(-1.0)*cos(psi)*cos(theta)*sin(phi)-sin(psi)*cos(phi)
		a[1,0]=(-1.0)*sin(theta)*cos(phi)
		a[1,1]=cos(theta)
		a[1,2]=sin(theta)*sin(phi)
                a[2,0]=sin(psi)*cos(theta)*cos(phi)+cos(psi)*sin(phi)
                a[2,1]=sin(psi)*sin(theta)
                a[2,2]=(-1.0)*sin(psi)*cos(theta)*sin(phi)+cos(psi)*cos(phi)
	end
	'313': begin
                a[0,0]=cos(psi)*cos(phi)-sin(psi)*cos(theta)*sin(phi)
                a[0,1]=cos(psi)*sin(phi)+sin(psi)*cos(theta)*cos(phi)
                a[0,2]=sin(psi)*sin(theta)
		a[1,0]=(-1.0)*sin(psi)*cos(phi)-cos(psi)*cos(theta)*sin(phi)
		a[1,1]=(-1.0)*sin(psi)*sin(phi)+cos(psi)*cos(theta)*cos(phi)
		a[1,2]=cos(psi)*sin(theta)
                a[2,0]=sin(theta)*sin(phi)
                a[2,1]=(-1.0)*sin(theta)*cos(phi)
                a[2,2]=cos(theta)
	end
	'323': begin
                a[0,0]=cos(psi)*cos(theta)*cos(phi)-sin(psi)*sin(phi)
                a[0,1]=cos(psi)*cos(theta)*sin(phi)+sin(psi)*cos(phi)
                a[0,2]=(-1.0)*cos(psi)*sin(theta)
		a[1,0]=(-1.0)*sin(psi)*cos(theta)*cos(phi)-cos(psi)*sin(phi)
		a[1,1]=(-1.0)*sin(psi)*cos(theta)*sin(phi)+cos(psi)*cos(phi)
		a[1,2]=sin(psi)*sin(theta)
                a[2,0]=sin(theta)*cos(phi)
                a[2,1]=sin(theta)*sin(phi)
                a[2,2]=cos(theta)
	end
endcase
return,a
end




