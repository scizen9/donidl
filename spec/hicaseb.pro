function hicaseb,t,n,lineno,silent=silent
;
; return the case B ratio given temperature and electron density
; from Osterbrock 1989
;
; check lineno
valid_lines = [3,5,6,7,8,9,10,15,20]
p=where(valid_lines eq lineno, nf)
if nf le 0 then begin
	print,'Illegal line number, choose:'
	print,'Ha  6563 -  3'
	print,'Hg  4340 -  5'
	print,'Hd  4101 -  6'
	print,'He  3970 -  7'
	print,'H8  3889 -  8'
	print,'H9  3835 -  9'
	print,'H10 3797 - 10'
	print,'H15      - 15'
	print,'H20      - 20'
	return,-1.
endif else i=p(0)
;
x = $	; temperature
	[[replicate(5000.,9)], $	; density = 10^2
	 [replicate(5000.,9)], $	; density = 10^4
	 [replicate(10000.,9)], $	; density = 10^2
	 [replicate(10000.,9)], $	; denisty = 10^4
	 [replicate(10000.,9)], $	; density = 10^6
	 [replicate(20000.,9)], $	; density = 10^2
	 [replicate(20000.,9)]]  	; density = 10^4
;
y = $	; density
	[[replicate(100.,9)], $	; temperature = 5000
	 [replicate(10000.,9)], $	; temperature = 5000
	 [replicate(100.,9)], $	; temperature = 10000.
	 [replicate(10000.,9)], $	; temperature = 10000.
	 [replicate(1000000.,9)],$; temperature = 10000.
	 [replicate(100.,9)], $	; temperature = 20000.
	 [replicate(10000.,9)]]  	; temperature = 20000.
;
z = $	; ratios
	[[3.04,0.458,0.251,0.154,0.102,0.0709,0.0515,0.0153,0.0066], $
	 [3.00,0.460,0.253,0.155,0.102,0.0714,0.0520,0.0163,0.0083], $
	 [2.86,0.468,0.259,0.159,0.105,0.0731,0.0530,0.0156,0.0066], $
	 [2.85,0.469,0.260,0.159,0.105,0.0734,0.0533,0.0162,0.0075], $
	 [2.81,0.471,0.262,0.163,0.110,0.0786,0.0590,0.0214,0.0105], $
	 [2.75,0.475,0.264,0.163,0.107,0.0746,0.0540,0.0158,0.0066], $
	 [2.74,0.476,0.264,0.163,0.107,0.0746,0.0541,0.0161,0.0072]]
;
; get line
x = x(i,*)
y = y(i,*)
z = z(i,*)
;
; get triangles
triangulate,x,y,tr,b
;
;interpolate
surf = trigrid(x,y,z,tr,xgrid=xg,ygrid=yg,extrap=b)
;
; get value at point
res = interp2d(surf,xg,yg,[t],[n],/regular)
;
return,res
end
