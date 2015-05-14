function get_grid, space, nx, ny, ng
;+
;	get_grid - return spacing matrix for even gridding of image with
;		dimensions of nx, ny
;
; INPUTS:
;	space - spacing interval, integer
;	nx,ny - dimensions of image, integers
;
; OUTPUTS:
;	ng - number of grid points, integer
;
; RETURNS:
;	intarr(6,ng) containing x0, x1, y0, y1 boundaries of subimage grids
;	plus x,y position of center of grid
;-
radius = space / 2 - 1
step = fix(space * 0.8 + 0.5)	; 20% overlap
;
res = intarr(6)
; initial values
ix = radius
iy = radius
repeat begin
    repeat begin
    	grd = get_index(space, nx, ny, ix, iy)
	res = [[res],[grd]]
	ix = ix + step
    endrep until grd(1) eq nx-1
    ix = radius
    iy = iy + step
endrep until grd(3) eq ny-1
ng = n_elements(res)/6 - 1
res = res(*,1:ng)
;
return,res
end
