function get_index, space, nx, ny, ix, iy
;+
;
;-
lim = space - 1
radius = space / 2 - 1
;
x0 = max([0,ix-radius])
x1 = min([(nx-1), x0+lim])
y0 = max([0,iy-radius])
y1 = min([(ny-1), y0+lim])
if x1 eq (nx-1) then x0 = nx - space
if y1 eq (ny-1) then y0 = ny - space
iix = (x1 - x0) / 2 + x0
iiy = (y1 - y0) / 2 + y0
res = [x0,x1,y0,y1,iix,iiy]
;
return,res
end
