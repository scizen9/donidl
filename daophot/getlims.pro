pro getlims,x,xran,xdel,y,yran,ydel,fu1=fu1,fd1=fd1,fu2=fu2,fd2=fd2
;
xran=[min(x),max(x)]
xdel=xran(1)-xran(0)

if keyword_set(fu1) then $
	xran(1) = xran(1) + fu1*xdel
if keyword_set(fd1) then $
	xran(0) = xran(0) - fd1*xdel
xdel=xran(1)-xran(0)

if n_params(0) lt 4 then return

yran=[min(y),max(y)]
ydel=yran(1)-yran(0)

if keyword_set(fu2) then $
	yran(1) = yran(1) + fu2*ydel
if keyword_set(fd2) then $
	yran(0) = yran(0) - fd2*ydel
ydel=yran(1)-yran(0)
;
return
end
