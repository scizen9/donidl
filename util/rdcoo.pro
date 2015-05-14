pro rdcoo,ifile,xout,yout,idout,magout,merout
;
; check for *.mag file
if strpos(ifile,'mag') gt 0 then $
	do_mags = (1 eq 1) $
else	do_mags = (1 eq 0)
;
openr,ilun,ifile,/get_lun
;
xs = [0.]
ys = xs
ids = [0]
mags = xs
merrs = xs
;
x=0.
y=0.
c3=0.
c4=0.
n=0L
;
while not eof(ilun) do begin
	readf,ilun,x,y,c3,c4
	xs = [xs,x]
	ys = [ys,y]
	if do_mags then begin
		ids = [ids, n+1]
		mags = [mags, c3]
		merrs = [merrs, c4]
	endif else begin
		ids = [ids,fix(c3)]
		mags = [mags,c4]
		merrs = [merrs, 0.]
	endelse
	n = n + 1L
endwhile
free_lun,ilun
;
xout = xs(1:n)
yout = ys(1:n)
idout = ids(1:n)
magout = mags(1:n)
merout = merrs(1:n)
;
return
end	; pro rdcoo
