pro readsne,file,names,data,hdr
;
maxsn=1000
names=strarr(maxsn)
data=dblarr(12,maxsn)
;
openr,il,file,/get_lun
hdr=''
readf,il,hdr
print,hdr
rec=''
p=0
while not eof(il) and p lt maxsn do begin
	readf,il,rec
	names(p) = gettok(rec,' ')
	for i=0,11 do $
		data(i,p) = double(gettok(rec,' '))
	p = p + 1
endwhile
free_lun,il
;
names=names(0:(p-1))
data=data(*,0:(p-1))
;
return
end
