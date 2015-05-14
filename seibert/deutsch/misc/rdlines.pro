pro rdlines,file,wave,names,offsets
;
; read line lists
;
close,1
openr,1,file
st=''
n=0
wave=fltarr(200)
names=strarr(200)
offsets=fltarr(2,200)
while not eof(1) do begin
  readf,1,st
  if ((fix(byte(strmid(st,0,1))))(0) ne 9) then names(n)=gettok(st,' ')
  wave(n)=gettok(st,' ')
  if st ne '' then offsets(0,n)=gettok(st,' ')
  if st ne '' then offsets(1,n)=st else offsets(1,n)=offsets(0,n)
  n = n+1
  end
wave=wave(0:n-1)
names=names(0:n-1)
offsets=offsets(*,0:n-1)
close,1
return
end
