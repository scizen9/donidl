pro get_parsum,ifile,pnam,ptyp,pval
;
; initialize
maxn=10
pnam=strarr(maxn)
ptyp=strarr(maxn)
pval=dblarr(5,maxn)
p=-1
;
; get params 
openr,il,ifile,/get_lun
rec=''
while not eof(il) do begin
	readf,il,rec
	if strpos(rec,'Parameter:') ge 0 then begin
		p=p+1
		for i=0,1 do val=gettok(rec,' ')
		pnam(p)=val
	endif
	if strpos(rec,'Fit type:') ge 0 then begin
		for i=0,2 do val=gettok(rec,' ')
		ptyp(p)=val
	endif
	if strpos(rec,'Value:') ge 0 then begin
		for i=0,1 do val=gettok(rec,' ')
		pval(0,p)=double(val)
	endif
	if strpos(rec,'Most likely value:') ge 0 then begin
		for i=0,3 do val=gettok(rec,' ')
		pval(0,p)=double(val)
	endif
	if strpos(rec,'1D marginalized peak:') ge 0 then begin
		for i=0,3 do val=gettok(rec,' ')
		pval(1,p)=double(val)
	endif
	if strpos(rec,'1D marginalized expectation value:') ge 0 then begin
		for i=0,4 do val=gettok(rec,' ')
		pval(2,p)=double(val)
		for i=3,4 do begin
			val=gettok(rec,' ')
			pval(i,p)=double(val)
		endfor
	endif
endwhile
free_lun,il
;
pnam=pnam(0:p)
ptyp=ptyp(0:p)
pval=pval(*,0:p)
;
return
end
