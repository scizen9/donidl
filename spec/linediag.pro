pro linediag,fspec
;
; perform line diagnostics on each measurement file and print out a log
;
flist=file_search(fspec,count=nf)
;
;
fmt1='(a,2f9.3)'
fmt2='(a,2g11.4,f9.1)'
for i=0,nf-1 do begin
	file=flist(i)
	print,' '
	print,'Diagnosing: ',file
	readcol,file,name,c1,c2,cl,v,npx,flx,flxe,s2n, $
		format='a,f,f,f,f,i,f,f,f',/silent
	rute=gettok(file,'.')
	ext=file
;
; init variables
	lgte  = 0.
	lgtes = fltarr(10)
	lgne  = 0.
	lgnes = fltarr(10)
	wgts  = fltarr(10)
	rats  = fltarr(10)
;
;	[SII] diagnostics
	fx1=lineflx(6717.,cl,flx,flxe,s2n,c2,name,point=p1)
	fx2=lineflx(6731.,cl,flx,flxe,s2n,c2,name,point=p2)
	fx3=lineflx(4068.,cl,flx,flxe,s2n,c2,name,point=p3)
	fx4=lineflx(4076.,cl,flx,flxe,s2n,c2,name,point=p4)
	if fx1 gt 0. then begin
		if fx2 gt 0. then begin
			r1=fx1/fx2
			s1=fx1+fx2
			lgr1=alog10(r1)
			print,'6716/6731 (R1), alog10(R1): ',r1,lgr1,form=fmt1
			rats(0) = r1
		endif else begin
			fx2=0.
			s1=fx1
		endelse
		if fx3 gt 0. then begin
			r2=fx3/s1
			lgr2=alog10(r2)
			print,'4068/ S1  (R2), alog10(R2): ',r2,lgr2,form=fmt1
			read,'Enter lgT, lgNe: ',lgte,lgne
			lgtes(0) = lgte
			lgnes(0) = lgne
			if  s2n(p3) ge 3. then $
				wgts(0) = sqrt(s2n(p3)^2+s2n(p1)^2+s2n(p2)^2) $
			else	wgts(0) = 1.e-9
			rats(1) = r2
		endif else fx3=0.
		if fx4 gt 0. then begin
			r3=fx4/s1
			lgr3=alog10(r3)
			print,'4076/ S1  (R3), alog10(R3): ',r3,lgr3,form=fmt1
			read,'Enter lgT, lgNe: ',lgte,lgne
			lgtes(1) = lgte
			lgnes(1) = lgne
			if s2n(p4) ge 3. then $
				wgts(1) = sqrt(s2n(p4)^2+s2n(p1)^2+s2n(p2)^2) $
			else	wgts(1) = 1.e-9
			rats(2) = r3
		endif else fx4=0.
		s2=fx3+fx4
		if s1 gt 0. then begin
			print,'(4068+4076)/(6716+6731)  : ',(s2/s1)/1.12, $
				form=fmt1
			read,'Enter lgT, lgNe: ',lgte,lgne
			lgtes(2) = lgte
			lgnes(2) = lgne
			wgts(2) = sqrt(wgts(0)^2+wgts(1)^2)
			rats(3) = s2/s1
		endif
	endif
;
; [NII] diagnostics
	fx5=lineflx(5755.,cl,flx,flxe,s2n,c2,name,point=p5)
	fx6=lineflx(6548.,cl,flx,flxe,s2n,c2,name,point=p6)
	fx7=lineflx(6584.,cl,flx,flxe,s2n,c2,name,point=p7)
	if fx5 gt 0. then begin
		fxt=fx6+fx7
		if fxt gt 0. then begin
			r4=fx5/fxt
			print,'5754/(6548+6584)          : ',r4,form=fmt1
			read,'Enter lgT, lgNe: ',lgte,lgne
			lgtes(3) = lgte
			lgnes(3) = lgne
			if s2n(p5) ge 3. then $
				wgts(3) = sqrt(s2n(p5)^2+s2n(p6)^2+s2n(p7)^2) $
			else	wgts(3) = 1.e-9
			rats(4) = r4
		endif
	endif
;
; get averages
	g=where(wgts gt 1., ng)
	if ng ge 1 then begin
		lgte=wmean(lgtes(g),1./wgts(g))
		lgtesig=wstdev(lgtes(g),1./wgts(g))
		lgne=wmean(lgnes(g),1./wgts(g))
		lgnesig=wstdev(lgnes(g),1./wgts(g))
		print,'<lgT>, <lgNe>: ',lgte,lgne,format=fmt1
	endif else begin
		print,'No average found'
		read,'Enter lgT, lgNe: ',lgte,lgne
	endelse
;
; Balmer ratios
	fxa=lineflx(6563.,cl,flx,flxe,s2n,c2,name,point=pa)
	fxb=lineflx(4861.,cl,flx,flxe,s2n,c2,name,point=pb)
	fxg=lineflx(4340.,cl,flx,flxe,s2n,c2,name,point=pg)
	fxd=lineflx(4101.,cl,flx,flxe,s2n,c2,name,point=pd)
	fxe=lineflx(3970.,cl,flx,flxe,s2n,c2,name,point=pe)
	fx8=lineflx(3889.,cl,flx,flxe,s2n,c2,name,point=p8)
	fx9=lineflx(3835.,cl,flx,flxe,s2n,c2,name,point=p9)
	fx10=lineflx(3797.,cl,flx,flxe,s2n,c2,name,point=p10)
	if fxb gt 0. then begin
		aflx = cl*0. + 1.
		ccm_unred,cl,aflx,-1.0
		amg = 2.5*alog10(aflx)
		;
		; Ha/Hb
		hahb = hicaseb(10.^lgte,10.^lgne,3)
		a = 2.5 / (amg(pa) - amg(pb))
		ebmvha = a * alog10( (fxa/fxb) / hahb )
		print,'Ha/Hb, E(B-V)(Ha/Hb) = ',hahb,ebmvha,form=fmt1
		;
		; Hg/Hb
		if pg ge 0 then if s2n(pg) ge 3.0 then begin
			hghb = hicaseb(10.^lgte,10.^lgne,5)
			a = 2.5 / (amg(pg) - amg(pb))
			ebmvhg = a * alog10( (fxg/fxb) / hghb )
			print,'Hg/Hb, E(B-V)(Hg/Hb) = ',hghb,ebmvhg,form=fmt1
		endif
		;
		; Hd/Hb
		if pd ge 0 then if s2n(pd) ge 3.0 then begin
			hdhb = hicaseb(10.^lgte,10.^lgne,6)
			a = 2.5 / (amg(pd) - amg(pb))
			ebmvhd = a * alog10( (fxd/fxb) / hdhb )
			print,'Hd/Hb, E(B-V)(Hd/Hb) = ',hdhb,ebmvhd,form=fmt1
		endif
		;
		; He/Hb
		if pe ge 0 then if s2n(pe) ge 3.0 then begin
			hehb = hicaseb(10.^lgte,10.^lgne,7)
			a = 2.5 / (amg(pe) - amg(pb))
			ebmvhe = a * alog10( (fxe/fxb) / hehb )
			print,'He/Hb, E(B-V)(He/Hb) = ',hehb,ebmvhe,form=fmt1
		endif
		;
		; H8/Hb
		if p8 ge 0 then if s2n(p8) ge 3.0 then begin
			h8hb = hicaseb(10.^lgte,10.^lgne,8)
			a = 2.5 / (amg(p8) - amg(pb))
			ebmvh8 = a * alog10( (fx8/fxb) / h8hb )
			print,'H8/Hb, E(B-V)(H8/Hb) = ',h8hb,ebmvh8,form=fmt1
		endif
		;
		; H9/Hb
		if p9 ge 0 then if s2n(p9) ge 3.0 then begin
			h9hb = hicaseb(10.^lgte,10.^lgne,9)
			a = 2.5 / (amg(p9) - amg(pb))
			ebmvh9 = a * alog10( (fx9/fxb) / h9hb )
			print,'H9/Hb, E(B-V)(H9/Hb) = ',h9hb,ebmvh9,form=fmt1
		endif
		;
		; H10/Hb
		if p10 ge 0 then if s2n(p10) ge 3.0 then begin
			h10hb = hicaseb(10.^lgte,10.^lgne,10)
			a = 2.5 / (amg(p10) - amg(pb))
			ebmvh10 = a * alog10( (fx10/fxb) / h10hb )
			print,'H10/Hb, E(B-V)(H10/Hb) = ',h10hb,ebmvh10,form=fmt1
		endif
	endif ; Balmer ratios
;
; De-redden
    ebmv = 0.
    read,'Enter E(B-V): ',ebmv
;
; should we de-redden?
    if ebmv gt 0. then begin
	ccm_unred,cl,flx,ebmv,rflx
;
; sort
	s=sort(cl)
;
	for j=0,n_elements(flx)-1 do print,cl(s(j)),flx(s(j)),rflx(s(j)), $
		rflx(s(j))/flx(s(j)),form='(f9.1,2g14.3,f9.3)'
;
; print out
	openw,ol,rute+'.dered',/get_lun
	printf,ol,'# Line Diag: '+systime(0)
	printf,ol,'# <lgT>, <lgNe>: ',lgte,lgne,form=fmt1
	printf,ol,'# E(B-V)       : ',ebmv,form=fmt1
	printf,ol,'#     Line    Centr1    Centr2    LabCentr   V(kms)   Npx  Flux       Error        S/N'
	fmt='(a10,2x,4f10.3,i5,2g11.4,f7.1)'
	for j=0,n_elements(flx)-1 do $
		printf,ol,name(j),c1(j),c2(j),cl(j),v(j),npx(j), $
		rflx(j),flxe(j),s2n(j),form=fmt
	free_lun,ol
    endif else if strpos(ext,'dered') ge 0 then begin
	openw,ol,rute+'.diag',/get_lun
	printf,ol,'# Line Diag: '+systime(0)
	printf,ol,'# <lgT>, <lgNe>: ',lgte,lgne,form=fmt1
	printf,ol,'# 6713/6731              : ',rats(0),format=fmt1
	printf,ol,'# 4068/(6717+6731)       : ',rats(1),format=fmt1
	printf,ol,'# lgT, lgNe              : ',lgtes(0),lgnes(0),format=fmt1
	printf,ol,'# 4076/(6717+6731)       : ',rats(2),format=fmt1
	printf,ol,'# lgT, lgNe              : ',lgtes(1),lgnes(1),format=fmt1
	printf,ol,'# (4068+4076)/(6717+6731): ',rats(3),format=fmt1
	printf,ol,'# lgT, lgNe              : ',lgtes(2),lgnes(2),format=fmt1
	printf,ol,'# 5755/(6548+6583)       : ',rats(4),format=fmt1
	printf,ol,'# lgt, lgNe              : ',lgtes(3),lgnes(3),format=fmt1
	free_lun,ol
    endif
endfor
;
return
end
