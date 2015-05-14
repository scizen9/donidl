pro snadcompare,output=output,ps=ps
;
; compare SN coords from asiago and SAI
;
read_asiago_sn,asn,host,hra,hdec,ara,adec,/silent
read_sai_sn,ssn,shost,shra,shdec,sra,sdec,/silent
;
; log file
if keyword_set(output) then begin
	lfile=!SNE_DATA+'/snadcompare.log'
;
; move original file
	filestamp,lfile,/arch,/verbose
	openw,ol,lfile,/get_lun
	printf,ol,'SN       C RA--Asiago---Dec--------   RA----SAI----Dec--------  RA-Asiago-SAI-Dec--------   asec->dRA     dDec       dR    Host             RA----Host---Dec--------'
endif
;
; setups
nasne=n_elements(ara)
radif = dblarr(nasne)
dedif = dblarr(nasne)
match = intarr(nasne)
nmis  = 0L
;
; loop over asiago sne
for i=0,nasne-1 do begin
    if ara(i) ge 0. and adec(i) ge -90. then begin
	s=where(strpos(ssn,asn(i)) ge 0, ns)
	if ns eq 1 then begin
		s=s(0)
		if sra(s) ge 0. and sdec(s) ge -90. then begin
			gcircd,2,ara(i),adec(i),sra(s),sdec(s),r,dra,ddec
			radif(i) = dra
			dedif(i) = ddec
			match(i) = 1
			if keyword_set(output) then begin
			    dra=sra(s)-ara(i)
			    ddec=sdec(s)-adec(i)
			    dstr=adstrn(abs(dra),abs(ddec),1,delim=':')
			    if dra lt 0. then $
				    strput,dstr,'-',0 $
			    else    strput,dstr,'+',0
			    if ddec lt 0. then strput,dstr,'-',14
			    astr=adstrn(ara(i),adec(i),1,delim=':')
			    sstr=adstrn(sra(s),sdec(s),1,delim=':')
			    hstr=adstrn(hra(i),hdec(i),1,delim=':')
			    if r gt 2.0 then $
			    	printf,ol,asn(i),'-',astr,sstr,dstr,radif(i), $
				    dedif(i),r,host(i),hstr, $
			format='(a-8,1x,a1,a,2x,a,2x,a,2x,3f10.1,2x,a-14,2x,a)'
			endif
		endif
	endif else begin
		if keyword_set(output) then begin
			astr=adstrn(ara(i),adec(i),1,delim=':')
			printf,ol,asn(i),'-',astr,format='(a-8,1x,a1,a)'
		endif
		nmis = nmis + 1L
	endelse
    endif
endfor
if keyword_set(output) then $
	free_lun,ol
;
; plot setup
font_store=!p.font
if keyword_set(ps) then begin
	psf=!SNE_DATA+'/snadcompare'
	psfile,psf
	!p.font=1
endif
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
th=3
si=1.5
a=[findgen(40)*(!pi*2/40.),0.]
usersym,cos(a),sin(a),thick=th
!p.multi=[0,2,2]
sran=[-10.,10]
;
; get matched ones
g=where(match eq 1, ng)
;
; plot 'em
; Del RA vs Del Dec
plot,radif(g),dedif(g),xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	psym=3, $
	xtitle=textoidl('RA_{Asiago} - RA_{SAI} (arcsec)'),xran=sran,xsty=1, $
	ytitle=textoidl('Dec_{Asiago} - Dec_{SAI} (arcsec)'),yran=sran,ysty=1
;
; Del RA vs RA
plot,ara(g),radif(g),xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	psym=3, $
	ytitle=textoidl('RA_{Asiago} - RA_{SAI} (arcsec)'),yran=sran,ysty=1, $
	xtitle=textoidl('RA_{Asiago}'),xran=[-1,361],xsty=1
;
; Dec vs Del Dec
plot,dedif(g),adec(g),xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	psym=3, $
	ytitle=textoidl('Dec_{Asiago}'),yran=[-91,91],ysty=1, $
	xtitle=textoidl('Dec_{Asiago} - Dec_{SAI} (arcsec)'),xran=sran,xsty=1
;
; RA, Dec
plot,ara(g),adec(g),xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	psym=3, title=systime(0), $
	xtitle=textoidl('RA_{Asiago}'),xran=[-1,361],xsty=1, $
	ytitle=textoidl('Dec_{Asiago}'),yran=[-91,91],ysty=1
;
if !d.name eq 'PS' then $
	psclose
;
!p.font=font_store
!p.multi=0
;
return
end
