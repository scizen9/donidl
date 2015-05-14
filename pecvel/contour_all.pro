pro contour_all,imd,wom=wom,omol=omol,scale=scale,indiv=indiv
;
pars='wom'
type='indiv'
;
if keyword_set(omol) then pars='omol'
if keyword_set(wom) then pars='wom'
if keyword_set(scale) then type='scale'
if keyword_set(indiv) then type='indiv'
;
ofil=pars+'_values_'+type+'.txt'
;
if n_params(0) lt 1 then begin
	m0=14
	m1=16
	dh1=0.
	dhe=0.
	contourshow,'mod00/combined_'+pars+'_output.txt', $
		values=vals,/trans
	get_parsum,'mod00/'+pars+'_parsum.txt',pnam,ptyp,pval
	at=where(strpos(pnam,'alpha') ge 0)
	at=at(0)
	bt=where(strpos(pnam,'beta' ) ge 0)
	bt=bt(0)
	openw,ol,ofil,/get_lun
	printf,ol,0,dh1,dhe,vals,pval(2,at),pval(3,at),pval(4,at), $
		pval(2,bt),pval(3,bt),pval(4,bt),form='(i6,2x,14f10.5)'
endif else begin
	openw,ol,ofil,/get_lun,/append
	m0=imd
	m1=imd
endelse
;
;
rec=''
for i=m0,m1 do begin
    mst='mod'+string(i,form='(i02)')
    mfile=mst+'/combined_'+pars+'_output_'+type+'.txt'
    if file_test(mfile) then begin
    	contourshow,mfile,values=vals,/trans
    	mfile=mst+'/baryon_'+pars+'_output_'+type+'.txt'
    	contourshow,mfile,/overplot,/trans
    	mfile=mst+'/'+mst+'_'+pars+'_output_'+type+'.txt'
    	contourshow,mfile,/overplot,/trans
    	p1lab = 'P1 = '+string(vals(0),form='(f6.3)')+ $
    	      ' +'+string(vals(1),form='(f5.3)')+ $
    	      ' -'+string(vals(2),form='(f5.3)')
    	p2lab = 'P2 = '+string(vals(3),form='(f6.3)')+ $
    	      ' +'+string(vals(4),form='(f5.3)')+ $
    	      ' -'+string(vals(5),form='(f5.3)')
    	legend,[mst,type,p1lab,p2lab],box=0,/right
;
;    if n_params(0) lt 1 then begin
    	dfile='mod'+string(i,form='(i02)')+'/mod'+string(i,form='(i02)')+'.dat'
    	openr,il,dfile,/get_lun
    	readf,il,rec
    	readf,il,dh1,dhe
    	free_lun,il
	get_parsum,mst+'/'+mst+'_'+pars+'_parsum_'+type+'.txt',pnam,ptyp,pval
	at=where(strpos(pnam,'alpha') ge 0)
	at=at(0)
	bt=where(strpos(pnam,'beta' ) ge 0)
	bt=bt(0)
	printf,ol,i,dh1,dhe,vals,pval(2,at),pval(3,at),pval(4,at), $
		pval(2,bt),pval(3,bt),pval(4,bt),form='(i6,2x,14f10.5)'
    endif
endfor
;
free_lun,ol
;
return
end
