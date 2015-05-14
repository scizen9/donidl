pro scale_local_sne,mno,write_params=write_params
;
if keyword_set(write_params) then begin
; get params
	openr,1,'no_scale/Astier_param.txt'
	parms=strarr(13)
	readf,1,parms
	close,1
;
; wom fit params
        openr,1,'no_scale/Astier_wom_param.txt'
        parmswom=strarr(13)
        readf,1,parmswom
        close,1
endif
;
; get sne data
openr,1,'no_scale/Astier_sne.dat'
hdr=''
readf,1,hdr
names=strarr(115)
nam=''
data=dblarr(12,115)
dat=dblarr(12)
for i=0,114 do begin
    readf,1,nam,dat,format='(a10,12f12.6)'
    names(i)=nam
    data(*,i)=dat(*)
endfor
close,1
;
; check for model number
if n_params(0) ge 1 then $
	model = mno $
else	model = -1
;
; get scales
openr,1,'avg_scales.dat'
rec=''
d=1
while not eof(1) do begin
    readf,1,rec
    if strpos(rec,'---') ge 0 then begin
    	dir='mod'+string(d,form='(i02)')
	dfile=dir+'_sne_scale.dat'
	odata=data
    endif
    if strpos(rec,'neill') ge 0 then begin
    	type=gettok(rec,' ')
	jnk=gettok(rec,' ')
	sigv=float(gettok(rec,' '))
	jnk=gettok(rec,' ')
	pscz=gettok(rec,' ')
	jnk=gettok(rec,' ')
	beta=float(gettok(rec,' '))
	jnk=gettok(rec,' ')
	dipole=gettok(rec,' ')
    endif
    if strpos(rec,'DelH1') ge 0 then begin
    	jnk=gettok(rec,' ')
	jnk=gettok(rec,' ')
	dh1=double(gettok(rec,' '))
	moff = -5.d0 * alog10(1.d0+dh1)
	jnk=gettok(rec,' ')
	dh1er=double(gettok(rec,' '))
;
; write out model data file
	if model lt 0 or model eq d then begin
		openw,2,dir+'/'+dir+'.dat'
		printf,2,type,sigv,pscz,beta,dipole,form='(a,i5,2x,a,f6.1,2x,a)'
		printf,2,dh1,dh1er,form='(2f9.4)'
		print,'Writing: ',dir+'/'+dir+'.dat'
		close,2
	endif
;
; write params?
    	if keyword_set(write_params) then begin
		oparms=parms
		oparmswom=parmswom
;
; write out runfitter param file
		opf=dir+'/'+dir+'_omol_param_scale.txt'
		ofile=dir+'_omol_output_scale.txt'
		abfile=dir+'_omol_alpha_beta_output_scale.txt'
		exfile=dir+'_omol_extout_scale.txt'
		psfile=dir+'_omol_parsum_scale.txt'
		oparms(1) = 'DATAFILE '+dfile
		oparms(2) = 'OUTFILE '+ofile
		oparms(3) = 'ALBETAOUTFILE '+abfile
		oparms(10)= 'EXTENDEDOUTFILE '+exfile
		oparms(12)= 'PARAMSUMMARYFILE '+psfile
		if model lt 0 or model eq d then begin
			openw,2,opf
			for i=0,12 do printf,2,oparms(i)
			close,2
		endif
;
; write out runfitter param file (wom fit)
        	opf=dir+'/'+dir+'_wom_param_scale.txt'
        	ofile=dir+'_wom_output_scale.txt'
        	exfile=dir+'_wom_extout_scale.txt'
		psfile=dir+'_wom_parsum_scale.txt'
        	oparmswom(1) = 'DATAFILE '+dfile
        	oparmswom(2) = 'OUTFILE '+ofile
        	oparmswom(11)= 'EXTENDEDOUTFILE '+exfile
        	oparmswom(12)= 'PARAMSUMMARYFILE '+psfile
		if model lt 0 or model eq d then begin
        		openw,2,opf
        		for k=0,12 do printf,2,oparmswom(k)
        		close,2
		endif
		print,'Wrote param files.'
	endif
;
; re-scale local sample
	for i=0,114 do begin
	    if strmid(strtrim(names(i),2),0,1) ne '0' then begin
	    	odata(3,i)=odata(3,i)+moff
	    endif
	endfor
;
; write out scaled sne data
	if model lt 0 or model eq d then begin
		openw,2,dir+'/'+dfile
		printf,2,hdr
		for i=0,114 do printf,2,names(i),odata(*,i),form='(a10,12f12.6)'
		close,2
		print,'Wrote SN data file.'
	endif
	d = d + 1
    endif
endwhile
close,1
return
end
