pro correct_extdlocal,write_params=write_params
;
if keyword_set(write_params) then begin
; get params
	openr,1,'mod00/Astier_param.txt'
	parms=strarr(13)
	readf,1,parms
	close,1
;
; wom fit params
	openr,1,'mod00/Astier_wom_param.txt'
	parmswom=strarr(13)
	readf,1,parmswom
	close,1
endif
;
; get sne data
readsne,'mod00/Astier_sne.dat',names,data,hdr
;
; speed of light
c=299792.458d0
;
; model 01 and 02
rec=''
mno=1
i0=15
i1=16
for i=i0,i1 do begin
    dir='mod'+string(i,form='(i02)')
    print,'Writing in dir: ',dir
;
; set up outputs
    odata=data
    dfile=dir+'_sne_indiv.dat'
;
; write params?
    if keyword_set(write_params) then begin
    	oparms=parms
    	oparmswom=parmswom
;
; write out runfitter param file (omol fit)
    	opf=dir+'/'+dir+'_omol_param_indiv.txt'
    	ofile=dir+'_omol_output_indiv.txt'
    	abfile=dir+'_omol_alpha_beta_output_indiv.txt'
    	exfile=dir+'_omol_extout_indiv.txt'
	psfile=dir+'_omol_parsum_indiv.txt'
    	oparms(1) = 'DATAFILE '+dfile
    	oparms(2) = 'OUTFILE '+ofile
    	oparms(3) = 'ALBETAOUTFILE '+abfile
    	oparms(10)= 'EXTENDEDOUTFILE '+exfile
	oparms(12)= 'PARAMSUMMARYFILE '+psfile
    	openw,2,opf
    	for k=0,12 do printf,2,oparms(k)
    	close,2
;
; write out runfitter param file (wom fit)
    	opf=dir+'/'+dir+'_wom_param_indiv.txt'
    	ofile=dir+'_wom_output_indiv.txt'
    	exfile=dir+'_wom_extout_indiv.txt'
	psfile=dir+'_wom_parsum_indiv.txt'
    	oparmswom(1) = 'DATAFILE '+dfile
    	oparmswom(2) = 'OUTFILE '+ofile
    	oparmswom(11)= 'EXTENDEDOUTFILE '+exfile
	oparmswom(12)= 'PARAMSUMMARYFILE '+psfile
    	openw,2,opf
    	for k=0,12 do printf,2,oparmswom(k)
    	close,2
	print,'Wrote param files.'
    endif
;
; get new velocities
    flist=file_search(dir+'/*.clusvp1',count=nf)
    if nf eq 1 then begin
        readcol,flist(0),inam,l,b,cz,czcmb,czcor,pvel,form='a,f,f,f,f,f,f'
	loc=where(strpos(names,'sn') ge 0,nin)
	for j=0,nin-1 do begin
	    w=where(strpos(inam,names(loc(j))) ge 0, n)
	    if n eq 1 then begin
	    	z0 = odata(0,loc(j))
		mb0 = odata(3,loc(j))
	    	z1 = (1.+z0)/(1.+(pvel(w(0))/c)) - 1.
		mb1 = mb0-5.0*alog10(1.+(pvel(w(0))/c))
		odata(0,loc(j)) = z1
		odata(3,loc(j)) = mb1
	    endif else print,'Not found: ',names(loc(j))
	endfor
;
; write out corrected sne data
	openw,2,dir+'/'+dfile
	printf,2,hdr
	for k=0,n_elements(names)-1 do printf,2,names(k),odata(*,k), $
						form='(a10,9f12.6,2x,3f12.8)'
	close,2
	print,'Wrote SN data file.'
    endif else print,' No velocity data found for: ',dir
endfor
;
return
end
