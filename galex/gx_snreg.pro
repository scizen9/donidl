pro gx_snreg,odir,host,hra,hdec,sn,snra,sndec,htyp
;
; output ds9 reg file
;
tsn=sn
ssn=gettok(tsn,'?')
dfile=odir+ssn+'.reg'
openw,ol,dfile,/get_lun
;
; SN
printf,ol,'fk5;circle('+snra+','+sndec+',10.0") # text={'+sn+ $
	'} color=red font="helvetica 18 bold" width=4'
;
; host
lab = host+','+htyp
printf,ol,'fk5;text('+hra+','+hdec+') # text={'+lab+ $
	'} color=yellow font="helvetica 18 bold"'
;
free_lun,ol
;
return
end
