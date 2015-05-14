pro rddaomch, file, filt, imfiles, offsets, ph, phe
;
openr,mlun,file,/get_lun
;
rec=''
imfiles = ['']
offsets = fltarr(2,200)
nrec = 0
ph=[0.]
phe=[0.]
while not eof(mlun) do begin
	readf,mlun,rec
	junk = gettok(rec,"'")
	imfiles = [imfiles, gettok(rec,'.')]
	junk = gettok(rec,"'")
	offsets(0,nrec) = float(gettok(rec,' '))
	offsets(1,nrec) = float(gettok(rec,' '))
	for i=0,3 do junk = gettok(rec,' ')
	ph=[ph,float(gettok(rec,' '))]
	phe=[phe,float(gettok(rec,' '))]
	nrec = nrec + 1
endwhile

imfiles = imfiles[1:(n_elements(imfiles)-1)]
filt = strupcase(strmid(imfiles(0),0,1))
offsets = offsets(*,0:nrec-1)
ph=ph(1:*)
phe=phe(1:*)
;
free_lun,mlun

return
end	; pro rddaomch
