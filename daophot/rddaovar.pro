pro rddaovar, vfile, nframes, chi, sharp, var, blunder
;
openr,vlun,vfile,/get_lun
;
rddaohdr,vlun
;
nframes = [0]
chi = [0.]
sharp = [0.]
var = [0.]
blunder = [0.]

rec=''
while not eof(vlun) do begin
	readf,vlun,rec
	for i=0,5 do junk=gettok(rec,' ')
	nframes = [nframes, fix(gettok(rec,' ')) ]
	chi = [chi, float(gettok(rec,' ')) ]
	sharp = [sharp, float(gettok(rec,' ')) ]
	var = [var, float(gettok(rec,' ')) ]
	blunder = [blunder, float(gettok(rec,' ')) ]
endwhile

nframes = nframes(1:(n_elements(nframes)-1))
chi	=     chi(1:(n_elements(chi)-1))
sharp	=   sharp(1:(n_elements(sharp)-1))
var	=     var(1:(n_elements(var)-1))
blunder = blunder(1:(n_elements(blunder)-1))

free_lun,vlun

return
end	; pro rddaovar
