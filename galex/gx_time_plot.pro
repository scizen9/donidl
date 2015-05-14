pro gx_time_plot,yr,dy0,ndy,tarr,verbose=verbose
;+
;	gx_time_plot	- read in a galex timing file and plot deltas
;
; INPUTS:
;	yr	- year (YYYY)
;	dy0	- beginning day of year
;	ndy	- number of days
;
; KEYWORDS:
;	verbose	- print status as we read
;
; OUTPUTS:
;	None
;
; HISTORY:
;	written by jdn 14-JAN-2010
;-
fspec = '/Users/neill/ref/calib/galex/timing/time_correction*'+ $
	string(yr,form='(i4)')+'*.log'
flist = file_search(fspec,count=nf)
if nf le 0 then begin
	print,'No files found: ',fspec
	return
endif
;
; loop and get correct file
i=0
ifil = flist[i]
for i=1,nf-1 do begin
	dyn = fix(strmid(flist[i],strpos(flist[i],'.')+1,3))
	if dyn gt dy0 then break
	ifil = flist[i]
endfor
;
print,'Reading days: ',dy0,' - ',dy0+ndy
print,'Reading file: ',ifil
openr,il,ifil,/get_lun
;
rec=''
tarr = [[0.d,0.d,0.d,0.d]]
srch = string(yr,form='(i4)')+':'+string(dy0,form='(i03)')
start = (0 eq 1)
while not eof(il) do begin
	readf,il,rec
	if strpos(rec,'timestamp') lt 0 then begin
		day = fix(strmid(rec,5,3))
		if day gt dy0 + ndy then break
		if day ge dy0 then begin
			tarr = [[tarr],[gx_time_parse(rec)]]
			if keyword_set(verbose) then $
				print,string(13B),rec,format='($,a1,a-75)'
		endif
	endif
endwhile
;
print,' '
print,'Done reading.'
free_lun,il
;
; trim
tarr = tarr[*,1:*]
;
return
end

