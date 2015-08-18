pro nova_rate,dmod,ntrials,trate,inputfile=inputfile,logfile=logfile
;+
;	nova_rate - calculate nova rate given frame limits and observed rate
;
; INPUTS:
;	dmod	- distance modulus of galaxy observed
;	ntrials	- number of monte carlo trials to make (> 1000)
;	trate	- observed nova rate (number of nova actually observed)
;
; KEYWORDS:
;	inputfile - a list of frame limits for each epoch with one line per
;			epoch in the following format:
;				<julianDate> <LimitingMag> <LimitingMagErr>
;	logfile - a file in which to log the results
;
;-
if n_params(0) lt 3 then begin
	print,'Usage: - nova_rate, dist_mod, ntrials, true_rate, inputfile=inputfile, logfile=logfile'
	return
endif
;
; check inputs
if keyword_set(inputfile) then $
	ifil = inputfile $
else	ifil = 'flims.dat'
;
; get field
field=''
read,'Field ID: ',field
;
readcol,ifil,ljd,lmg,lme,format='d,f,f'
ratelim=100
ljd = fix(ljd - min(ljd))
nsam = n_elements(ljd)
;
a=[findgen(40)*(!pi*2/40.),0.]
if keyword_set(logfile) then $
	do_log = ( 1 eq 1 ) $
else	do_log = ( 1 eq 0 )
;
if do_log then begin
	openw,llun,logfile,/get_lun
	printf,llun,'# Read in '+strn(nsam)+' points for field: '+field+$
		'  '+systime(0)
	printf,llun,'# Ntrials: '+strn(ntrials)+'  ObsRate: '+strn(trate)+$
		'  RateLim: '+strn(ratelim), '  Dmod: '+ $
			string(dmod,format='(f5.2)')
endif
;
nn = 500
simnovae,nn,nmgs,drs,/real
npts = 365
nmgs = nmgs + dmod
;
matches = lonarr(ratelim+1)
;
if trate eq 0 then matches(trate) = ntrials
;
rate=0
trial=ntrials
print,'Rate: ',rate,', Trial: ',trial, ', Matches: ',matches(rate), $
	format='(a,i2,a,i7,a,i7)'
if do_log then $
	printf,llun,rate,matches(rate),form='(i4,i9)'
;
past_zeros = (1 eq 0)
for rate = 1,ratelim do begin
    for trial = 1L,ntrials do begin
	pick = fix( randomu(seed,rate) * nn )
	jd = fix( randomu(seed,rate) * 365.25 )
	;
	obrate = 0L
	for n = 0,rate-1 do begin
	    days = ljd - jd(n)
	    good = where(days ge 0, ngood)
	    bad = where(days lt 0, nbad)
	    nnodets = 0L
	    if ngood gt 0 then begin
	    	omags = lmg(good) - nmgs(days(good),pick(n))
		dets = where(omags gt 0, ndets)
		nodets = where(omags le 0, nnodets)
	    endif else ndets = 0L
	    if ndets ge 2 and (nnodets ge 1 or nbad ge 1) then $
	    	obrate = obrate + 1
	endfor
	if obrate eq trate then matches(rate) = matches(rate) + 1L
	if (trial mod ((ntrials/10)<1000)) eq 0 then $
	print,string(13B),'Rate: ',rate,', Trial: ',trial, $
		', Matches: ',matches(rate), format='($,a1,a,i2,a,i7,a,i7)'
    endfor
    print,' '
    if do_log then $
    	printf,llun,rate,matches(rate),form='(i4,i9)'
    if past_zeros and matches(rate) le 0 then begin
	print,'Done.'
	break
    endif
    if not past_zeros and matches(rate) gt 5 then $
    	past_zeros = (1 eq 1)
endfor
;
;
if do_log then begin
    free_lun,llun
    pltmc,logfile
endif else begin
    matches = (matches / float(ntrials)) * 100.
    plot,matches,xtitle='Nova Rate',ytitle='Probability(%)',ythick=3,charsiz=2,$
	xthick=3,charthick=3,thick=3,title=field,xran=[-1,rate+1],xsty=1,$
	yran=[-0.5,min([max(matches)+1,100])],ysty=1
endelse
;
return
end	; pro nova_rate
