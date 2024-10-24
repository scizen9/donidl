pro crplt,prefix,kset,ps=ps,low_muon_win=low_muon_win,high_muon_win=high_muon_win
;+
; crplt - plot cosmic ray histograms
;-
;
if n_params(0) lt 2 then kset = 1
;
; keywords
if keyword_set(low_muon_win) then $
    mw0 = low_muon_win $
else $
    mw0 = 0.5
if keyword_set(high_muon_win) then $
    mw1 = high_muon_win $
else $
    mw1 = 0.6
;
ifl = prefix+strn(kset)+'/master.cat'
readcol,ifl,num,x,y,arm,brm,theta,format='l,f,f,f,f', $
	comment='#',/silent
use = intarr(n_elements(brm))
bad = where(brm eq 0.289 or y eq 1.0, nbad)
if nbad gt 0 then use[bad] = 1
good = where(use eq 0)
brm = brm[good]
;
laboff = 0.07
if keyword_set(ps) then begin
	psfile,'crplt_'+strn(kset)
	!p.font=0
	laboff = 0.085
endif
;
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
th=5
si=1.7
bs=0.02
;
plothist,brm,bin=bs,peak=1.,xran=[0.,2.0],/xs,xtitle='RMS!Db!N(px)', $
	yran=[-0.05,1.05],/ys,ytitle='N(norm)',xthick=th,ythick=th, $
	charsi=si,charthi=th, title=prefix+strn(kset)
oplot,[mw0,mw0],[0,!y.crange[1]],linesty=2
oplot,[mw1,mw1],[0,!y.crange[1]],linesty=2
oplot,!x.crange,[0,0],linesty=5
xyouts,mw0+laboff,0.05,'muons',ori=90,charsi=3.0,charthi=th
xyouts,1.20,0.3,'worms',charsi=3.0,charthi=th
;
if keyword_set(ps) then psclose
;
return
end
