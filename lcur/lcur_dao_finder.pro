pro lcur_dao_finder
;
COMMON dao_data
COMMON dao_imdata
;
pfile = lcur_com_get('pfile')
root = gettok(pfile,'.')
ch=strmid(root,strpos(root,'c')+1,1)
ich=fix(ch)
;
s=size(dao_im)
nx=s(1)
ny=s(2)
;
ofilts = ['u','b','v']
nofilts=n_elements(ofilts)
omags = fltarr(nofilts)
omerrs= fltarr(nofilts)
for i=0,nofilts-1 do begin
	file = strmid(root,0,6)+ofilts(i)+'.dat'
	test = file_search(file,count=gotit)
	if gotit eq 1 then begin
		openr,lun,file,/get_lun
		ndates=0L & nstars = 0L
		readu,lun,ndates,nstars
		jd = dblarr(ndates)
		readu,lun,jd
		id = lonarr(nstars)
		x  = fltarr(nstars)
		y  = fltarr(nstars)
		mags = fltarr(nstars)
		errs = fltarr(nstars)
		readu,lun,id,x,y,mags,errs
		free_lun,lun
		;
		t = where(id eq dao_cid, nf)
		if nf eq 1 then begin
			omags(i) = mags(t(0))
			omerrs(i) = errs(t(0))
		endif else begin
			omags(i) = 99.9
			omerrs(i) = 99.9
		endelse
	endif else begin
		omags(i) = 99.9
		omerrs(i) = 99.9
	endelse
endfor
;
chip = fix(strmid(root,5,1))
field= fix(strmid(root,3,1))
;
ijd = lcur_com_twin('jd')
img = lcur_com_twin('mags')
ime = lcur_com_twin('merrs')
phs = lcur_com_twin('phase')
erl = lcur_com_get('merrlim')
flt = strupcase(lcur_com_get('filt'))
;
; get good points
good = where(ime le erl and img lt 30.0, ngood)
if ngood lt 2 then begin
	print,'LCUR_DAO_ADD: ERROR, error limit too strict; only ', $
		strn(ngood), ' points left.'
	return
endif
ijd = ijd(good)
img = img(good)
ime = ime(good)
phs = phs(good)
;
mmg=mean(img)
delmg=max(img)-min(img)
print,'Imag = ', mmg, '  Delmag = ',delmg,form='(a,f5.2,a,f5.2)'
;
; U-B
if omags(0) lt 30.0 and omags(1) lt 30.0 then begin
	umb = omags(0) - omags(1)
	umberr = sqrt(omerrs(0)^2 + omerrs(1)^2)
endif else begin
	umb = 99.9
	umberr = 99.9
endelse
;
; B-V
if omags(1) lt 30.0 and omags(2) lt 30.0 then begin
	bmv = omags(1) - omags(2)
	bmverr = sqrt(omerrs(1)^2 + omerrs(2)^2)
endif else begin
	bmv = 99.9
	bmverr = 99.9
endelse
;
; V-I
if omags(2) lt 30.0 then begin
	vmi = omags(2) - mmg
	vmierr = sqrt(omerrs(2)^2 + delmg^2)
endif else begin
	vmi = 99.9
	vmierr = 99.9
endelse
print,'U-B=',umb,'+-',umberr,', B-V=',bmv,'+-',bmverr,', V-I=',vmi,'+-',vmierr,$
	form='(6(a,f5.2))'
;
qual = 0
note = ''
;
read,'Classification Quality, 1 (best) - 4 (worst), 9 (bad): ',qual
if qual lt 1 then return
qual = qual<9
;
read,'Var Type: ',note
;
q=''
do_phase = ( 1 eq 0 )
if qual le 4 then begin
	read,'Plot Phase diagram? (Y/n): ',q
	if strlowcase(strtrim(q,2)) ne 'n' then do_phase = ( 1 eq 1 )
endif
;

if do_phase then $
	per = lcur_com_get('period') $
else	per = 9.99999

switch ich of
	0:
	1:
	2:
	3: begin
		xx=dao_cy
		yy=dao_cx
		break
	end
	4:
	5:
	6:
	7: begin
		xx=ny-dao_cy+1.0
		yy=dao_cx
		break
	end
	else: print,'Error - unknown chip number: ',ich
endswitch
;
spawn,'xy2sky ref'+ch+'.fits '+strn(xx)+' '+strn(yy),coostr
rastr  = gettok(coostr,' ')
decstr = gettok(coostr,' ')
;print,'RA:  ',rastr
;print,'DEC: ',decstr

strid = 'f'+strn(field)+'c'+strn(chip)

openw,olun,root+'.cnd',/get_lun,/append
;
fmt='(a4, i6, 2f8.2, 1x, a12, 1x, a12, f6.1, f6.2, 6f6.1, f7.2, i3, f10.5,a1,a)'
printf,olun,strid,dao_cid,dao_cx,dao_cy,rastr,decstr,mmg,delmg, $
	umb,umberr,bmv,bmverr,vmi,vmierr,dao_cvaro,qual,per,' ',note, form=fmt
print,strid,dao_cid,dao_cx,dao_cy,rastr,decstr,mmg,delmg, $
	umb,umberr,bmv,bmverr,vmi,vmierr,dao_cvaro,qual,per,' ',note, form=fmt
;
free_lun,olun
;
; get out if bad
if qual gt 4 then begin
	print,'Done.'
	return
endif
;
; print light curve data
zd = 2400000.d0
t = where(phs eq min(phs))
jd0 = ijd(t(0)) + zd - (10.d0 * per)
openw,olun,'fc_'+strtrim(dao_cid,2)+'.dat',/get_lun
printf,olun,'# LCUR data written on '+systime(0)
printf,olun,'# Data from MDM 2.4m 8k Mosaic Camera'
printf,olun,'# Filter : ',flt
printf,olun,'# Field  : ',root
printf,olun,'# Star ID: ',dao_cid,form='(a,i7)'
printf,olun,'# Quality: ',qual,form='(a,i7)'
printf,olun,'# Type   : ',note
printf,olun,'# JD0    : ',jd0,form='(a,f13.5)'
printf,olun,'# Period : ',per
printf,olun,'# PeakMag: ',min(img)
printf,olun,'# Del Mag: ',delmg
printf,olun,'# U - B  : ',umb,' +- ',umberr,form='(a,f6.1,a,f6.1)'
printf,olun,'# B - V  : ',bmv,' +- ',bmverr,form='(a,f6.1,a,f6.1)'
printf,olun,'# V - I  : ',vmi,' +- ',vmierr,form='(a,f6.1,a,f6.1)'
printf,olun,'# Dao Var: ',dao_cvaro
printf,olun,'# X, Y   : ',dao_cx,dao_cy
printf,olun,'# RAJ2000: ',rastr
printf,olun,'# Dec    : ',decstr
printf,olun,'#'
printf,olun,'# JD              MAG      WGT'
wgt = 1./ime^2 * min(ime)^2 * 10.
for i=0,n_elements(ijd)-1 do printf,olun,ijd(i)+zd,img(i),wgt(i), $
	format='(f15.5,f9.3,f9.3)'
free_lun,olun
;
; make hardcopy
print,'Making finder chart...'
psfile,'fc_'+strtrim(dao_cid,2)
;lcur_dao_zoom,chip,dao_cid,pa,mmg,qual,coostr,note,root,/finder1
;
; get box size
bx = 800
mag = 1
bx1 = bx - 1
hbx = bx / 2
hbx1 = hbx - 1
;
; get x,y position in image
x=fix(dao_cx-.5)
y=fix(dao_cy-.5)
;
; get subim limits
x0=max([0,min([x-hbx1,nx-bx])])
x1=x0+bx1 < (nx-1)
y0=max([0,min([y-hbx1,ny-bx])])
y1=y0+bx1 < (ny-1)
;
; extract subim and get stats
sim=dao_im(x0:x1,y0:y1)
ims,sim,mn,sg
;
; check offsets so we center star in window
xoff = x0 - (x-hbx1)
yoff = y0 - (y-hbx1)
if xoff ge 0 then begin
	xd0 = xoff
	xs0 = 0
	xs1 = bx1 - xoff < (nx-1)
endif else begin
	xd0 = 0
	xs0 = -xoff
	xs1 = bx1 < (nx-1)
endelse
xsz = xs1 - xs0
xd1 = xd0 + xsz
if yoff ge 0 then begin
	yd0 = yoff
	ys0 = 0
	ys1 = bx1 - yoff < (ny-1)
endif else begin
	yd0 = 0
	ys0 = -yoff
	ys1 = bx1 < (ny-1)
endelse
ysz = ys1 - ys0
yd1 = yd0 + ysz
;
; create display sub im making sure we center star in subimage
dsim = replicate(0.0,bx,bx)
dsim(xd0:xd1,yd0:yd1) = sim(xs0:xs1,ys0:ys1)
;
; display subim
if chip le 3 then $
	dsim = transpose(dsim) $
else	dsim = rotate(dsim,1)
dsim(0,0) = mn-3*sg
dsim(1,0) = mn+9*sg
osim = replicate(mn+9*sg,800,800)
osim(0:(bx-1),0:(bx-1)) = dsim(*,*)
tvscl,osim>(mn-3*sg)<(mn+9*sg)
rr = 50.
pscircle, 399., 399., rr, 800., 800., color=120
xscl = 1000.0 * 17.78 / 24.13
yscl = 1000.0
xx1 = (399. + rr)*xscl
xx2 = (399. + rr*0.25)*xscl 
yy1 = 399.*yscl
yy2 = 399.*yscl
oplot,[xx1,xx2],[yy1,yy2],color=180,thick=3
xx1 = (399. - rr)*xscl
xx2 = (399. - rr*0.25)*xscl 
yy1 = 399.*yscl
yy2 = 399.*yscl
oplot,[xx1,xx2],[yy1,yy2],color=180,thick=3
yy1 = (399. + rr)*yscl
yy2 = (399. + rr*0.25)*yscl 
xx1 = 399.*xscl
xx2 = 399.*xscl
oplot,[xx1,xx2],[yy1,yy2],color=180,thick=3
yy1 = (399. - rr)*yscl
yy2 = (399. - rr*0.25)*yscl 
xx1 = 399.*xscl
xx2 = 399.*xscl
oplot,[xx1,xx2],[yy1,yy2],color=180,thick=3
;
if dao_cvar gt 100. then $
	cvar = dao_cvar - 100. $
else	cvar = dao_cvar
;
xyouts,600000,775000,root,charsize=2,color=0,charthick=3
xyouts,600000,725000,'ID = '+string(dao_cid,form='(i6)'),$
	charsize=2,color=0,charthick=3
xyouts,600000,675000,'IMAG = '+string(mmg,form='(f5.1)'),$
	charsize=2,color=0,charthick=3
xyouts,600000,625000,'DMAG = '+string(delmg,form='(f5.2)'),$
	charsize=2,color=0,charthick=3
xyouts,600000,575000,'VAR = '+string(cvar,form='(f4.1)'), $
	charsize=2,color=0,charthick=3
xyouts,600000,525000,'<V>-<I> = '+string(vmi,form='(f5.1)'),$
	charsize=2,color=0,charthick=3
xyouts,600000,475000,'<B>-<V> = '+string(bmv,form='(f5.1)'),$
	charsize=2,color=0,charthick=3
xyouts,600000,425000,'<U>-<B> = '+string(umb,form='(f5.1)'),$
	charsize=2,color=0,charthick=3
xyouts,600000,375000,'TYPE = '+note,charsize=2,color=0,charthick=3
xyouts,600000,325000,'QUAL = '+string(qual,form='(i3)'), $
	charsize=2,color=0,charthick=3
;
xyouts,600000,225000,'FOV = 144"',charsize=2,color=0,charthick=3
xyouts,600000,175000,'X,Y = '+string(fix(dao_cx+0.5),form='(i4)')+','+ $
	string(fix(dao_cy+0.5),form='(i4)'),charsize=2,color=0,charthick=3
xyouts,600000,125000,'RA,DEC (J2000)',charsize=2,color=0,charthick=3
xyouts,600000,075000,rastr, charsize=2,color=0,charthick=3
xyouts,600000,025000,decstr, charsize=2,color=0,charthick=3
;
xyouts,646000,-55000,root+':'+string(dao_cid,form='(i6)'), $
	charsize=2,color=0,charthick=3
xyouts,5000,825000,systime(0),charsize=2,color=0,charthick=3
;
; plot phase?
if do_phase then begin
	per = lcur_com_get('period')
	xyouts,600000,275000,'PER = '+string(per,form='(f8.4)')+'d', $
		charsize=2,color=0,charthick=3
endif

;
; plot mags and phase diagram
if do_phase then begin
	lcur_plt_mags,finder=10
	lcur_plt_phase,/finder,/mags
endif else $
	lcur_plt_mags,finder=11

;
psclose
print,'Done.'
!p.background=colordex('white')
!p.color=colordex('black')
;
return
end	; pro lcur_dao_finder
