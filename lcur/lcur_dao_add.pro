pro lcur_dao_add
;
COMMON dao_data
;
pfile = lcur_com_get('pfile')
root = gettok(pfile,'.')
;
chip = fix(strmid(root,5,1))
;
img = lcur_com_twin('mags')
ime = lcur_com_twin('merrs')
erl = lcur_com_get('merrlim')
;
; get good points
good = where(ime le erl and img lt 30.0, ngood)
if ngood lt 2 then begin
	print,'LCUR_DAO_ADD: ERROR, error limit too strict; only ', $
		strn(ngood), ' points left.'
	return
endif
img = img(good)
;
vmean=mean(img)
;
qual = 0
note = ''
;
read,'CV quality, 1 (best) - 4 (worst): ',qual
if qual lt 1 then return
qual = qual<4
;
openw,olun,root+'.cnd',/get_lun,/append
;
read,'Note: ',note
;
coostr=''
read,'ra dec: ',coostr
;
wset,2
;
print,'mark ref star'
cursor,xx,yy,/device
mycircle,xx,yy,15,512,512
rx = (xx - 256.) / 4. + dao_cx
ry = (yy - 256.) / 4. + dao_cy
;
hyp = sqrt( (rx - dao_cx)^2 + (ry - dao_cy)^2 )
adj = rx - dao_cx
if chip le 3 then $
	opp = dao_cy - ry $
else	opp = ry - dao_cy
pa = asin(opp/hyp) * 180.d0 / !DPI
if opp lt 0. and adj gt 0. then pa = 180.d0 + pa
if opp lt 0. and adj lt 0. then pa = abs(pa)
if opp gt 0. and adj lt 0. then pa = 180.d0 - pa
;
print,'RF x,y, PA: ',rx,ry,pa
wset,0
;

printf,olun,dao_cid,vmean,dao_cvar,pa,qual, $
	'  ',coostr, '  ',note, form='(i6, 3f9.3, i4, a,a,a, a)'
print,dao_cid,vmean,dao_cvar,pa,qual, $
	'  ',coostr, '  ',note, form='(i6, 3f9.3, i4, a,a,a, a)'
;
free_lun,olun
;
; make hardcopy
print,'Making finder chart...'
psfile,'fc_'+strtrim(dao_cid,2)
lcur_dao_zoom,chip,dao_cid,pa,vmean,qual,coostr,note,root,/finder1
psclose
print,'Done.'
;
return
end	; pro lcur_data_add
