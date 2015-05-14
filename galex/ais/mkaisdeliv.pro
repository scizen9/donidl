pro mkaisdeliv,startpid=startpid

; delivery path
dpath = './deliv/'
readcol,dpath+'chains.list',clist,form='a',/silent
ncf = n_elements(clist)

ver = ''
read,'AIS Version String: ',ver
ver = strtrim(ver,2)
;
; get starting plan id
if keyword_set(startpid) then begin
	planid = startpid
endif else begin
	plist = file_search(dpath+'*.pdb',count=nf)
	planid = 0L
	if nf gt 0 then begin
		for i=0,nf-1 do begin
		readcol,plist[i],pids,form='l',/silent
			if planid lt max(pids) then planid = max(pids) + 1L
		endfor
	endif else read,'Starting Plan ID: ',planid
endelse
;
print,'Starting Plan ID: ',planid

orute=dpath+'chains-'+ver

grid=mrdfits('/Users/neill/galex/ais_chains/data/gridv2.fits',1,/silent)
; ADD gnum which is not in file!
grid.gnum=findgen(n_elements(grid))

print,'Reading ',ncf,' chain files: '
; read chain files
readcol,clist[0],chnum,gnum,form='i,l',/silent
print,1,' ',clist[0]
if ncf gt 1 then begin
	for i=1,ncf-1 do begin
		print,i+1,' ',clist[i]
		readcol,clist[i],ch,gn,form='i,l',/silent
		chnum = [chnum,ch]
		gnum  = [gnum, gn]
	endfor
endif
print,' '

chptr=where(chnum eq 0)
nchain=n_elements(chptr)
chptr=[chptr,n_elements(chnum)]

print, 'Printing to '+orute+'.tdb/pdb'

tfil = orute + '.tdb'
filestamp,tfil
openw,tl,tfil,/get_lun


pfil = orute + '.pdb'
filestamp,pfil
openw,pl,pfil,/get_lun

for i=0,nchain-1 do begin
    chlen=chptr[i+1]-chptr[i]
    chnarr=gnum[chptr[i]:chptr[i+1]-1]

    skygrid=string(grid[chnarr[0]].skygrid,format='(I3.3)')

    targid='AIS'+ver+'_'+skygrid+'_'+string(chnarr[0],format='(I5.5)')

    g=where(chnarr(0) eq grid.gnum)
    q1=rdt2q([grid[g[0]].ra,grid[g[0]].dec,0.0d])

    qarr=dblarr(4,chlen)
    rdtarr=dblarr(3,chlen)
    qarr(*,0)=q1

    rdtarr[*,0]=q2rdt(qarr[*,0])

    for j=1,chlen-1 do begin
        g=where(chnarr(j) eq grid.gnum)
        q2zero=rdt2q([grid[g[0]].ra,grid[g[0]].dec,0.0])
        qdiff=qmult(qnorm(qconj(qarr[*,j-1])),qnorm(q2zero))
        qdiff[2] = 0.0d
        qdelta=qnorm(qdiff)
        qarr[*,j]=qmult(qnorm(qarr[*,j-1]),qdelta)
;        print,grid[g[0]].ra,grid[g[0]].dec,q2rdt(qarr[*,j])
        rdtarr[*,j]=q2rdt(qarr[*,j])
    endfor

    g=where(rdtarr[0,*] lt 0.0)
    if g[0] ne -1 then begin
        rdtarr[0,g]=rdtarr[0,g]+360.0
    endif

    printf,tl,targid+' -360 Dither '+string(rdtarr[*,0],format='(3F11.6)')+'  0.00000 0.00000'
    for j=0,chlen-1 do begin
        durcalc=string(fix(180+j*(2000.0/float(chlen))),format='(I5)')
        printf,tl,targid+' '+durcalc+' Dither '+string(rdtarr[*,j],format='(3F12.6)')+'  0.00000 0.00000'
    endfor

    minsecs=string(fix(chlen*140.0),format='(I4)')
    printf,pl,strtrim(string(planid,format='(I7)')+' '+targid+' AIS G2 2453644.50 2455936.50 '+minsecs+' 0 1.00 1 0 7 9999999.0 0.00 999999.0 999999.0',2)
    print,string(planid,format='(I7)')+' '+targid
    planid=planid+1

    
endfor

free_lun,tl,pl

end
