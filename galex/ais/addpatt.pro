pro addpatt,file=file

;echo "addpatt,file='chains-315.dat'" | idl

if not(keyword_set(file)) then file='chains.dat'

ofile=strmid(file,0,strlen(file)-4)

grid=mrdfits('/Users/neill/galex/ais_chains/data/gridv2.fits',1,/silent)
; ADD gnum which is not in file!
grid.gnum=findgen(n_elements(grid))

readcol,file,chnum,gnum,form='f,f',/silent
chptr=where(chnum eq 0)
nchain=n_elements(chptr)
chptr=[chptr,n_elements(chnum)]

print, 'Printing to '+ofile+'.tdb/pdb'
openw,1,ofile+'.tdb'

planid=19000

openw,2,ofile+'.pdb'

for i=0,nchain-1 do begin
    chlen=chptr[i+1]-chptr[i]
    chnarr=gnum[chptr[i]:chptr[i+1]-1]

    skygrid=string(grid[chnarr[0]].skygrid,format='(I3.3)')

    targid='AISCHV6_'+skygrid+'_'+string(chnarr[0],format='(I5.5)')

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

    printf,1,targid+' -360 Dither '+string(rdtarr[*,0],format='(3F11.6)')+'  0.00000 0.00000'
    for j=0,chlen-1 do begin
        durcalc=string(fix(180+j*(2000.0/float(chlen))),format='(I5)')
        printf,1,targid+' '+durcalc+' Dither '+string(rdtarr[*,j],format='(3F12.6)')+'  0.00000 0.00000'
    endfor

    minsecs=string(fix(chlen*140.0),format='(I4)')
    printf,2,strtrim(string(planid,format='(I7)')+' '+targid+' AIS G2 2453644.50 2455936.50 '+minsecs+' 0 1.00 1 0 7 9999999.0 0.00 999999.0 999999.0',2)
    print,string(planid,format='(I7)')+' '+targid
    planid=planid+1

    
endfor

close,1,2

end

