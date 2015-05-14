
do_sdss=1
clean=1.
delerr=0.
do_galex=0
do_sings=0

lvl=mrdfits('../info/mk06_info.fits',1)
;ngs=mrdfits('~/SFR_FIELDS/Nearby/SINGS_DATA2/ngs_051024.fits',1)
;names=file_search()
;good=where((strpos(names,'ngc') GE 0) or strpos(nameugc)
;names=names[good]
;sn=strcompress(strlowcase(sings.name),/remove_all)

names=strcompress(strlowcase(lvl.name),/remove_all)
;names=repstr(names,'[','')
;names=repstr(names,"'",'')
;names=repstr(names,']','_')
;names=repstr(names,'messier104','ngc4594')

;mnam=['ngc3741','ugc06782','ugc07639','ugc08508'];,'kk98_230','fm2000_1','arpsloop']
;mnam=['ngc3351','ngc5340']
;mnam=['ugc08837'];,'ngc4490']
;mnam=['ngc4395']
;nmn=n_elements(mnam)
;ii=fltarr(nmn)
;for i=0,nmn-1 do ii[i]=where(names EQ mnam[i])

ii=findgen(n_elements(names))

;stop

names=names[ii]
lvl=lvl[ii]

sra=lvl.ra
sdec=lvl.dec
sz=lvl.d25_major
sz[where(sz LT 0)]=6.

ng=n_elements(names)


band=['u','g','r','i','z']
gband=['f','n']
siband=['1','2','3','4']
smband=['24','70','160']

for ig=83,ng-1 do begin
;  this=where(sn EQ names[ig])
;  ra=sings[this].ra
;  dec=sings[this].dec
  ra=sra[ig]
  dec=sdec[ig]
  size=min([sz[ig]*60.*2.5,6000])
  if file_test(names[ig]+'/sdss/fpC*fit*') then begin
    print,names[ig]

;stop
;do sdss mosaic
  if keyword_set(do_sdss) then begin
    dir='~/SFR_FIELDS/MK06/data/'+names[ig]+'/sdss/'
    spawn,'gunzip '+dir+'mimap*gz'
    for iband=0,4 do begin
      mimap_sdss_combine,dir,band[iband],names[ig]+'_'+band[iband]+'.fits',$
                         ra,dec,ps=[0.5,0.5],size=[size,size],/skytweak
    endfor
    if keyword_set(clean) then spawn,'rm '+dir+'mimap_*mms*fits'
    spawn,'gzip '+dir+'mimap*fit &'
    if keyword_set(delerr) then spawn,'rm '+dir+'mimap_*mjyErr*fits'
  endif


;do galex reproject
  if keyword_set(do_galex) then begin
    dir='~/SFR_FIELDS/Nearby/LVL/data/'+names[ig]+'/galex/'
    for iband=0,1 do begin
      mimap_galex_combine,dir,gband[iband],names[ig]+'_'+gband[iband]+'.fits',$
                          ra,dec,ps=[0.5,0.5],size=[size,size],$
                          osample='4',/cskip,rtype='BILINEAR'
    endfor
  endif

;do sings reproject
  if keyword_set(do_sings) then begin
    dir='~/SFR_FIELDS/Nearby/LVL/data/'+names[ig]+'/spitzer/IRAC/'
    for iband=0,3 do begin
      mimap_sings_combine,dir,siband[iband],$
                          names[ig]+'_irac'+siband[iband]+'.fits',$
                          ra,dec,ps=[0.5,0.5],size=[size,size],$
                          osample='4',rtype='LANCZOS3' 
    endfor

    dir='~/SFR_FIELDS/Nearby/LVL/data/'+names[ig]+'/spitzer/MIPS/'
    for iband=0,2 do begin
      mimap_sings_combine,dir,smband[iband],$
                          names[ig]+'_mips'+smband[iband]+'.fits',$
                          ra,dec,ps=[0.5,0.5],size=[size,size],$
                          osample='4',rtype='LANCZOS3',wtype='NONE'
    endfor
  endif

endif

endfor


end
