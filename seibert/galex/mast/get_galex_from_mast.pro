pro get_galex_from_mast, base, path, $ 
    stagepath=stagepath, stagesubdir=stagesubdir, $
    fuv=fuv, nuv=nuv,$
    int=int, cnt=cnt, rr=rr

;base = string array of GR67 base names
;path =  string array of paths on MAST archive
; compiled in GR67_MAST_mains.fits.gz  or GR67_MAST_visits.fits.gz 
;
;stagepath = must exist
;stagesubdir = will be created uder stagepath if it does not exist


; determine band

 if keyword_set(nuv) then band ='-nd'
 if keyword_set(fuv) then band ='-fd'
 if (keyword_set(fuv) and keyword_set(nuv)) or $
    (not keyword_set(fuv) and not keyword_set(nuv))  then begin
   print,'ERROR get_Galex_mast: must specify NUV or FUV band'
   return
 endif 

; determine stageing location

 if not keyword_set(stagepath) then stagepath='./'
 if not keyword_set(stagesubdir) then stagesubdir='./'

 pcheck=file_info(stagepath)
 if not pcheck.directory then begin
   print,'ERROR get_Galex_mast: stagepath does not exists'
   return
 endif 

 dcheck=file_info(stagepath+'/'+stagesubdir)
 if not dcheck.directory then spawn,'mkdir '+stagepath+'/'+stagesubdir
 
 dcheck=file_info(stagepath+'/'+stagesubdir)
 if not dcheck.directory then begin
   print,'ERROR get_Galex_mast: unable to create '+stagepath+'/'+stagesubdir
   return
 endif 

; get the date


  ;stagedir=stagepath+'/'+strcompress(id[i],/rem)+'/'
  ;spawn,'mkdir '+stagedir

  stagedir=stagepath+'/'+stagesubdir+'/'

  ;remember current directory
  spawn,'pwd',cdir

  ;go to staging location
  cd,stagedir


  print,'Downloading data from MAST: '+$
    string(n_elements(base),format='(i4)')+' files'


  for j=0,n_elements(base)-1 do begin

   cntfile=strcompress(base[j]+band+'-cnt.fits.gz',/rem)
   rrhrfile=strcompress(base[j]+band+'-rrhr.fits.gz',/rem)
   intfile=strcompress(base[j]+band+'-int.fits.gz',/rem)
   subpath=strcompress(path[j],/rem)+'/'

   mastrrfile='http://galex.stsci.edu/data/'+$
    subpath+rrhrfile
   mastcntfile='http://galex.stsci.edu/data/'+$
    subpath+cntfile 
   mastintfile='http://galex.stsci.edu/data/'+$
    subpath+intfile 

   if not file_exist(cntfile) and keyword_set(cnt) then begin
    cmd='wget -q -t 3 '+mastcntfile
    print,cmd
    spawn,cmd,result
   endif

   if not file_exist(rrhrfile) and keyword_set(rr) then begin
    cmd='wget -q -t 3 '+mastrrfile
    print,cmd
    spawn,cmd,result
   endif

   if not file_exist(intfile) and keyword_set(int) then begin
    cmd='wget -q -t 3 '+mastintfile
    print,cmd
    ;spawn,cmd,result
   endif


 endfor  

  ;return to original directory
  cd,cdir

 return

end  
