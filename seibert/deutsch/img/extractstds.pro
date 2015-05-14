pro extractstds,srchspec,filter

  ROOTNAME='CATALOG' & AIRMASS=1.000
  lin=''

  FILTERS=strupcase(filter)
  UBVRInms=['U','B','V','R','I'] & filti=-1
  for i=0,n_elements(UBVRInms)-1 do if (FILTERS eq UBVRInms(i)) then filti=i
  if (filti eq -1) then begin
    print,'Filter '+FILTERS+' not found'
    return
  endif

  catalogs=['/astro/iraf/noao/digiphot/photcal/catalogs/porter.dat', $
    '/host/dione/u1/deutsch/stds/landolt1992.dat']
  namelens=[13,11]

  for cat=0,n_elements(catalogs)-1 do begin
    openr,1,catalogs(cat)
    while not EOF(1) do begin
      readf,1,lin
      if (strpos(strmid(lin,0,namelens(cat)),srchspec) ne -1) then begin
        NAME=strn(strmid(lin,0,namelens(cat)))+'              '
        if (cat eq 0) then begin
          reads,strmid(lin,14,199),V,sV,UmB,sUmB,BmV,sBmV,VmR,sVmR,RmI,sRmI
          VmI=VmR+RmI & sVmI=sqrt(sVmR^2+sRmI^2)
          endif
        if (cat eq 1) then reads,strmid(lin,32,199),V,BmV,UmB,VmR,RmI,VmI,n,m,sV,sBmV,sUmB,sVmR,sRmI,sVmI
        colrs=[UmB,BmV,VmR,RmI]
        UBVRI=[V+BmV+UmB,V+BmV,V,V-VmR,V-VmI]
        sUBVRI=[sqrt(sV^2+sBmV^2+sUmB^2),sqrt(sV^2+sBmV^2),sV,sqrt(sV^2+sVmR^2),sqrt(sV^2+sVmI^2)]
        colr1=colrs(([0,1,1,2,3])(filti))
        colr2=colrs(([1,0,2,3,2])(filti))
        print,ROOTNAME,AIRMASS,FILTERS,NAME,colr1,colr2,UBVRI(filti),sUBVRI(filti), $
          format='(a8,2x,f5.3,2x,a1,2x,a11,2x,2f9.2,f9.3,f7.3)'
        endif
      endwhile
    close,/all
    endfor

  return

end
