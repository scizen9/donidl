datadir='~/SFR_FIELDS/MK06/data/'
dir=datadir+'sdss_get_scripts/'

fn=dir+'objmultipos.list'
readcol,fn,idx,pra,pdec,name,f='F,F,F,A'
name=strlowcase(strcompress(name,/remove_all))
;name[where(name EQ 'messier104')]='ngc5494'
np=n_elements(pra)

im=file_search(dir+'fpC*u*gz',/expand_tilde)
nim=n_elements(im)

;stop

;goto,skip

stat=fltarr(nim,np)

for iim=0,nim-1 do begin

   hdr=headfits(im[iim])
   adxy,hdr,pra,pdec,px,py
   inim=where(px GE 0 and px LE 2048 and py GE 0 and py LE 1489,count)
   if count GE 1 then stat[iim,inim]=1

endfor

skip:

nc=total(stat,1)
cov=where(nc GT 0,numcov)
im=repstr(im,'u','*')
ts=repstr(im,'fpC','tsField')
ts=repstr(ts,'.gz','')
ts=repstr(ts,'*6','6-*')
ts=repstr(ts,'*1','1-*')
ts=repstr(ts,'*2','2-*')
ts=repstr(ts,'*3','3-*')
ts=repstr(ts,'*4','4-*')
ts=repstr(ts,'*5','5-*')
dr=repstr(ts,'tsField','drField')


;stop
close,1
openw,1,'cp_script_fits_sort'
for i=0,numcov-1 do begin
   inc=where(stat[*,cov[i]] GT 0,nin)
   if name[cov[i]] EQ 'ugc00695' then trail='stripe82/' else trail=''
   if file_test(datadir+name[cov[i]]) EQ 0 then begin
      printf,1,'mkdir '+datadir+name[cov[i]]
      printf,1,'mkdir '+datadir+name[cov[i]]+'/sdss'
   endif
   for j=0,nin-1 do begin
      printf,1,'cp '+im[inc[j]]+' '+datadir+name[cov[i]]+'/sdss/'+trail
   
      printf,1,'cp '+ts[inc[j]]+' '+datadir+name[cov[i]]+'/sdss/'+trail

      printf,1,'cp '+dr[inc[j]]+' '+datadir+name[cov[i]]+'/sdss/'+trail

   endfor
endfor
close,1

end
