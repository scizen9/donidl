PRO readfoot,file,outname,outdir=outdir,indir=indir,imdir=imdir

;file='foota.html'
nfoot=n_elements(file)

alink=''
ara=0.
adec=0.
arun=0
arerun=0.
acamcol=0.
afield=0.
arowc=0.
acolc=0.



for i=0,nfoot-1 do begin
  spawn,"sed 's/<\/a>//g' "+indir+file[i]+' > tmp1'
  spawn,"sed 's/>/ /g' tmp1 > tmp2"
  spawn,"sed 's/\,/ /g' tmp2 > tmp3"
  spawn,'mv tmp3 '+indir+'foot'+ts(i)+'.dat'
  spawn,'rm tmp*'

  readcol,indir+'foot'+ts(i)+'.dat',l1,link,ra,dec,run,rerun,camcol,field,rowc,colc,f='A,A,F,F,F,F,F,F,F,F'

  alink=[alink,link]
  ara=[ara,ra]
  adec=[adec,dec]
  arun=[arun,run]
  arerun=[arerun,rerun]
  acamcol=[acamcol,camcol]
  afield=[afield,field]
  arowc=[arowc,rowc]
  acolc=[acolc,colc]

endfor

readcol,indir+'objmultipos.list',id,pra,pdec,name,f='F,F,F,A'



;make wget script for fits data 

wstrt='wget -nd http://das.sdss.org/DR6/data/imaging/'
band=['u','g','r','i','z']
nband=n_elements(band)

close,1
for iband=0,nband-1 do begin

  openw,1,outdir+'wget_script_'+outname+'_'+band[iband]

  np=n_elements(arun)
  for ip=1,np-1 do begin
    if arun[ip] GT 0 then begin
      printf,1,wstrt+strcompress(fix(string(arun[ip])),/remove_all)+'/'+$
        strcompress(string(fix(arerun[ip])),/remove_all)+'/corr/'+$
      strcompress(string(fix(acamcol[ip])),/remove_all)+'/fpC-'+$
      strcompress(string(fix(arun[ip]), format='(I6.6)'),/remove_all)+'-'+$
      band[iband]+strcompress(string(fix(acamcol[ip])),/remove_all)+'-'+$
      strcompress(string(fix(afield[ip]), format='(I4.4)'),/remove_all)+'.fit.gz'
    endif
  endfor
  printf,1,'rm *.gz.*'
  close,1

  if iband EQ 0 then $
    make_tsfield_wget,arun,arerun,acamcol,afield,outname,outdir=outdir

endfor

;make cleanup script to throw images into proper directories by object
inames='fpC-'+$
      strcompress(string(fix(arun), format='(I6.6)'),/remove_all)+'-'+$
      '*'+strcompress(string(fix(acamcol)),/remove_all)+'-'+$
      strcompress(string(fix(afield), format='(I4.4)'),/remove_all)+'.fit.gz'

nim=n_elements(inames)

prun=strcompress(string(arun),/remove_all)+'/'
prerun=strcompress(string(fix(arerun)),/remove_all)
pcamcol=strcompress(string(acamcol,format='(I1.1)'),/remove_all)
psrun=strcompress(string(arun, format='(I6.6)'),/remove_all)
pfield=strcompress(string(afield, format='(I4.4)'),/remove_all)

tsname='tsField-'+psrun+'-'+pcamcol+'-'+prerun+'-'+pfield+'.fit'


match_circ,ara,adec,pra,pdec,5.0,1E7,s1,s2,d1
dir=strarr(nim)+imdir
nn=repstr(name,"'",'')
nn=repstr(nn,']','_')
nn=repstr(nn,'[','')
nn=strlowcase(nn)
dir[s1]=dir[s1]+strlowcase(nn[s2])+'/'


mstart='mv '
openw,1,outdir+'mv_script_'+outname
for ip=0,nim -1 do begin
   if (arun[ip] NE 0) or (arerun[ip] NE 0) then begin
   if file_test(dir[ip]) EQ 0 then begin
     printf,1,'mkdir '+dir[ip]
     printf,1,'mkdir '+dir[ip]+'sdss/'
   endif
   printf,1,'cp '+inames[ip]+' '+dir[ip]+'sdss/'
   printf,1,'cp '+tsname[ip]+' '+dir[ip]+'sdss/'
endif
endfor
close,1

stop

end
