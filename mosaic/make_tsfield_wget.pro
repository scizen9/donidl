;given a list of run, rerun, camcol, and field, print a wget file that
;will return the necessary SDSS tsField calibration files (DR6)

PRO make_tsfield_wget,arun,arerun,acamcol,afield,outname,no_targ=no_targ,chunkdir=chunkdir,outdir=outdir

if keyword_set(chunkdir) EQ 0 then chunkdir='~/IDL/mimap/get_sdss/'
nobj=n_elements(arun)

strt='wget -o wgetlog -nd http://das.sdss.org/DR6/data/imaging/inchunk_'

prun=strcompress(string(arun),/remove_all)+'/'
prerun=strcompress(string(fix(arerun)),/remove_all)
pcamcol=strcompress(string(acamcol,format='(I1.1)'),/remove_all)
psrun=strcompress(string(arun, format='(I6.6)'),/remove_all)
pfield=strcompress(string(afield, format='(I4.4)'),/remove_all)

tsname='tsField-'+psrun+'-'+pcamcol+'-'+prerun+'-'+pfield+'.fit'

chunkdat=chunkdir+'tschunk.dr6.best.par.txt'
readcol,chunkdat,junk,run,camcol,rerun,stripe,strip,smu,emu,f0,nf,status,smus,ver,f='A,F,F,F,F,A,D,D,F,F,A,D,F'

bad=0
check_chnk=-99
close,1
openw,1,outdir+'wget_script_tsfield_sdss_'+outname
for iobj=0,nobj-1 do begin
  chnk=where((arun[iobj] EQ run) and (acamcol[iobj] EQ camcol) and $
             (arerun[iobj] EQ rerun))
  print,chnk
  print,'check',check_chnk
  if ((chnk[0] GE 0) and ((where(chnk[0] EQ check_chnk))[0] LT 0)) then begin
    strng=strt+'best/stripe'+string(stripe[chnk],format='(I2.2)')+'_mu'+$
      strcompress(string(smus[chnk],format='(I)'),/remove_all)+'_1/'+$
      string(camcol[chnk],format='(I1.1)')+'/' 
    printf,1,strng+tsname[iobj]
    ;check_chnk=[check_chnk,chnk[0]]
  endif else  if chnk[0] EQ -1 then bad=[bad,iobj]
     
endfor



if (keyword_set(no_targ) EQ 0) and (n_elements(bad) GT 1) then begin

bad=bad[1:*]


  check_chnk=-99
  chunkdat=chunkdir+'tschunk.dr6.target.par'
  readcol,chunkdat,junk,run,camcol,rerun,stripe,strip,smu,emu,f0,nf,status,smus,ver,f='A,F,F,F,F,A,D,D,F,F,A,D,F'

  for ibad=0,n_elements(bad) -1 do begin
    iobj=bad[ibad]
    chnk=where((arun[iobj] EQ run) and (acamcol[iobj] EQ camcol) and $
               (arerun[iobj] EQ rerun))
    if ((chnk[0] GE 0) and ((where(chnk[0] EQ check_chnk))[0] LT 0)) then begin
      strng=strt+'target/stripe'+string(stripe[chnk],format='(I2.2)')+'_mu'+$
        strcompress(string(smus[chnk],format='(I)'),/remove_all)+'_0/'+$
        string(camcol[chnk],format='(I1.1)')+'/'
      printf,1,strng+tsname[iobj] 
      check_chnk=[check_chnk,chnk[0]]
    endif else if chnk[0] EQ -1 then print,iobj
  endfor

endif

no_targ=bad

printf,1,'rm tsField*.fit.*'
close,1

end
