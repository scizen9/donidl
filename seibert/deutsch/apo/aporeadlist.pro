pro aporeadlist,inputlistfile

  if (n_params(0) ne 1) then begin
    print,'Call> aporeadlist,inputlistfile'
    print,"e.g.> aporeadlist,'nov12run.targs'"
    return
    endif

  if (not exist(inputlistfile)) then begin
    print,"ERROR: Unable to open '",inputlistfile,"'"
    return
    endif

  openr,1,inputlistfile
  lin=''
  DSSDIR='/DSSDIR/not/specified'
  SPIFIELDS=''

  openw,3,'idlplotsbatch'
  printf,3,'!quiet=1'

  openw,4,'movefiles'
  printf,4,'#!/pub/bin/tcsh'
  printf,4,'echo You will find the output files by anonymous ftp to:'
  printf,4,'echo ftp://ftp.astro.washington.edu/pub/apoguide'
  printf,4,'echo Note that if you add a .Z to any file, our server will compress it for you.'
  printf,4,'echo If there are no files or not all objects below, something went wrong.'
  printf,4,'echo These files will be automatically deleted after 24 hours.'
  printf,4,'echo ---'
  printf,4,'/usr/bin/mv '+inputlistfile+'* /host/mimas/net/ftp/pub/apoguide'
  buff='( cd /host/mimas/net/ftp/pub/apoguide ; ll '
  buff=buff+inputlistfile+"* "

  while (strmid(lin,0,19) ne '-----------------  ') do begin
    readf,1,lin
    if (strmid(lin,0,7) eq 'DSSDIR=') then DSSDIR=strmid(lin,7,99)
    endwhile
  if (strn(DSSDIR) eq '/incoming/apoguide') then DSSDIR='/host/mimas/net/ftp/incoming/apoguide'
  if (strmid(DSSDIR,strlen(DSSDIR),1) ne '/') then DSSDIR=DSSDIR+'/'
  print,'DSSDIR=',DSSDIR
  afflag=0

  objctr=0
  while (not EOF(1)) and (objctr lt 21) do begin
    readf,1,lin
    if (strn(lin) ne '') then begin
      print,lin

      name=strmid(lin,0,17)
      if (name eq '   Target Name   ') then goto,SKIP10
      if (name eq '-----------------') then goto,SKIP10
      if (name eq 'Example Entry    ') then begin
        print,'Target name called Example Entry is ignored.'
        goto,SKIP10
        endif

      if (strmid(lin,0,10) eq 'SPIFIELDS=') then begin
        SPIFIELDS=strmid(lin,10,99)
        if (exist(SPIFIELDS)) then print,'File SPIFIELDS=',SPIFIELDS,' found.' $
        else begin
          print,'File SPIFIELDS=',SPIFIELDS,' not found!'
          SPIFIELDS=''
          endelse
        goto,SKIP10
        endif

      if (strmid(lin,0,7) eq 'DSSDIR=') then begin
        DSSDIR=strmid(lin,7,99)
        if (strn(DSSDIR) eq '/incoming/apoguide') then DSSDIR='/host/mimas/net/ftp/incoming/apoguide'
        if (strmid(DSSDIR,strlen(DSSDIR),1) ne '/') then DSSDIR=DSSDIR+'/'
        print,'DSSDIR=',DSSDIR
        goto,SKIP10
        endif


      coords=strn(strmid(lin,19,24))
      stringad,coords,ra,dec
      if (ra lt 0) or (ra ge 360.) or (dec lt -90) or (dec gt 90) then begin
        print,'Target coordinates are out of bounds.  Entry ignored.'
        goto,SKIP10
        endif

      instr=strn(strmid(lin,45,4))

      InstAng=strn(strmid(lin,51,7))
      if (InstAng eq '') then InstAng='0'


      name2=''
      for i=0,strlen(name)-1 do begin
        char=strmid(name,i,1)
        if (char ge 'a' and char le 'z') or (char ge 'A' and char le 'Z') or $
          (char ge '0' and char le '9') then name2=name2+char
        endfor
      name22='"'+name2+'"'


      fldsize=2/3.0
      if (strupcase(instr) eq 'MRO') then fldsize=1.0
      if (strupcase(instr) eq 'FC') then begin
        fldsize=float(InstAng)+1
        if (fldsize eq 0) then fldsize=10.0
        if (fldsize lt 2) then fldsize=2.0
        if (fldsize gt 60) then fldsize=60.0
        fldsize=fldsize/60.0 
       endif


      dssfile=strn(strmid(lin,60,99))
      charttype=dssfile
      if (dssfile eq 'autofetch') or (dssfile eq 'usnofetch') or (dssfile eq '') then begin
        if (afflag eq 0) then begin
          openw,5,'dssfetch'
          printf,5,'#!/bin/csh'
          printf,5,''
          printf,5,'setenv TERM vt100'
          printf,5,'unset noclobber'
          printf,5,'echo " "'
          printf,5,''
          afflag=1
          endif

        coords2="r='" & coords3="ra='" & spctr=0 & pch='' & fudge=0
        for jj=0,strlen(coords)-1 do begin
          ch=strmid(coords,jj,1)
          if (ch eq '+') then ch=''
          if (ch eq ' ') or (ch eq ':') then begin
            if (pch ne ' ') and (pch ne '') then begin
              ch=' '
              spctr=spctr+1
              if (spctr eq 3) then begin
                ch='' & pch=' ' & fudge=1
                coords2=coords2+"' d='"
                coords3=coords3+"' dec='"
                endif
            endif else ch=''
            endif
          if (fudge eq 0) then pch=ch
          fudge=0
          coords2=coords2+ch
          coords3=coords3+ch
          endfor

        sfldsize="h="+strn(fldsize*60.0,format='(f8.1)')+" w="+ $
          strn(fldsize*60.0,format='(f8.1)')

        if (dssfile eq 'autofetch') then begin
          printf,5,'echo "Requesting image '+name2+'.fits.gz from STScI.  This will take a few minutes..."'
          printf,5,'~deutsch/bin/webquery.pl host=stdatu.stsci.edu url=/cgi-bin/dss_search file='+name2+'.fits.gz v=1 '+coords2+"' e=J2000 "+sfldsize+' f=fits c=gz'
          printf,5,'if ( -e '+name2+'.fits.gz ) ls -al '+name2+'.fits.gz'
          printf,5,'if ( -e '+name2+'.fits.gz ) egrep "CD-ROM|DSS" '+name2+'.fits.gz'
          printf,5,'if ( -e '+name2+'.fits.gz ) /pub/bin/gunzip -c '+name2+'.fits.gz > '+name2+'.fits'
          printf,5,''
          dssfile=",DSSfile='"+name2+".fits'"
          endif $
        else dssfile=""

        printf,5,'echo "Requesting USNO A2.0 data '+name2+'.usno2.dat from NOFS.  This will take a minute..."'
        costr=adstring(ra,dec,2)
        rastr=strn(strmid(costr,0,13))
        decstr=strn(strmid(costr,15,12))
        coords8=rastr+' '+decstr
        vals=getopt(coords8,'F',6) & sign1=''
        if (strpos(coords8,'-') ne -1) and (vals(3) eq 0) then sign1='-'
        widthdeg=strn(fldsize,format='(f10.4)')
        printf,5,'~deutsch/bin/webquery.pl host=www.nofs.navy.mil url=/cgi-bin/chart.cgi'+ $
          ' file=zztmp.html method=POST'+ $
          ' rh='+strn(fix(vals(0)))+' rm='+strn(fix(vals(1)))+ $
          ' rs='+strn(vals(2))+ $
          ' dd='+sign1+strn(fix(vals(3)))+' dm='+strn(fix(vals(4)))+ $
          ' ds='+strn(vals(5))+' wa='+widthdeg+' wd='+widthdeg+ $
          ' ob=deutsch co=red'+ $
          ' br=0 fa=25 gr=no bx=10.0s'
        printf,5,'~deutsch/bin/webquery.pl host=www.nofs.navy.mil url=/tmp/deutsch.gz'+ $
          ' file=zztmp.dat.gz'
        printf,5,'if ( -e '+name2+'.usno2.dat ) /usr/bin/rm '+name2+'.usno2.dat'
        printf,5,'gunzip -c zztmp.dat.gz > '+name2+'.usno2.dat'
        printf,5,'if ( -e zztmp.html ) /usr/bin/rm zztmp.html'
        printf,5,'if ( -e zztmp.dat.gz ) /usr/bin/rm zztmp.dat.gz'
        printf,5,''


      endif else begin
        if (dssfile ne '') then dssfile=",DSSfile='"+DSSDIR+dssfile+"'"
        endelse

      printf,4,"/pub/bin/gzip -c "+name2+".ps > "+name2+".ps.gz"
      printf,4,"/usr/bin/mv "+name2+".* /host/mimas/net/ftp/pub/apoguide"
      buff=buff+name2+".* "

      progname="apoguide,'"
      if (strupcase(instr) eq 'MRO') then progname="mroguide,'"
      if (strupcase(instr) eq 'FC') then begin $
        trailer=''
        if (SPIFIELDS ne '') then trailer=',spifields="'+SPIFIELDS+'"'
        gscfile="A10file='"+name2+".usno2.dat',"
        printf,3,"mkfchart,'"+coords+"',"+gscfile+ $
          "objname='"+strn(strmid(lin,0,17))+"',ps='"+ $
          name2+".ps'"+dssfile+',fieldsize='+strn(fldsize*60.0-1)+trailer
        endif $
      else printf,3,progname+instr+"','"+coords+"',A10file='"+name2+ $
        ".usno2.dat',objname='"+strn(strmid(lin,0,17))+"',/suggest,ps='"+ $
        name2+".ps'"+dssfile+',InstAng='+InstAng

      objctr=objctr+1
      endif

SKIP10:
    endwhile

  printf,3,'exit'

  buff=buff+' ) | grep rw'
  printf,4,buff

  close,/all
  spawn,'chmod a+x dssfetch'

  return


end
