pro conv_mail,searchspec
;+
; NAME:
;   CONV_MAIL
; PURPOSE:
;   This procedure converts a VMS mail folder to a unix-compatible mail folder.
; CALLING SEQEUNCE:
;   VMSMAIL> set folder mail 
;   VMSMAIL> extract/all vms.mailfol 
;   Call> conv_mail,searchspec'
;   e.g.> conv_mail,'*.mailfol'"
; INPUT:
;   SEARCHSPEC  Search specification for files to be converted.
; MODIFICATION HISTORY:
;	01-AUG-95 Added properly handling on lines beginning with "From " in
;		the message body.   E. Deutsch
;-

  if (n_params(0) ne 1) then begin
    print,'CONV_MAIL: Convert a VMS mail folder to a unix compatible folder'
    print,'Call> conv_mail,searchspec'
    print,"e.g.> conv_mail,'*.mailfol'"
    return
    endif


  files=findfile(searchspec)
  if (files(0) eq '') then begin
    print,'No files matched ''',searchspec,''''
    return
    endif


  months='Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec'
  lin='  '


  for ifile=0,n_elements(files)-1 do begin
    openr,1,files(ifile)
    openw,2,files(ifile)+'.new'
    ctr=0
    print,'Contverting ',files(ifile),' to ',files(ifile)+'.new'
    startflag=1

    while not EOF(1) do begin
      readf,1,lin

      if (strmid(lin(0),0,5) eq 'Subj:') then begin
        tmp='Subject: '+strmid(lin(0),6,230)
        lin=tmp
        endif

      if (strmid(lin(0),0,5) eq 'From ') then begin
        tmp='>From '+strmid(lin(0),5,230)
        lin=tmp
        endif

      if (total(fix(byte(strmid(lin(0),0,1)))) eq 12) then begin
        lin=''
        if (startflag eq 0) then printf,2,lin else startflag=0
        readf,1,lin

        if (strmid(lin(0),0,5) eq 'From:') then begin

          junk=strpos(lin,'IN%"')
          if (junk ne -1) then begin
            lin=strdel(lin,junk,4)
            lin=strdel(lin,strpos(lin,'"',1),1)
            endif

          junk=strpos(lin,' "')
          if (junk ne -1) then lin=strdel(lin,junk+1,strpos(lin,'" ')-junk)

          tmp=strarr(10) & i=0
          for i=0,9 do tmp(i)=getwrd(lin,i)
          tmp(0)=strmid(tmp(0),6,100)

          junk=strpos(tmp(0),'::')
          if (junk ne -1) then begin
            t=strmid(tmp(0),junk+2,99)
            tmp(0)=t
            endif
;          t=strlowcase(tmp(0)) & tmp(0)=t

          da=1
          if (strmid(tmp(1),0,1) lt '0') or (strmid(tmp(1),0,1) gt '9') then begin
            tmp(1)='' & da=2
            endif
          t=tmp(da)
          date=strmid(t,0,strpos(t,'-'))
          date=strn(date,length=2,padtype=1)
          month=strmid(t,strpos(t,'-')+1,3)
          month=strmid(month,0,1)+strlowcase(strmid(month,1,2))
          year=strmid(t,strpos(t,'-')+5,4)


          dofw=strmid(weekday(year,strpos(months,month)/4+1,date),0,3)
          tim=strmid(tmp(da+1),0,8)

          lin='From '+tmp(0)+' '+dofw+' '+month+' '+date+' '+tim+' '+year
          print,ctr,':  ',lin & ctr=ctr+1

          printf,2,lin

;  *** Add the "Date: ...." line
          lin='Date: '+dofw+', '+date+ $
            ' '+month+' '+strmid(year,2,2)+' '+tim+' -0700'
          printf,2,lin

;  *** Add the "from: ...." line
          lin='From: '+tmp(0)

          endif
        endif
      printf,2,lin
      endwhile
    close,/all
    endfor

  return
end
