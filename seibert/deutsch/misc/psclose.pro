pro psclose,autoprint,queue,silent=silent,autoprint=auto
;+
; NAME:
;   PSCLOSE
;
; PURPOSE:
;   This procedure closes the POSTSCRIPT output mode and returns the mode to
;   the device when SETPS was called.  The POSTSCRIPT output is sent to the 
;   PostScript file and may also optionally be sent automatically to the printer.
;   It is designed to be used in conjunction with SETPS.
;
; CALLING SEQEUNCE:
;   PSCLOSE,[autoprint],[queue],[/silent,/autoprint,autoprint='']
;
; OPTIONAL INPUT:
;   AUTOPRINT Specifies whether IDL.PS should automatically be sent to
;               the printer queue (0=NO, 1=Last or Default Printer,
;               2=Choose Printer from list)
;   QUEUE     Specifies the queue to which the plot is automatically
;               sent in AUTOPRINT=1   (Default from cluster info. file)
;
; OPTIONAL KEYWORDS:
;   AUTOPRINT Specifies whether IDL.PS should automatically be sent to
;               the printer determined by:
;                 0=NO
;                 1=Last Printer printed to (PSCLOSE) or the Default Printer
;                 2=Choose Printer from list
;                 string=assumed to be the Queue Name.
;   SILENT    Indicates that none of the informational messages are printed.
;
; HISTORY:
;   11-JUL-90 Version 1 written by Eric Deutsch
;   26-MAY-92 Proper header and other minor modifications. EWD
;   23-OCT-92 Modified to use list of printers and to work with UNIX.  EWD
;   06-FEB-93 Added support for filenames other than idl.ps.  EK & EWD
;   96-OCT-16 Get rid of FOS_POST, as it is gone. rcb
;-

  COMMON SETPS_ComBlk,CurDev,LastPrn,PSfilename

  PRINT_QUEUE='' & GetDefVal,PRINT_QUEUE,'PSTSCRPT'
  LISTFILE='' & GetDefVal,LISTFILE,'PSQLSTFL'

  arg=n_params(0)
  if (arg gt 2) then begin
    if (PRINT_QUEUE eq '') then PRINT_QUEUE='s423ps'
    print,'Call: IDL> PSCLOSE,[autoprint],[queue],[autoprint=]'
    print,"e.g.: IDL> PSCLOSE,1                 (Automatically send to "+PRINT_QUEUE+")"
    print,"e.g.: IDL> PSCLOSE,1,'"+PRINT_QUEUE+"'"
    return
    endif

  if (strupcase(!d.name) ne 'PS') then begin
    print,'Current device is not PostScript.' & return
    endif

  if (arg lt 1) then autoprint=0
  if (autoprint eq 1) and (n_elements(LastPrn) eq 1) then PRINT_QUEUE=LastPrn
  if (arg lt 2) then queue=PRINT_QUEUE
  if (n_elements(silent) eq 0) then silent=0
  s=size(auto)
  if (s(1) eq 7) then begin & autoprint=1 & queue=auto & endif
  if (s(1) eq 2) then autoprint=auto

  device,/close
  if (n_elements(CurDev) eq 0) then CurDev='X'
  set_plot,CurDev


  if (autoprint eq 2) then begin
    if (LISTFILE eq '') then begin
      print,'Printer List not specified in defaults file.  Not sending...'
      autoprint=0 & goto,SKIP1
      endif
    on_ioerror,NOFILE
    get_lun,unit & openr,unit,LISTFILE & lin='' & lis=strarr(50) & i=0
    while not EOF(unit) do begin & readf,unit,lin & lis(i)=lin & i=i+1 & endwhile
    close,unit & lis=lis(0:i-1)
    if (CurDev eq 'X') then choice=wmenu(['Available Printers',lis],title=0)-1 $
    else begin
      for t=0,i-1 do print,strn(t),'. ',lis(t)
      str='' & read,'Enter Number: ',str
      if (strnumber(str,choice) eq 0) then t=999 $
      else if (choice lt 0) or (choice gt i-1) then t=999
      if (t eq 999) then begin
        print,'Unrecognized Printer' & autoprint=0 & goto,SKIP1 & endif
      endelse
    queue=lis(choice) & autoprint=1
    endif
  goto,SKIP1

NOFILE:
  print,'File ',LISTFILE,' not found.  Not Sending...'
  autoprint=0

SKIP1:
  if (autoprint eq 1) then LastPrn=queue

  if (!VERSION.OS ne "vms") then begin
    if not silent then begin
      print,'File idl.ps closed.  Graphics Output returned to ',CurDev
      if (autoprint eq 0) then print,'To print:  $lpr -P'+queue+' -s '+PSfilename
      endif
    if (autoprint eq 1) then begin
      if not silent then print,'Sending '+PSfilename+' to queue: ',queue
      tmp='lpr -P'+queue+' -s '+PSfilename
      spawn,tmp
      endif
  endif else begin
    if not silent then begin
      print,'File '+PSfilename+' closed.  Graphics Output returned to ',CurDev
      if (autoprint eq 0) then begin
        print,'To print:'
        print,'$PRINT/noti/que='+queue+' '+PSfilename
        endif
      endif
    if (autoprint eq 1) then begin
      if not silent then print,'Sending '+PSfilename+' to queue: ',queue
      tmp='$PRINT/QUE='+queue+'/NOTIFY/NOFLAG/NOBURST '+PSfilename
      spawn,tmp
      endif
    endelse

  return
end
