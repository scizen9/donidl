pro defansi,define
;+
; NAME:
;   DEFANSI
; DESCRIPTION:
;   This procedure defines a few ANSI screen control sequences and puts
;   them in a COMMON block.  To use these sequences, include the COMMON
;   block at the beginning of the procedure and call the procedure.
; CALLING SEQUENCE:
;   DefANSI,define
; INPUT:
;   DEFINE    The variable controls whether the sequences are defined or
;               cleared.  (0=Clear,1=Define) (Default=Defined=1)
;               It is desirable to clear the ANSI variables when doing a HELP,
;               because they cause the display to beep, clear, etc.
; OUTPUT:
;   COMMON ANSI,CR,LF,esc,UP,CLRSCRN,BELL,DOWN,RIGHT,LEFT,NORMAL,BOLD, $
;     REVERSE,BLINKING
; HISTORY:
;   03-JUL-90 Version 1 written by Eric Deutsch
;   24-AUG-91 Added a few more ANSI sequences   (E. Deutsch)
;-

  if (n_params(0) eq 0) then define=1

  COMMON ANSI,CR,LF,esc,UP,CLRSCRN,BELL,DOWN,RIGHT,LEFT,NORMAL,BOLD, $
    REVERSE,BLINKING

  if (define eq 1) then begin
    CR=string(10b) & LF=string(13b)
    ESC=string(27B) & BELL=string(7b)
    UP=ESC+'[A' & DOWN=ESC+'[B' & RIGHT=ESC+'[C' & LEFT=ESC+'[D'
    BOLD=ESC+'[1m' & REVERSE=ESC+'[7m' & BLINKING=ESC+'[5m' & NORMAL=ESC+'[0m'
    CLRSCRN=ESC+'[;H'+ESC+'[2J'
  endif else begin
    CR='' & LF='' & BELL='' & ESC='' & UP='' & CLRSCRN='' & DOWN=''
    RIGHT='' & LEFT='' & BOLD='' & REVERSE='' & BLINKING='' & NORMAL=''
    endelse

  return
end
