pro  RMNSD_RMV1,A,TOLSIG,NITERMAX,RMEAN,STDDEV,PRINT=PRINT
;       SUBROUTINE RMNSD_RMV1 (A,N,TOLSIG,NITERMAX,PRINT,RMEAN,
;     &   STDDEV)
;
; Same as RMNSD, except removes outliers iteratively - i.e. takes
; median. Iterates a maximum of NITERMAX times, removing outliers
; beyond TOLSIG*SIGMA each time. This is a new version that doesn't
; need weights (all initial weights are assumed to be unity).
;
;       REAL A(1)
;       REAL*8 SUM,SUMSQ,SUM0,SUMSQ0
       if keyword_set(print) then begin
           openw,ll,'rmnsd_rmv1.out',/get_lun
       endif
       ITER=1
       n=n_elements(a)
       nuse0=n_elements(a)
       sum0=total(double(a))
       sumsq0=total(double(a)^2)
       RMEAN=SUM0/NUSE0
       if (sumsq0-(sum0^2)/nuse0) le 0.d0 then $
	  stddev=0. $
       else $
          STDDEV=SQRT((SUMSQ0-(SUM0^2)/NUSE0)/(NUSE0-1))

       FMT='(5X,"ITER #",I3,":  MEAN =",g16.7,5X,"SIGMA =",g16.7,"  (",I6," POINTS)")'
       IF keyword_set(PRINT) then print,ITER,RMEAN,STDDEV,NUSE0,form=fmt
       IF keyword_set(PRINT) then printf,ll,ITER,RMEAN,STDDEV,NUSE0,form=fmt

       NUSE=NUSE0

       for ITER=1,NITERMAX-1 do begin

       NUSEOLD=NUSE
       NUSE=NUSE0
       SUM=SUM0
       SUMSQ=SUMSQ0
       RLO=RMEAN-TOLSIG*STDDEV
       RHI=RMEAN+TOLSIG*STDDEV

       for I=0L,N-1 do begin
       IF A(I) LT RLO OR A(I) GT RHI THEN begin
         NUSE=NUSE-1
         SUM=SUM-double(A(I))
         SUMSQ=SUMSQ-double(A(I))^2
       ENDIF
       endfor

       if nuse le 1 then begin
           if keyword_set(print) then free_lun,ll
           return
       endif

       RMEAN=SUM/NUSE
       if (sumsq-(sum^2)/nuse) le 0.d0 then $
	  stddev=0. $
       else $
          STDDEV=SQRT((SUMSQ-(SUM^2)/NUSE)/(NUSE-1))

       IF keyword_set(PRINT) then print,ITER,RMEAN,STDDEV,NUSE
       IF keyword_set(PRINT) then printf,ll,ITER,RMEAN,STDDEV,NUSE
       if keyword_set(print) then free_lun,ll
       IF NUSE EQ NUSEOLD then RETURN
       endfor
       RETURN
       END
