function gmagnlc, mag, fuv=fuv, nuv=nuv

; non-linear correction for point sources via P.M.'s formula

mag=mag*1.0

if keyword_set(fuv) then begin

 c0=-0.297
 c1=1.468
 c2=-0.174
 zp=18.82
 rolloffstartrate=35 

endif else begin

 c0=-1.220
 c1=1.986
 c2=-0.204
 zp=20.08
 rolloffstartrate=200

endelse


logf=0.4*(zp-mag)
bad=where(logf gt alog10(rolloffstartrate),countbad)
magtrue=mag

if countbad gt 0 then begin

 logftrue=(-c1+sqrt(c1^2 - 4*c2*(c0-logf[bad])))/(2*c2)
 magtrue[bad]=zp-2.5*logftrue

endif

return, magtrue < mag

end
