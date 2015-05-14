pro flagimg,inimg,wtimg,imval,wtval
;
;	flag inimg with imval where wtimg eqals wtval
;
temp=inimg
ofile=gettok(temp,'.')
if strpos(inimg,'fit') ge 0 then begin
	im=readfits(inimg,inh)
endif else begin
	irafrd,im,inh,inimg
endelse
if strpos(wtimg,'fit') ge 0 then begin
	wt=readfits(wtimg,wth)
endif else begin
	irafrd,wt,wth,wtimg
endelse
;
t=where(wt eq wtval, nwt)
if nwt le 0 then $
	print,'No pixels to flag at: ',wtval $
else begin
	im(t) = imval
	sxaddpar,inh,'FLAGWGT',wtval,' Flag at this weight'
	sxaddpar,inh,'FLAGVAL',imval,' Flagged with this value'
	sxaddpar,inh,'FLAGNPT',nwt  ,' Number of pixels flagged'
;	writefits,ofile,im,inh
        irafwrite,im,inh,ofile
	print,'Wrote: ',ofile+'.imh'
endelse
;
return
end
