pro get_lga,obj,reread=reread,silent=silent
;
; see if it's in archive
lgadir=!SNE_HOSTS+'2MASS/LGA/'
flist=file_search(lgadir+obj+'*.fits',count=nf)
;
; if not, use wget to retrieve NED data
if nf lt 1 or keyword_set(reread) then begin
    cmd=[ 'wget -N "http://irsa.ipac.caltech.edu/data/LGA/images/'+ $
	    obj+'/'+obj+'_mosaic_h.fits"', $
    	  'wget -N "http://irsa.ipac.caltech.edu/data/LGA/images/'+ $
	    obj+'/'+obj+'_mosaic_j.fits"', $
    	  'wget -N "http://irsa.ipac.caltech.edu/data/LGA/images/'+ $
	    obj+'/'+obj+'_mosaic_k.fits"', $
    	  'wget -N "http://irsa.ipac.caltech.edu/data/LGA/images/'+ $
	    obj+'/'+obj+'_mosaic.jpg"', $
    	  'wget -N "http://irsa.ipac.caltech.edu/data/LGA/images/'+ $
	    obj+'/'+obj+'_mosaic-SB.tbl"']
    if not keyword_set(silent) then begin
	    print,'wgetting from LGA...'
	    print,cmd
    endif
    cd,lgadir,cur=cwd
    for i=0,4 do spawn,cmd(i),res,eres
    cd,cwd
;
endif else begin
	if not keyword_set(silent) then $
		print,obj,' already in archive.'
endelse
;
return
end
