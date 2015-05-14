function twomass_unique_images,flist
;+
; twomass_unique_images - return a list of unique 2MASS images
;
;	compares rundate + scan id info to derive unique image
;-
; strip down to rundate+scanid
	temp=flist
	test=gettok(temp,'.')
	for i=0,2 do junk=gettok(test,'_')
;
; sort and uniq
	temp=flist
	s=sort(test)
	u=uniq(test(s))
	temp=temp(s)
	temp=temp(u)
	return,temp
end
