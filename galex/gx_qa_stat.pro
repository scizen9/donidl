function gx_qa_stat,try_dir,qa_status,coadd_status
;+
;	gx_qa_stat - check QA results
;
;	stat = gx_qa_stat(try_dir,coadd_status)
;
;	stat is -2: file not found
;	        -1: QA NA
;	         0: QA FAIL
;		 1: QA PASS
;-
qa_status=-2
coadd_status=-2
flist=file_search(try_dir+'/qa/manual/*-qa.txt',count=nf)
if nf gt 0 then begin
	qa_status=intarr(nf) - 1
	coadd_status=intarr(nf) - 1
	for i=0,nf-1 do begin
		readcol,flist[i],key,form,eql,val,form='a,a,a,a',/silent
		t=where(strpos(key,'Grade') ge 0, nt)
		if nt eq 1 then begin
			t=t[0]
			if strpos(val[t],'PASS') ge 0 then $
				qa_status[i] = 1 $
			else if strpos(key[t],'FAIL') ge 0 then $
				qa_status[i] = 0
		endif
		t=where(strpos(key,'Coadd') ge 0, nt)
		if nt eq 1 then begin
			t=t[0]
			if strpos(val[t],'PASS') ge 0 then $
				coadd_status[i] = 1 $
			else if strpos(key[t],'FAIL') ge 0 then $
				coadd_status[i] = 0
		endif
	endfor
endif
;
return,qa_status
end
