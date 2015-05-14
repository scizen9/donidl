pro aischk,file=file

; echo "aischk,file='chains-CHV6.tdb'" | idl | & tee chains-CHV6-aischk.op

if not(keyword_set(file)) then file='chains-CHV6.tdb'

;readcol,'aispattv4.tdb',grid,offset,mode,ra,dec,twist,scanx,scany,format='A,F,A,D,D,D,F,F'
readcol,file,grid,offset,mode,ra,dec,twist,scanx,scany,format='A,F,A,D,D,D,F,F'

ptr=where(offset eq -360)
nchain=n_elements(ptr)
print, 'Examining '+string(nchain)+' chains'
ptr=[ptr,n_elements(grid)]

eularr=replicate(0.0,n_elements(grid))
print, ''
print, nchain
mk=0
for i=0,nchain-1 do begin
    chlen=ptr[i+1]-ptr[i]
	for j=0,chlen-2  do begin
		p=ptr[i]+j
		q1=qnorm(rdt2q([ra[p],dec[p],twist[p]]))
		q2=qnorm(rdt2q([ra[p+1],dec[p+1],twist[p+1]]))
		qdelt=qnorm(qmult(qconj(q1),q2))
		euler=acos(qdelt[3])*2.0*!RADEG
                eularr[p]=euler
if (euler gt 1.3) and mk eq 0 then print, 'Bad Chains'
                if (euler gt 1.3) then mk=1
                if (euler gt 1.3) then print,grid[p],j,euler,ra[p],dec[p]
if ((j ne 0) and (euler lt .6)) and mk eq 0 then print, 'Bad Chains'
                if ((j ne 0) and (euler lt .6)) then mk=1
                if ((j ne 0) and (euler lt .6)) then print,j,grid[p],euler


            endfor

endfor

end
