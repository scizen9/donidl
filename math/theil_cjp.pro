pro theil_cjp, x, y, slopemin, slopemax, dslope, slopemdn, y0mdn, sigma

; 	subroutine theil (x, y, temp, n, slopemin, slopemax, dslope,
;     &    slopemdn, y0mdn, sigma, plot)
;
    nbinmax=10000
;
    nbin=nint((slopemax-slopemin)/dslope)
    if nbin gt nbinmax then begin
        print,'Not enough bins for Theil test; enlarge nbin.'
        return
    endif
;
    n=n_elements(x)
    if n_elements(y) ne n then begin
        print,'x and y vectors must have same number of elements.'
        return
    endif

    bin=fltarr(nbin)
    np=0L
    for i=1L,n-1 do $
    for j=0L,i-1 do begin
	dx=x(i)-x(j)
	dy=y(i)-y(j)
	if (dx ne 0.) then begin
	    slope=dy/dx
	    ibin=nint((slope-slopemin)/dslope)
	    ibin=min([nbin-1,max([0,ibin])])
	    bin(ibin)=bin(ibin)+1
	    np=np+1L
;            print,'slope,ibin,np,yi,yj',slope,ibin,np,y(i),y(j)
        endif
    endfor

    rnphalf=float(np)/2
    cum=0.
    for ibhalf=0,nbin-1 do begin
	cum=cum+bin(ibhalf)
	if cum ge rnphalf then break
    endfor

    cumm1=cum-bin(ibhalf)
    slopemdn=slopemin+(ibhalf-1)*dslope+(rnphalf-cumm1)/bin(ibhalf)*dslope

    temp=y-slopemdn*x

    rmnsd_rmv1 ,temp, 2.3, 20, y0mdn, sigma

    return
    end

; -----------------------------------------------------------------
;  Chris Pritchet               pritchet at uvic.ca
;  Dept of Physics & Astronomy  http://www.astro.uvic.ca/~pritchet
;  University of Victoria
;  P.O. Box 3055                250-721-7744 (office)
;  Victoria, BC V8W 3P6         250-884-5047 (cell)
;  CANADA                       250-721-7715 (FAX)
; -----------------------------------------------------------------
