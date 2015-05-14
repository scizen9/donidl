pro labelrc3, jpegfilein, jpegfileout, hdr, $
   quality=quality,charsize=charsize, thick=thick, $
   smoothtext=smoothtext

   ;jpegfilein='/home/mseibert/Virgo_half_bgs_2color.jpg'
   ;jpegfileout='/home/mseibert/Virgo_half_bgs_2color_labled.jpg'
   ;hdr=headfits(headersource)   

   xdim=sxpar(hdr,'NAXIS1')
   ydim=sxpar(hdr,'NAXIS2') 

   extast,hdr,astr
   xcen=fix(xdim/2)
   ycen=fix(ydim/2)
   
   XY2AD, xcen, ycen, astr, ra_cen, dec_cen
   radius=abs(astr.cd[0])*60 * xcen * 2; armins

   offset=90 ;arsec
   offset=offset/(abs(astr.cd[0])*3600) ;pixels

   !priv=2
   dbopen,'~/bin/myidlpro/rc3/rc3',1
   desc = db_item_info('description')

   list = dbcircle(ra_cen/15., dec_cen, radius, dist2center,/to_b1950,/sil)
   if list[0] eq -1 then goto, jump1

   dbext,list,'d25,r25,pa,t',d25,r25,pa,t
   rc3diam=(.1*10^d25) ; diam in arcmin
   rc3ratio=(10^r25)

   dbext,list,'name1,name2,name3',nameA,nameB,nameC
   nameA=STRCOMPRESS(nameA,/remove)
   nameB=STRCOMPRESS(nameB,/remove)
   nameC=STRCOMPRESS(nameC,/remove)

   dbext,list,'RA,DEC',rarc3,decrc3 ; decimal hours, degrees
   rarc3=rarc3*15
   precess,rarc3,decrc3, 1950, 2000

   AD2XY, rarc3, decrc3, astr, x, y
   x1=fix(x+1)
   y1=fix(ydim-(y+1))

   ;;;;;;;;;;;;;;;;;
   ;read jpeg image

   read_jpeg,jpegfilein,image

   red=image[0,*,*]
   green=image[1,*,*]
   blue=image[2,*,*]

   set_plot,'z' ; generate as virtual image because I write to image
               ; can be avoided if no text in border needed
   device,set_resolution=[xdim,ydim]

   erase

   g=where(x le xdim and x ge 0 and y ge 0 and y le ydim)

   ;forprint,nameA,nameB,nameC,prin_gal 

   if not keyword_set(charsize) then charsize=2

   for r=0, n_elements(g)-1 do begin 
     ;jpegfile=target[i]+'_2color.jpg'
     rname=namea[g[r]]
     if rname eq '' then rname=nameb[g[r]]
     if rname eq '' then rname=namec[g[r]]
     if rname eq '' then rname='NAME?'
     ;cmd = "convert -fill green -pointsize 48 -draw"
     ;cmd=cmd+ " 'text "+strn(x1[g[r]])+","+strn(y1[g[r]])+' "'
     ;cmd=cmd+ nameb[g[r]]+'"'+"' "+jpegfile+' '+jpegfile 
     ;spawn, cmd, result
      xyouts,x[g[r]]+offset,y[g[r]]+offset,'!6'+rname+'!x',$
       charsize=charsize,charthick=1,/dev
     ;plots,[x[g[r]],x[g[r]]+offset],[y[g[r]],y[g[r]]+offset],/dev
   endfor

   if keyword_set(smoothtext) then txtpln1=smooth(tvrd(),2) else txtpln1=tvrd()

   if not keyword_set(thick) then thick=2

   for r=0, n_elements(g)-1 do begin 
     plots,[x[g[r]],x[g[r]]+offset],[y[g[r]],y[g[r]]+offset],thick=thick,/dev
   endfor

   txtpln2=tvrd()   

   t=where(txtpln1+txtpln2 gt 0)

   device,/close ; close z device

   red[t]=0
   green[t]=255
   blue[t]=0
  
   image[0,*,*]=red
   image[1,*,*]=green
   image[2,*,*]=blue 

   if not keyword_set(quality) then quality=75

   write_jpeg,jpegfileout,image,/true,quality=quality

   jump1:

end
