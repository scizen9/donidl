pro get_sai_cat
;
; check for original file
afile=!SNE_DATA+'/sai.txt'
filestamp,afile,/arch,/verbose
wcmd = 'wget -F "http://zeus.sai.msu.ru/sn/sncat/sncat_latest_view.txt" -O '+afile
spawn,wcmd
;
return
end
