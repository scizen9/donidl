pro get_asiago_cat
;
; check for original file
afile=!SNE_DATA+'/asiago.txt'
filestamp,afile,/arch,/verbose
wcmd = 'wget -F "http://graspa.oapd.inaf.it/asnc/cat.txt" -O '+afile
;wcmd = 'wget -F "http://graspa.oapd.inaf.it/index.php?option=com_content&view=article&id=63&Itemid=82&dir=%2Fvar%2Fwww%2Fhtml%2Fjsmallfib_top%2Fcat&download_file=jsmallfib_top%2Fcat%2Fcat.txt" -O '+afile
;wcmd = 'wget -F "http://web.oapd.inaf.it/supern/cat/cat.txt" -O '+afile
spawn,wcmd
;
return
end
