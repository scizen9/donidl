#!/bin/csh
set MYSQLARGS = " "
foreach  tbl ( filters observations otherproperties papers photometry sources alternatenames zeropoints companions )
        mysqldump -u mperrin astronomy $tbl $MYSQLARGS
end

