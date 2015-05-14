
MySQL DataBase (mdb_*) Routines for IDL

by Marshall Perrin <mperrin@berkeley.edu> 
and heavily relying on routines by Mark Buie at Lowell.


Mark Buie wrote the basic IDL/MySQL bridge code, which works by piping text
back and forth between IDL and a text-mode mysql connection, and Marshall
Perrin built on top of that a series of routines to implement a database of
stars including SEDs, other properties, observation logs, and so on.


First, the generic MYSQL routines.
---- Routines by Mark Buie: ----

OPENMYSQL	opens a mysql database and returns a LUN to the database pipe.
MYSQLCMD	execute a command
MYSQLQUERY	execute a query and return the result as IDL variables (kind of like readcol)


---- Routines by Marshall Perrin: ----

MYSQLCHECK		wrapper for openmysql. Open a LUN if necessary, don't if already open.
IS_MYSQL_PRESENT	use the database or not?

   ---- routines which implement an object- and observation- tracking database using the above ----

    - routines to put information into the database
mdb_add2mass
mdb_addcompanion
mdb_addflux
mdb_addfluxfromfile
mdb_addfluxfromfile2
mdb_addiras
mdb_addproperty
mdb_addpropfromfile
mdb_addspectraltype
mdb_addzeropoint
mdb_insert_object
mdb_recordobs
mdb_rename
mdb_updatespt
mdb_markdone

    - routines to get information out of the database
mdb_checknames
mdb_getcoords
mdb_getmag
mdb_getprop
mdb_getzeropoint
mdb_listobs
mdb_nameresolve
mdb_sed			plot sed
mdb_simbadcoords
mdb_surveylist

    - backup database to text file
mdb_backup.sh

    - helper routines to get information out of Vizier
query_2mass
query_usnob
query_iras


====================================================


Quick tutorial for the genericl mysql access:
   (see the various mdb_* routines for more examples)




mysqlcheck, SQLHandle	; opens handle if necessary

c = "Some SQL Command"

mysqlcmd, SQLHandle, c	; executes the command

q2 = "Some SQL Query"

mysqlquery, SQLHandle, q, col1, col2, col3, col4, format='A,A,I,F' ; or whatever the right format is
	; results end up in the col1, col2, etc variables. 




