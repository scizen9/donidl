;+
; NAME:
;     pgSqlCon
;
; PURPOSE:
;     A PostgreSQL database object.  Wraps libpq DLM functions.
;
; TYPE:
;     OBJECT
;
; CATEGORY:
;     Data structures, Database
;
; CALLING SEQUENCE:
;     myDb = OBJ_NEW ('pgSqlCon')
;
; INPUTS:
;     NONE
;
; KEYWORDS:
;     NONE
;
; DEPENDENCIES:
;     NONE
;
; PUBLIC METHODS:
;     
;  getTables (FUNCTION) - Gets a listing of tables availible given for a 
;                         connection
;         Inputs: None
;        Outputs: String array of table names
;       Keywords: None
;
;  Status (FUNCTION) - Determines if we have a valid connection
;         Inputs: None
;        Outputs: 0/1
;       Keywords: verbose
;
;  Connect (PROCEDURE) - Connect to a database
;         Inputs: PostgreSQL database connection string
;       Keywords: None
;
;  Disconnect (PROCEDURE) - Disconnect from a database
;         Inputs: None
;       Keywords: None
;
; EXAMPLE:
;
;     ; Create an instance of a PostgreSQL database
;     IDL> myDb=obj_new('pgSql')
;
; MODIFICATION HISTORY:
;
;     1.0 RHS - Jan 2005 Creation
;-


; METHODNAME:
;   pgSqlCon::Status
;
; PURPOSE:
;   Get the status of the result
;
; REQUIRED INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; INPUT KEYWORDS:
;   None
;
; OUTPUT:
;   0/1
;
; TYPE:
;   public
;
;-
function pgSqlCon::Status, VERBOSE=verbose

  if keyword_set(verbose) then verbose = 1 else verbose = 0

  status = 1
  status_str = PGSQL_PQstatus(self.connection_idx)

  if (strcmp(status_str, "CONNECTION_OK") ne 1) then begin

    status = 0

    if (verbose eq 1) then begin
      error = PGSQL_PQerrorMessage(self.connection_idx)
      message, "Result status is " + string(status,form='(i2)') + $
	       " (" + error + ")", /CONTINUE
    endif

  endif

  return, status

end

;--------------------------------------------------------------------------
; METHODNAME:
;   pgSqlCon::getTables
;
; PURPOSE:
;   Gets a listing of tables availible given for a connection
;
; REQUIRED INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; INPUT KEYWORDS:
;   None
;
; OUTPUT:
;   String array of availible tables or -1
;
; TYPE:
;   public
;
;-
function pgSqlCon::getTables

  compile_opt idl2

;  catch, error_status
;  if ( error_status ne 0 ) then begin
;    catch, /cancel
;    message, !error_state.msg, /continue
;    return, -1
;  endif    

  ; Check we're connected
  if (self->Status() ne 1) then message, "Not connected to a database."

  ; SELECT relname FROM pg_class WHERE relkind='<value>' AND relname !~ '^pg_'
  query = "SELECT relname FROM pg_stat_user_tables ORDER BY relname"

  objRes = obj_new("pgSqlCmd", self, query)
  n = objRes->Execute(query)

  if (n lt 1) then message, "No tables found"

  result = objRes->GetField("relname")

  obj_destroy, objRes

  return, result

end

;--------------------------------------------------------------------------
; METHODNAME:
;   pgSqlCon::Connect
;
; PURPOSE:
;   Connect a PostgreSQL database object
;
; REQUIRED INPUTS:
;   connection string
;
; OPTIONAL INPUTS:
;   None
;
; INPUT KEYWORDS:
;   None
;
; TYPE:
;   public
;
;-
function pgSqlCon::Connect, str

  ; Ensure the connection string is actually a string
  if size(str, /type) ne 7 then $
    message, "Passed argument must be a connection string."

  self.connection_str = str
  self.connection_idx = PGSQL_Connect(str)

  if (self.connection_idx lt 0) then return, 0 else return, 1

end

;--------------------------------------------------------------------------
; METHODNAME:
;   pgSqlCon::Disconnect
;
; PURPOSE:
;   Disconnect from a PostgreSQL database object
;
; REQUIRED INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; INPUT KEYWORDS:
;   None
;
; TYPE:
;   public
;
;-
pro pgSqlCon::Disconnect

  compile_opt idl2

  if (self.connection_idx ge 0) then PGSQL_Disconnect, self.connection_idx

end

;--------------------------------------------------------------------------
; METHODNAME:
;   pgSqlCon::GetIndex
;
; PURPOSE:
;   Returns the internal connection index
;
; REQUIRED INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; INPUT KEYWORDS:
;   None
;
; OUTPUT:
;   Connection index
;
; TYPE:
;   private
;
;-
function pgSqlCon::GetIndex
  
  compile_opt idl2, hidden

  return, self.connection_idx

end

;--------------------------------------------------------------------------
; METHODNAME:
;   pgSqlCon::Cleanup
;
; PURPOSE:
;   Cleanup a pgSqlCon object
;
; REQUIRED INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; INPUT KEYWORDS:
;   None
;
; TYPE:
;   private
;
;-
pro pgSqlCon::Cleanup

  compile_opt idl2, hidden

  if (self.connection_idx ge 0) then PGSQL_Disconnect, self.connection_idx

end

;--------------------------------------------------------------------------
; METHODNAME:
;   pgSqlCon::Init
;
; PURPOSE:
;   Initializes a PostgreSQL database object
;
; REQUIRED INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   str - the PostgreSQL connection string (see Connect method)
;
; INPUT KEYWORDS:
;   None
;
; TYPE:
;   private
;
;-
function pgSqlCon::init, str

  compile_opt idl2, hidden

  np = n_params(0)
  status = 1

  ; Default connection indicating no connection
  self.connection_idx = 0

  ; Optional if connection string is given
  if (np eq 1) then status *= self->Connect(str)

  return, status

end

;--------------------------------------------------------------------------
; CLASSNAME:
;   pgSqlCon
;
; PURPOSE:
;   Defines a pgSqlCon object
;
;-
pro pgSqlCon__define

  compile_opt idl2, hidden

  ; The container inheritance allows for a one to many relationship with
  ; results.  A single database connection can have many different commands
  ; being executed and results.  The issuing of each command results in
  ; 
  s2 = { pgSqlCon,               $  ; The class name
         connection_str: "",     $  ; Connection string
         connection_idx: 0L      $  ; Connection index
       }

end
