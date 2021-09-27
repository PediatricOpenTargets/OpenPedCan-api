# This file defines a function connect_db to return a DBIConnection to
# OpenPedCan-api database.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls source("db/r_interfaces/connect_db.R")
#
# Defined variables:
#
# - connect_db



# Connect to OpenPedCan-api database
#
# Args:
# - db_env_vars: a list of environment variable values defined in
#   db/r_interfaces/db_env_vars.R.
#
# Returns a DBIConnection to OpenPedCan-api database.
#
# Notes:
#
# - The environment variable values in db_env_vars, which is defined in
#   db/r_interfaces/db_env_vars.R, are passed to the database building process
#   or API HTTP server at deployment time.
# - The returned connection needs to be disconnected by DBI::dbDisconnect.
connect_db <- function(db_env_vars) {
  conn <- DBI::dbConnect(
    odbc::odbc(), Driver = db_env_vars$Driver,
    Server = db_env_vars$Server, Port = db_env_vars$Port,
    Uid = db_env_vars$Uid, Pwd = db_env_vars$Pwd,
    Database = db_env_vars$Database)

  return(conn)
}
