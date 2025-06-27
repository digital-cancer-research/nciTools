#' Get NCI thesaurus codes from preferred terms
#' @description
#' Given one or more preferred terms (i.e. first synonyms in NCI thesaurus), look up relevant NCI thesaurus codes.
#'
#' @param PTs the preferred term(s)
#' @param db_connection connection to a database that includes the thesaurus_flat table
#' @returns Matching NCI thesaurus code(s)
#' @export
getCodes <- function(PTs, db_connection) {
  formatted_PTs = sprintf('"%s"', PTs) |>
    toString()

  query = 'SELECT DISTINCT code FROM thesaurus_flat WHERE PT IN (%s)' |>
    sprintf( formatted_PTs)

  codes = DBI::dbGetQuery(db_connection, query) |>
    dplyr::pull(code)

  return(codes)
}
