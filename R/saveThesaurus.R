#' Save annotated trial data to SQLite database
#'
#' @description
#' This is the final step in the trial annotation pipeline.\cr
#' Data are saved to a local SQLite database for ingestion by user interfaces.
#' @param thesaurus_processed A list of dataframes containing processed information from NCI thesaurus.
#' @param db_connection connection to a SQLite database
#' @returns Returns the relative path to the populated SQLite database
#' @details list-columns are flattened into pipe-delimited strings
#' @export
saveThesaurus = function(thesaurus_processed, db_connection) {
  ## flatten list-columns in the thesaurus
  thesaurus_flat <- thesaurus_processed$thesaurus |>
    dplyr::mutate(
      across(c(parents, synonyms, semantic_type), ~ purrr::map_chr(., ~ paste(., collapse = "|")))
    )

  parents = thesaurus_processed$parents
  synonyms = thesaurus_processed$synonyms

  ## write tables to DB
  DBI::dbWriteTable(conn = db_connection,name = "thesaurus_flat", thesaurus_flat , overwrite=TRUE)
  DBI::dbWriteTable(conn = db_connection,name = "parents", parents , overwrite=TRUE)
  DBI::dbWriteTable(conn = db_connection,name = "synonyms", synonyms , overwrite=TRUE)

  ## disconnect
  DBI::dbDisconnect(db_connection)

  ## return path to populated db
  # return(db_path)
  return(NULL)
}
