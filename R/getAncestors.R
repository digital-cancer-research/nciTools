#' Retrieve Ancestors of NCI Thesaurus Terms
#'
#' Given a set of codes, finds all ancestor concepts (e.g., parents, grandparents) from the NCI Thesaurus hierarchy.
#'
#' @details
#' This function builds and executes a recursive SQL query to retrieve all ancestor nodes for a given list of concept codes.\cr
#' Ancestors are identified using a parent-child relationship table (`NCIt_parents`).\cr
#' The results are then merged with a pruned thesaurus (`NCIt_pruned`) to attach preferred terms (`PT`) for easier interpretation.
#'
#' Internally, the function:
#' - Quotes and formats the codes for SQL `IN` syntax\cr
#' - Uses a recursive CTE (Common Table Expression) to traverse upward through the concept hierarchy\cr
#' - Merges results with the pruned thesaurus for human-readable output
#'
#' @param codes A character vector of NCI Thesaurus concept codes whose ancestors should be retrieved.
#' @param db_connection connection to a database that includes the `thesaurus_flat` and `parents` tables
#' @returns A data frame of ancestor concepts, including their codes and preferred terms.
#' @seealso [sqldf::sqldf()], [dplyr::select()], [base::merge()]
#' @export
getAncestors <- function(codes, db_connection) {
  formatted_codes = sprintf('"%s"', codes) |>
    toString()

  ## recursive SQL query on parents table...
  ## include a measure of distance from input codes
  query <- 'WITH RECURSIVE ancestors AS (
    SELECT code AS a, 0 AS distance
    FROM parents
    WHERE code IN (%s)
    UNION ALL
    SELECT parents.parents, ancestors.distance + 1
    FROM ancestors
    JOIN parents ON ancestors.a = parents.code
  )   SELECT DISTINCT tf.code, tf.PT, distance FROM ancestors anc INNER JOIN thesaurus_flat tf ON anc.a = tf.code ORDER BY distance;' |>
    sprintf( formatted_codes)

  ancestors = DBI::dbGetQuery(db_connection, statement = query)

  return(ancestors)
}
