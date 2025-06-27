#' Retrieve Descendants of NCI Thesaurus Terms
#'
#' Given a set of codes, finds all descendant concepts (e.g., children, grandchildren) from the NCI Thesaurus hierarchy.
#'
#' @details
#' This function builds and executes a recursive SQL query to retrieve all descendant nodes for a given list of concept codes.\cr
#' Descendants are identified using a parent-child relationship table (`NCIt_parents`).\cr
#' The results are returned as a unique character vector of descendant codes.
#'
#' Internally, the function:
#' - Quotes and formats the input codes for SQL `IN` syntax\cr
#' - Uses a recursive CTE (Common Table Expression) to traverse downward through the concept hierarchy\cr
#' - Returns a de-duplicated vector of descendant concept codes
#'
#' @param codes A character vector of NCI Thesaurus concept codes whose descendants should be retrieved.
#' @param db_connection connection to a database that includes the `thesaurus_flat` and `parents` tables
#' @return A character vector of descendant concept codes.
#'
#' @seealso [sqldf::sqldf()], [dplyr::pull()], [base::unique()]
#' @export
getDescendants <- function(codes, db_connection) {
  formatted_codes = sprintf('"%s"', codes) |>
    toString()

  query <- 'WITH RECURSIVE children AS (
    -- Base Case: Start with given parent codes
    SELECT code AS c, 0 AS distance
    FROM parents
    WHERE code IN (%s)

    UNION ALL

    -- Recursive Case: Find children of current level
    SELECT p.code, ch.distance + 1
    FROM children ch
    JOIN parents p ON ch.c = p.parents  -- Link child (code) to parent (parents)
  )
  SELECT DISTINCT tf.code, tf.PT, ch.distance
  FROM children ch
  LEFT JOIN thesaurus_flat tf ON ch.c = tf.code
  ORDER BY ch.distance;' |>
    sprintf( formatted_codes)

  descendants = DBI::dbGetQuery(db_connection, statement = query)

  return(descendants)
}
