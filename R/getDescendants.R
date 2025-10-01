# Copyright 2025 Cancer Research Technology Limited.
#
# Licensed under a software academic use license provided with this software package (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at [link to package location of the license]
# For commercial use, please contact Cancer Research Horizons at crh-cpmarketing@cancer.org.uk
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.
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
#' @seealso [dplyr::pull()], [base::unique()]
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
