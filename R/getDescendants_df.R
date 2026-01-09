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
#' Internally, the function uses a recursive CTE (Common Table Expression) to traverse downward through the concept hierarchy\cr
#'
#' @param codes A character vector of NCI Thesaurus concept codes whose descendants should be retrieved.
#' @param parents_df a dataframe with columns `code` and `parents` as a source of parent-child relationships
#' @return A dataframe with descendant codes and distance (number of hops from source codes).
#'
#' @seealso [dplyr::pull()], [base::unique()]
#' @export
getDescendants_df <- function(codes, parents_df) {
  temp_conn = DBI::dbConnect(RSQLite::SQLite())

  # Write table into temp DB
  DBI::dbWriteTable(temp_conn, "parents", parents_df, overwrite = TRUE)

  ## parameterised query
  query = 'WITH RECURSIVE children AS (
        SELECT code , 0 AS distance
        FROM parents
        WHERE code IN ($1)

        UNION ALL

        SELECT p.code, ch.distance + 1
        FROM children ch
        JOIN parents p ON ch.code = p.parents
    )
    SELECT DISTINCT code, distance
    FROM children ch
    ORDER BY ch.distance;'

  descendants = DBI::dbGetQuery(conn = temp_conn, statement = query, params = list(codes))

  return(descendants)
}
