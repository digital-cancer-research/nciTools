# Copyright 2025 Cancer Research Technology Limited.
#
# Licensed under a software academic use license provided with this software package (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at [link to package location of the license]
# For commercial use, please contact Cancer Research Horizons at crh-cpmarketing@cancer.org.uk
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.
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
