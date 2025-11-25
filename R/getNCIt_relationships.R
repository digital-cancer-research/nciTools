# Copyright 2025 Cancer Research Technology Limited.
#
# Licensed under a software academic use license provided with this software package (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at [link to package location of the license]
# For commercial use, please contact Cancer Research Horizons at crh-cpmarketing@cancer.org.uk
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.
#' Get NCIt Relationships from NCI Thesaurus API
#'
#' This function queries the NCI Thesaurus to retrieve relationships based on a given code and inclusion criteria.
#' It fetches information from the National Cancer Institute's EVS API for a given concept and returns the specified relationships.
#'
#' @param code A character string representing the NCIt code (concept ID) for which to retrieve relationships.
#' @param include_what A character string indicating the type of relationships to include. Valid options are:
#'   `roles`, `inverseRoles`, `associations`, or `inverseAssociations` `minimal`, `summary`, `full`, `associations`, `children`, `definitions`, `disjointWith`, `history`, `inverseAssociations`, `inverseRoles`, `maps`, `parents`, `properties`, `roles`, `synonyms`
#'   This determines which relationship information will be returned for the specified code.
#'
#' @return A data frame containing the specified relationships for the NCIt code. If no relationships are found, it returns `NULL`.
#'
#' @details
#' This function constructs a request to the NCI EVS API endpoint to search for the specified NCIt code and includes the relationships
#' based on the `include_what` parameter. The result is parsed into a tibble (data frame), with each relationship type presented
#' in separate columns.
#'
#' Examples for `include_what`:
#'
#' - `roles`: Returns roles associated with the concept.
#' - `inverseRoles`: Returns inverse roles related to the concept.
#' - `associations`: Returns associations linked to the concept.
#' - `inverseAssociations`: Returns inverse associations for the concept.
#'
#' @import httr
#' @import tibble
#' @import tidyr
#'
#' @seealso \url{https://evsexplore.semantics.cancer.gov/evsexplore/evsapi}
#' @seealso \url {https://github.com/NCIEVS/evsrestapi-client-SDK/blob/main/doc/INCLUDE.md}
#' @export
getNCIt_relationships <- function(code, include_what) {
  ## set base parameters for querying NCI thesaurus API endpoint
  base_url <- 'https://api-evsrest.nci.nih.gov'
  endpoint <- "/api/v1/concept/ncit/search"
  evs_url <- paste0(base_url, endpoint)

  query_params <- list(
    term = code,
    type = "match",
    include = include_what,    ## only options allowed are c('roles', 'inverseRoles', 'associations', 'inverseAssociations')
    pageSize = 1
  )

  # Make the GET request with query parameters
  response <- httr::GET(evs_url, query = query_params)
  # Parse the response content
  content <- httr::content(response, "parsed")

  ## content is a named list
  ## subset it based on the include_what argument

  result = content$concepts[[1]][[include_what]]

  if(!is.null(result)) {
    dataframe = result |>
      tibble::tibble() |>
      tidyr::unnest_wider(result) |>
      # dplyr::filter(relatedCode != 'C36347') |>          ## NCIt erroneously includes many Gene_Mutant_Encodes_Gene_Product_Sequence_Variation relationships to t(8;14)(q24;q11)
      as.data.frame()

    return(dataframe)
  }  else return(result)

}
