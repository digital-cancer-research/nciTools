# Copyright 2025 Cancer Research Technology Limited.
#
# Licensed under a software academic use license provided with this software package (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at [link to package location of the license]
# For commercial use, please contact Cancer Research Horizons at crh-cpmarketing@cancer.org.uk
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.
#' Expand Molecular Dysfunctions
#' @description
#' Expand a set of NCI thesaurus codes related to cell/molecular dysfunctions to include all protein and DNA terms, and all of their ancestors.\cr
#' For example, `C98365` (KRAS G12C, a gene product variation) is expanded to include the respective gene variation(s) (e.g. `C98366`), plus all their ancestors (e.g. `C135715` KRAS exon 2 mutation, and `C98362` KRAS Protein Variant etc)
#' @param codes One or more codes for NCI thesaurus entities of semantic type `Cell or Molecular Dysfunction`.
#' @param db_connection connection to database that includes thesaurus used to get ancestors.
#' @returns The expanded set of codes
#' @details Relationships described by `Gene_Product_Sequence_Variation_Encoded_By_Gene_Mutant` and `Gene_Mutant_Encodes_Gene_Product_Sequence_Variation` are added to the specified codes, and then all codes are expanded to include all ancestor terms
#' @seealso \code{\link{getNCIt_relationships}} get relationships from NCI thesaurus API.
#' @seealso \code{\link{getAncestors}} get higher level terms
#' @export
expandAlterations <- function(codes, db_connection) {
  # ensure that both DNA and protein codes are included
  alteration_codes = codes
  for(i in 1:length(alteration_codes)) {
    relations = nciTools::getNCIt_relationships(alteration_codes[i], include_what = 'inverseRoles')
    if(!is.null(relations)) {
      relation_codes = relations |>
        dplyr::filter(type %in% c('Gene_Product_Sequence_Variation_Encoded_By_Gene_Mutant', 'Gene_Mutant_Encodes_Gene_Product_Sequence_Variation')) |>
        dplyr::pull(relatedCode) |>
        unique()

      alteration_codes = unique(c(alteration_codes, relation_codes))   }
  }

  ## get ancestors
  ancestors = nciTools::getAncestors(codes = alteration_codes, db_connection = db_connection) |>
    dplyr::pull(code) |>
    unique()

  ## combine and return
  alteration_codes = unique(c(alteration_codes, ancestors))
  return(alteration_codes)

}
