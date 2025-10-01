# Copyright 2025 Cancer Research Technology Limited.
#
# Licensed under a software academic use license provided with this software package (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at [link to package location of the license]
# For commercial use, please contact Cancer Research Horizons at crh-cpmarketing@cancer.org.uk
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.
#' Create dictionaries based on NCI thesaurus
#'
#' @description
#' Uses the `createDictionary` function to create `quanteda` dictionaries of cancer types, drugs, genes and molecular alterations.\cr
#' Dictionary keys are NCI thesaurus codes, dictionary values are NCI thesaurus synonyms.\cr
#' @param thesaurus (processed) NCI thesaurus.
#' @returns A list containing four dictionaries:
#' - `cancer_dict`: Entities of semantic type 'Neoplastic Process' .
#' - `drug_dict`: Entities of semantic type 'Pharmacologic Substance',
#'   'Biologically Active Substance', 'Clinical Drug', 'Steroid', 'Immunologic Factor',
#'   and 'Therapeutic or Preventive Procedure'.
#' - `gene_dict`: Entities of semantic type 'Gene or Genome'.
#' - `alteration_dict`: Entities of semantic type 'Cell or Molecular Dysfunction'.
#'
#' @export
createDictionaries = function(thesaurus) {

  ## cancer type dict
  cancer_dict = nciTools::createDictionary(source_data = thesaurus, semantic_types = 'Neoplastic Process')

  drug_dict = nciTools::createDictionary(source_data = thesaurus, semantic_types = c("Pharmacologic Substance", "Biologically Active Substance", "Clinical Drug", "Steroid", "Immunologic Factor", "Therapeutic or Preventive Procedure"))

  gene_dict = nciTools::createDictionary(source_data = thesaurus, semantic_types = 'Gene or Genome')

  alteration_dict = nciTools::createDictionary(source_data = thesaurus, semantic_types = 'Cell or Molecular Dysfunction')


  return(list(
    cancer_dict = cancer_dict,
    drug_dict = drug_dict,
    gene_dict = gene_dict,
    alteration_dict = alteration_dict
  )
  )

}
