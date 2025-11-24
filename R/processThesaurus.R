# Copyright 2025 Cancer Research Technology Limited.
#
# Licensed under a software academic use license provided with this software package (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at [link to package location of the license]
# For commercial use, please contact Cancer Research Horizons at crh-cpmarketing@cancer.org.uk
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.
#' Process the downloaded NCI Thesaurus
#'
#' @description
#' This function processes the NCI Thesaurus from a flat file format, applying a series of transformations to:
#' filter obsolete concepts,
#' extract key information,
#' prepare the data for analysis.
#'
#' The transformations include normalising synonyms, identifying solid neoplasm children, and handling generic entities and synonyms.
#'
#' @param thesaurus_file The path to the downloaded (raw) version of the NCI Thesaurus.
#'
#' @return Returns a list containing three dataframes:
#' - `thesaurus`: The processed NCI Thesaurus data with relevant fields extracted and
#'   formatted for analysis, including codes, preferred terms (PT), parents, synonyms,
#'   and semantic types.
#' - `parents`: A table mapping codes to their parent codes to facilitate hierarchical
#'   analysis of the thesaurus entries.
#' - `synonyms`: A table of synonyms for each code, expanded from the compressed format
#'   in the source file to aid in text matching and lookup tasks.
#'
#' @details
#' The NCI Thesaurus is processed through several steps to make it suitable for analysis:
#' - Column names are set.
#' - A new column 'PT' (preferred term) is added.
#' - The first synonym is used as the PT for each entity.
#' - Obsolete and retired concepts are removed.
#' - New parent-child relationships are added, e.g. between C9292 Solid Neoplasm and various solid tumour types.
#' - Generic entities and synonyms that could lead to ambiguous matches are removed.
#' - New synonyms are added where appropriate.
#' - Parent-child relationships and synonyms are extracted into separate tables for convenient reuse in the pipeline.
#' @export
processThesaurus = function(NCIt) {


  # names(NCIt) <- c("code", "concept_IRI", "parents", "synonyms", "definition", "display_name", "concept_status", "semantic_type", "concept_in_subset")

  names(NCIt)[1:8] <- c("code", "concept_IRI", "parents", "synonyms", "definition", "display_name", "concept_status", "semantic_type")

  ## as per https://evs.nci.nih.gov/ftp1/NCI_Thesaurus/ReadMe.txt
  ## The first entry in the "synonyms" field is the preferred name of the concept.

  NCIt <- NCIt |>
    dplyr::filter(!grepl(pattern = 'Obsolete_Concept|Retired_Concept', x=NCIt$concept_status) ) |>
    dplyr::mutate(PT = gsub("\\|.*", "", synonyms)) |>  ##  use the first synonym as PT
    dplyr::select(code, PT, parents, synonyms, semantic_type) |>
    unique() |>
    dplyr::mutate(parents = strsplit(parents, split = '\\|')) |>
    dplyr::mutate(synonyms = strsplit(synonyms, split = '\\|')) |>
    dplyr::mutate(semantic_type = strsplit(semantic_type, split = '\\|'))

  ## parents, synonyms and semantic_type are now list-columns - need to unnest or use e.g. purrr::map for filtering etc

  ## specify solid neoplasms as a parent of all children of C3263 Neoplasm By Site, except C35813 Hematopoietic and Lymphatic System Neoplasm....
  ## also, all children of C4741 Neoplasm by Morphology except C27134 Hematopoietic and Lymphoid Cell Neoplasm
  solid_neoplasm_children <- NCIt |>
    dplyr::filter(purrr::map_lgl(parents, ~ any(c('C3263', 'C4741') %in% .))) |>
    merge(by.x = 'code', y = unique(dplyr::select(NCIt, code, PT)), by.y = 'code') |>
    dplyr::filter(!(code %in% c('C35813', 'C27134'))) |>
    dplyr::select(code, PT.x, parents) |>
    unique()

  solid_neoplasm_children

  for(i in 1:nrow(solid_neoplasm_children)) {
    child_code = solid_neoplasm_children$code[i]

    original_parents = NCIt |>
      dplyr::filter(code == child_code) |>
      dplyr::pull(parents) |>
      unlist()

    ## append C9292 (Solid Neoplasm) to original parents
    updated_parents = c(original_parents, 'C9292', 'C8101') |>       ## include C9292 Solid Neoplasm and C8101 Adult Solid Neoplasm
      unique() |>
      list()

    NCIt$parents[NCIt$code == child_code] = updated_parents

  }


  ## add  'RAS Q61X' as a synonym of C177785 Activating RAS Q61X
  NCIt <- NCIt |>
    dplyr::mutate(
      synonyms = purrr::map_if(synonyms, code == "C177785", ~ c(.x, "RAS Q61X"))
    )

  # Add parent-child relationships from C177785 to:
  # C203325 HRAS NP_005334.1:p.Q61X
  NCIt <- NCIt |>
    dplyr::mutate(
      parents = purrr::map_if(parents, code == "C203325", ~ c(.x, "C177785"))
    )
  # C107478 KRAS NP_004976.2:p.Q61X
  NCIt <- NCIt |>
    dplyr::mutate(
      parents = purrr::map_if(parents, code == "C107478", ~ c(.x, "C177785"))
    )
  # C107485 NRAS NP_002515.1:p.Q61X
  NCIt <- NCIt |>
    dplyr::mutate(
      parents = purrr::map_if(parents, code == "C107485", ~ c(.x, "C177785"))
    )



  generic_entities <- c('C36541', 'C178120', 'C97927', 'C9484', 'C93102', 'C3420', 'C19296', 'C45576', 'C45581', 'C45584', 'C171188', 'C20195', 'C19798', "C17354", "C45596", 'C177682', 'C165233', 'C189957', 'C18093', 'C189956', 'C36280', 'C177693', 'C177694', 'C18060', 'C36391')

  ## preview what will be removed
  NCIt |>
    dplyr::filter(code %in% generic_entities) |>
    dplyr::select(code, PT)

  generic_synonyms <- c("Positive", "POSITIVE", "Yes", "Negative", "NEGATIVE", "Normal", 'Amplified')

  ## preview what will be removed
  NCIt |>
    tidyr::unnest(cols = 'synonyms') |>
    as.data.frame() |>
    dplyr::filter(synonyms %in% generic_synonyms) |>
    dplyr::select(code, PT, synonyms)

  NCIt_pruned <- NCIt |>
    dplyr::filter(!code %in% generic_entities) |>        ## delete generic entities entirely
    dplyr::mutate(synonyms = purrr::map(synonyms, ~ setdiff(.x, generic_synonyms))) |>        ## drop generic synonyms for each entity (but keep non generic synonyms for each entity)
    # dplyr::mutate(synonyms = purrr::map(synonyms, ~ gsub("\\(|\\)", "", .x))) |>   ## delete any parentheses within synonyms values, as they cause issues with dictionary lookup
    dplyr::mutate(synonyms = purrr::map(synonyms, ~ .x[!stringr::str_detect(.x, "\\(|\\)")])) |>    ## omit any rows where synonyms value contains parentheses, as these cause issues with later use as dictionary
    dplyr::mutate(
      synonyms = dplyr::if_else(condition = code == "C118396", true = purrr::map(synonyms, ~ .x[.x != "TP53"]), false = synonyms)
    ) |> ## delete 'TP53' as a synonym of TP53 gene mutation
    dplyr::mutate(
      synonyms = dplyr::if_else(condition = code == "C71428", true = purrr::map(synonyms, ~ .x[!(.x %in% c('triple-negative breast cancer', 'TNBC'))]), false = synonyms)
    ) |>     ## delete this as a synonym of TNBC finding (to avoid clash with TNBC as cancer type)
    dplyr::mutate(
      synonyms = dplyr::if_else(condition = code == "C118809", true = purrr::map(synonyms, ~ .x[.x != "Breast Cancer"]), false = synonyms)
    ) |> ## delete 'Breast Cancer' as a synonym of Childhood Breast Carcinoma
    dplyr::mutate(
      synonyms = dplyr::if_else(condition = code == "C96271", true = purrr::map(synonyms, ~ .x[.x != "PIK3CA"]), false = synonyms)
    ) |> ## delete 'PIK3CA' as a synonym of PIK3CA Gene Mutation
    dplyr::mutate(
      synonyms = dplyr::if_else(condition = code == "C116397", true = purrr::map(synonyms, ~ .x[.x != "Exon 12"]), false = synonyms)
    ) |> ## delete 'Exon 12' as a synonym of PDGFRA Exon 12 Mutation
    dplyr::mutate(
      synonyms = dplyr::if_else(condition = code == "C19635", true = purrr::map(synonyms, ~ .x[.x != "BRCA1"]), false = synonyms)
    ) |> ## delete 'BRCA1' as a synonym of BRCA1 Gene Mutation
    dplyr::mutate(
      synonyms = dplyr::if_else(condition = code == "C2194", true = purrr::map(synonyms, ~ .x[tolower(.x) != "sorafenib"]), false = synonyms)
    ) |> ## delete 'sorefenib' as a synonym of Sorafenib tosylate
    dplyr::mutate(
      synonyms = dplyr::if_else(condition = code == "C9107", true = purrr::map(synonyms, ~ .x[.x != "Solid Tumor"]), false = synonyms)
    ) |> ## delete 'Solid tumor' as a synonym of Childhood Solid Neoplasm
    dplyr::mutate(
      synonyms = dplyr::if_else(condition = code == "C8101", true = purrr::map(synonyms, ~ .x[.x != "Solid Tumor"]), false = synonyms)
    ) |> ## delete 'Solid tumor' as a synonym of Adult Solid Neoplasm
    unique()



  # return(NCIt_pruned)
  ## create a table of parent-child relationships
  NCIt_parents = NCIt_pruned |>
    dplyr::select(code, parents) |>
    tidyr::unnest(cols = 'parents') |>
    as.data.frame() |>
    unique()

  ## create a table of synonyms
  NCIt_synonyms = NCIt_pruned |>
    dplyr::select(code, synonyms) |>
    tidyr::unnest(cols = 'synonyms') |>
    as.data.frame() |>
    unique()


  return(list(
    thesaurus = NCIt_pruned,
    parents = NCIt_parents,
    synonyms = NCIt_synonyms
  ))
}
