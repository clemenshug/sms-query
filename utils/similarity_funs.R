tas_weighted_jaccard <- function(query_ids, target_ids = NULL, min_n = 6) {
  query_ids <- convert_compound_ids(query_ids)
  target_ids <- convert_compound_ids(target_ids)

  query_tas <- data_tas[
    lspci_id %in% query_ids,
    .(query_lspci_id = lspci_id, lspci_target_id, tas)
  ] %>%
    unique()
  target_tas <- data_tas[
    if (is.null(target_ids)) TRUE else lspci_id %in% target_ids,
    .(target_lspci_id = lspci_id, lspci_target_id, tas)
  ] %>%
    unique()
  target_tas[
    query_tas,
    on = .(lspci_target_id),
    nomatch = NULL,
    allow.cartesian = TRUE
  ][
    ,
    mask := tas < 10 | i.tas < 10
  ][
    ,
    if (sum(mask) >= min_n) .(
      "tas_similarity" = sum(pmin(tas[mask], i.tas[mask])) / sum(pmax(tas[mask], i.tas[mask])),
      "n_tas" = sum(mask),
      "n_prior_tas" = .N
    ) else .(
      tas_similarity = double(),
      n_tas = integer(),
      n_prior_tas = integer()
    ),
    by = .(query_lspci_id, target_lspci_id)
  ] %>%
    merge_compound_names()
}

chemical_similarity <- function(query_ids, target_ids = NULL) {
  query_ids <- convert_compound_ids(query_ids)
  target_ids <- convert_compound_ids(target_ids)

  query_ids %>%
    set_names() %>%
    map(
      ~data_fingerprints$tanimoto_all(.x) %>%
        setDT() %>% {
          .[
            if (is.null(target_ids)) TRUE else id %in% target_ids
          ]
        }
    ) %>%
    rbindlist(idcol = "query_lspci_id") %>% {
      .[
        ,
        .(
          query_lspci_id = as.integer(query_lspci_id),
          target_lspci_id = id,
          structural_similarity
        )
      ]
    } %>%
    merge_compound_names()
}

phenotypic_similarity <- function(query_ids, target_ids = NULL, min_n = 6) {
  query_ids <- convert_compound_ids(query_ids)
  target_ids <- convert_compound_ids(target_ids)

  query_pfps <- data_pfp[
    lspci_id %in% query_ids
  ] %>%
    unique()
  target_pfps <- data_pfp[
    if (is.null(target_ids)) TRUE else lspci_id %in% target_ids
  ] %>%
    unique()

  merge(
    query_pfps,
    target_pfps,
    by = "assay_id",
    all = FALSE,
    suffixes = c("_1", "_2")
  )[
    ,
    mask := abs(rscore_tr_1) >= 2.5 | abs(rscore_tr_2) >= 2.5
  ][
    ,
    if(sum(mask) >= min_n) .(
      "phenotypic_correlation" = cor(rscore_tr_1, rscore_tr_2),
      "n_pfp" = sum(mask),
      "n_prior_pfp" = .N
    ) else .(
      phenotypic_correlation = double(),
      n_pfp = integer(),
      n_prior_pfp = integer()
    ),
    by = .(lspci_id_1, lspci_id_2)
  ] %>%
    setnames(
      c("lspci_id_1", "lspci_id_2"),
      c("query_lspci_id", "target_lspci_id")
    )
}

find_compound_ids <- function(compound_names) {
  if (is.null(compound_names))
    return(NULL)
  key_matches <- reduce(
    compound_names,
    function(df, query) {
      matches <- str_detect(
        data_compound_names[["name"]], fixed(query, ignore_case = TRUE)
      )
      df[
        ,
        `:=`(
          matched = matched | matches,
          original_query = magrittr::inset(original_query, matches, query)
        )
      ]
    }, .init = copy(data_compound_names)[
      , `:=`(matched = FALSE, original_query = NA_character_)
    ]
  )
  key_matches[
    matched == TRUE
  ][
    ,
    match_len := str_length(name)
  ][
    order(
      match_len
    )
  ][
    ,
    head(.SD, n = 1),
    by = .(lspci_id),
    .SDcols = c("name", "original_query")
  ] %>%
    unique()
}

merge_compound_names <- function(df) {
  reduce(
    array_branch(str_match(names(df), "^(.*)lspci_id$"), margin = 1),
    function(df, match) {
      lspci_id_col <- match[1]
      compound_col <- paste0(match[2], "compound")
      if (any(is.na(c(lspci_id_col, compound_col))))
        return(df)
      merge(
        df,
        data_compounds[lspci_id %in% df[[lspci_id_col]]][
          , .(lspci_id, pref_name)
        ] %>%
          setnames("pref_name", compound_col),
        by.x = lspci_id_col, by.y = "lspci_id", all = FALSE
      )
    }, .init = df
  )
}

convert_compound_ids <- function(ids) {
  if (is.null(ids))
    NULL
  else if (is.numeric(ids))
    # Assume it's already lspci_ids
    set_names(ids)
  else {
    find_compound_ids(ids)[["lspci_id"]] %>%
      with(set_names(lspci_id, original_query))
  }
}
