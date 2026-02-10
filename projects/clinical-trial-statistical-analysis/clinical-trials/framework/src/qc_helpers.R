# shared/src/qc_helpers.R

qc_assert <- function(condition, msg) {
  if (!isTRUE(condition)) stop(msg, call. = FALSE)
}

qc_write <- function(lines, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
}

qc_fmt_table <- function(x) paste(capture.output(print(x)), collapse = "\n")

# ---- Dataset QC (post-cleaning) ----
qc_dataset_cleaning <- function(df, required_cols, trt_col = "trt", endpoint_col = "event_bin") {
  
  qc_assert(is.data.frame(df), "QC: analysis dataset is not a data.frame.")
  qc_assert(all(required_cols %in% names(df)),
            paste0("QC: missing required columns: ",
                   paste(setdiff(required_cols, names(df)), collapse = ", ")))
  
  qc_assert(!anyDuplicated(df$usubjid), "QC: duplicate usubjid detected.")
  
  # Treatment checks
  qc_assert(trt_col %in% names(df), paste0("QC: missing treatment column: ", trt_col))
  qc_assert(!any(is.na(df[[trt_col]])), "QC: NA values present in treatment assignment.")
  qc_assert(all(levels(df[[trt_col]]) %in% c("Control", "Active")),
            "QC: treatment levels should include Control and Active.")
  
  # Endpoint checks
  qc_assert(endpoint_col %in% names(df), paste0("QC: missing endpoint column: ", endpoint_col))
  ok_endpoint <- all(is.na(df[[endpoint_col]]) | df[[endpoint_col]] %in% c(0L, 1L))
  qc_assert(ok_endpoint, "QC: endpoint must be coded as 0/1 (or NA).")
  
  # Summary objects to return
  list(
    n = nrow(df),
    n_by_trt = table(df[[trt_col]]),
    miss = sort(colSums(is.na(df)), decreasing = TRUE),
    event_rate_by_trt = tapply(df[[endpoint_col]], df[[trt_col]], mean, na.rm = TRUE)
  )
}

# ---- Table 1 QC (post-EDA) ----
qc_table1 <- function(df, tbl_vars, trt_col = "trt") {
  
  qc_assert(trt_col %in% names(df), paste0("QC: missing treatment column: ", trt_col))
  qc_assert(all(tbl_vars %in% names(df)),
            paste0("QC: Table 1 variables missing: ",
                   paste(setdiff(tbl_vars, names(df)), collapse = ", ")))
  
  n_by_trt <- table(df[[trt_col]])
  
  # Spot-check calculations (returned for logging)
  cont_checks <- lapply(tbl_vars[sapply(df[tbl_vars], is.numeric)], function(v) {
    data.frame(
      var = v,
      trt = names(tapply(df[[v]], df[[trt_col]], mean, na.rm = TRUE)),
      mean = as.numeric(tapply(df[[v]], df[[trt_col]], mean, na.rm = TRUE)),
      sd   = as.numeric(tapply(df[[v]], df[[trt_col]], sd, na.rm = TRUE))
    )
  })
  
  cat_checks <- lapply(tbl_vars[sapply(df[tbl_vars], function(x) is.factor(x) || is.character(x))], function(v) {
    tab <- table(df[[v]], df[[trt_col]], useNA = "ifany")
    prop <- prop.table(tab, margin = 2)
    list(var = v, counts = tab, perc = round(100 * prop, 1))
  })
  
  list(
    n_by_trt = n_by_trt,
    cont_checks = cont_checks,
    cat_checks = cat_checks
  )
}
