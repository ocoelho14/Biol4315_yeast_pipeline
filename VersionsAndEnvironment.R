#!/usr/bin/env Rscript

## Report versions for Sequali, QUAST, Flye, Filtlong, BUSCO
## Works even when tools are inside a conda env and not on PATH.

# ------------ Helper: get version text ----------------

get_version_from_path <- function(path, version_args = c("--version", "")) {
  if (!file.exists(path)) {
    return(list(found = FALSE, version = NA_character_))
  }
  
  for (va in version_args) {
    args <- if (va == "") character(0) else va
    
    out <- tryCatch(
      system2(path, args = args, stdout = TRUE, stderr = TRUE),
      error = function(e) character()
    )
    
    if (length(out) > 0) {
      first <- trimws(out[trimws(out) != ""][1])
      if (nzchar(first)) {
        return(list(found = TRUE, version = first))
      }
    }
  }
  
  return(list(found = TRUE, version = NA_character_))
}

# ------------ Known tool locations --------------------
# Adjust these if needed, but these match your environment.

tool_paths <- list(
  sequali  = "/Users/ocoelho00/miniconda3/envs/sequali_env/bin/sequali",
  filtlong = "/Users/ocoelho00/miniconda3/envs/sequali_env/bin/filtlong",
  chopper  = "/Users/ocoelho00/miniconda3/envs/sequali_env/bin/chopper",
  flye     = "/Users/ocoelho00/miniconda3/envs/sequali_env/bin/flye",
  
  # QUAST is usually installed here in conda:
  quast    = "/Users/ocoelho00/miniconda3/envs/sequali_env/bin/quast.py",
  
  # BUSCO (python entry point):
  busco    = "/Users/ocoelho00/miniconda3/envs/sequali_env/bin/busco"
)

# ------------ Collect versions ------------------------

tool_rows <- lapply(names(tool_paths), function(name) {
  res <- get_version_from_path(tool_paths[[name]])
  data.frame(
    tool      = name,
    path      = tool_paths[[name]],
    found     = res$found,
    version   = res$version,
    stringsAsFactors = FALSE
  )
})

tool_df <- do.call(rbind, tool_rows)

# ------------ Environment info ------------------------

env_info <- list(
  R_version   = R.version$version.string,
  Platform    = R.version$platform,
  OS          = R.version$os,
  System      = Sys.info()[["sysname"]],
  Release     = Sys.info()[["release"]],
  Machine     = Sys.info()[["machine"]],
  NodeName    = Sys.info()[["nodename"]],
  R_home      = R.home(),
  CPU_cores   = tryCatch(parallel::detectCores(), error = function(e) NA_integer_)
)

# ------------ Print results ---------------------------

cat("=== Computational Environment ===\n")
print(env_info)
cat("\n")

cat("=== Tool Versions ===\n")
print(tool_df, row.names = FALSE)
cat("\n")
cat("Notes:\n")
cat("- Versions come directly from each binary inside your conda env.\n")
cat("- 'found = TRUE' means the tool exists at the specified path.\n")
cat("- If any show found=FALSE, fix the path in tool_paths.\n")
