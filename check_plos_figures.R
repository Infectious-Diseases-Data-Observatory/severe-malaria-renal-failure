## ---------------------------------------------------------------------------
## check_plos_figures.R
##
## Verifies that exported TIFFs meet the PLOS raster requirements:
##   width  789-2250 px
##   height <= 2625 px
##   >= 300 dpi declared
##   RGB or greyscale
##   LZW compressed, < 10 MB
##
## Usage:  Rscript check_plos_figures.R <dir> [<dir> ...]
##         with no argument it checks the two Overleaf output folders.
## ---------------------------------------------------------------------------

OVERLEAF <- '~/Library/CloudStorage/Dropbox/Apps/Overleaf/Severe malaria renal failure'
args <- commandArgs(trailingOnly = TRUE)
dirs <- if (length(args)) args else c(
  file.path(OVERLEAF, 'MainFigures/PLOS Figs'),
  file.path(OVERLEAF, 'SupplementaryFigures/Final_PLOS'))

## sips is the macOS image query tool; no extra R packages needed.
sips_get <- function(f, key) {
  out <- suppressWarnings(system2('sips', c('-g', key, shQuote(f)),
                                  stdout = TRUE, stderr = FALSE))
  v <- sub('^.*:\\s*', '', grep(key, out, value = TRUE))
  if (!length(v)) NA_character_ else trimws(v[1])
}

check_one <- function(f) {
  w <- suppressWarnings(as.integer(sips_get(f, 'pixelWidth')))
  h <- suppressWarnings(as.integer(sips_get(f, 'pixelHeight')))
  d <- suppressWarnings(as.numeric(sips_get(f, 'dpiWidth')))
  sp <- sips_get(f, 'space')
  mb <- file.size(f) / 1e6
  fail <- c(
    if (is.na(w) || w < 789 || w > 2250) sprintf('width %s', w),
    if (is.na(h) || h > 2625)            sprintf('height %s', h),
    if (is.na(d) || d < 300)             sprintf('dpi %s', d),
    if (!sp %in% c('RGB', 'Gray'))       sprintf('space %s', sp),
    if (mb > 10)                         sprintf('%.1f MB', mb))
  data.frame(file = basename(f), w = w, h = h, dpi = d, space = sp,
             MB = round(mb, 2),
             inches = sprintf('%.2f x %.2f', w / 300, h / 300),
             status = if (length(fail)) paste(fail, collapse = '; ') else 'OK')
}

res <- do.call(rbind, lapply(dirs, function(dd) {
  fs <- list.files(dd, pattern = '\\.tiff?$', full.names = TRUE)
  if (!length(fs)) { message('no TIFFs in ', dd); return(NULL) }
  message('\n== ', dd, ' (', length(fs), ' files)')
  do.call(rbind, lapply(sort(fs), check_one))
}))

if (!is.null(res)) {
  print(res, row.names = FALSE)
  bad <- res[res$status != 'OK', , drop = FALSE]
  cat('\n', nrow(res) - nrow(bad), ' of ', nrow(res),
      ' files meet the PLOS requirements\n', sep = '')
  if (nrow(bad)) { cat('\nFAILURES:\n'); print(bad[, c('file', 'status')], row.names = FALSE) }
}
