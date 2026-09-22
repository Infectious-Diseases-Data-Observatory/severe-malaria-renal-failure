## ---------------------------------------------------------------------------
## make_figures.R
##
## Regenerates the manuscript figures straight to PLOS-compliant TIFF.
##
## Written in response to the PLOS editorial request that axes begin at zero (or
## show a break). Previously the figures were exported by hand from the knitted
## HTML at inconsistent scales, so there was no reproducible path from code to
## submitted figure. This script provides one.
##
## Method: each Rmd is tangled with knitr::purl, the code is evaluated up to and
## including the chunk that builds a given figure, and the resulting grob is
## written directly to TIFF. grid.arrange() is shadowed during evaluation so that
## every composed figure is captured rather than drawn, which lets one chunk yield
## several figures (e.g. baseline_creatinine_BUN produces both Fig 4 and S6 Fig).
##
## PLOS raster requirements applied: width 789-2250 px, height <= 2625 px,
## 300 dpi, RGB, LZW compression, < 10 MB.
##
## Coverage: the seven main figures (Fig 7 is the Euler diagram, which has no
## ggplot code and no numeric axes, so it is not regenerated here) and the
## supplementary figures built by renal_analysis.Rmd / Creatinine_Analysis.Rmd.
## Not covered, because they come from elsewhere in the pipeline:
##   S1, S3 Fig  multiple_imputation.R  (needs the mice fit; run that file with
##               RUN_IMPUTE_MODEL = FALSE and SUPP_FIG_DIR set)
##   S2 Fig      hb_data_imputation.R   (its axes are already xlim/ylim from 0)
##   S4 Fig      supplied CONSORT diagram, not generated from code
##   S17 Fig     weight_for_age_zscores chunk of Make_Analysis_dataset.Rmd
##
## Usage:  Rscript make_figures.R
##         MAIN_FIG_DIR / SUPP_FIG_DIR may be set beforehand to redirect output,
##         and FIGS to a vector of figure names to export only those.
## ---------------------------------------------------------------------------

suppressMessages({
  library(knitr); library(ggplot2); library(gridExtra); library(grid); library(gtable)
})

OVERLEAF <- '~/Library/CloudStorage/Dropbox/Apps/Overleaf/Severe malaria renal failure'
if (!exists('MAIN_FIG_DIR')) MAIN_FIG_DIR <- file.path(OVERLEAF, 'MainFigures/PLOS Figs')
if (!exists('SUPP_FIG_DIR')) SUPP_FIG_DIR <- file.path(OVERLEAF, 'SupplementaryFigures/Final_PLOS')
PX_W <- 2250                           # PLOS maximum raster width, in pixels
DPI  <- 300                            # the resolution the submitted file declares

## Registry: which chunk builds which figure.
##   pick = "arrange:<n>"  -> the nth grid.arrange() call within the chunk
##   pick = "value"        -> the value of the chunk's last expression
##   w, h                  -> the chunk's own fig.width / fig.height, in inches
##                            (the file-level default is 8 x 8)
##
## Each figure is drawn on a canvas of exactly the size its author designed it
## for, and the resolution is then chosen so that the canvas comes out 2250 px
## wide. Rendering everything on a common 7.5 in canvas instead would give the
## same pixel counts but enlarge every label relative to its panel, which makes
## the denser figures (Fig 6's contingency-table panel in particular) collide.
fig_registry <- function() rbind(
  data.frame(out='Main_Figure1', dir='main', src='renal_analysis.Rmd',
             chunk='weight_age',                              pick='arrange:1', w=12, h=12),
  data.frame(out='Main_Figure2', dir='main', src='renal_analysis.Rmd',
             chunk='creatinine_v_bun',                        pick='arrange:3', w=12, h=8),
  data.frame(out='Main_Figure3', dir='main', src='renal_analysis.Rmd',
             chunk='non_para_mortality',                      pick='arrange:1', w=12, h=8),
  data.frame(out='Main_Figure4', dir='main', src='Creatinine_Analysis.Rmd',
             chunk='baseline_creatinine_BUN',                 pick='arrange:1', w=12, h=9),
  data.frame(out='Main_Figure5', dir='main', src='Creatinine_Analysis.Rmd',
             chunk='combined_fig',                            pick='arrange:1', w=12, h=10),
  data.frame(out='Main_Figure6', dir='main', src='Creatinine_Analysis.Rmd',
             chunk='bun_Cr_potassium',                        pick='arrange:1', w=10, h=8),
  data.frame(out='Figure5',      dir='supp', src='renal_analysis.Rmd',
             chunk='bun_istat',                               pick='value',     w=8, h=8),
  data.frame(out='Figure6',      dir='supp', src='Creatinine_Analysis.Rmd',
             chunk='baseline_creatinine_BUN',                 pick='arrange:2', w=12, h=9),
  data.frame(out='Figure7',      dir='supp', src='Creatinine_Analysis.Rmd',
             chunk='creatinine_fc_baseline_mortality_Pottel', pick='arrange:1', w=8, h=8),
  data.frame(out='Figure8',      dir='supp', src='Creatinine_Analysis.Rmd',
             chunk='creatinine_fc_baseline_mortality_Schwartz',pick='arrange:1', w=8, h=8),
  data.frame(out='Figure12',     dir='supp', src='renal_analysis.Rmd',
             chunk='bun_by_study',                            pick='value',     w=12, h=7),
  data.frame(out='Figure13',     dir='supp', src='renal_analysis.Rmd',
             chunk='creatinine_by_study',                     pick='value',     w=12, h=7),
  data.frame(out='Figure15',     dir='supp', src='Creatinine_Analysis.Rmd',
             chunk='age_based_thresholds',                    pick='value',     w=8, h=8),
  data.frame(out='Figure16',     dir='supp', src='Creatinine_Analysis.Rmd',
             chunk='temporal_trend',                          pick='value',     w=8, h=8)
)

## Tangle an Rmd once and split it into (label, code) pieces.
tangle <- function(rmd) {
  tmp <- tempfile(fileext = '.R')
  purl(rmd, output = tmp, documentation = 1, quiet = TRUE)
  l   <- readLines(tmp)
  at  <- grep('^## ----', l)
  lbl <- trimws(vapply(strsplit(sub('-*$', '', sub('^## ----', '', l[at])), ','),
                       `[`, character(1), 1))
  ends <- c(at[-1] - 1, length(l))
  list(lines = l, at = at, label = lbl, end = ends)
}

## Evaluate a tangled script up to the end of `chunk`, capturing the
## grid.arrange() calls made *within that chunk* plus the value of its final
## expression.
##
## The prefix (everything before the target chunk) is evaluated first and its
## captures are discarded. This matters: several chunks earlier in the same file
## also call grid.arrange(), so capturing across the whole prefix would make
## "the first composed figure" refer to some unrelated earlier chunk.
build <- function(tg, chunk) {
  i <- which(tg$label == chunk)
  if (!length(i)) stop('chunk not found: ', chunk)
  i <- i[1]
  prefix <- if (tg$at[i] > 1) tg$lines[1:(tg$at[i] - 1)] else character(0)
  target <- tg$lines[tg$at[i]:tg$end[i]]

  caught <- list()
  env <- new.env(parent = globalenv())
  assign('grid.arrange', function(...) {
    g <- gridExtra::arrangeGrob(...)
    caught[[length(caught) + 1]] <<- g
    invisible(g)
  }, envir = env)

  quiet <- function(expr) withCallingHandlers(expr,
    warning = function(w) invokeRestart('muffleWarning'),
    message = function(m) invokeRestart('muffleMessage'))

  pdf(NULL); on.exit(dev.off(), add = TRUE)
  if (length(prefix))
    quiet(eval(parse(text = paste(prefix, collapse = '\n')), envir = env))
  caught <- list()                      # discard anything the prefix composed
  val <- quiet(eval(parse(text = paste(target, collapse = '\n')), envir = env))
  list(arranged = caught, value = val)
}

## Coerce a ggplot or gtable to something ggsave can write.
as_grob <- function(x) if (inherits(x, 'ggplot')) ggplotGrob(x) else x

## The render resolution above is whatever makes the canvas 2250 px wide, which
## is below the 300 dpi PLOS asks for. Only the declared resolution differs: at
## 300 dpi a 2250 px image prints 7.5 in wide, which is PLOS's maximum column
## width, so the tag is rewritten rather than the pixels. sips preserves the LZW
## compression and the RGB space and adds an sRGB profile.
retag <- function(path) {
  st <- suppressWarnings(system2('sips',
          c('-s', 'dpiWidth', DPI, '-s', 'dpiHeight', DPI, shQuote(path)),
          stdout = FALSE, stderr = FALSE))
  if (!identical(st, 0L))
    warning('could not set the resolution tag on ', basename(path),
            '; the file is still ', PX_W, ' px wide but declares a lower dpi')
  invisible(st)
}

main <- function() {
  dir.create(MAIN_FIG_DIR, recursive = TRUE, showWarnings = FALSE)
  dir.create(SUPP_FIG_DIR, recursive = TRUE, showWarnings = FALSE)
  reg <- fig_registry()
  ## Optional subset, for re-exporting one figure after a change:
  ##   FIGS <- c('Main_Figure3'); source('make_figures.R'); main()
  if (exists('FIGS')) reg <- reg[reg$out %in% FIGS, , drop = FALSE]
  tgs <- list()
  ok <- 0

  for (src in unique(reg$src)) {
    message('tangling ', src)
    tgs[[src]] <- tangle(src)
  }

  for (chunk in unique(paste(reg$src, reg$chunk, sep = '||'))) {
    parts <- strsplit(chunk, '\\|\\|')[[1]]
    src <- parts[1]; ch <- parts[2]
    rows <- reg[reg$src == src & reg$chunk == ch, , drop = FALSE]
    message('building ', ch, ' (', src, ') -> ', paste(rows$out, collapse = ', '))
    res <- try(build(tgs[[src]], ch), silent = TRUE)
    if (inherits(res, 'try-error')) {
      message('  FAILED: ', attr(res, 'condition')$message); next
    }
    for (k in seq_len(nrow(rows))) {
      r <- rows[k, ]
      g <- if (grepl('^arrange:', r$pick)) {
        idx <- as.integer(sub('^arrange:', '', r$pick))
        if (length(res$arranged) < idx) {
          message('  ', r$out, ': only ', length(res$arranged),
                  ' grid.arrange call(s), wanted #', idx); next
        }
        res$arranged[[idx]]
      } else res$value
      if (is.null(g)) { message('  ', r$out, ': nothing captured'); next }
      out <- file.path(if (r$dir == 'main') MAIN_FIG_DIR else SUPP_FIG_DIR,
                       paste0(r$out, '.tif'))
      res_dpi <- PX_W / r$w              # e.g. a 12 in wide figure renders at 187.5
      ggsave(out, as_grob(g), width = r$w, height = r$h, dpi = res_dpi,
             device = 'tiff', compression = 'lzw')
      retag(out)
      message('  wrote ', basename(out), '  (', PX_W, ' x ',
              round(r$h * res_dpi), ' px, ', r$w, ' x ', r$h, ' in canvas)')
      ok <- ok + 1
    }
  }
  message('\n', ok, ' of ', nrow(reg), ' figures written')
}

if (!interactive()) main()
