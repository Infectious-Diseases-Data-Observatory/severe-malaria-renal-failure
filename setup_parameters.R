sqrt_breaks = c(1,4,9,16,25,36,50,64,81)
breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

SEsmooth=F
my_alpha=0.2
bun_limits = c(2,50)
crea_limits = c(20,500)

mortality_threshold = 0.05
f_threshold = function(x, mortality_threshold) mean(x>mortality_threshold)


binomial_smooth <- function(...) {
  geom_smooth(method = "gam", method.args = list(family = "binomial"), ...)
}


## ---------------------------------------------------------------------------
## Axis helpers, added in response to the PLOS editorial request that axes begin
## at zero, or else show a break.
##
##   f_zero(p, axes)       linear axes -> begin at zero
##   f_axis_break(p, axis) square-root axes (and linear axes where zero is
##                         physiologically meaningless, e.g. height, haematocrit,
##                         calendar year) -> draw a break marker instead
##
## Log axes are left untouched: forcing zero onto a log scale is undefined.
## ---------------------------------------------------------------------------

## Anchor one or both axes at zero. Uses expand_limits() deliberately: the
## alternative scale_*_continuous(limits = c(0, NA)) DROPS observations outside
## the limits and silently overrides any existing scale_*_continuous() call.
f_zero <- function(p, axes = 'y') {
  if (grepl('x', axes)) p <- p + ggplot2::expand_limits(x = 0)
  if (grepl('y', axes)) p <- p + ggplot2::expand_limits(y = 0)
  p
}

## Draw the conventional axis-break marker - two short parallel diagonal slashes
## straddling the axis line just inside the panel edge - to signal that the axis
## is not linearly spaced and/or does not begin at zero.
##
## Implemented by adding a grob to the built gtable's axis cell rather than as an
## annotation layer, so the marker is placed in axis coordinates and is therefore
## independent of the scale transform (sqrt, log, identity) and of the data range.
## Needs only ggplot2 + grid + gtable; no ggbreak/ggforce/ggh4x dependency.
##
## NOTE: returns a gtable, not a ggplot. grid.arrange() and ggsave() both accept
## it, but you cannot keep adding layers with `+` afterwards - so apply all other
## ggplot modifications BEFORE calling this.
f_axis_break <- function(p, axis = c('x', 'y'), at = 0.022, half = 0.17,
                         gap = 0.026, slant = 0.009, lwd = 0.9) {
  axis <- match.arg(axis)
  g  <- if (inherits(p, 'gtable')) p else ggplot2::ggplotGrob(p)
  nm <- if (axis == 'x') 'axis-b' else 'axis-l'
  ## Faceted plots name these cells axis-b-1-2 etc, so match on the prefix, and
  ## skip the empty (zeroGrob) cells that facet_wrap creates for interior panels.
  k  <- grep(paste0('^', nm), g$layout$name)
  k  <- k[!vapply(g$grobs[k], function(z) inherits(z, 'zeroGrob'), logical(1))]
  if (!length(k)) {
    warning('f_axis_break: no drawn ', nm, ' cell found; plot returned unchanged')
    return(g)
  }
  for (i in k) {
    o <- c(0, gap)
    sl <- if (axis == 'x') {
      grid::segmentsGrob(x0 = grid::unit(at + o - slant, 'npc'),
                         y0 = grid::unit(1 - half, 'npc'),
                         x1 = grid::unit(at + o + slant, 'npc'),
                         y1 = grid::unit(1 + half, 'npc'),
                         gp = grid::gpar(lwd = lwd, lineend = 'butt'))
    } else {
      grid::segmentsGrob(x0 = grid::unit(1 - half, 'npc'),
                         y0 = grid::unit(at + o - slant, 'npc'),
                         x1 = grid::unit(1 + half, 'npc'),
                         y1 = grid::unit(at + o + slant, 'npc'),
                         gp = grid::gpar(lwd = lwd, lineend = 'butt'))
    }
    g$grobs[[i]] <- grid::grobTree(g$grobs[[i]], sl)
  }
  g
}
