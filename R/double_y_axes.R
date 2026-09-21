#' Smallest "nice" number at or above x
#'
#' From the 1 / 2 / 2.5 / 5 / 10 ladder scaled by powers of ten — the gaps
#' people expect to see between axis labels.
#'
#' @param x positive finite numeric scalar
#' @return numeric scalar
#' @keywords internal
nice_step <- function(x) {
  if (!is.finite(x) || x <= 0) stop("x must be a positive finite number")
  mag <- 10^floor(log10(x))
  candidates <- c(1, 2, 2.5, 5, 10) * mag
  candidates[which(candidates >= x * (1 - 1e-9))[1]]
}

#' One axis on a nice grid with exactly n intervals
#'
#' With \code{j} supplied the axis is anchored so that zero falls exactly on
#' the j-th break, which is what lets two axes share a zero line. With
#' \code{j = NULL} the range floats to whatever covers the data most tightly,
#' padded up to n intervals — never pushing an all-positive axis below zero.
#'
#' @param lo,hi data range to cover
#' @param n number of intervals (there are n + 1 breaks)
#' @param j intervals below zero, or NULL to let the range float
#' @param ref reference line (usually 0 or 100)
#' @return list(ylim, y_breaks, step), or NULL if no nice step fits
#' @keywords internal
axis_on_grid <- function(lo, hi, n, j = NULL, ref = 0) {
  if (hi < lo) stop("hi must be >= lo")
  if (isTRUE(all.equal(lo, hi))) {
    pad <- if (lo == 0) 1 else abs(lo) * 0.05
    lo <- lo - pad; hi <- hi + pad
  }
  if (!is.null(j)) {
    if (j < 0 || j > n) return(NULL)
    need <- c(if (j > 0) (ref - lo) / j, if (j < n) (hi - ref) / (n - j))
    need <- need[is.finite(need) & need > 0]
    if (!length(need)) return(NULL)
    step <- nice_step(max(need))
    ylim <- ref + c(-j, n - j) * step
    if (ylim[1] > lo + 1e-9 || ylim[2] < hi - 1e-9) return(NULL)
  } else {
    lo_r <- lo - ref; hi_r <- hi - ref          # work relative to the reference
    step <- nice_step((hi_r - lo_r) / n)
    repeat {
      k <- ceiling(hi_r / step - 1e-9) - floor(lo_r / step + 1e-9)
      if (k <= n) break
      step <- nice_step(step * 1.0000001)
    }
    base_lo <- floor(lo_r / step + 1e-9) * step
    below <- floor((n - k) / 2)
    if (lo_r >= 0) below <- min(below, base_lo / step)
    ylim <- ref + c(base_lo - below * step, base_lo + (n - below) * step)
  }
  list(ylim = ylim, y_breaks = seq(ylim[1], ylim[2], by = step), step = step)
}


#' Two y scales that share their gridlines
#'
#' Returns one scale per axis with the same number of intervals, so the
#' horizontal gridlines serve both. When both sides straddle zero, the zero
#' line is forced onto the same gridline. Either side can be fixed with
#' \code{ylim}/\code{ylim2}; the other is then derived to match it.
#'
#' @param values_left,values_right numeric vectors of the data on each axis
#' @param ylim,ylim2 numeric(2) manual limits, or NULL to derive
#' @param n_range interval counts to consider
#' @param ref reference (usually 0 or 100)
#' @param ref2 reference (usually 0 or 100)
#' @return list(left, right, problems), each side shaped like
#'   \link{find_pretty_ylim} (ylim, y_breaks)
#' @export
pair_y_scales <- function(values_left, values_right,
                          ylim = NULL, ylim2 = NULL,
                          ref = 0, ref2 = 0, n_range = 3:7) {
  side <- function(v, lim) if (!is.null(lim)) sort(as.numeric(lim)) else range(v, na.rm = TRUE)
  L <- side(values_left, ylim)
  R <- side(values_right, ylim2)

  fixed <- function(lims, n) {
    list(ylim = lims, y_breaks = seq(lims[1], lims[2], length.out = n + 1),
         step = diff(lims) / n)
  }
  ref_j <- function(lims, n, r) {
    if (!(lims[1] < r && lims[2] > r)) return(NULL)
    jr <- n * (r - lims[1]) / diff(lims)
    if (abs(jr - round(jr)) < 1e-8) round(jr) else NULL
  }
  waste <- function(a, rng) (diff(a$ylim) - diff(rng)) / max(diff(rng), .Machine$double.eps)

  if (!is.null(ylim) && !is.null(ylim2)) {
    # both given: respect them verbatim, but pick a break count that makes the
    # labels round on both sides if any in n_range does
    is_round <- function(n) {
      steps <- c(diff(L), diff(R)) / n
      all(vapply(steps, \(x) isTRUE(all.equal(nice_step(x), x)), logical(1)))
    }
    ok <- Filter(is_round, n_range)
    n <- if (length(ok)) ok[1] else n_range[1]
    out <- list(left = fixed(L, n), right = fixed(R, n))
  } else if (!is.null(ylim) || !is.null(ylim2)) {
    lims <- if (is.null(ylim)) R else L
    dat  <- if (is.null(ylim)) L else R
    r_fixed <- if (is.null(ylim)) ref2 else ref
    r_free  <- if (is.null(ylim)) ref  else ref2
    best <- NULL
    for (n in n_range) {
      cand <- axis_on_grid(dat[1], dat[2], n, j = ref_j(lims, n, r_fixed), ref = r_free)
      if (is.null(cand)) next
      pair <- if (is.null(ylim)) list(left = cand, right = fixed(R, n))
      else               list(left = fixed(L, n), right = cand)
      penalty <- if (length(check_axis_pair(pair$left, pair$right, ref, ref2))) 1000 else 0
      score <- waste(cand, dat) + penalty + 0.05 * n
      if (is.null(best) || score < best$score) best <- list(pair = pair, score = score)
    }
    if (is.null(best)) stop("no scale compatible with the fixed limits was found")
    out <- best$pair

  } else {
    # anchor both axes on their reference by default, even where the reference
    # sits outside one series' range - a shared reference line is worth the lost
    # zoom, and ylim/ylim2 are there for when it is not
    best <- NULL
    for (n in n_range) {
      for (j in as.list(0:n)) {
        l <- axis_on_grid(L[1], L[2], n, j = j, ref = ref)
        r <- axis_on_grid(R[1], R[2], n, j = j, ref = ref2)
        if (is.null(l) || is.null(r)) next
        score <- waste(l, L) + waste(r, R) + 0.05 * n
        if (is.null(best) || score < best$score) {
          best <- list(pair = list(left = l, right = r), score = score)
        }
      }
    }
    if (is.null(best)) {                      # no shared anchor fits: let them float
      for (n in n_range) {
        l <- axis_on_grid(L[1], L[2], n, ref = ref)
        r <- axis_on_grid(R[1], R[2], n, ref = ref2)
        if (is.null(l) || is.null(r)) next
        score <- waste(l, L) + waste(r, R) + 0.05 * n
        if (is.null(best) || score < best$score) {
          best <- list(pair = list(left = l, right = r), score = score)
        }
      }
    }
    if (is.null(best)) stop("no compatible pair of scales was found")
    out <- best$pair
  }

  out$problems <- check_axis_pair(out$left, out$right, ref, ref2)
  out
}

#' Are two y scales compatible?
#'
#' Compatible means the gridlines can serve both: equal break counts, and —
#' when both straddle zero — zero at the same height on both, on a gridline.
#'
#' @param left,right scales shaped like \link{find_pretty_ylim} output
#' @param tol relative tolerance
#' @param ref reference (usually 0 or 100)
#' @param ref2 reference (usually 0 or 100)
#' @return character vector of problems, empty when the pair is fine
#' @export
check_axis_pair <- function(left, right, ref = 0, ref2 = 0, tol = 1e-8) {
  problems <- character(0)
  nl <- length(left$y_breaks); nr <- length(right$y_breaks)
  if (nl != nr) {
    problems <- c(problems, sprintf(
      "different number of breaks (%d left, %d right) - gridlines cannot serve both", nl, nr))
  }
  crosses <- function(a, r) a$ylim[1] < r && a$ylim[2] > r
  height <- function(a, r) (r - a$ylim[1]) / diff(a$ylim)
  on_grid <- function(a, r) any(abs(a$y_breaks - r) <= tol * diff(a$ylim))
  if (crosses(left, ref) && crosses(right, ref2)) {
    if (abs(height(left, ref) - height(right, ref2)) > tol) {
      problems <- c(problems, sprintf(
        "the reference lines (%g left, %g right) sit at different heights", ref, ref2))
    }
    if (!on_grid(left, ref) || !on_grid(right, ref2)) {
      problems <- c(problems, sprintf(
        "the reference lines (%g left, %g right) do not fall on a gridline", ref, ref2))
    }
  }
  problems
}
#' Things worth thinking twice about on a dual-axis chart
#'
#' Advice rather than errors: every one of these can be the right thing to do
#' on purpose, and none of them is safe to do by accident, so
#' \link{view_chart} emits each as a warning and draws the chart anyway.
#' Emitted from the renderer rather than \link{prep_chart} because that is
#' where the two scales are known, and because both entry points converge
#' there — nothing escapes by going through the CSV pipeline.
#'
#' Four things are checked. Whether a single axis would have served both
#' series, judged by how much of their combined range each one would still
#' cover — this is the one that catches a second scale added for emphasis
#' rather than necessity, and it is worded more sharply when the two axis
#' labels are identical. Whether there are bars on both sides, which cannot
#' be compared by eye at two different scales. Whether exactly one axis
#' crosses its reference value, which leaves the emphasised gridline
#' meaningful for one series and arbitrary for the other. And whatever
#' \link{check_axis_pair} reported about the two scales not lining up.
#'
#' Deliberately not checked: the ratio of the two spans. Two series can have
#' similar spreads at completely different levels — an index around 100 and a
#' growth rate around zero — where a shared axis would squash both, so span
#' similarity flags good charts as bad. \code{min_share} measures the thing
#' that actually decides it.
#'
#' @param values_left,values_right numeric vectors of the data on each axis
#' @param label_left,label_right the two axis labels, or NULL
#' @param types_left,types_right series types ("line", "bar", "area") on each axis
#' @param scales output of \link{pair_y_scales}
#' @param ref,ref2 reference value for each axis — 0, or 100 for an index
#' @param min_share how much of the combined range each series must still
#'   cover before a single axis is judged to have been enough. At the default
#'   0.3 this fires only when a shared axis would clearly have worked; around
#'   0.15 it also catches a second axis used to magnify a nearly flat series,
#'   at the cost of more false positives.
#'
#' @return character vector of advice, empty when nothing looks suspect
#' @keywords internal
dual_axis_advice <- function(values_left, values_right,
                             label_left, label_right,
                             types_left, types_right, scales,
                             ref = 0, ref2 = 0, min_share = 0.3) {
  advice <- character(0)

  norm <- function(x) {
    if (is.null(x)) return("")
    x <- as.character(x)[1]
    if (is.na(x)) return("")
    tolower(trimws(gsub("\\s+", " ", x)))
  }

  # --- would one axis have done? ---
  L <- range(values_left, na.rm = TRUE)
  R <- range(values_right, na.rm = TRUE)
  if (all(is.finite(c(L, R)))) {
    shared <- range(c(L, R))
    frac <- c(diff(L), diff(R)) / max(diff(shared), .Machine$double.eps)
    if (all(frac > min_share)) {
      same_unit <- nzchar(norm(label_left)) && identical(norm(label_left), norm(label_right))
      advice <- c(advice, paste0(
        if (same_unit) paste0("Both axes are labelled '", label_left, "', and both series ")
        else "Both series ",
        "would still be readable on a single axis - each covers about ",
        paste(sprintf("%.0f%%", frac * 100), collapse = " and "),
        " of their combined range. A second scale here invents a relationship the ",
        "numbers do not have; put them on one axis."))
    }
  }

  # --- bars against bars ---
  if (any(types_left %in% "bar") && any(types_right %in% "bar")) {
    advice <- c(advice, paste0(
      "There are bars on both axes. Two sets of bars drawn at different scales ",
      "cannot be compared by eye - keep the bars on one axis and use lines on the other."))
  }

  # --- a reference line that only means something on one side ---
  crosses <- function(a, r) a$ylim[1] < r && a$ylim[2] > r
  cl <- crosses(scales$left, ref)
  cr <- crosses(scales$right, ref2)
  if (cr && !cl) {
    advice <- c(advice, sprintf(paste0(
      "The right axis crosses %g but the left one does not. The emphasised line is ",
      "drawn against the left axis only, so no line is drawn at all - set emphasis ",
      "explicitly, or give the left axis limits that reach %g."), ref2, ref))
  } else if (cl && !cr) {
    advice <- c(advice, sprintf(paste0(
      "The left axis crosses %g but the right one does not, so the emphasised line ",
      "means something for the left series and lands at an arbitrary height for the ",
      "right. Consider emphasis = FALSE."), ref))
  }

  # --- scales that do not line up ---
  if (length(scales$problems)) {
    advice <- c(advice, paste0(
      "The two scales are not aligned: ", paste(scales$problems, collapse = "; "),
      ". The gridlines cannot be read against both axes."))
  }

  advice
}

#' The value an axis is anchored and emphasised on
#'
#' Index axes are anchored on 100, everything else on 0. Three places used to
#' decide this independently — \link{empty_plot} with a case-sensitive
#' \code{"nde(ks|x)"}, \link{draw_emphasis} with a case-insensitive
#' \code{"indeks|index"}, and \link{base_barplot} with a Slovenian-only
#' \code{"ndeks"} — so an English "Index" label got the 100 line on a line
#' chart and not on a bar chart. They should all call this.
#'
#' @param label axis label, or NULL
#' @return 100 for an index axis, otherwise 0
#' @keywords internal
axis_reference <- function(label) {
  if (is.null(label) || !length(label) || is.na(label[1])) return(0)
  if (grepl("nde(ks|x)", label[1], ignore.case = TRUE)) 100 else 0
}

#' One axis's slice of the config
#'
#' Shallow copy with only that axis's series, and for the right axis the
#' second label moved into \code{y_axis_label} — so the same label-width code
#' serves both sides. Same trick \link{split_by_unit} uses.
#'
#' @param config internal config list
#' @param which 1 or 2
#' @return config list
#' @keywords internal
config_for_axis <- function(config, which = 1L) {
  axis_of <- vapply(config$series,
                    \(s) if (is.null(s$axis)) 1L else as.integer(s$axis), integer(1))
  out <- config
  out$series <- config$series[axis_of == which]
  if (which == 2L) out$y_axis_label <- config$y2_axis_label
  out
}

#' Prepare right axis labels and get width
#'
#' Mirror of \link{left_axis_label_width}: same measurement at the same point
#' size, same title wrapping to the plot height, but it sets the right margin
#' instead of the left. Must run after \link{get_top_margin_and_title}, which
#' resets \code{mar[4]}.
#'
#' @param config config for the right axis, from \link{config_for_axis}
#' @param y_axis the right-hand scale from \link{pair_y_scales}
#' @param language language, options en and si, default si
#' @param edge_pad for padding right margin
#'
#' @return list of axis title, axis labels, positions and number of lines
#' @export
right_axis_label_width <- function(config, y_axis, language = "si",
                                   edge_pad = 0.35) {
  axis_labels_num <- y_axis$y_breaks
  axis_positions <- y_axis$y_breaks
  unit <- unique(unlist(purrr::map(config$series, ~ .x$unit)))
  mio_eur <- unique(unlist(purrr::map(config$series, ~ .x$mio_eur)))
  if (length(unit) == 1 && unit == "EUR" & mio_eur) {
    axis_labels_num <- axis_labels_num / 1000000
    unit <- "Mio EUR"
  }
  axis_labels <- format_number(axis_labels_num, language)

  old_ps <- par("ps")
  par(ps = 8.5)
  widths <- strwidth(axis_labels, units = "inches")
  y_axis_label <- if (is.null(config$y_axis_label)) unit else config$y_axis_label
  y_axis_label <- wrap_to_height(y_axis_label, par("pin")[2] * 0.95)
  par(ps = old_ps)

  n_title_lines <- length(strsplit(y_axis_label, "\n", fixed = TRUE)[[1]])
  y_lab_lines <- max(widths) / par("csi") + 0.5
  current_mar <- par("mar")
  current_mar[4] <- y_lab_lines + n_title_lines + edge_pad
  par(mar = current_mar)
  mget(c("unit", "axis_labels", "axis_positions", "y_lab_lines", "y_axis_label"))
}
