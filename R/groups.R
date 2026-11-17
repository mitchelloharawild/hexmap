# Groups are given to layouts as an unevaluated expression (e.g.
# `layout_hierarchy(state / region)`), which is captured when the layout is
# created and evaluated when it runs, against the attribute data of the map
# and then the environment the layout was created in. `/` separates nested
# levels, outermost first.

# Captures the group expression of a layout. Returns a function of the
# regions (see hexmap()) that evaluates the levels of groups, as a data frame
# with one column per level (outermost first), or NULL without groups.
capture_group <- function(expr, env) {
  force(env)
  if (is.null(expr)) return(function(regions) NULL)
  levels <- split_levels(expr)
  function(regions) eval_levels(levels, regions$data, env, length(regions$geometry))
}

# Splits an expression into its levels at each `/`
split_levels <- function(expr) {
  if (is.call(expr) && identical(expr[[1]], as.name("/")) && length(expr) == 3) {
    return(c(split_levels(expr[[2]]), split_levels(expr[[3]])))
  }
  list(expr)
}

# Evaluates each level as a column of data or a variable of env. A string
# naming a column gives that column, and a character vector of column names
# (as a single level) gives one level for each.
eval_levels <- function(levels, data, env, n) {
  values <- list()
  for (expr in levels) {
    label <- paste(deparse(expr), collapse = " ")
    value <- tryCatch(eval(expr, data, env), error = function(e) {
      stop("Could not find the groups `", label, "`, which must be columns of ",
           "the map or variables: ", conditionMessage(e), call. = FALSE)
    })
    if (is.character(value) && length(value) != n && length(value) >= 1 &&
        all(value %in% names(data))) {
      if (length(levels) > 1 && length(value) > 1) {
        stop("Give several levels of groups either separated by `/` or as a ",
             "character vector of column names, not both.", call. = FALSE)
      }
      values[value] <- data[value]
    } else {
      values[[label]] <- value
    }
  }
  for (k in seq_along(values)) check_level(values[[k]], names(values)[k], n)
  as.data.frame(values, optional = TRUE, stringsAsFactors = FALSE)
}

check_level <- function(value, label, n) {
  if (!is.atomic(value) || length(value) != n) {
    stop("The groups `", label, "` must have one value for each region, such ",
         "as a column of the map given unquoted (e.g. `layout_hierarchy(state)`).",
         call. = FALSE)
  }
  if (anyNA(value)) {
    row <- which(is.na(value))
    stop("The groups `", label, "` must not contain missing values (in rows ",
         paste(row[seq_len(min(5, length(row)))], collapse = ", "),
         if (length(row) > 5) ", ...", ").", call. = FALSE)
  }
}

# The groups of the first k levels combined, as integers. Groups of an inner
# level are nested in their outer groups, so the same name in two outer
# groups is two groups.
nest_key <- function(groups, k = ncol(groups)) {
  if (is.null(groups)) return(NULL)
  key <- do.call(paste, c(unname(lapply(groups[seq_len(k)], as.character)), sep = "\r"))
  as.integer(factor(key, levels = unique(key)))
}

# The outermost groups, or a single group without groups
outer_group <- function(groups, n) {
  if (is.null(groups)) rep(1L, n) else groups[[1]]
}

# Checks gap, the width of the gap between groups at each level
check_gap <- function(gap) {
  if (!is.numeric(gap) || !length(gap) || anyNA(gap) || any(gap < 0 | gap > 1)) {
    stop("`gap` must be numbers between 0 and 1.", call. = FALSE)
  }
}

# The gap for each level of groups
level_gap <- function(gap, groups) {
  if (is.null(groups)) return(numeric(0))
  if (length(gap) != 1 && length(gap) != ncol(groups)) {
    stop("`gap` must have one value, or one for each of the ", ncol(groups),
         " levels of groups.", call. = FALSE)
  }
  rep_len(gap, ncol(groups))
}

# Completes the result of a layout that placed only the outermost groups:
# the hexagons of each group are split between its inner groups, level by
# level, and (with iterations) the borders between inner groups are repaired.
nest_result <- function(result, groups, gap, regions, iterations = 0) {
  if (!is.null(groups) && ncol(groups) > 1) {
    result$tile <- nest_tiles(result$tile, groups, regions$land$landmass,
                              result$point, from = 2)
    if (iterations > 0) {
      result$groups <- groups
      result <- repair_levels(result, regions, from = 2, iterations = iterations)
    }
  }
  result$groups <- groups
  result$gap <- level_gap(gap, groups)
  result
}

# Splits the hexagons of each group between its inner groups, for levels from
# `from` inwards. Within each group (on each landmass), each hexagon starts
# with the inner group of the region placed on it, the inner groups are made
# contiguous, and the regions of each inner group are placed on its hexagons
# nearest to their points (or to the hexagons they had).
#' @importFrom sf st_area st_centroid st_coordinates
nest_tiles <- function(tile, groups, landmass, point = NULL, from = 2) {
  if (from > ncol(groups)) return(tile)
  cellsize <- sqrt(2 * stats::median(as.numeric(st_area(tile))) / sqrt(3))
  for (k in seq(from, ncol(groups))) {
    centre <- st_centroid(tile)
    xy <- st_coordinates(centre)
    at <- if (is.null(point)) xy else point
    parent <- interaction(nest_key(groups, k - 1), landmass, drop = TRUE)
    child <- nest_key(groups, k)
    new <- tile
    for (i in split(seq_along(tile), parent)) {
      if (length(unique(child[i])) < 2) next
      label <- contiguous_groups(child[i], hex_adjacency(centre[i], cellsize))
      if (!attr(label, "contiguous")) {
        warning("Could not make the tiles of every group contiguous.", call. = FALSE)
      }
      for (g in unique(child[i])) {
        ig <- i[child[i] == g]
        jg <- i[label == g]
        new[ig] <- tile[jg][solve_assignment(at[ig, , drop = FALSE], xy[jg, , drop = FALSE])]
      }
    }
    tile <- new
  }
  tile
}

# Repairs the borders between groups at each level from `from` inwards (see
# repair_groups()). Inner groups only swap hexagons with groups of the same
# outer group, and are split again within any outer group that changed.
repair_levels <- function(result, regions, from = 1, iterations = 30, border = 0) {
  groups <- result$groups
  n_level <- if (is.null(groups)) 1 else ncol(groups)
  landmass <- regions$land$landmass
  n <- length(regions$geometry)
  for (k in seq(from, n_level)) {
    key <- if (is.null(groups)) rep(1L, n) else nest_key(groups, k)
    parent <- if (k > 1) nest_key(groups, k - 1)
    result$tile <- repair_groups(result$tile, regions$geometry, key, landmass,
                                 iterations = round(iterations * n),
                                 border = if (k == n_level) border else 0,
                                 point = result$point, warp = result$warp,
                                 parent = parent)
    if (k < n_level) {
      result$tile <- nest_tiles(result$tile, groups, landmass, result$point, from = k + 1)
    }
  }
  result
}

# Evaluates code with a fixed seed, without changing the user's random numbers
with_seed <- function(seed, code) {
  if (exists(".Random.seed", globalenv())) {
    old <- get(".Random.seed", globalenv())
    on.exit(assign(".Random.seed", old, globalenv()))
  } else {
    on.exit(rm(".Random.seed", envir = globalenv()))
  }
  set.seed(seed)
  code
}
