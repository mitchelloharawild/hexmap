# Lays out groups as a spatial treemap of hexagons (Wood & Dykes, 2008), so
# that the borders between groups are long straight lines. Each landmass is
# a rectangle of hexagons with the aspect ratio of its bounding box, which is
# recursively cut in two by horizontal (between rows) or vertical (zigzagging
# around a column) cuts, with each side getting exactly the hexagons of its
# groups. Where a side doesn't fill whole rows or columns, the cut steps once.
#
# Each cut is chosen from the groups' centroids: groups are sorted along an
# axis and every split point is tried, preferring cuts that separate
# neighbouring groups (weighted by the length of their shared border) along
# the cut's direction, and that keep the pieces from becoming thin. `tree`
# optionally fixes the splits, as a nested list of group names.
#
# Smaller landmasses are placed in their direction from the largest one,
# about two hexagons away.
#
# Returns the hexagons and the hexagon of each region.
#' @importFrom sf st_as_sfc st_bbox st_centroid st_coordinates st_make_grid
#' @importFrom sf st_point_on_surface st_union
slice_tiles <- function(geometry, group, region_land, cellsize, anchor = NULL,
                        tree = NULL) {
  row_height <- cellsize * sqrt(3) / 2
  point <- st_coordinates(st_point_on_surface(geometry))

  # The pieces of each group on each landmass
  piece <- interaction(as.integer(factor(group, exclude = NULL)), region_land,
                       drop = TRUE)
  region_piece <- as.integer(piece)
  first <- match(seq_len(nlevels(piece)), region_piece)
  piece_group <- as.character(group[first])
  piece_land <- region_land[first]
  shape <- do.call(c, lapply(split(geometry, piece), st_union))
  size <- tabulate(region_piece)
  centroid <- st_coordinates(st_centroid(shape))[, 1:2, drop = FALSE]
  if (!is.null(anchor)) {
    pinned <- match(piece_group, rownames(anchor))
    centroid[!is.na(pinned), ] <- anchor[pinned[!is.na(pinned)], , drop = FALSE]
  }
  piece_box <- t(vapply(seq_along(shape), function(k) st_bbox(shape[k])[1:4],
                        numeric(4)))
  border <- shared_border(shape, cellsize / 10)
  weight <- border / max(border, 1) + 0.05
  # Anchored groups take their direction to other groups from their anchor
  free <- if (is.null(anchor)) rep(TRUE, length(shape)) else is.na(pinned)
  direction <- border_direction(shape, centroid, border * outer(free, free),
                                cellsize / 10)

  # A lattice of hexagons covering the map, with room for the blocks
  box <- st_bbox(geometry)
  box[] <- box + c(-1, -1, 1, 1) * cellsize * (sqrt(length(geometry)) + 6)
  lattice <- st_make_grid(st_as_sfc(box), cellsize = cellsize, square = FALSE)
  to <- st_coordinates(st_centroid(lattice))
  # Integer positions: half hexagon steps across, and rows upwards
  across <- round((to[, 1] - min(to[, 1])) / (cellsize / 2))
  row <- round((to[, 2] - min(to[, 2])) / row_height)
  key <- paste(across, row)
  adjacent <- lapply(seq_along(lattice), function(j) {
    k <- match(paste(across[j] + c(-2, 2, -1, 1, -1, 1), row[j] + c(0, 0, -1, -1, 1, 1)), key)
    k[!is.na(k)]
  })
  in_row <- split(seq_along(lattice), row)

  # A block of n hexagons with the given aspect ratio centred on xy, with
  # any missing hexagons taken from the middle of its bottom row
  block <- function(n, aspect, xy) {
    rows <- seq_len(n)
    cols <- ceiling(n / rows)
    extra <- rows * cols - n
    fit <- abs(log(cols * cellsize / (rows * row_height) / aspect)) + 0.3 * extra / cols
    fit[extra >= cols] <- Inf
    rows <- rows[which.min(fit)]
    cols <- ceiling(n / rows)
    top <- round((xy[2] - min(to[, 2])) / row_height + (rows - 1) / 2)
    unlist(lapply(seq_len(rows), function(k) {
      j <- in_row[[as.character(top - k + 1)]]
      if (is.null(j)) return(NA)
      width <- if (k == rows) n - (rows - 1) * cols else cols
      j[order(abs(to[j, 1] - xy[1]))][seq_len(width)]
    }))
  }

  # The score of cutting cells between children (sets of pieces) along an
  # axis (1 for a vertical cut, 2 for a horizontal cut). Returns the children
  # in order, with their cells.
  cut_cells <- function(children, cells, axis) {
    members <- lapply(children, unlist)
    centre <- t(vapply(members, function(p) colSums(centroid[p, , drop = FALSE] * size[p]) /
                         sum(size[p]), numeric(2)))
    along <- function(xy) if (axis == 1) xy[, 1] else -xy[, 2]
    o <- order(along(centre))
    children <- children[o]
    members <- members[o]
    centre <- centre[o, , drop = FALSE]

    # Sort the cells across the cut, with the partial row or column of the
    # cut going to the side nearest each child
    perp <- if (axis == 1) -centre[, 2] else centre[, 1]
    flip <- if (perp[1] <= perp[length(perp)]) 1 else -1
    key <- if (axis == 1) cbind(across[cells], -flip * row[cells]) else
      cbind(-row[cells], flip * across[cells])
    cells <- cells[order(key[, 1], key[, 2])]
    n <- vapply(members, function(p) sum(size[p]), numeric(1))
    chunks <- split(cells, rep(seq_along(children), n))

    # Neighbours should be separated along the cut's axis
    id <- rep(seq_along(members), lengths(members))
    p <- unlist(members)
    dx <- direction$x[p, p, drop = FALSE]
    dy <- direction$y[p, p, drop = FALSE]
    d_along <- if (axis == 1) dx else -dy
    d_perp <- abs(if (axis == 1) dy else dx)
    misplaced <- ifelse(d_along < 0, 1, d_perp / pmax(d_along + d_perp, 1e-9))
    pair <- outer(id, id, "<")
    w <- weight[p, p, drop = FALSE] * pair
    order_cost <- sum(w * misplaced) / sum(w)

    # Each side should be near its groups' positions, scaled to the cells
    p_box <- piece_box[p, , drop = FALSE]
    real <- c(range(p_box[, c(1, 3)]), range(p_box[, c(2, 4)]))
    shift <- vapply(seq_along(chunks), function(k) {
      target <- c(rescale(centre[k, 1], real[1:2], range(to[cells, 1])),
                  rescale(centre[k, 2], real[3:4], range(to[cells, 2])))
      sqrt(sum((colMeans(to[chunks[[k]], , drop = FALSE]) - target)^2))
    }, numeric(1))
    shift_cost <- sum(n * shift) / sum(n) / sqrt(length(cells) * cellsize * row_height)

    # Thin pieces are hard to read, though a side of m groups can still be
    # cut into m pieces along its length
    thin <- vapply(seq_along(chunks), function(k) {
      j <- chunks[[k]]
      width <- (diff(range(across[j])) / 2 + 1) * cellsize
      height <- (diff(range(row[j])) + 1) * row_height
      max(abs(log(width / height)) - log(length(members[[k]])), 0)
    }, numeric(1))
    list(children = children, cells = chunks,
         score = order_cost + shift_cost + 0.5 * mean(thin))
  }

  # The score of the blocks of pieces p: pairs of neighbouring pieces should
  # keep their direction and touch, and blocks should be near their scaled
  # positions and not thin
  layout_score <- function(p, cells) {
    own <- match(owner[cells], p)
    xy <- rowsum(to[cells, , drop = FALSE], own) / tabulate(own)
    real <- centroid[p, , drop = FALSE]
    dx <- outer(xy[, 1], xy[, 1], function(a, b) b - a)
    dy <- outer(xy[, 2], xy[, 2], function(a, b) b - a)
    cosine <- (dx * direction$x[p, p] + dy * direction$y[p, p]) /
      pmax(sqrt(dx^2 + dy^2), 1e-9)
    w <- weight[p, p, drop = FALSE]
    diag(w) <- 0
    turned <- if (sum(w) > 0) sum(w * (1 - cosine) / 2) / sum(w) else 0

    edge <- edges[edges[, 1] %in% cells & edges[, 2] %in% cells, , drop = FALSE]
    touch <- matrix(FALSE, length(p), length(p))
    touch[cbind(owner_index(edge[, 1], p), owner_index(edge[, 2], p))] <- TRUE
    b <- border[p, p, drop = FALSE]
    apart <- if (sum(b) > 0) sum(b * !touch) / sum(b) else 0
    # Touching pieces that aren't neighbours are misleading
    false <- sum(touch & t(touch) & b == 0 & upper.tri(b)) / length(p)

    box <- c(range(piece_box[p, c(1, 3)]), range(piece_box[p, c(2, 4)]))
    target <- cbind(rescale(real[, 1], box[1:2], range(to[cells, 1])),
                    rescale(real[, 2], box[3:4], range(to[cells, 2])))
    shift <- sum(size[p] * sqrt(rowSums((xy - target)^2))) / sum(size[p]) /
      sqrt(length(cells) * cellsize * row_height)

    thin <- mean(vapply(split(cells, own), function(j) {
      width <- (diff(range(across[j])) / 2 + 1) * cellsize
      height <- (diff(range(row[j])) + 1) * row_height
      abs(log(width / height))
    }, numeric(1)))
    turned + apart + false + shift + 0.25 * thin
  }
  owner_index <- function(j, p) match(owner[j], p)
  edges <- cbind(rep(seq_along(adjacent), lengths(adjacent)), unlist(adjacent))

  # Cuts the cells of a node (a piece, a vector of pieces to cut automatically,
  # or a list of nodes) between its pieces. Each cut is chosen by the score of
  # the blocks after completing the layout below it with the best cuts by
  # their own score, for nodes of up to 12 pieces.
  owner <- integer(length(lattice))
  slice <- function(node, cells, look_ahead = TRUE) {
    if (!is.list(node) && length(node) == 1) {
      owner[cells] <<- node
      return(invisible())
    }
    options <- list()
    for (axis in 1:2) {
      if (is.list(node)) {
        options <- c(options, list(cut_cells(node, cells, axis)))
        next
      }
      along <- if (axis == 1) centroid[node, 1] else -centroid[node, 2]
      sorted <- node[order(along)]
      for (k in seq_len(length(node) - 1)) {
        sides <- list(sorted[seq_len(k)], sorted[-seq_len(k)])
        options <- c(options, list(cut_cells(sides, cells, axis)))
      }
    }
    score <- vapply(options, `[[`, numeric(1), "score")
    look_ahead <- look_ahead && length(unlist(node)) <= 12
    if (look_ahead) {
      score <- vapply(options, function(option) {
        for (k in seq_along(option$children)) {
          slice(option$children[[k]], option$cells[[k]], look_ahead = FALSE)
        }
        layout_score(unlist(node), cells)
      }, numeric(1))
    }
    best <- options[[which.min(score)]]
    for (k in seq_along(best$children)) {
      slice(best$children[[k]], best$cells[[k]], look_ahead)
    }
  }

  # Lay out each landmass, largest first
  index <- integer(length(geometry))
  taken <- rep(FALSE, length(lattice))
  lands <- unique(piece_land)
  lands <- lands[order(-vapply(lands, function(l) sum(size[piece_land == l]), numeric(1)))]
  for (land in lands) {
    p <- which(piece_land == land)
    n <- sum(size[p])
    real <- c(range(piece_box[p, c(1, 3)]), range(piece_box[p, c(2, 4)]))
    aspect <- (real[2] - real[1]) / max(real[4] - real[3], cellsize)
    middle <- colSums(centroid[p, , drop = FALSE] * size[p]) / n
    node <- if (is.null(tree)) p else tree_pieces(tree, stats::setNames(p, piece_group[p]))
    if (land != lands[1]) {
      # Place the block as near as possible to its scaled position from the
      # first landmass, at least two hexagons from other blocks
      middle <- c(rescale(middle[1], main_real[1:2], main_block[1:2]),
                  rescale(middle[2], main_real[3:4], main_block[3:4]))
      near <- taken | vapply(adjacent, function(k) any(taken[k]), logical(1))
      blocked <- near | vapply(adjacent, function(k) any(near[k]), logical(1))
    }

    # Try blocks of several aspect ratios, keeping the best layout
    best <- Inf
    for (stretch in c(0.8, 1, 1.25, 1.5, 1.75)) {
      if (land == lands[1]) {
        cells <- block(n, stretch * aspect, middle)
      } else {
        cells <- NULL
        for (j in order((to[, 1] - middle[1])^2 + (to[, 2] - middle[2])^2)) {
          candidate <- block(n, stretch * aspect, to[j, ])
          if (!anyNA(candidate) && length(candidate) == n && !any(blocked[candidate])) {
            cells <- candidate
            break
          }
        }
        if (is.null(cells)) stop("Could not find room for every landmass.", call. = FALSE)
      }
      slice(node, cells)
      score <- layout_score(p, cells) + 0.25 * abs(log(stretch))
      if (score < best) {
        best <- score
        best_cells <- cells
        best_owner <- owner[cells]
      }
    }
    cells <- best_cells
    owner[cells] <- best_owner
    taken[cells] <- TRUE
    if (land == lands[1]) {
      main_real <- real
      main_block <- c(range(to[cells, 1]), range(to[cells, 2]))
    }

    # Make each group's hexagons contiguous, then allocate them to regions
    tile_group <- contiguous_groups(owner[cells], lapply(adjacent[cells], function(k) {
      k <- match(k, cells)
      k[!is.na(k)]
    }))
    if (!attr(tile_group, "contiguous")) {
      warning("Could not make the tiles of every group contiguous.", call. = FALSE)
    }
    for (g in p) {
      ig <- which(region_piece == g)
      jg <- cells[tile_group == g]
      # Each region's position within its piece, scaled to the piece's block
      layout <- cbind(rescale(point[ig, 1], piece_box[g, c(1, 3)], range(to[jg, 1])),
                      rescale(point[ig, 2], piece_box[g, c(2, 4)], range(to[jg, 2])))
      index[ig] <- jg[solve_assignment(layout, to[jg, , drop = FALSE])]
    }
  }
  list(tile = lattice, index = index)
}

# Linearly maps x from one range to another
rescale <- function(x, from, to) {
  if (diff(from) <= 0) return(rep(mean(to), length(x)))
  to[1] + (x - from[1]) / diff(from) * diff(to)
}

# Converts a tree of group names to a tree of pieces (named by their group),
# dropping groups without pieces. Character vectors of several groups are
# split automatically.
tree_pieces <- function(node, pieces) {
  if (is.character(node)) {
    p <- unname(pieces[names(pieces) %in% node])
    return(if (length(p)) p else NULL)
  }
  children <- Filter(length, lapply(node, tree_pieces, pieces))
  if (!length(children)) return(NULL)
  if (length(children) == 1) return(children[[1]])
  children
}

# The direction from each shape to each other shape, as unit vectors. Across
# a shared border, this is the normal to the border's main direction (from
# the length weighted orientation of its segments), blended with the
# direction between centroids as the border becomes less elongated (e.g. an
# enclosed shape). Shapes without a shared border use the direction between
# centroids.
#' @importFrom sf st_boundary st_buffer st_coordinates st_intersection
border_direction <- function(shape, centroid, border, tolerance) {
  dx <- outer(centroid[, 1], centroid[, 1], function(a, b) b - a)
  dy <- outer(centroid[, 2], centroid[, 2], function(a, b) b - a)
  d <- pmax(sqrt(dx^2 + dy^2), 1e-9)
  dx <- dx / d
  dy <- dy / d

  boundary <- st_boundary(shape)
  near <- st_buffer(shape, tolerance)
  for (a in seq_along(shape)) for (b in seq_along(shape)) {
    if (a >= b || border[a, b] <= 0) next
    line <- st_coordinates(st_intersection(boundary[a], near[b]))
    if (!nrow(line)) next
    piece <- line[, ncol(line)]
    same <- piece[-1] == piece[-nrow(line)]
    sx <- diff(line[, 1])[same]
    sy <- diff(line[, 2])[same]
    # The main direction of the border and how elongated it is
    sxx <- sum(sx^2)
    syy <- sum(sy^2)
    sxy <- sum(sx * sy)
    angle <- atan2(2 * sxy, sxx - syy) / 2
    spread <- sqrt((sxx - syy)^2 + 4 * sxy^2) / max(sxx + syy, 1e-9)
    normal <- c(-sin(angle), cos(angle))
    if (sum(normal * c(dx[a, b], dy[a, b])) < 0) normal <- -normal
    blend <- spread * normal + (1 - spread) * c(dx[a, b], dy[a, b])
    blend <- blend / max(sqrt(sum(blend^2)), 1e-9)
    dx[a, b] <- blend[1]
    dy[a, b] <- blend[2]
    dx[b, a] <- -blend[1]
    dy[b, a] <- -blend[2]
  }
  list(x = dx, y = dy)
}
