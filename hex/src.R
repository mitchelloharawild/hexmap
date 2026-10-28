devtools::load_all(quiet = TRUE)
library(sf)

if (!exists("out_file")) out_file <- "hex/hexmap.svg"

# Wordmark face, with its ink measured per em: width of "hexmap", height of the first
# letter and of the rest, depth below the baseline, and width of the first letter.
# Cinzel sets lowercase as small capitals; it is bundled in hex/fonts (SIL OFL).
if (!exists("font")) {
  font <- list(family = "Cinzel", weight = 700, stack = "'Trajan Pro', Georgia, serif",
               width = 4.00, top_first = .60, top_rest = .63, bottom = .02, first = .68)
}

# Decoration of the sea behind the map: "graticule", "neatline", "hexgrid", "compass",
# "rhumb" or "none"
if (!exists("background")) background <- "compass"

# ---- Canvas and palette (Old chart) ------------------------------------------

W <- 520; H <- 600
CX <- 260; CY <- 300; R <- 292

pal <- list(
  paper = "#f3e3bd", paper2 = "#e6cc96", edge = "#a9773f", ink = "#3f2a17",
  sepia = "#6b4a2b", red = "#a3302a", ochre = "#b8572f", shade = "#c9a66b", tree = "#7d8c4f",
  land = c(WA = "#dcc28a", NT = "#d2b47a", SA = "#dabb80", QLD = "#cfb47c",
           NSW = "#d8bf88", VIC = "#d1b67d", TAS = "#d4b780", ACT = "#d8bf88")
)

# ---- SVG helpers -------------------------------------------------------------

fmt <- function(v) {
  s <- sprintf("%.1f", v)
  sub("\\.0$", "", s)
}
pts <- function(x, y) paste0(fmt(x), ",", fmt(y), collapse = " ")
svg_path <- function(x, y, close = FALSE) {
  paste0("M", paste0(fmt(x), ",", fmt(y), collapse = " L"), if (close) " Z" else "")
}
group <- function(content, x = 0, y = 0, rot = 0, scale = 1) {
  sprintf(
    '<g transform="translate(%s %s) rotate(%s) scale(%s)">%s</g>',
    fmt(x), fmt(y), fmt(rot), paste(signif(scale, 3), collapse = " "), paste(content, collapse = "")
  )
}
hexagon <- function(r = R) {
  a <- seq(0, 300, by = 60) * pi / 180
  list(x = CX + r * sin(a), y = CY - r * cos(a))
}

# ---- The map: hexmap of the 2025 federal electorates ------------------------

elec <- st_transform(aus_election2025, 3577)
hx_map <- hexmap(elec, group = state)

# Mitred buffers keep the coastline hexagonal
coastline <- function(d) {
  st_union(st_buffer(st_geometry(hx_map), d, joinStyle = "MITRE", mitreLimit = 3))
}
land <- coastline(1500)
# Ripples run parallel to the whole coastline, rather than around each hexagon
offshore <- function(px) st_buffer(land, px / s, joinStyle = "MITRE", mitreLimit = 3)

# ---- Wordmark placement: along the lower-left edge ---------------------------

wm <- local({
  hx <- hexagon()
  mx <- (hx$x[4] + hx$x[5]) / 2; my <- (hx$y[4] + hx$y[5]) / 2
  up <- c(CX - mx, CY - my) / sqrt((CX - mx)^2 + (CY - my)^2)
  size <- 315 / font$width
  inset <- 46 + font$bottom * size
  list(x = mx + up[1] * inset, y = my + up[2] * inset, up = up, size = size, len = font$width * size,
       ang = atan2(hx$y[4] - hx$y[5], hx$x[4] - hx$x[5]) * 180 / pi)
})

# ---- Fit: the largest map that clears the border and the wordmark ------------

ripple_gap <- 6.5   # px between coastline ripples; three ripples, mitred at the corners
reach <- 3 * ripple_gap / cos(pi / 6)
border_gap <- 14 + 10 * (background == "neatline")  # px of open sea inside the border
coast_xy <- st_coordinates(land)[, 1:2]
bb <- st_bbox(land)
mid <- c((bb[1] + bb[3]) / 2, (bb[2] + bb[4]) / 2)

# Points on the sticker that fall within padding `pad` of the wordmark's letters
on_wordmark <- function(px, py, pad) {
  # Wordmark frame: u along the baseline, v towards the centre
  ux <- px - wm$x; uy <- py - wm$y
  u <- ux * wm$up[2] * -1 + uy * wm$up[1]; v <- ux * wm$up[1] + uy * wm$up[2]
  # The first letter can stand taller than the rest of the word
  top <- ifelse(u < -wm$len / 2 + font$first * wm$size, font$top_first, font$top_rest) * wm$size
  abs(u) < wm$len / 2 + pad & v > -font$bottom * wm$size - pad & v < top + pad
}
# Points on the sticker within `r` of the centre, measured to the nearest edge
in_hexagon <- function(px, py, r) {
  dx <- abs(px - CX); dy <- abs(py - CY)
  dx <= r * sqrt(3) / 2 & dy + dx / sqrt(3) <= r
}
fits <- function(s, cx, cy) {
  px <- cx + (coast_xy[, 1] - mid[1]) * s; py <- cy - (coast_xy[, 2] - mid[2]) * s
  all(in_hexagon(px, py, R - 6 - border_gap - reach)) && !any(on_wordmark(px, py, reach + 6))
}
fit <- local({
  for (s in seq(1.6e-4, 0.6e-4, by = -.01e-4)) {
    grid <- expand.grid(cx = seq(180, 340, by = 3), cy = seq(200, 360, by = 3))
    ok <- mapply(fits, s, grid$cx, grid$cy)
    # Centre the map within the positions that fit
    if (any(ok)) return(c(s = s, colMeans(grid[ok, ])))
  }
})
s <- fit[["s"]]
to_px <- function(xy) cbind(fit[["cx"]] + (xy[, 1] - mid[1]) * s, fit[["cy"]] - (xy[, 2] - mid[2]) * s)

# sf geometry -> SVG path data, one subpath per ring
geom_d <- function(g) {
  xy <- st_coordinates(g)
  ring <- interaction(as.data.frame(xy[, grep("^L", colnames(xy)), drop = FALSE]), drop = TRUE)
  p <- to_px(xy[, 1:2])
  paste(vapply(split(seq_len(nrow(p)), ring), function(i) {
    q <- p[i, , drop = FALSE]
    q <- q[!duplicated(round(q, 1)), , drop = FALSE]
    svg_path(q[, 1], q[, 2], TRUE)
  }, ""), collapse = " ")
}

centre <- to_px(st_coordinates(suppressWarnings(st_centroid(st_geometry(hx_map)))))
r_hex <- max(dist(st_coordinates(st_geometry(hx_map)[1])[, 1:2])) / 2 * s

# ---- Terrain: coarse landforms placed where they sit on a map of Australia --

# Each hexagon's own position, read as a longitude and latitude
hex_pt <- suppressWarnings(st_centroid(st_geometry(hx_map)))
lonlat <- st_coordinates(st_transform(hex_pt, 4326))
# Rings inland from the coast (1 = coastal), with the gaps between states closed
r_in <- sqrt(3) / 2 * r_hex / s
ring <- round((as.numeric(st_distance(hex_pt, st_boundary(coastline(r_in / 2)))) / r_in + 1) / 2)

# Rows of the hexmap, and each hexagon's place in its row counted from the east coast
row <- round(centre[, 2] / (1.5 * r_hex))
from_east <- ave(-centre[, 1], row, FUN = rank)
vic <- hx_map$state == "VIC"
tas <- hx_map$state == "TAS"
indi <- which(hx_map$division == "Indi")
step <- sqrt(3) * r_hex

terrain <- rep("none", nrow(hx_map))
terrain[lonlat[, 1] < 141 & lonlat[, 2] > -33 & ring >= 2] <- "desert"
# Karri country, Cape York and Tasmania
terrain[(lonlat[, 1] < 120 & lonlat[, 2] < -30) | (lonlat[, 1] > 141 & lonlat[, 2] > -17) | tas] <- "forest"
# Victorian forests: the high country behind the Gippsland coast, and the south-west
terrain[vic & row >= row[indi] & ring >= 2 & centre[, 1] > centre[indi, 1] - 1.1 * step] <- "forest"
terrain[vic & row >= row[indi] - 1 & centre[, 1] < min(centre[vic, 1]) + 2.6 * step] <- "forest"
# The Great Dividing Range: one hexagon in from the east coast from central Queensland
# down to Indi, then the Victorian Alps
alps <- which(vic & row == row[indi] - 1 & ring >= 2)
alps <- alps[order(-centre[alps, 1])][1:2]
terrain[(from_east == 2 & lonlat[, 2] < -20.5 & row <= row[indi]) | seq_along(terrain) %in% alps] <- "mountain"
# Tasmania's highlands in the west, open country in the east
tas_south <- which(tas & row == max(row[tas]))
tas_south <- tas_south[order(centre[tas_south, 1])]
terrain[tas_south[2]] <- "mountain"
terrain[tas_south[length(tas_south)]] <- "none"
# Uluru sits in the division that holds it, wherever that lands on the hexmap
terrain[hx_map$division == "Lingiari"] <- "uluru"

glyph <- list(
  mountain = c(
    sprintf('<path d="M-12,6 L-4,-8 L4,6 Z M-2,6 L5,-4 L12,6 Z" fill="%s" stroke="%s" stroke-width="1.4" stroke-linejoin="round"/>', pal$paper, pal$ink),
    sprintf('<path d="M-4,-8 L4,6 L-2,6 Z M5,-4 L12,6 L7,6 Z" fill="%s"/>', pal$shade),
    sprintf('<path d="M-4,-8 L-7,-3 L-5,-3.5 L-4,-2 Z" fill="%s"/>', pal$ink)),
  forest = vapply(list(c(-5, 1), c(5, -1), c(0, 6)), function(p) sprintf(
    '<path d="M%1$s,%2$s L%3$s,%4$s L%5$s,%4$s Z" fill="%6$s" stroke="%7$s" stroke-width="1.2" stroke-linejoin="round"/><path d="M%1$s,%4$s L%1$s,%8$s" stroke="%7$s" stroke-width="1.4"/>',
    fmt(p[1]), fmt(p[2] - 9), fmt(p[1] - 4.5), fmt(p[2] + 2), fmt(p[1] + 4.5), pal$tree, pal$ink, fmt(p[2] + 4.5)), ""),
  desert = sprintf('<path d="M-11,-1 q5,-6 10,0 M-1,6 q5,-6 10,0 M3,-5 q4,-4.5 8,0" fill="none" stroke="%s" stroke-width="1.4" stroke-linecap="round"/>', pal$sepia),
  uluru = c(
    sprintf('<path d="M-11,5 C-10,-3 -7,-6 -2,-6.5 C4,-7 8.5,-4 11,5 Z" fill="%s" stroke="%s" stroke-width="1.4" stroke-linejoin="round"/>', pal$ochre, pal$ink),
    sprintf('<path d="M-6,4 C-6,0 -5,-3 -4,-5 M-1,4 C-1,0 -1,-3 0,-6 M4,4 C4,0 4.5,-3 5,-5" fill="none" stroke="%s" stroke-width="1" stroke-linecap="round" opacity=".7"/>', pal$ink),
    sprintf('<path d="M-12.5,5 L12.5,5" stroke="%s" stroke-width="1.4" stroke-linecap="round"/>', pal$ink)),
  none = character()
)
# Glyphs are drawn within a radius of 13.5, kept inside the hexagon with a margin
g_scale <- r_hex * sqrt(3) / 2 * .8 / 13.5
terrain_svg <- vapply(seq_len(nrow(hx_map)), function(i) {
  if (!length(glyph[[terrain[i]]])) return("")
  group(glyph[[terrain[i]]], centre[i, 1], centre[i, 2] + r_hex * .05, scale = g_scale)
}, "")

# ---- Map layers: coast ripples, cells, coastline -----------------------------

ripples <- vapply(1:3, function(k) sprintf(
  '<path d="%s" fill="none" stroke="%s" stroke-width="1" opacity="%s"/>',
  geom_d(offshore(k * ripple_gap)), pal$sepia, fmt(.62 - .16 * k)), "")
sea_wash <- sprintf('<path d="%s" fill="%s" opacity=".35"/>', geom_d(offshore(ripple_gap)), pal$paper)

cells <- vapply(seq_len(nrow(hx_map)), function(i) sprintf(
  '<path d="%s" fill="%s" stroke="%s" stroke-width=".9" stroke-opacity=".55"/>',
  geom_d(st_geometry(hx_map)[i]), pal$land[[hx_map$state[i]]], pal$sepia), "")
coast <- sprintf('<path d="%s" fill="none" stroke="%s" stroke-width="2.6" stroke-linejoin="round"/>',
                 geom_d(land), pal$ink)

# ---- Open sea: points clear of the ripples, the border and the wordmark ------

sea <- local({
  grid <- expand.grid(x = seq(0, W, by = 4), y = seq(0, H, by = 4))
  grid <- grid[in_hexagon(grid$x, grid$y, R - 6) & !on_wordmark(grid$x, grid$y, 8), ]
  pt <- st_sfc(lapply(seq_len(nrow(grid)), function(i) st_point(c(
    mid[1] + (grid$x[i] - fit[["cx"]]) / s, mid[2] - (grid$y[i] - fit[["cy"]]) / s))), crs = st_crs(land))
  # Room around each point: to the outer ripple, and to the inside of the border
  coast <- as.numeric(st_distance(pt, offshore(3 * ripple_gap))) * s
  inner <- R - 6
  dx <- abs(grid$x - CX); dy <- abs(grid$y - CY)
  border <- pmin(inner * sqrt(3) / 2 - dx, (inner - dy - dx / sqrt(3)) * sqrt(3) / 2)
  # and to the box around the wordmark, in its own frame
  ux <- grid$x - wm$x; uy <- grid$y - wm$y
  u <- -ux * wm$up[2] + uy * wm$up[1]; v <- ux * wm$up[1] + uy * wm$up[2]
  du <- pmax(abs(u) - wm$len / 2, 0)
  dv <- pmax(v - max(font$top_first, font$top_rest) * wm$size, -font$bottom * wm$size - v, 0)
  grid$room <- pmin(coast, border, sqrt(du^2 + dv^2))
  grid[grid$room > 0, ]
})

# ---- Compass rose, set in the most open water ----------------------------------

star <- function(r_out, r_in, rot = 0) {
  # Each point split into a light and a dark half, as on engraved roses
  vapply(seq(0, 270, by = 90) + rot, function(deg) {
    a <- deg * pi / 180; b <- a + pi / 4
    tip <- c(sin(a), -cos(a)) * r_out
    l <- c(sin(a - pi / 4), -cos(a - pi / 4)) * r_in
    rr <- c(sin(b), -cos(b)) * r_in
    sprintf('<path d="%s" fill="%s" stroke="%s" stroke-width="1.1" stroke-linejoin="round"/><path d="%s" fill="%s" stroke="%s" stroke-width="1.1" stroke-linejoin="round"/>',
            svg_path(c(0, tip[1], l[1]), c(0, tip[2], l[2]), TRUE), pal$paper, pal$ink,
            svg_path(c(0, tip[1], rr[1]), c(0, tip[2], rr[2]), TRUE), pal$sepia, pal$ink)
  }, "")
}
rose <- c(
  sprintf('<circle r="31" fill="%s" stroke="%s" stroke-width="1.4"/>', pal$paper, pal$ink),
  sprintf('<circle r="26" fill="none" stroke="%s" stroke-width=".8"/>', pal$ink),
  vapply(seq(0, 350, by = 10) * pi / 180, function(a) sprintf(
    '<path d="M%s,%s L%s,%s" stroke="%s" stroke-width=".8"/>',
    fmt(26 * sin(a)), fmt(-26 * cos(a)), fmt(31 * sin(a)), fmt(-31 * cos(a)), pal$ink), ""),
  star(25, 7, 45), star(38, 8),
  sprintf('<circle r="2.6" fill="%s" stroke="%s" stroke-width="1"/>', pal$red, pal$ink),
  # Fleur-de-lis finial marking north
  sprintf('<path d="M0,-38 C-4,-44 -9,-43 -9,-49 C-5,-47 -2,-48 0,-53 C2,-48 5,-47 9,-49 C9,-43 4,-44 0,-38 Z" fill="%s" stroke="%s" stroke-width="1.1" stroke-linejoin="round"/>', pal$red, pal$ink)
)
# The rose reaches 53 units from its centre at the finial
rose_at <- unlist(sea[which.max(sea$room), c("x", "y")])
rose_r <- min(max(sea$room) - 4, 40)

# ---- Waves --------------------------------------------------------------------

wave <- function(x, y) sprintf(
  '<path d="M%s,%s q4,-4 8,0 q4,4 8,0 q4,-4 8,0" fill="none" stroke="%s" stroke-width="1.4" stroke-linecap="round" opacity=".75"/>',
  fmt(x - 12), fmt(y), pal$sepia)
# Spread waves over the open sea; a wave spans 24 x 8px about its centre
waves <- local({
  grid <- sea[sea$room > 16, ]
  if (background == "compass") {
    grid <- grid[sqrt((grid$x - rose_at[1])^2 + (grid$y - rose_at[2])^2) > rose_r + 16, ]
  }
  # Farthest-point sampling from the point deepest in open water
  pick <- which.max(grid$y)
  for (k in 2:6) {
    d <- apply(as.matrix(dist(grid[, c("x", "y")]))[, pick, drop = FALSE], 1, min)
    pick <- c(pick, which.max(d))
  }
  vapply(pick, function(i) wave(grid$x[i], grid$y[i]), "")
})

# ---- Backdrops ------------------------------------------------------------------

line_d <- function(g) {
  xy <- st_coordinates(g)
  p <- to_px(xy[, 1:2])
  paste(vapply(split(seq_len(nrow(p)), xy[, "L1"]), function(i) svg_path(p[i, 1], p[i, 2]), ""), collapse = " ")
}

# Rhumb lines radiating from a wind rose, as on portolan charts
rhumbs <- function(at) vapply(seq(0, 337.5, by = 22.5) * pi / 180, function(a) sprintf(
  '<path d="M%s,%s L%s,%s" stroke="%s" stroke-width="%s" opacity=".28"/>',
  fmt(at[1]), fmt(at[2]), fmt(at[1] + 900 * sin(a)), fmt(at[2] - 900 * cos(a)),
  pal$sepia, if (round(a / (pi / 4), 6) %% 1 == 0) 1.2 else .7), "")

# Meridians and parallels every 10 degrees, curved as in the map's equal-area projection,
# with the Tropic of Capricorn dashed
graticule <- local({
  ln <- function(lon, lat) st_linestring(cbind(lon, lat))
  grid <- c(lapply(seq(90, 180, 10), function(lon) ln(lon, seq(-60, 10, .5))),
            lapply(seq(-60, 10, 10), function(lat) ln(seq(90, 180, .5), lat)))
  proj <- function(g) st_transform(st_sfc(g, crs = 4326), st_crs(land))
  c(sprintf('<path d="%s" fill="none" stroke="%s" stroke-width=".8" opacity=".35"/>', line_d(proj(grid)), pal$sepia),
    sprintf('<path d="%s" fill="none" stroke="%s" stroke-width="1.2" stroke-dasharray="6 4" opacity=".55"/>',
            line_d(proj(list(ln(seq(90, 180, .5), -23.44)))), pal$sepia))
})

# The hexagon tiling the map was cut from, carried on across the sea
hexgrid <- local({
  r_m <- r_hex / s; dx <- sqrt(3) * r_m; dy <- 1.5 * r_m
  c0 <- st_coordinates(hex_pt[1])
  cells <- expand.grid(i = -30:30, j = -30:30)
  hx <- c0[1] + cells$i * dx + (cells$j %% 2) * dx / 2
  hy <- c0[2] + cells$j * dy
  p <- to_px(cbind(hx, hy))
  keep <- in_hexagon(p[, 1], p[, 2], R + r_hex)
  a <- seq(30, 330, by = 60) * pi / 180
  d <- vapply(which(keep), function(k) svg_path(p[k, 1] + r_hex * cos(a), p[k, 2] + r_hex * sin(a), TRUE), "")
  sprintf('<path d="%s" fill="none" stroke="%s" stroke-width=".7" opacity=".3"/>', paste(d, collapse = " "), pal$sepia)
})

# A scale-bar border just inside the frame: eight alternating bars along each edge
neatline <- local({
  band <- hexagon(R - 11); rule <- hexagon(R - 14.5)
  bar <- (R - 11) / 8
  c(sprintf('<polygon points="%s" fill="none" stroke="%s" stroke-width="6" stroke-linejoin="miter"/>', pts(band$x, band$y), pal$paper),
    sprintf('<polygon points="%s" fill="none" stroke="%s" stroke-width="6" stroke-dasharray="%s %s" stroke-linejoin="miter"/>',
            pts(band$x, band$y), pal$ink, fmt(bar), fmt(bar)),
    sprintf('<polygon points="%s" fill="none" stroke="%s" stroke-width="1" stroke-linejoin="miter"/>', pts(rule$x, rule$y), pal$ink))
})

backdrop <- switch(background,
  rhumb = rhumbs(c(70, 110)), compass = rhumbs(rose_at), graticule = graticule, hexgrid = hexgrid,
  character())
overlay <- switch(background,
  compass = group(rose, rose_at[1], rose_at[2], scale = rose_r / 53), neatline = neatline,
  character())

# ---- Wordmark along the lower-left edge ---------------------------------------

wordmark <- function() {
  sprintf(paste0('<text transform="translate(%s %s) rotate(%s)" text-anchor="middle" ',
                 'font-family="%s, %s" font-weight="%s" font-size="%s" fill="%s">hexmap</text>'),
          fmt(wm$x), fmt(wm$y), fmt(wm$ang), font$family, font$stack, font$weight, fmt(wm$size), pal$ink)
}

# ---- Sticker --------------------------------------------------------------------

defs <- c(
  '<radialGradient id="hexmap-paper" cx="50%" cy="46%" r="62%">',
  sprintf('<stop offset="0" stop-color="%s"/><stop offset=".7" stop-color="%s"/><stop offset="1" stop-color="%s"/></radialGradient>',
          pal$paper, pal$paper2, pal$edge),
  '<filter id="hexmap-stain" x="0" y="0" width="100%" height="100%">',
  '<feTurbulence type="fractalNoise" baseFrequency=".012" numOctaves="3" seed="11"/>',
  '<feColorMatrix values="0 0 0 0 .55  0 0 0 0 .36  0 0 0 0 .16  0 0 0 1.1 -.42"/></filter>',
  '<filter id="hexmap-grain" x="0" y="0" width="100%" height="100%">',
  '<feTurbulence type="fractalNoise" baseFrequency=".8" numOctaves="2" seed="4"/>',
  '<feColorMatrix values="0 0 0 0 .4  0 0 0 0 .26  0 0 0 0 .12  0 0 0 .5 -.12"/></filter>'
)

body <- c(
  sprintf('<rect width="%s" height="%s" fill="url(#hexmap-paper)"/>', W, H),
  sprintf('<rect width="%s" height="%s" filter="url(#hexmap-stain)" opacity=".55"/>', W, H),
  sprintf('<rect width="%s" height="%s" filter="url(#hexmap-grain)" opacity=".3"/>', W, H),
  backdrop, waves, sea_wash, ripples, cells, coast, terrain_svg,
  overlay, wordmark()
)

hx <- hexagon()
svg <- paste0(
  sprintf('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %s %s" role="img" aria-label="hexmap hex sticker">', W, H),
  sprintf('<defs>%s<clipPath id="clip-hexmap"><polygon points="%s"/></clipPath></defs>',
          paste(defs, collapse = ""), pts(hx$x, hx$y)),
  sprintf('<g clip-path="url(#clip-hexmap)">%s</g>', paste(body, collapse = "")),
  sprintf('<polygon points="%s" fill="none" stroke="%s" stroke-width="12" stroke-linejoin="miter"/></svg>', pts(hx$x, hx$y), pal$ink)
)

# ---- Outline the wordmark so the sticker needs no installed fonts ------------

conf <- tempfile(fileext = ".conf")
writeLines(sprintf(paste0(
  '<?xml version="1.0"?><!DOCTYPE fontconfig SYSTEM "fonts.dtd"><fontconfig>',
  '<include ignore_missing="yes">/etc/fonts/fonts.conf</include><dir>%s</dir></fontconfig>'),
  normalizePath("hex/fonts")), conf)
raw <- tempfile(fileext = ".svg")
writeLines(svg, raw)
withr::with_envvar(c(FONTCONFIG_FILE = conf), system2("inkscape", c(
  raw, "--export-text-to-path", "--export-plain-svg", paste0("--export-filename=", out_file)
), stdout = FALSE, stderr = FALSE))
