# This function extract drought periods from a PDSI time series
# A drought period is a period with consecutive negative PDSI
# We can implement a tolerance if needed
# For example, if tol = 0.1 then a drought period is a consecutive
# period with PDSI < 0.1
get_droughts <- function(x, tol = 0) {
  N <- length(x)
  neg.ind <- which(x < tol)
  seq.start <- neg.ind[which(diff(c(0, neg.ind)) > 1)]
  if (x[1] < tol) seq.start <- c(1, seq.start)
  # End of drought sequence is start of pluvial sequence minus one
  pos.ind <- which(x >= tol)
  seq.end <- pos.ind[which(diff(c(0, pos.ind)) > 1)] - 1
  if (x[N] < tol) seq.end <- c(seq.end, N)
  
  data.table(
    dn = 1:length(seq.start),
    start = seq.start,
    final = seq.end,
    dur = seq.end - seq.start + 1,
    peak = sapply(1:length(seq.start), function(k) min(x[seq.start[k]:seq.end[k]])),
    mean = sapply(1:length(seq.start), function(k) mean(x[seq.start[k]:seq.end[k]])),
    deficit = sapply(1:length(seq.start), function(k) sum(x[seq.start[k]:seq.end[k]]))
  )
}

# This function calculates drought area index (DAI)
get_dai <- function(DT, start, end) {
  numPoints <- DT[year == first(year), .N]
  DT[year %between% c(start, end), .(pdsi = mean(pdsi)), by = .(lon, lat)
  ][pdsi < 0, .N] / numPoints
}

# This function plots the spatial distribution of a drought 
plot_drought_map <- function(DT, col.lim = NULL) {
  if (is.null(col.lim)) col.lim <- abs_range(DT$pdsi)
  ggplot(DT) +
    geom_tile(aes(lon, lat, fill = pdsi)) +
    geom_polygon(aes(long, lat, group = group), 
                 map_data('state'),
                 fill = NA,
                 color = 'gray10') +
    scale_x_continuous(expand = c(0, 0), labels = degree_lon) +
    scale_y_continuous(expand = c(0, 0), labels = degree_lat) +
    scale_fill_distiller(palette = 'BrBG', direction = 1, limits = col.lim) +
    labs(x = NULL, y = NULL, fill = 'PDSI') +
    coord_quickmap(xlim = range(DT$lon), ylim = range(DT$lat))
}
