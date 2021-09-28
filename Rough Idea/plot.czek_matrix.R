plot.czek_matrix = function(x, values = NULL, type = "symbols", plot_pch = NULL, plot_title = "Czekanowski's diagram",
                            legend = FALSE, axis = TRUE, tl.cex = 0.9, tl.offset = 0.4, tl.srt = 90, ...){

  oldpar = par(mar = c(0, 0, 3, 0))
  on.exit(par(oldpar))

  n_classes <- attr(x, "n_classes")
  levels <- attr(x, "levels")
  partition_boundaries <- attr(x, "partition_boundaries")
  new_order <- attr(x, "order")
  names = colnames(x)[new_order]
  x <- x[new_order, new_order]

  if (class(values) %in% c("numeric", "character") & length(values) == n_classes) {
    values <- values
  }else if (type == "symbols") {
    values <- rep(0, n_classes)
    values[1] <- 1
    for (i in 2:(n_classes - 1)) {
      values[i] <- values[i - 1]/2
    }
    values[n_classes] <- 0
  }else if (type == "col") {
    values <- round(seq(0, 100, length.out = n_classes))
    values <- paste("gray", values, sep = "")
  }else stop("type should be either 'col' or 'symbols'")

  n <- nrow(x)
  plot_values <- values[x]
  mat <- matrix(plot_values, nrow = n, ncol = n, byrow = FALSE)
  dimnames(mat) = list(names, names)
  plot_y <- rep(n:1, n)
  plot_x <- rep(1:n, rep(n, n))

  # draw a matrix
  plot.new()
  xlabwidth = max(strwidth(names, cex = tl.cex))
  ylabwidth = max(strwidth(names, cex = tl.cex))
  laboffset = strwidth("W", cex = tl.cex)
  heigh_title = strheight("W", cex = par()$cex.main)

  for (i in 1:50) {
    xlim = c(1 - 0.5 - laboffset - xlabwidth,
             n + 0.5 + xlabwidth * abs(cos(tl.srt * pi/180)))
    ylim = c(1 - 0.5 - laboffset - ylabwidth * abs(sin(tl.srt * pi/180)),
             n + 0.5 + laboffset)

    plot.window(xlim, ylim, asp = 1, xaxs = "i", yaxs = "i")

    x.tmp = max(strwidth(names, cex = tl.cex))
    y.tmp = max(strwidth(names, cex = tl.cex))
    laboffset.tmp = strwidth("W", cex = tl.cex) * tl.offset
    if (max(x.tmp - xlabwidth, y.tmp - ylabwidth, laboffset.tmp - laboffset) < 0.001) {
      break
    }
    xlabwidth = x.tmp
    ylabwidth = y.tmp
    laboffset = laboffset.tmp
    if (i == 50) {
      warning(c("Not been able to calculate text margin, ",
                "please try again with a clean new empty window using ",
                "{plot.new(); dev.off()} or reduce tl.cex"))
    }
  }

  if (.Platform$OS.type == "windows") {
    grDevices::windows.options(width = 7, height = 7 * diff(ylim)/diff(xlim))
  }

  xlim = xlim + diff(xlim) * 0.01 * c(-1, 1)
  ylim = ylim + diff(ylim) * 0.01 * c(-1, 1)

  plot.window(xlim = xlim, ylim = ylim, asp = 1, xlab = "", ylab = "", xaxs = "i", yaxs = "i")

  if (is.character(plot_values)) {
    symbols(plot_x, plot_y, add = TRUE, inches = FALSE, squares = rep(1, length(plot_x)), fg = plot_values, bg = plot_values)
  }else if (is.numeric(plot_values)) {
    symbols(plot_x, plot_y, add = TRUE, inches = FALSE, circles = plot_values^0.5/2, bg = "black")

    # hide the zero points
    ind.p = which(plot_values == 0)
    symbols(plot_x[ind.p], plot_y[, 2][ind.p],
            inches = FALSE, squares = rep(1, length(plot_x[ind.p])), fg = "white", bg = "white", add = TRUE)
  }

  rect(0.5, 0.5, n + 0.5, n + 0.5, border = "black")

  cex = seq(0.5, 1, by = 0.1)
  height = sapply(cex, function(cex) strheight("A", cex = cex)) * n
  short_aes = min(diff(xlim), diff(ylim))
  tl.cex = cex[which.min(abs(height - short_aes))]

  # x and y labels
  pos.xlabel = cbind((1:n), -5)
  pos.ylabel = cbind(1 - 0.5- tl.offset, n:1)

  graphics::axis(1, at = pos.xlabel[, 1], tick = FALSE, labels = names, las = 2, cex.axis = tl.cex, pos = pos.xlabel)
  graphics::axis(2, at = pos.ylabel[, 2], tick = FALSE, labels = names, las = 1, cex.axis = tl.cex, pos = pos.ylabel)

  title(main = plot_title)
}




