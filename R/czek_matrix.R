#' @title Preprocess data to produce Czekanowski's Diagram.
#'@param x  a numeric matrix, data frame or a 'dist' object.
#'@param order specifies which seriation method should be applied. The standard setting is the seriation method OLO. If NA or NULL, then no seriation is done and the original ordering is saved.  The user may provide their own ordering, through a number vector of indices. Also in this case no rearrangement will be done.
#'@param n_classes specifies how many classes the distances should be divided into. The standard setting is 5 classes.
#'@param interval_breaks specifies the partition boundaries for the distances. As a standard setting, each class represents an equal amount of distances. If the interval, breaks are positive and sum up to 1, then it is assumed that they specify percentages of the distances in each interval. Otherwise if provided as a numeric vector not summing up to 1, they specify the exact boundaries for the symbols representing distance groups.
#'@param monitor specifies if the distribution of the distances should be visualized. The standard setting is that the distribution will not be visualized. TRUE and "cumulativ_plot" is available.
#'@param distfun specifies which distance function should be used. Standard setting is the dist function which uses the Euclidean distance. The first argument of the function has to be the matrix or data frame containing the data.
#'@param scale_data specifies if the data set should be scaled. The standard setting is that the data will be scaled.
#'@param focal_obj Numbers or names of objects (rows if x is a dataset and not 'dist' object) that are not to take part in the reordering procedure. These observations will be placed as last rows and columns of the output matrix. See Details.
#'@param as_dist If TRUE, then the distance matrix of x is returned, with object ordering, instead of the matrix with the levels assigned in place of the original distances.
#'@param original_diagram If TRUE, then the returned matrix corresponds as close as possible to the original method proposed by Czekanowski (1909). The levels are column specific and not matrix specific. See Details
#'@param column_order_stat_grouping If original_diagram is TRUE, then here one can pass the partition boundaries for the ranking in each column.
#'@param dist_args specifies further parameters that can be passed on to the distance function.
#'@param cluster If TRUE, Czekanowski’s clustering is performed.
#'@param cluster_type pecifies the cluster type and it can be ’exact’ or ’fuzzy’.
#'@param num_cluster pecifies the number of clusters.
#'@param scale_bandwidth A ratio to control the wideness of the reaching range.
#'@param sig.lvl The threshold that decides the significance of change point
#'@param min.size Minimum number of observations between change points.
#'@param ... specifies further parameters that can be passed on to the seriate function in the seriation package.
#'
#'@return The function returns a matrix with class czek_matrix. The return from the function is expected to be passed to the plot function. If as_dist is passed as TRUE, then a czek_matrix_dist object is returned and this is not suitable for the plotting. As an attribute of the output the optimized criterion value is returned. However, this is a guess based on seriation::seriate()'s and seriation::criterion()'s manuals. If something else was optimized, e.g. due to user's parameters, then this will be wrong. If unable to guess, then NA saved in the attribute.
#'
#'
czek_matrix = function (x, order = "OLO", n_classes = 5, interval_breaks = NULL,
                        monitor = FALSE, distfun = dist, scale_data = TRUE, focal_obj = NULL,
                        as_dist = FALSE, original_diagram = FALSE, column_order_stat_grouping = NULL,
                        dist_args = list(), cluster = FALSE, cluster_type = "exact",
                        num_cluster = 3, sig.lvl = 0.05, scale_bandwidth = 0.05, min.size = 30, ...)
{
  if (!inherits(x, "dist")) {
    if (scale_data) {
      x <- scale(x)
    }
    x <- do.call(distfun, c(list(x), dist_args))
  }
  ordering_method <- order
  if (!is.null(focal_obj)) {
    x <- as.matrix(x)
    if (is.character(focal_obj)) {
      focal_obj <- which(is.element(colnames(x), focal_obj))
    }
    if ((length(focal_obj) > 0) && (is.numeric(focal_obj)) &&
        (min(focal_obj) > 0) && (max(focal_obj) < (ncol(x) + 1))) {
      if (length(unique(focal_obj)) == ncol(x)) {
        order <- NA
        v_order_nofocal <- NA
        x_org <- NA
        focal_obj <- NULL
        ordering_method <- "user provided ordering"
        warning("Parameter focal_obj spans all the objects! Treating as if no ordering was requested!")
      }
      else {
        if (length(focal_obj) != length(unique(focal_obj))) {
          focal_obj <- unique(focal_obj)
          warning("Repeated columns in focal_obj parameter, removing them!")
        }
        v_order_nofocal <- (1:ncol(x))[-focal_obj]
        x_org <- x
        x <- x[-focal_obj, -focal_obj, drop = FALSE]
      }
    }
    else {
      warning("There is some problem with the focal_obj parameter, ignoring it.")
      v_order_nofocal <- NA
      x_org <- NA
      focal_obj <- NULL
    }
    x <- as.dist(x)
  }
  res_seriate <- NA
  if ((all(is.numeric(order))) && (length(order) == attr(x, "Size"))) {
    new_order <- order
    ordering_method <- "user provided ordering"
  }
  else if (inherits(order[1], "character")) {
    if (order[1] == "ga") {
      .register_seriate_ga()
      order <- ".seriate_ga"
    }
    new_order <- seriation::get_order(seriation::seriate(x, method = order[1], ...))
  }
  else if ((is.na(order[1])) || (is.null(order[1]))) {
    new_order <- 1:attr(x, "Size")
    ordering_method <- "Identity"
    order <- NA
  }
  else {
    new_order <- 1:attr(x, "Size")
    ordering_method <- "Identity"
    order <- NA
  }
  if (!is.null(focal_obj)) {
    x <- x_org
    new_order <- c(v_order_nofocal[new_order], focal_obj)
  }
  x <- as.matrix(x)
  if (!as_dist) {
    if (!original_diagram) {
      if (is.null(interval_breaks)) {
        interval_breaks <- stats::quantile(x[upper.tri(x)], probs = seq(0, 1, length.out = n_classes + 1), na.rm = TRUE)
        interval_breaks[1] <- 0
      }
      else if ("equal_width_between_classes" %in% interval_breaks) {
        interval_breaks <- max(x[upper.tri(x)])/n_classes *
          (0:n_classes)
        probs <- (stats::ecdf(x[upper.tri(x)]))(interval_breaks)
        names(interval_breaks) <- paste(round(probs, 7) * 100, "%", sep = "")
      }
      else if ((all(interval_breaks >= 0)) && (sum(interval_breaks) ==
                                               1)) {
        probs <- c(0, cumsum(interval_breaks))
        interval_breaks <- stats::quantile(x[upper.tri(x)], probs = probs, na.rm = TRUE)
        interval_breaks[1] <- 0
      }
      else {
        interval_breaks[1] <- 0
        interval_breaks[length(interval_breaks)] <- max(x)
        probs <- ecdf(x[upper.tri(x)])(interval_breaks)
        names(interval_breaks) <- paste(round(probs, 7) * 100, "%", sep = "")
      }
      cut_the_values <- cut(x, interval_breaks, include.lowest = TRUE)
      czek_matrix <- matrix(as.numeric(cut_the_values), ncol = ncol(x))
      l_cut_the_values <- levels(cut_the_values)
    }
    else {
      b_default_ord_grouping <- FALSE
      if (is.null(column_order_stat_grouping)) {
        column_order_stat_grouping <- c(3, 4, 5, 6)
        b_default_ord_grouping <- TRUE
      }
      if (max(column_order_stat_grouping) > (ncol(x) - 1)) {
        if (!b_default_ord_grouping) {
          warning("Provided or default order statistics grouping is not compatible with matrix size. Correcting but please consider providing a compatible one.")
        }
        v_OKgroups <- which(column_order_stat_grouping < ncol(x))
        nOKgroups <- length(v_OKgroups)
        if (nOKgroups == 0) {
          if (ncol(x) == 0) {
            stop("")
          }
          else if (ncol(x) == 1) {
            column_order_stat_grouping <- 1
          }
          else if (ncol(x) == 2) {
            column_order_stat_grouping <- 1
          }
          else if (ncol(x) == 3) {
            column_order_stat_grouping <- 2
          }
          else if (ncol(x) == 4) {
            column_order_stat_grouping <- c(2, 3)
          }
          else if (ncol(x) == 5) {
            column_order_stat_grouping <- c(2, 3, 4)
          }
          else if (ncol(x) == 6) {
            column_order_stat_grouping <- c(2, 3, 4, 5)
          }
          else if (ncol(x) > 7) {
            column_order_stat_grouping <- c(3, 4, 5, 6)
          }
          else {
            stop("Something is wrong with provided order statistics groupings!")
          }
        }
        else if ((nOKgroups > 0) && (nOKgroups < 5)) {
          column_order_stat_grouping <- column_order_stat_grouping[v_OKgroups]
          while ((max(column_order_stat_grouping) < ncol(x) - 1) && (nOKgroups < 6)) {
            column_order_stat_grouping <- c(column_order_stat_grouping, max(column_order_stat_grouping) + 1)
            nOKgroups <- nOKgroups + 1
          }
        }
        else if (nOKgroups > 4) {
          column_order_stat_grouping <- column_order_stat_grouping[v_OKgroups]
        }
        else {
          stop("Something is wrong with provided order statistics groupings!")
        }
      }
      if ((length(column_order_stat_grouping) != length(unique(column_order_stat_grouping))) ||
          (!all(column_order_stat_grouping == cummax(column_order_stat_grouping)))) {
        column_order_stat_grouping <- unique(column_order_stat_grouping)
        column_order_stat_grouping <- sort(column_order_stat_grouping)
        warning("Parameter column_order_stat_grouping was not strictly increasing. Correcting but please consider supplying a strictly increasing one.")
      }
      czek_matrix <- apply(x, 2, function(x_col, column_order_stat_grouping) {
        cut_the_values <- cut(base::rank(x_col), c(0, column_order_stat_grouping, length(x_col)), include_lowest = TRUE)
        czek_x_col <- as.numeric(cut_the_values)
        czek_x_col
      }, column_order_stat_grouping = column_order_stat_grouping)
      cut_the_values <- cut(1:ncol(x), c(0, column_order_stat_grouping, ncol(x)), include_lowest = TRUE)
      tmp_cut_the_values <- cut_the_values
      l_cut_the_values <- levels(cut_the_values)
      l_cut_the_values[1] <- paste0("[1,", column_order_stat_grouping[1], "]")
      interval_breaks <- "Column specific, levels are the cut values of the order statistics for each column."
    }
    attr(czek_matrix, "levels") <- l_cut_the_values
    attr(czek_matrix, "partition_boundaries") <- interval_breaks
    attr(czek_matrix, "n_classes") <- length(levels(czek_matrix))
  }
  else {
    czek_matrix <- x
  }
  attr(czek_matrix, "order") <- new_order
  attr(czek_matrix, "criterion_value") <- NA
  if (!is.na(order[1])) {
    criterion_method <- NA
    criterion_method <- switch(as.character(order[1]), ARSA = "LS",
                               BBURCG = "Gradient_raw", BBWRCG = "Gradient_weighted",
                               GW = "Path_length", GW_average = "Path_length",
                               GW_complete = "Path_length", GW_single = "Path_length",
                               GW_ward = "Path_length", HC = NA, HC_average = NA,
                               HC_complete = NA, HC_single = NA, HC_ward = NA,
                               Identity = NA, MDS = "Neumann_stress", MDS_angle = "Neumann_stress",
                               MDS_metric = "Neumann_stress", MDS_nonmetric = "Neumann_stress",
                               OLO = "Path_length", OLO_average = "Path_length",
                               OLO_complete = "Path_length", OLO_single = "Path_length",
                               OLO_ward = "Path_length", QAP_2SUM = "2SUM", QAP_BAR = "BAR",
                               QAP_Inertia = "Inertia", QAP_LS = "LS", R2E = NA,
                               Random = NA, SA = "Gradient_raw", Spectral = "2SUM",
                               Spectral_norm = "2SUM", SPIN_NH = NA, SPIN_STS = NA,
                               TSP = "Path_length", VAT = NA, NA)
    if (!is.na(criterion_method)) {
      attr(czek_matrix, "criterion_value") <- seriation::criterion(as.dist(x),
                                                                   order = seriation::ser_permutation(new_order),
                                                                   method = criterion_method)
    }
  }
  attr(czek_matrix, "Path_length") <- seriation::criterion(as.dist(x),
                                                           order = seriation::ser_permutation(new_order), method = "Path_length")
  attr(czek_matrix, "Um") <- Um_factor(x, order = attr(czek_matrix, "order"), inverse_um = FALSE)
  rownames(czek_matrix) <- rownames(x)
  colnames(czek_matrix) <- colnames(x)

  if(cluster){
    n = nrow(x)
    cluster_id = list()

    res_fuzzy = cmeans(x[new_order, new_order], num_cluster)

    d = e.divisive(as.matrix(res_fuzzy$membership), min.size=min.size, k = num_cluster-1)
    breakpoints = d$estimates
    cluster_res = d$cluster

    cluster_boundary = matrix(NA,nrow = num_cluster, ncol = 4)
    if(cluster_type == "exact"){
      for (i in 1:num_cluster) {
        cluster_boundary[i,] = c(breakpoints[i]-0.5, n-breakpoints[i]+1.5, breakpoints[i+1]-0.5, n-breakpoints[i+1]+1.5)
        # cluster_boundary[i,] = c(n-breakpoints[i], breakpoints[i], n-breakpoints[i+1], breakpoints[i+1])+0.5
        cluster_id[[i]] = new_order[breakpoints[i]:(breakpoints[i+1]-1)]
        # cluster_res = c(cluster_res, rep(i, length(cluster_id[[i]])))
      }
    }else if(cluster_type == "fuzzy"){
      d = e.divisive(as.matrix(res_fuzzy$membership), min.size=min.size, sig.lvl = sig.lvl)
      candidate = d$estimates
      bandwidth = ceiling(n * scale_bandwidth)

      for (i in 1:num_cluster) {
        lower = breakpoints[i]
        upper = breakpoints[i+1]
        id = candidate[which(candidate %in% c((lower-bandwidth):(upper+bandwidth)))]

        cluster_boundary[i,] = c(min(id, lower)-0.5, n-min(id, lower)+1.5, max(id, upper)-0.5, n-max(id, upper)+1.5)
        # cluster_boundary[i,] = c(n-min(id, lower), min(id, lower), n-max(id, upper), max(id, upper))+0.5
        # cluster_id[[i]] = c(min(id, lower):(max(id, upper)-1))
        # cluster_res = c(cluster_res, rep(i, length(lower:(upper-1))))
      }

      temp = unlist(cluster_id)
      fuzzy_id = temp[duplicated(temp)]

      for (i in fuzzy_id) {
        foo = which(sapply(cluster_id, function(x) i %in% x))
        prob = rep(0, num_cluster)
        prob[foo] = res_fuzzy$membership[i,foo]
        prob = prob/sum(prob)
        cluster_res[i] = sample(1:num_cluster, 1, prob = prob)
      }
    }else{
      stop("Wrong cluster method.")
    }

    # temp = which(cluster_boundary %in% c(n-0.5, n+1.5))
    # cluster_boundary[temp] = n + 0.5
    # temp = which(cluster_boundary %in% c(0-0.5, 0+1.5))
    # cluster_boundary[temp] = 0.5


    attr(czek_matrix, "cluster") <- TRUE
    attr(czek_matrix, "num_cluster") <- num_cluster
    attr(czek_matrix, "cluster_res") <- cluster_res[order(new_order)]
    attr(czek_matrix, "cluster_type") <- cluster_type
    attr(czek_matrix, "cluster_boundary") <- cluster_boundary
  }

  if ((!as_dist) && (!original_diagram)) {
    if (monitor %in% c(TRUE, "cumulativ_plot")) {
      if (monitor == TRUE)
        monitor <- "plot"
      cum_probs <- as.numeric(gsub(pattern = "%", replacement = "",
                                   x = names(interval_breaks)))
      plot_values <- cum_probs[-1]
      my_title <- "Cumulative distribution of distances in each class"
      if (monitor == "plot") {
        probs <- c()
        for (i in 2:(length(cum_probs))) {
          probs[i - 1] <- cum_probs[i] - cum_probs[i -
                                                     1]
        }
        plot_values <- probs
        my_title <- "Distribution of distances in classes"
      }
      names(plot_values) <- levels(cut_the_values)
      graphics::barplot(plot_values, main = my_title,
                        col = c("grey30"), xlab = "Classes of distances",
                        ylim = c(0, 100), yaxt = "n")
      graphics::axis(2, at = seq(0, 100, by = 10), labels = paste(seq(0, 100, by = 10), "%", sep = ""), las = 2)
      graphics::box(col = "black")
    }
    class(czek_matrix) <- "czek_matrix"
  }
  else {
    if (as_dist) {
      class(czek_matrix) <- "czek_matrix_dist"
    }
    else {
      class(czek_matrix) <- "czek_matrix"
    }
  }
  return(czek_matrix)
}
