# ggparci

#' @title Parallel Coordinates Plot with Groups medians and their Confidence Intervals
#' @description The function plots parallel coordinates (gg)plot, in which each line connects the variables medians for each group.
#'              Confidence "bands" for the medians are added to each line. This allows the assessment of groups (clusters) separations.
#'              The variables are normalized to [0,1] scale prior to plotting.
#' @param data a data.frame. includes the grouping variable
#' @param columns variables to include in the plot, as character vector with columns names or as integer vector with positions. the group variable is ignored, if specified.
#' @param groups_column grouping variable name (quoted)
#' @param conf_level confidence level for the bands, Default: 0.95
#' @param title Plot title, Default: ''
#' @param groups_in_facets logical. plot each group in a separate plot/facet? Default: FALSE
#' @param flip_coords logical. make the coordinates horizontal? Default: FALSE
#' @param obs_lines logical. add lines for the rows? Default: FALSE
#' @param alpha_lines transparency factor for the lines, Default: 0.1
#' @param alpha_bands transparency factor for the confidence bands, Default: 0.5
#' @param ... additional arguments to the plot
#' @return a ggplot object
#'
#' @examples
#'
#' \dontrun{
#'
#' ggparci(iris, groups_column = "Species")
#' ggparci(normalize(iris), groups_column = "Species") # already normalized in this version
#'
#' #select only some of the variables
#' # plot a line for each observation
#' # and dont plot the CIs
#' ggparci(mtcars,columns = 3:7, groups_column = "carb", obs_lines = T, alpha_bands = 0)
#'
#' # display each group in a different facet
#' ggparci(data = iris, groups_column = "Species", groups_in_facets = T)
#'
#' # flip the plot
#' p <- ggparci(data = iris, groups_column = "Species", flip_coords = T)
#'
#' }
#' @rdname ggparci
#' @export

ggparci <- function(data, columns = 1:ncol(data), groups_column, conf_level = 0.95, title = "",
  groups_in_facets = FALSE, flip_coords = FALSE, obs_lines = FALSE, alpha_lines = 0.2,alpha_bands = 0.5,...)
  {
  numeric_columns <- columns[sapply(data[columns], is.numeric)]
  if (is.numeric(data[,groups_column]))
      data[,groups_column] <- as.character(data[,groups_column])

  alpha <- 1-conf_level

  data_to_plot <-
    data %>%
    as_data_frame %>%
    dplyr::select(groups_column, numeric_columns) %>%
    dplyr::mutate_if(is.numeric,ggparci:::normalize.default) %>%
    tibble::rownames_to_column("obs_id") %>%
    tidyr::gather(
      key = "Variables", value = "value", -!!groups_column, -obs_id)

  if (obs_lines)
    obs_data_to_plot <-
      data_to_plot %>%
      dplyr::group_by(obs_id)

  summ_data_to_plot <-
    data_to_plot %>%
    dplyr::group_by(Variables) %>%
    dplyr::group_by_(groups_column, add = TRUE) %>%
    dplyr::summarise(
      median = median(value),
      L = sort(value)[max(qbinom(alpha/2, n(), 0.5), 1)],
      U = sort(value)[qbinom((1-alpha/2), n(), 0.5)])

  p <- ggplot() +
    aes( x = Variables ) +
    aes_string( color = groups_column )

  if (obs_lines)
  {
    p <- p +
      geom_line(
        data = obs_data_to_plot,
        aes(group = obs_id, y = value),
        show.legend = FALSE, alpha = alpha_lines)
  }

  p <- p +
    aes_string(
    color = groups_column,
    fill  = groups_column,
    group = groups_column) +
    geom_line(
      data = summ_data_to_plot,
      aes(y = median)) +
    geom_ribbon(
      data = summ_data_to_plot,
      aes(ymin = L, ymax = U),
      alpha = alpha_bands, color =  adjustcolor( "grey", alpha.f = alpha_bands)) +
    labs(title = title) +
    scale_y_continuous(breaks = c((0:10)/10)) +
    scale_x_discrete(expand = c(0.05,0))

  p <- p + theme_bw()

  if(groups_in_facets)
    p <- p + facet_grid(reformulate(groups_column,".")) + theme(legend.position="none") + scale_x_discrete(expand = waiver())

  if(flip_coords)
  {
    p <- p + coord_flip() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        axis.line.x  = element_line(colour = "grey"),
        # axis.ticks.y = element_blank(),
        # axis.text.y = element_blank(),
        #panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank())
    if(groups_in_facets)
      p <- p + facet_grid(reformulate(".",groups_column))
  } else
  {
    p <- p +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        axis.line.y  = element_line(colour = "grey"),
        # axis.ticks.y = element_blank(),
        # axis.text.y = element_blank(),
        #panel.grid.minor.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x=element_text(angle=90, hjust=1)
      )
  }
  return(p)
}
