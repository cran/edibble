
#' Interactive plot of the edibble design
#' @param x An edibble design.
#' @param which A string of either "factors" or "levels".
#' @param width,height The width and height of the plot.
#' @param seed A seed number so same plot is always generated.
#' @param title,subtitle,footer The title, subtitle or footer of the plot.
#'   By default it uses the name from the `x` object as the title while rest is empty.
#'   To modify the look of the text, you can pass a character string consisting of valid
#'   for input style value in an HTML object, e.g. "font-size: 18px;font-family:serif;" as a
#'   named vector where the name corresponds to the text to display, e.g. `c("Title" = "font-size:20px;")`.
#' @param background The background color of the plot. Default is transparent. The input
#'   can be a color name (e.g. "white"), a HEX value ("#FFFFFF"), or rgb/rgba in the format like rgba(0, 0, 0, 0).
#' @param view A string of either "show-buttons" (default), "hide-buttons", "static"
#' @param ... Currently unused.
#' @examples
#' plot(takeout(menu_crd(t = 4, n = 20)))
#' @return A plot.
#' @export
plot.edbl_design <- function(x, which = c("factors", "levels"),
                             width = "100%", height = NULL, seed = 1, title = NULL,
                             subtitle = NULL, footer = NULL, background = "transparent",
                             view = c("show-buttons", "hide-buttons", "static"), ...) {
  if(!requireNamespace("visNetwork")) abort("You need to install `visNetwork` package.")
  which <- match.arg(which)
  view <- match.arg(view)
  prep <- cook_design(x)

  nodes <- switch(which,
                  "factors" = prep$fct_nodes,
                  "levels" = prep$lvl_nodes)
  nodes$group <- switch(which,
                        "factors" = gsub("edbl_", "", nodes$class),
                        "levels" = nodes$var)
  nodes$label <- nodes$name
  class2shape <- c("edbl_unit" = "circle",
                   "edbl_trt" = "diamond",
                   "edbl_rcrd" = "database")
  nodes$shape <- class2shape[prep$fct_class(nodes$idvar)]

  main <- names(title) %||% title %||% x$name
  main_style <- ifelse(is_named(title), title, "")
  submain <- names(subtitle) %||% subtitle %||% ""
  submain_style <- ifelse(is_named(submain), submain, "")
  footer_ <- names(footer) %||% footer %||% ""
  footer_style <- ifelse(is_named(footer), footer, "")
  background <- ifelse(background=="transparent", "rgba(0, 0, 0, 0)", background)
  edges <- switch(which,
                  "factors" = prep$fct_edges,
                  "levels" = prep$lvl_edges)
  if(nrow(edges)) {
    if(which=="factors") {
      # this doesn't seem to work
      type2arrowtype <- c("cross" = NA, "depends" = "circle", "nest" = "arrow", "allot" = "arrow")
      edges$arrows.middle.type = type2arrowtype[edges$type]

      type2dash <- c("cross" = TRUE, "depends" = TRUE, "nest" = FALSE, "allot" = FALSE)
      type2arrow <- c("cross" = NA, "depends" = "middle", "nest" = "to", "allot" = "to")
      edges$dashes <- type2dash[edges$type]
      edges$arrows <- type2arrow[edges$type] # list(to = list(type = type2arrowtype[edges$type]))
    } else {
      edges$arrows <- "to"
    }
  }

  out <- visNetwork::visNetwork(nodes = nodes,
                               edges = edges,
                               width = width,
                               height = height,
                               main = list(text = main,
                                           style = main_style),
                               submain = list(text = submain,
                                              style = submain_style),
                               footer = list(text = footer_,
                                              style = footer_style),
                               background = background,
                               ) %>%
    visNetwork::visLayout(randomSeed = seed) %>%
    visNetwork::visNodes(id = "id") %>%
    visNetwork::visEdges(id = "id")
    #visNetwork::visHierarchicalLayout()
    #visNetwork::visLegend() %>%
  switch(view,
         "show-buttons" = out %>%
           visNetwork::visOptions(highlightNearest = TRUE,
                                  nodesIdSelection = TRUE,
                                  collapse = TRUE,
                                  manipulation = TRUE,
                                  selectedBy = "group") %>%
           visNetwork::visInteraction(navigationButtons = TRUE),
         "hide-buttons" = out,
         "static" = out %>%
           visNetwork::visInteraction(dragNodes = FALSE,
                                      dragView = FALSE,
                                      hover = FALSE,
                                      keyboard = FALSE,
                                      multiselect = FALSE,
                                      navigationButtons = FALSE,
                                      selectable = FALSE,
                                      zoomView = FALSE) )

}

#' @rdname plot.edbl_design
#' @export
plot.edbl_table <- function(x, ...) {
  des <- edbl_design(x)
  plot(des, ...)
}

#' @rdname plot.edbl_design
#' @export
plot_fct_graph <- function(x, width = "100%", height = NULL, seed = 1, title = NULL,
                             subtitle = NULL, footer = NULL, background = "transparent",
                             view = c("show-buttons", "hide-buttons", "static"), ...) {
  plot(x, which = "factors",
       width = width, height = height, seed = seed, title = title, subtitle = subtitle,
       footer = footer, view = view, ...)
}

#' @rdname plot.edbl_design
#' @export
plot_lvl_graph <- function(x, width = "100%", height = NULL, seed = 1, title = NULL,
                             subtitle = NULL, footer = NULL, background = "transparent",
                             view = c("show-buttons", "hide-buttons", "static"), ...) {
  plot(x, which = "levels",
       width = width, height = height, seed = seed, title = title, subtitle = subtitle,
       footer = footer, view = view, ...)
}
