#' Start the edibble design
#'
#' @description
#' This function doesn't really do much besides create a new edibble design object.
#'
#' @param name Optional name used as title for printing the design.
#' @inheritParams set_units
#' @param seed A seed number for reproducibility.
#' @param .data An edibble table.
#' @param kitchen An environment setup in a manner to manipulate, extract and query
#'   information on the design.
#' @return An empty `edbl_design` object.
#' @examples
#' design("My design")
#' @seealso Add variables to this design with [set_units()], [set_trts()], and
#' [set_rcrds()].
#' @family user-facing functions
#' @export
design <- function(name = NULL, .record = TRUE, seed = NULL, kitchen = Kitchen) {
  if(.record) record_step()
  save_seed(seed)
  structure(list(name = name,
                 graph = initialise_edibble_graph(),
                 kitchen = kitchen),
            class = c("edbl_design", "edbl"))
}

#' @rdname design
#' @export
redesign <- function(.data, name = NULL, .record = TRUE, seed = NULL, kitchen = Kitchen, ...) {
  des <- design(name = name, .record = .record, seed = seed, kitchen = kitchen)
  new_edibble(.data, ..., design = des)
}

# initialise graph structure -----------------------------------------------

initialise_edibble_graph <- function() {
  fnodes <- data.frame(id = integer(), name = character(), class = character(), stringsAsFactors = FALSE)
  lnodes <- data.frame(idvar = integer(), id = integer(), name = character(), stringsAsFactors = FALSE)
  edges <-  data.frame(from = integer(), to = integer(),
                       alloc = integer(), type = character(),
                       stringsAsFactors = FALSE)
  structure(list(nodes = fnodes,
                 edges = edges,
                 levels = list(nodes = lnodes,
                               edges = edges)),
            class = "edbl_graph")
}

