#' Define allotment of treatments to units
#'
#' @description
#' This function adds the edges between factor nodes to describe the
#' high-level relationship between factors.
#' This function does not actually assign edges between level nodes.
#'
#' @param ... One-sided or two-sided formula. If the input is a one-sided formula
#' then the whole treatment is applied to the specified unit.
#' @inheritParams assign_fcts
#' @inheritParams set_units
#' @family user-facing functions
#' @examples
#' design() %>%
#'   set_units(block = 10,
#'             plot = nested_in(block, 3)) %>%
#'   set_trts(treat = c("A", "B", "C"),
#'            pest = c("a", "b")) %>%
#'   allot_trts(treat ~ plot,
#'               pest ~ block)
#'
#' @return Return an edibble design.
#' @seealso assign_fcts
#' @export
allot_trts <- function(.edibble = NULL, ..., .record = TRUE) {
  if(is.null(.edibble)) return(structure(match.call(), env = rlang::caller_env(), class = c("edbl_fn", "edbl")))
  if(is_formula(.edibble)) {
    cl <- match.call()
    ncl <- length(cl)
    cl[3:(ncl + 1)] <- cl[2:ncl]
    cl$.edibble <- NULL
    return(structure(cl, env = rlang::caller_env(), class = c("edbl_fn", "edbl")))
  }
  dots <- list2(...)
  not_edibble(.edibble)
  des <- edbl_design(.edibble)
  prov <- activate_provenance(des)
  if(.record) prov$record_step()


  if(!is_null(des$allotment)) {
    des$allotment$trts <- c(des$allotment$trts, dots)
  } else {
    des$allotment <- list(trts = dots, units = NULL)
  }

  for(ialloc in seq_along(dots)) {
    trts <- all.vars(f_lhs(dots[[ialloc]]))
    # there should be only one unit
    unit <- all.vars(f_rhs(dots[[ialloc]]))
    prov$fct_exists(name = unit, role = "edbl_unit")
    uid <- prov$fct_id(name = unit)
    if(length(trts)) {
      prov$trt_exists(name = trts)
      tids <- prov$fct_id(name = trts)
    } else {
      prov$trt_exists()
      tids <- prov$trt_ids
    }
    prov$append_fct_edges(from = tids, to = uid, group = TRUE, type = "allot")

    if(is.data.frame(.edibble)) {

      uids_df <- prov$lvl_id(value = .edibble[[unit]], fid = uid)
      for(itrt in seq_along(trts)) {
        atrt <- trts[itrt]
        tids_df <- prov$lvl_id(value = .edibble[[atrt]], fid = tids[itrt])
        prov$append_lvl_edges(from = tids_df,
                                to = uids_df)
      }
    }
  }



  return_edibble_with_graph(.edibble, prov)
}



#' Define allotment of units to nested units
#'
#' @description
#' This function adds the edges between factor nodes to describe the
#' high-level relationship between factors.
#' This function does not actually assign edges between level nodes.
#'
#' @param ... A two-sided formula.
#' @inheritParams assign_fcts
#' @inheritParams set_units
#' @family user-facing functions
#' @examples
#' design() %>%
#'   set_units(block = 10,
#'             plot = 20) %>%
#'   allot_units(block ~ plot)
#'
#' @return Return an edibble design.
#' @seealso assign_fcts
#' @export
allot_units <- function(.edibble, ..., .record = TRUE) {
  if(is.null(.edibble)) return(structure(match.call(), env = rlang::caller_env(), class = c("edbl_fn", "edbl")))
  if(is_formula(.edibble)) {
    cl <- match.call()
    ncl <- length(cl)
    cl[3:(ncl + 1)] <- cl[2:ncl]
    cl$.edibble <- NULL
    return(structure(cl, env = rlang::caller_env(), class = c("edbl_fn", "edbl")))
  }
  dots <- list2(...)
  not_edibble(.edibble)
  prov <- activate_provenance(.edibble)
  if(.record) prov$record_step()
  des <- edbl_design(.edibble)

  if(!is_null(des$allotment)) {
    des$allotment$units <- c(des$allotment$units, dots)
  } else {
    des$allotment <- list(trts = NULL, units = dots)
  }

  for(ialloc in seq_along(dots)) {
    # there should be only one unit for `big`
    big <- all.vars(f_lhs(dots[[ialloc]]))
    small <- all.vars(f_rhs(dots[[ialloc]]))
    op <- as.character(as.list(f_rhs(dots[[ialloc]]))[[1]])
    prov$fct_exists(name = small, role = "edbl_unit")
    big_id <- prov$fct_id(name = big)
    prov$fct_exists(name = big, role = "edbl_unit")
    small_id <- prov$fct_id(name = small)

    if(!op %in% c("crossed_by", "nested_in")) {
      prov$append_fct_edges(from = big_id,
                            to = small_id[length(small_id)],
                            type = "nest",
                            group = TRUE)
      if(length(small) > 1) {
        prov$append_fct_edges(from = big_id,
                              to = small_id[length(small_id) - 1],
                              type = "depends")
      }
    }
  }
  if(is_edibble_table(.edibble)) {
    # Note: for crossed and nested, it's the opposite -> small = big, not big = small.
    if(op %in% c("crossed_by", "nested_in")) {
      for(ismall in seq_along(small_id)) {
        prov$append_fct_edges(data.frame(from = small_id[ismall],
                                         to = big_id,
                                         type = "nest"))
        if(op == "crossed_by") {
          cross_df <- expand.grid(from = small_id, to = small_id)
          cross_df <- cross_df[cross_df$from!=cross_df$to,]
          prov$append_fct_edges(from = cross_df$from, to = cross_df$to, type = "cross")
        }
        prov$append_lvl_edges(from = prov$lvl_id(name = as.character(.edibble[[small[ismall]]])),
                              to = prov$lvl_id(name = as.character(.edibble[[big]])))

      }

    } else {
      for(asmall in small) {
        prov$append_lvl_edges(from = prov$lvl_id(name = as.character(.edibble[[big]])),
                              to = prov$lvl_id(name = as.character(.edibble[[asmall]])))
      }
    }
  }
  return_edibble_with_graph(.edibble, prov)

}


#' Allot treatments to units and serve table
#'
#' This function is a short hand that combines `allot_trts()`, `assign_trts()`
#'  and `serve_table()`.
#'
#' @inheritParams allot_trts
#' @inheritParams serve_table
#' @inheritParams assign_fcts
#'
#' @export
allot_table <- function(.edibble = NULL, ..., order = "random", seed = NULL, constrain = nesting_structure(.edibble), label_nested = NULL, fail = "error", .record = TRUE) {
  if(is.null(.edibble)) return(structure(match.call(), env = rlang::caller_env(), class = c("edbl_fn", "edbl")))
  if(is_formula(.edibble)) {
    cl <- match.call()
    ncl <- length(cl)
    cl[3:(ncl + 1)] <- cl[2:ncl]
    cl$.edibble <- NULL
    return(structure(cl, class = c("edbl_fn", "edbl")))
  }
  prov <- activate_provenance(.edibble)
  if(.record) prov$record_step()
  .edibble %>%
    allot_trts(..., .record = FALSE) %>%
    assign_trts(order = order, seed = seed, constrain = constrain, .record = FALSE) %>%
    serve_table(.record = FALSE, label_nested = {{ label_nested }}, fail = fail)
}
