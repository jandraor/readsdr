#' Read an XMILE file into R
#'
#'\code{read_xmile} returns a list for constructing deSolve functions and graphs
#'
#' This function extracts the xml from the file specified via \code{filepath}
#' to generate a list of objects. Such a list contains a summary of the model,
#' the inputs for simulating through \link[deSolve]{deSolve}, and the inputs for
#' creating a \link[igraph]{igraph} object.
#' @param filepath A string that indicates a path to a file with extension .stmx
#'   or .xmile. Vensim files (.mdl) are not xmile files. They must be exported
#'   from Vensim with extension .xmile
#'
#' @return This function returns a list with three elements. The first element,
#' \emph{description}, is a list that contains the simulation parameters, and
#' the names and equations (including graphical functions) for each stock or
#' level, variable and constant. The second element, \emph{deSolve_components},
#' is a list that contains initial values, constants and the function for
#' simulating via deSolve. The third element, \emph{igraph} contains the
#' data.frames for creating a graph with igraph.
#' @export
#'
#' @examples
#' \donttest{
#' read_xmile("mdl.xmile")
#' }
#' \donttest{
#' read_xmile("mdl.stmx")
#' }

read_xmile <- function(filepath) {

  model_structure  <- extract_structure_from_XMILE(filepath)
  parameters       <- model_structure$parameters
  levels           <- model_structure$levels
  variables        <- model_structure$variables
  constants        <- model_structure$constants

  deSolve_components <- get_deSolve_elems(model_structure)

  nodes_df <- generate_nodes_df(levels, variables, constants)
  edges_df <- generate_edges_df(levels, variables, constants)

  list(
    description = list(
      parameters = parameters,
      levels     = levels,
      variables  = variables,
      constants  = constants),
    deSolve_components = deSolve_components,
    graph_dfs = list(
      nodes = nodes_df,
      edges = edges_df
    )
  )
}
