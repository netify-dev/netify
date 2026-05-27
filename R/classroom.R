#' Synthetic high-school friendship roster (nodes)
#'
#' A small synthetic roster of 30 students intended for examples and
#' teaching. Designed to support a typical survey-style workflow:
#' one row per student, one column per attribute, plus a separate
#' edgelist of reported friendship ties (\code{\link{classroom_edges}}).
#'
#' @format A data frame with 30 rows and 4 columns:
#' \describe{
#'   \item{\code{student}}{Student identifier, character (e.g.
#'     \code{"s01"} .. \code{"s30"}). Use this as the actor column when
#'     attaching attributes via \code{\link{add_node_vars}}.}
#'   \item{\code{gender}}{Reported gender, character, \code{"F"} or
#'     \code{"M"}.}
#'   \item{\code{grade}}{Grade level, integer 9-12.}
#'   \item{\code{gpa}}{Grade point average on the 0-4 scale, numeric.}
#' }
#'
#' @details
#' This dataset is \strong{synthetic} -- generated to illustrate how
#' netify handles standard student/peer survey data. It is not drawn
#' from any real classroom. Ties tend to form within the same grade
#' and (more weakly) within the same gender, so attribute-based
#' analyses such as \code{\link{homophily}} and
#' \code{\link{mixing_matrix}} produce meaningful (non-null) patterns.
#'
#' Pair with \code{\link{classroom_edges}} (an undirected friendship
#' edgelist on the same 30 students).
#'
#' @name classroom_nodes
#' @docType data
#' @usage data(classroom_nodes)
#' @keywords datasets
#'
#' @seealso \code{\link{classroom_edges}}, \code{\link{netify}},
#' \code{\link{netify_workflows}}.
#'
#' @examples
#' data(classroom_nodes)
#' head(classroom_nodes)
#' table(classroom_nodes$gender, classroom_nodes$grade)
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
NULL


#' Synthetic high-school friendship edgelist
#'
#' A small synthetic edgelist of reported friendships among 30
#' students (see \code{\link{classroom_nodes}}). Ties are
#' \strong{undirected} -- each row records that two students named
#' each other as friends.
#'
#' @format A data frame with about 50 rows and 2 columns:
#' \describe{
#'   \item{\code{from}}{Student identifier of one friend, character.}
#'   \item{\code{to}}{Student identifier of the other friend, character.}
#' }
#'
#' @details
#' The edgelist is synthetic and contains one row per friendship
#' (not two). When you build a netify object with
#' \code{symmetric = TRUE} (the default for undirected ties), the
#' constructor automatically fills in both directions.
#'
#' @name classroom_edges
#' @docType data
#' @usage data(classroom_edges)
#' @keywords datasets
#'
#' @seealso \code{\link{classroom_nodes}}, \code{\link{netify}},
#' \code{\link{netify_workflows}}.
#'
#' @examples
#' data(classroom_edges)
#' data(classroom_nodes)
#' head(classroom_edges)
#'
#' # Build a friendship network with student attributes attached.
#' net <- netify(
#'     classroom_edges,
#'     actor1 = "from", actor2 = "to",
#'     symmetric = TRUE,
#'     nodal_data = classroom_nodes
#' )
#' summary(net)
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
NULL
