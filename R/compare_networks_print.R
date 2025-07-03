#' Print method for netify_comparison objects
#'
#' Provides a clear, formatted output for network comparison results.
#' Handles different comparison types (temporal, cross-network, multilayer)
#' with appropriate formatting.
#'
#' @param x A netify_comparison object from compare_networks()
#' @param ... Additional arguments (currently unused)
#' @param n Maximum number of rows to print for summary tables (default 20)
#'
#' @return Invisibly returns the input object
#'
#' @examples
#' # Compare two networks
#' data(icews)
#' net1 <- netify(icews[icews$year == 2010,], actor1 = "i", actor2 = "j")
#' net2 <- netify(icews[icews$year == 2011,], actor1 = "i", actor2 = "j")
#' comp <- compare_networks(list("2010" = net1, "2011" = net2))
#' print(comp)
#'
#' @importFrom utils head
#' @method print netify_comparison
#' @export
print.netify_comparison <- function(x, ..., n = 20) {
    # Helper function to format numeric values intelligently
    format_numeric <- function(x, digits = 3) {
        if (!is.numeric(x)) return(x)
        
        # Handle special cases
        x_formatted <- sapply(x, function(val) {
            if (is.na(val)) return(NA)
            if (is.infinite(val)) return(val)
            
            # For very small values, use scientific notation
            if (abs(val) < 0.001 && val != 0) {
                return(format(val, scientific = TRUE, digits = digits))
            }
            # For very large values, use scientific notation
            else if (abs(val) >= 10000) {
                return(format(val, scientific = TRUE, digits = digits))
            }
            # For percentages and other values, round appropriately
            else if (abs(val) >= 10) {
                return(round(val, 1))
            }
            else if (abs(val) >= 1) {
                return(round(val, 2))
            }
            else {
                return(round(val, digits))
            }
        })
        
        return(x_formatted)
    }
    
    # Helper function to format data frames
    format_df <- function(df) {
        numeric_cols <- sapply(df, is.numeric)
        df[numeric_cols] <- lapply(df[numeric_cols], format_numeric)
        return(df)
    }
    
    cli::cli_h1("Network Comparison Results")
    
    # Basic info
    cli::cli_text("Comparison type: {.strong {x$comparison_type}}")
    cli::cli_text("Number of networks: {.val {x$n_networks}}")
    cli::cli_text("Comparison focus: {.field {x$what}}")
    
    # Show the method field (which contains the comparison type like "structural_comparison")
    if (!is.null(x$method)) {
        cli::cli_text("Method: {.field {x$method}}")
    }
    
    # Show the comparison algorithm for edge comparisons
    if (!is.null(x$comparison_method) && x$what == "edges") {
        cli::cli_text("Algorithm: {.field {x$comparison_method}}")
    }
    
    # Show additional settings for edge comparisons
    if (x$what == "edges") {
        if (!is.null(x$permutation_type)) {
            cli::cli_text("Permutation type: {.field {x$permutation_type}}")
        }
        if (!is.null(x$correlation_type)) {
            cli::cli_text("Correlation type: {.field {x$correlation_type}}")
        }
        if (!is.null(x$p_adjust) && x$p_adjust != "none") {
            cli::cli_text("P-value adjustment: {.field {x$p_adjust}}")
        }
        if (!is.null(x$seed_used)) {
            cli::cli_text("Random seed: {.val {x$seed_used}}")
        }
    }
    
    cli::cli_rule()
    
    # Print summary based on what was compared
    if (x$what == "structure") {
        cli::cli_h2("Structural Properties")
        
        if (!is.null(x$summary)) {
            # For many networks, check if we should group by network name patterns
            if (nrow(x$summary) > n) {
                cli::cli_alert_info("Showing first {n} of {nrow(x$summary)} comparisons. Use print(x, n = Inf) to see all.")
                print(format_df(head(x$summary, n)))
            } else {
                print(format_df(x$summary))
            }
        }
        
        if (!is.null(x$changes)) {
            cli::cli_h3("Changes Between Networks")
            print(format_df(x$changes))
        }
        
    } else if (x$what == "edges") {
        cli::cli_h2("Edge Comparison Summary")
        
        if (!is.null(x$summary)) {
            print(format_df(x$summary))
        }
        
        # Show edge changes if available
        if (!is.null(x$edge_changes) && length(x$edge_changes) > 0) {
            cli::cli_h3("Edge Changes")
            
            # Limit output for many comparisons
            n_changes <- length(x$edge_changes)
            if (n_changes > 5) {
                cli::cli_alert_info("Showing first 5 of {n_changes} pairwise comparisons.")
                changes_to_show <- head(x$edge_changes, 5)
            } else {
                changes_to_show <- x$edge_changes
            }
            
            for (pair in names(changes_to_show)) {
                change <- changes_to_show[[pair]]
                cli::cli_text("{.strong {pair}}:")
                cli::cli_text("  Added: {.val {change$added}} | Removed: {.val {change$removed}} | Maintained: {.val {change$maintained}}")
            }
        }
        
    } else if (x$what == "nodes") {
        cli::cli_h2("Node Composition")
        
        if (!is.null(x$summary)) {
            print(format_df(x$summary))
        }
        
        if (!is.null(x$node_changes) && length(x$node_changes) > 0) {
            cli::cli_h3("Node Changes")
            
            n_changes <- length(x$node_changes)
            if (n_changes > 5) {
                cli::cli_alert_info("Showing first 5 of {n_changes} comparisons.")
                changes_to_show <- head(x$node_changes, 5)
            } else {
                changes_to_show <- x$node_changes
            }
            
            for (pair in names(changes_to_show)) {
                change <- changes_to_show[[pair]]
                cli::cli_text("{.strong {pair}}:")
                cli::cli_text("  Added: {.val {length(change$added)}} | Removed: {.val {length(change$removed)}} | Maintained: {.val {length(change$maintained)}}")
            }
        }
        
    } else if (x$what == "attributes") {
        cli::cli_h2("Attribute Comparison")
        
        if (!is.null(x$summary)) {
            print(format_df(x$summary))
        }
    }
    
    # Print significance tests if available
    if (!is.null(x$significance_tests)) {
        cli::cli_h2("Statistical Tests")
        
        # Show number of permutations if available
        if (!is.null(x$significance_tests$qap_pvalues)) {
            n_perm <- attr(x$significance_tests$qap_pvalues, "n_perm")
            if (!is.null(n_perm)) {
                cli::cli_text("Permutations: {.val {n_perm}}")
            }
        }
        
        if (is.data.frame(x$significance_tests)) {
            print(format_df(x$significance_tests))
        } else {
            # Handle list format
            for (test_name in names(x$significance_tests)) {
                cli::cli_h3(test_name)
                if (is.data.frame(x$significance_tests[[test_name]])) {
                    print(format_df(x$significance_tests[[test_name]]))
                } else {
                    print(x$significance_tests[[test_name]])
                }
            }
        }
    }
    
    # Add helpful hints based on comparison type
    if (x$comparison_type == "temporal" && x$n_networks > 10) {
        cli::cli_alert_info("Tip: For clearer comparisons, consider subsetting to specific time periods using subset().")
    }
    
    if (x$what == "structure" && x$n_networks > 2) {
        cli::cli_alert_info("Tip: Use summary() on individual networks for more detailed statistics.")
    }
    
    invisible(x)
}

#' Summary method for netify_comparison objects
#'
#' Provides a concise summary of network comparison results.
#'
#' @param object A netify_comparison object from compare_networks()
#' @param ... Additional arguments (currently unused)
#'
#' @return A summary data frame or list depending on comparison type
#'
#' @method summary netify_comparison
#' @export
summary.netify_comparison <- function(object, ...) {
    # Return the main summary component
    if (!is.null(object$summary)) {
        return(object$summary)
    } else {
        # Create a basic summary if none exists
        summary_list <- list(
            comparison_type = object$comparison_type,
            n_networks = object$n_networks,
            what = object$what,
            method = object$method
        )
        
        # Add any available metrics
        if (!is.null(object$edge_changes)) {
            summary_list$n_edge_comparisons <- length(object$edge_changes)
        }
        
        if (!is.null(object$node_changes)) {
            summary_list$n_node_comparisons <- length(object$node_changes)
        }
        
        return(summary_list)
    }
}