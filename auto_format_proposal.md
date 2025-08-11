# Auto-Formatting Feature Proposal for plot.netify

## Current Situation
- The function already applies smart defaults via `get_smart_defaults()`
- These defaults are always applied unless the user explicitly overrides them
- Smart defaults include:
  - Point size based on number of nodes
  - Edge transparency based on density
  - Text labels for small networks
  - Curved edges for small dense networks
  - Remove isolates for large networks

## Proposed Solution

### Option 1: `auto_format` parameter (Recommended)
Add an `auto_format` parameter that controls whether smart defaults are applied:

```r
plot.netify <- function(x, ..., auto_format = TRUE) {
    # ...
    
    if (auto_format) {
        plot_args <- get_smart_defaults(netlet, msrmnts, plot_args)
    }
    
    # ...
}
```

**Pros:**
- Clear and intuitive naming
- Boolean flag is simple to understand
- Backward compatible (defaults to TRUE)
- Allows users to have full control when needed

**Cons:**
- All-or-nothing approach

### Option 2: `smart_defaults` parameter with levels
Allow more granular control:

```r
plot.netify <- function(x, ..., smart_defaults = "auto") {
    # smart_defaults can be:
    # - "auto" or TRUE: Apply all smart defaults (default)
    # - "minimal": Apply only essential defaults (e.g., prevent plot breaking)
    # - "none" or FALSE: No smart defaults
    # - A character vector: Apply only specified defaults
    #   e.g., c("density", "size") for density-based and size-based defaults
}
```

**Pros:**
- More flexible control
- Can selectively apply certain types of defaults

**Cons:**
- More complex API
- Harder to document clearly

### Option 3: `optimize_for` parameter
Allow optimization for specific use cases:

```r
plot.netify <- function(x, ..., optimize_for = "auto") {
    # optimize_for can be:
    # - "auto": Let the function decide (default)
    # - "publication": Optimize for publication-quality plots
    # - "exploration": Optimize for data exploration
    # - "presentation": Optimize for presentations
    # - "none": No optimization
}
```

**Pros:**
- Use-case driven
- Can bundle multiple settings per use case

**Cons:**
- Less transparent about what's being changed
- May not cover all user needs

## Recommendation

I recommend **Option 1** with the `auto_format` parameter because:
1. It's simple and clear
2. Maintains backward compatibility
3. Easy to document
4. Follows R conventions (similar to other packages)

## Implementation Example

```r
plot.netify <- function(x, auto_format = TRUE, ...) {
    # check if the input is a netify object
    netify_check(x)
    
    # extract attributes from the netify object
    obj_attrs <- attributes(x)
    
    # anything passed in goes to the plot arg dumpster
    plot_args <- list(...)
    
    # Validate parameters and warn about common mistakes
    validate_plot_params(plot_args, ...)
    
    # ... (existing code for time_filter, style, etc.)
    
    # Apply smart defaults if requested
    if (auto_format) {
        # Get measurements for smart defaults
        msrmnts <- netify_measurements(x)
        plot_args <- get_smart_defaults(x, msrmnts, plot_args)
    }
    
    # ... (rest of the function)
}
```

## Documentation Update

```r
#' @param auto_format Logical. If TRUE (default), automatically adjusts plot
#'   parameters based on network characteristics such as size, density, and
#'   structure. This includes:
#'   \itemize{
#'     \item Node size scaling based on network size
#'     \item Edge transparency based on network density
#'     \item Automatic text labels for small networks
#'     \item Curved edges for small dense networks
#'     \item Isolate removal for large networks
#'   }
#'   Set to FALSE to disable all automatic formatting and have full control
#'   over plot parameters. Individual parameters can still be overridden even
#'   when auto_format is TRUE.
```

## Usage Examples

```r
# Default behavior - auto formatting enabled
plot(net_obj)

# Disable auto formatting for full control
plot(net_obj, auto_format = FALSE, 
     point_size = 2, 
     edge_alpha = 0.5)

# Auto formatting with selective overrides
plot(net_obj, auto_format = TRUE,
     curve_edges = FALSE)  # Override just the curved edges
```