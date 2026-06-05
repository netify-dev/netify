# Compilation Notes for netify

## RcppEigen Warnings

When compiling the netify package, you may see warnings like:

```
warning: variable 'count' set but not used [-Wunused-but-set-variable]
warning: variable 'nsuper_et_post' set but not used [-Wunused-but-set-variable]
```

These warnings originate from the RcppEigen/Eigen library headers, not from the netify source code. They are harmless and can be safely ignored. The warnings occur because:

1. The Eigen library contains some variables that are set but not used in certain template instantiations
2. Modern C++ compilers (especially clang on macOS) are very aggressive about warning on unused variables
3. These warnings cannot be easily suppressed without potentially hiding real issues in our own code

## Current Build Configuration

The package does not ship custom `Makevars` files. R selects the compiler
standard and platform flags during installation. If Eigen warning output appears
on a local compiler, it is usually inherited from the installed R toolchain and
not from package-specific build flags.

## Note for Package Maintainers

If you need to debug compilation issues, inspect the compiler output from
`R CMD INSTALL` or `R CMD check` first. Avoid adding warning suppressions unless
they are necessary for CRAN portability.
