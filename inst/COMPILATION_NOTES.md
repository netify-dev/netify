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

## Suppressing the Warnings

The package includes Makevars files that attempt to suppress these specific warnings:
- `src/Makevars`: For Unix-like systems (Linux, macOS)
- `src/Makevars.win`: For Windows systems

These files add the flags `-Wno-unused-but-set-variable` and `-Wno-unused-variable` to reduce warning noise during compilation.

## Note for Package Maintainers

If you need to debug compilation issues, you can temporarily remove these warning suppressions by commenting out the PKG_CXXFLAGS line in the Makevars files.