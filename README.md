# VM2026

## Linting in RStudio

This project uses a repository-level `.lintr` file so RStudio and command-line linting use the same rules.

1. Install `lintr` in R: `install.packages("lintr")`
2. Open `/home/runner/work/VM2026/VM2026/VM2026.Rproj` in RStudio
3. Enable R diagnostics and `lintr` integration in **Tools → Global Options → Code → Diagnostics**
4. Restart RStudio if the diagnostics do not appear immediately

You can also run linting manually from the R console:

```r
lintr::lint_dir()
```
