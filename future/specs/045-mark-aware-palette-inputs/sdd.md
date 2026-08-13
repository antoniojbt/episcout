# Software Design

Spec ID: `045-mark-aware-palette-inputs`
Status: Active

`fill_values` accepts `NULL`, a complete unnamed character vector in displayed level order, or an exact named character mapping. Every colour is validated by `grDevices::col2rgb()`. Named mappings require non-empty unique names and exactly the displayed categories; explicit vectors are never recycled. Factor levels, including declared unused levels, establish display order; character input uses observed order. Missing values remain unassigned.

The helpers add `scale_fill_manual()` only when `fill_values` is present. `epi_plot_bar(custom_palette = ...)` remains released-compatible: it warns that it is deprecated, recycles positionally as before, and cannot be combined with `fill_values`. The grouped box plot routes its new mapping only to the box plot fill scale; point and outline aesthetics remain unchanged.
