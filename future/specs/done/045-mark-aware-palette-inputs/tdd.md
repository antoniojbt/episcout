# Test Design

Spec ID: `045-mark-aware-palette-inputs`
Status: Completed

Independent factor levels `low`, `medium`, `high` establish positional mapping order. Tests assert named reordering, rejected short/invalid/missing/extra mappings, conflict with the legacy argument, legacy recycling plus warning, grouped box fill routing, unchanged layers, and unchanged default plots. The changed bar and box SVG snapshots are regenerated and visually inspected at their delivered size.
