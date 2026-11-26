# NBR revision scripts (November 2026)

Copies of the scripts used to answer the *Neuroscience and Biobehavioral Reviews* reviewers from release `bayesian/results/run_0c04f897e2d1`. They were run from the manuscript project folder, which is not under version control, and are kept here as the record of how each number, figure and tracked change was produced. Paths inside the Python scripts point to that folder.

| Script | Purpose |
|---|---|
| `regression_gate.R` | Compares the new store with the archived `abc9305d0d01` store, keyed on scientific identifiers. Every row is classified in advance as identical, changed, removed or new. Result: 0 unexpected rows. |
| `tab_nbr.R` | Extracts the Table 2 values, the prior grid and the sensitivity refits from a store, with each percentage's release or withhold status. |
| `nbr_figures.R` | Journal artwork for Figure 2 and Figure S1: 190 mm wide, Arial, at least 7 pt; vector PDF and EPS, with a 1000 dpi TIFF fallback. |
| `make_prior_grid_table.py` | Supplementary Table S23 (prior grid). Direction, magnitude, interval and prediction are shown separately; withheld percentages are labelled, not printed. |
| `track_main.py` | Tracked changes, as Ricardo Twumasi, to the manuscript, Table 2 and the Abstract. Every number is formatted from the release extracts. |
| `verify_tracked.py` | Checks that rejecting only the new revisions restores each original, that earlier revisions and comments are intact, and that every new number traces to the release. |
| `docx_track.py` | Word tracked-changes helper used by the two scripts above. |
