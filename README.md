Dataset: manova_mancova_practice.csv
Units: Students (unbalanced across treatments), grouped by School
Columns:
- ID: Pseudonymous identifier.
- Treatment: Instructional method with 3 levels: Control, MethodA, MethodB.
(Between-subjects factor)
- School: School site with 4 levels: North, South, East, West. (Blocking factor /
potential fixed effect)
- Gender: 'F' or 'M'.
- Age: Age in years (approx. 15-18).
- SES_Index: Socioeconomic status index (z-score). ~7% missing.
- Pre_Math, Pre_Reading, Pre_Science: Baseline test scores (0-100). ~3% missing
each.
- Post_Math, Post_Reading, Post_Science: Endline test scores (0-100). ~4% missing
each.
Design notes:
- Group sizes are unbalanced: Control=62, MethodA=73, MethodB=49.
- Schools impart small fixed effects on POST outcomes.
- Post outcomes depend on pretreatment scores, SES, and Age (so MANCOVA is
appropriate).
- Covariance matrices differ slightly by Treatment to challenge homogeneity (Box's
M).
- A mild Gender interaction is embedded.
- 5 outliers were injected in post outcomes.
- Missingness is mostly MCAR at variable-specific rates.
