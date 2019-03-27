# Addressing the Longevity Gap between the Rich and Poor: The Role of Income Mobility

- [Tables (overleaf)](https://www.overleaf.com/read/bgdrnkfrtnqp)

## Reproduce analysis

- `src` contains all the code and functions to reproduce the analysis.
-  In `R`, set the working directory as the main folder of the repository. Then run the `source.R` file.

## Priors

```
                 prior     class           coef group resp dpar nlpar bound
1                               b
6  student_t(3, 42, 10) Intercept
7   student_t(3, 0, 10)        sd
8                              sd                state
9                              sd      Intercept state
10  student_t(3, 0, 10)     sigma
```

## R Packages used

- `data.table`, `brms`, `ggplot2`, `ggthemes`, `texreg`, `sdazar`,
  `haven`, `xtable`, `stringr`
