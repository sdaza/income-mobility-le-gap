# Addressing the Longevity Gap between the Rich and Poor: The Role of Income Mobility

- [Tables with results](https://www.overleaf.com/read/bgdrnkfrtnqp)

## Reproduce analysis

- `src` contains all the code and functions to reproduce the analysis
-  Set the working directory as the main folder of the repository in R, and then run the `source.R` file

## Priors

```
                 prior     class           coef group resp dpar nlpar bound
1                               b
6  student_t(3, 42, 10) Intercept
7   student_t(3, 0, 10)        sd
8                              sd                state
9                              sd      Intercept state
10  student_t(3, 0, 10)     sigma
``
