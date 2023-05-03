statistics readme
================
Patrick Cherry

- <a href="#emmeans-estimated-marginal-means"
  id="toc-emmeans-estimated-marginal-means">emmeans: Estimated marginal
  means</a>

### emmeans: Estimated marginal means

[Link to
output](https://github.com/pdcherry/statistics/blob/main/emmeans_estimated_marginal_means.md)

Estimated marginal means (EMMs, previously known as least-squares means
in the context of traditional regression models) are derived by using a
model to make predictions over a regular grid of predictor combinations
(called a reference grid).

I use estimated marginal means to estimate the effect sizes when
interaction effects are present, when multiple effects are present, but
are not scaled the same way (*e.g.* one effect is linear, one effect is
reciprocal (1/x) ), or when variability is not homoscedastic (*e.g.* as
in a `glm`) and I could use the confidence intervals offered by
`emmeans` that I don’t get with ordinary means.
