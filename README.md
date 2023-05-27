statistics readme
================
Patrick Cherry

- <a href="#emmeans-estimated-marginal-means"
  id="toc-emmeans-estimated-marginal-means">emmeans: Estimated marginal
  means</a>
- <a href="#doe-design-of-experiment"
  id="toc-doe-design-of-experiment">DoE: Design of Experiment</a>

### emmeans: Estimated marginal means

[Link to
output](https://github.com/pdcherry/statistics/blob/main/emmeans_estimated_marginal_means.md)

Estimated marginal means (EMMs, previously known as least-squares means
in the context of traditional regression models) are derived by using a
model to make predictions over a regular grid of predictor combinations
(called a reference grid).

I use estimated marginal means to estimate the effect sizes when:

- interaction effects are present,
- when multiple effects are present—but are not scaled the same way
  (*e.g.* one effect is linear, one effect is reciprocal (1/x) ),
- when variability is not homoscedastic (*e.g.* as in a `glm`) and I
  could use the confidence intervals offered by `emmeans` that I don’t
  get with ordinary means,
- or when the experiment’s sampling is not balanced, causing some
  conditions to be over-weighted in the ordinary means.

### DoE: Design of Experiment

[Link to
output](https://github.com/pdcherry/statistics/blob/main/doe_design_of_experiment_with_library_prep.md)

Design of Experiment principles seek to configure the samples,
variables, and controls in a scientific experimental plan to answer the
question posed or test the hypothesis while controlling for known
sources of variability and confounding due to the methods and materials
used to carry out the experiment.

Here, I use the and the standard `R` function `gen.factorial` to make a
full factorial design and then use the package `AlgDesign` to add
blocking for two operators who will be carrying out the experiment (with
n = 3 replicates for each unique sample condition).
