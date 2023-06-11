Random forest model hyperparameters
================
Patrick Cherry
2023-06-09

- [Get data](#get-data)
- [Preliminary exploration of data](#preliminary-exploration-of-data)
  - [Legal status](#legal-status)
  - [NAs in data](#nas-in-data)
  - [Species](#species)
  - [plot_size](#plot_size)
  - [Prepare data for model](#prepare-data-for-model)
  - [Quick plot/map of data](#quick-plotmap-of-data)
  - [Build Model](#build-model)
  - [Feature engineering for the
    date](#feature-engineering-for-the-date)
    - [Review data preprocessing
      results](#review-data-preprocessing-results)
- [Set up model hyperparameters](#set-up-model-hyperparameters)
  - [Set up workflow](#set-up-workflow)
  - [Train-test some model
    hyperparameters](#train-test-some-model-hyperparameters)
    - [view results](#view-results)
  - [Train-test some model
    hyperparameters](#train-test-some-model-hyperparameters-1)
    - [view results](#view-results-1)
- [Finalize the model](#finalize-the-model)
  - [Understand final model](#understand-final-model)
- [Apply the final model](#apply-the-final-model)
  - [Make predictions](#make-predictions)

The goal is the make a predictor of whether a tree tracked in San
Francisco is a Department of Public Works maintained legal status tree,
or some other legal status.

# Get data

This is a 2020-01-28 [Tidy
Tuesday](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-01-28/readme.md)
dataset. These data are from the San Francisco Public Works’ Bureau of
Urban Forestry.

``` r
sftrees <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv")
```

    ## Rows: 192987 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (6): legal_status, species, address, site_info, caretaker, plot_size
    ## dbl  (5): tree_id, site_order, dbh, latitude, longitude
    ## date (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# Preliminary exploration of data

``` r
head(sftrees)
```

<div class="kable-table">

| tree_id | legal_status   | species                                | address            | site_order | site_info                    | caretaker | date       | dbh | plot_size | latitude | longitude |
|--------:|:---------------|:---------------------------------------|:-------------------|-----------:|:-----------------------------|:----------|:-----------|----:|:----------|---------:|----------:|
|   53719 | Permitted Site | Tree(s) ::                             | 2963 Webster St    |          1 | Sidewalk: Curb side : Cutout | Private   | 1955-09-19 |  NA | NA        | 37.79787 | -122.4341 |
|   30313 | Permitted Site | Tree(s) ::                             | 501 Arkansas St    |          3 | Sidewalk: Curb side : Cutout | Private   | 1955-10-20 |  NA | NA        | 37.75984 | -122.3981 |
|   30312 | Permitted Site | Tree(s) ::                             | 501 Arkansas St    |          2 | Sidewalk: Curb side : Cutout | Private   | 1955-10-20 |  NA | NA        | 37.75984 | -122.3981 |
|   30314 | DPW Maintained | Pittosporum undulatum :: Victorian Box | 501 Arkansas St    |          1 | Sidewalk: Curb side : Cutout | Private   | 1955-10-20 |  16 | NA        | 37.75977 | -122.3981 |
|   30315 | Permitted Site | Acacia melanoxylon :: Blackwood Acacia | 1190 Sacramento St |          5 | Sidewalk: Curb side : Cutout | Private   | 1955-10-24 |  NA | NA        | 37.79265 | -122.4124 |
|   30316 | Permitted Site | Acacia melanoxylon :: Blackwood Acacia | 1190 Sacramento St |          6 | Sidewalk: Curb side : Cutout | Private   | 1955-10-24 |  NA | NA        | 37.79265 | -122.4124 |

</div>

## Legal status

``` r
sftrees %>%
  count(legal_status, sort = TRUE) %>%
  mutate(percent = round( n / sum(n) * 100, digits = 1))
```

<div class="kable-table">

| legal_status                 |      n | percent |
|:-----------------------------|-------:|--------:|
| DPW Maintained               | 141725 |    73.4 |
| Permitted Site               |  39732 |    20.6 |
| Undocumented                 |   8106 |     4.2 |
| Significant Tree             |   1648 |     0.9 |
| Planning Code 138.1 required |    971 |     0.5 |
| Property Tree                |    316 |     0.2 |
| Section 143                  |    230 |     0.1 |
| Private                      |    163 |     0.1 |
| NA                           |     54 |     0.0 |
| Landmark tree                |     42 |     0.0 |

</div>

``` r
sftrees %>% count(legal_status, caretaker, sort = TRUE) %>% head(20)
```

<div class="kable-table">

| legal_status                 | caretaker           |      n |
|:-----------------------------|:--------------------|-------:|
| DPW Maintained               | Private             | 113102 |
| Permitted Site               | Private             |  38312 |
| DPW Maintained               | DPW                 |  26963 |
| Undocumented                 | Private             |   7463 |
| Significant Tree             | Private             |   1505 |
| Planning Code 138.1 required | Private             |    947 |
| DPW Maintained               | SFUSD               |    898 |
| Permitted Site               | Port                |    536 |
| DPW Maintained               | Rec/Park            |    281 |
| Permitted Site               | Rec/Park            |    262 |
| Section 143                  | Private             |    229 |
| Permitted Site               | PUC                 |    208 |
| Property Tree                | Private             |    202 |
| Undocumented                 | DPW                 |    200 |
| DPW Maintained               | Port                |    182 |
| Undocumented                 | Rec/Park            |    165 |
| Private                      | Private             |    155 |
| DPW Maintained               | DPW for City Agency |    111 |
| Undocumented                 | SFUSD               |    100 |
| Permitted Site               | Purchasing Dept     |     81 |

</div>

So the `legal_status` of “DPW Maintained” does not equate with a
`caretaker` of “DPW”—in fact, most of the time, DPW-legal status trees
are privately taken care of.

``` r
col_plot_legalstatus_by_caretaker <- sftrees %>%
  count(legal_status, caretaker) %>%
  add_count(caretaker, wt = n, name = "caretaker_count") %>%
  filter(caretaker_count > 50) %>%
  group_by(legal_status) %>%
  mutate(percent_legal = n / sum(n)) %>%
  ggplot(aes(percent_legal, caretaker, fill = legal_status)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.7, na.value = "grey50") +
  labs(fill = NULL,
       x = "proportion of trees in each category")
col_plot_legalstatus_by_caretaker
```

![](random_forest_hyperparameters_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## NAs in data

``` r
sftrees %>%
  summarise(across(everything(), ~ sum(is.na(.x))),
            "n" = n()) %>%
  relocate(n) %>%
  t() %>% as_tibble(.name_repair = "minimal", rownames = "col_name")
```

<div class="kable-table">

| col_name     |        |
|:-------------|-------:|
| n            | 192987 |
| tree_id      |      0 |
| legal_status |     54 |
| species      |      0 |
| address      |   1487 |
| site_order   |   1634 |
| site_info    |      0 |
| caretaker    |      0 |
| date         | 124610 |
| dbh          |  41819 |
| plot_size    |  50013 |
| latitude     |   2832 |
| longitude    |   2832 |

</div>

The glimpse just turns the data to print left to right. The `n` column
at the start shows how many rows are in the dataframe; the other named
columns show how many `NA`s are in the data in each column. The `date`
and `dhb` [(Diameter at breast
height)](https://en.wikipedia.org/wiki/Diameter_at_breast_height)
columns show significant levels of NAs (64.5% and 21.7%, respectively).

## Species

``` r
sftrees %>% count(species, sort = TRUE) %>% head(20)
```

<div class="kable-table">

| species                                                                   |     n |
|:--------------------------------------------------------------------------|------:|
| Tree(s) ::                                                                | 11629 |
| Platanus x hispanica :: Sycamore: London Plane                            | 11557 |
| Metrosideros excelsa :: New Zealand Xmas Tree                             |  8744 |
| Lophostemon confertus :: Brisbane Box                                     |  8581 |
| Tristaniopsis laurina :: Swamp Myrtle                                     |  7197 |
| Pittosporum undulatum :: Victorian Box                                    |  7122 |
| Prunus cerasifera :: Cherry Plum                                          |  6716 |
| Magnolia grandiflora :: Southern Magnolia                                 |  6285 |
| Arbutus ‘Marina’ :: Hybrid Strawberry Tree                                |  5702 |
| Ficus microcarpa nitida ‘Green Gem’ :: Indian Laurel Fig Tree ‘Green Gem’ |  5624 |
| Prunus serrulata ‘Kwanzan’ :: Kwanzan Flowering Cherry                    |  4025 |
| Acacia melanoxylon :: Blackwood Acacia                                    |  3956 |
| Maytenus boaria :: Mayten                                                 |  3899 |
| Olea europaea :: Olive Tree                                               |  3694 |
| Corymbia ficifolia :: Red Flowering Gum                                   |  3575 |
| Callistemon citrinus :: Lemon Bottlebrush                                 |  3266 |
| Ginkgo biloba :: Maidenhair Tree                                          |  3212 |
| Pyrus calleryana :: Ornamental Pear                                       |  2969 |
| Prunus serrulata :: Ornamental Cherry                                     |  2696 |
| Eriobotrya deflexa :: Bronze Loquat                                       |  2402 |

</div>

## plot_size

``` r
sftrees %>% count(plot_size, sort = TRUE) %>% head(20)
```

<div class="kable-table">

| plot_size  |     n |
|:-----------|------:|
| NA         | 50013 |
| Width 3ft  | 36343 |
| 3x3        | 29166 |
| Width 0ft  | 17017 |
| Width 4ft  | 13745 |
| 3X3        | 12073 |
| Width 2ft  |  7363 |
| Width 5ft  |  4547 |
| 4X4        |  2761 |
| Width 6ft  |  2455 |
| 4x4        |  2232 |
| Width 8ft  |  1475 |
| Width 10ft |  1361 |
| 60         |   782 |
| Width 1ft  |   645 |
| 3X4        |   628 |
| Width 7ft  |   503 |
| 5x5        |   483 |
| 10         |   378 |
| 20         |   375 |

</div>

## Prepare data for model

``` r
trees_formodel <- sftrees %>% #trees_df
  mutate(
    "legal_status" = case_when(
      legal_status == "DPW Maintained" ~ legal_status,
      TRUE ~ "Other"),
    "plot_size" = parse_number(plot_size)) %>%
  select(-address) %>%
  na.omit() %>%
  mutate_if(is.character, factor)
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `plot_size = parse_number(plot_size)`.
    ## Caused by warning:
    ## ! 109 parsing failures.
    ##   row col expected actual
    ## 10979  -- a number    TR 
    ## 13245  -- a number    CUT
    ## 13495  -- a number    TR 
    ## 13501  -- a number    TR 
    ## 13502  -- a number    TR 
    ## ..... ... ........ ......
    ## See problems(...) for more details.

``` r
head(trees_formodel)
```

<div class="kable-table">

| tree_id | legal_status   | species                                | site_order | site_info                    | caretaker | date       | dbh | plot_size | latitude | longitude |
|--------:|:---------------|:---------------------------------------|-----------:|:-----------------------------|:----------|:-----------|----:|----------:|---------:|----------:|
|   30372 | DPW Maintained | Ulmus parvifolia :: Chinese Elm        |          1 | Sidewalk: Curb side : Cutout | Private   | 1956-03-02 |  10 |         3 | 37.76005 | -122.3983 |
|   30460 | DPW Maintained | Pittosporum undulatum :: Victorian Box |          1 | Sidewalk: Curb side : Cutout | Private   | 1956-05-11 |  19 |         4 | 37.80074 | -122.4073 |
|   30454 | DPW Maintained | Pittosporum undulatum :: Victorian Box |          1 | Sidewalk: Curb side : Cutout | Private   | 1956-05-11 |   8 |         3 | 37.80081 | -122.4057 |
|   30428 | DPW Maintained | Pittosporum undulatum :: Victorian Box |          1 | Sidewalk: Curb side : Cutout | Private   | 1956-05-11 |  13 |         7 | 37.80082 | -122.4066 |
|   30468 | DPW Maintained | Melaleuca quinquenervia :: Cajeput     |          2 | Sidewalk: Curb side : Cutout | Private   | 1956-05-29 |   8 |         3 | 37.80061 | -122.4073 |
|   30470 | DPW Maintained | Melaleuca quinquenervia :: Cajeput     |          3 | Sidewalk: Curb side : Cutout | Private   | 1956-05-29 |   8 |         3 | 37.80062 | -122.4073 |

</div>

``` r
col_plot_legalstatus_by_caretaker <- trees_formodel %>%
  count(legal_status, caretaker) %>%
  add_count(caretaker, wt = n, name = "caretaker_count") %>%
  filter(caretaker_count > 50) %>%
  group_by(legal_status) %>%
  mutate(percent_legal = n / sum(n)) %>%
  ggplot(aes(percent_legal, caretaker, fill = legal_status)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.7, na.value = "grey50") +
  labs(fill = NULL,
       x = "proportion of trees in each category")
col_plot_legalstatus_by_caretaker
```

![](random_forest_hyperparameters_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## Quick plot/map of data

``` r
tree_loc_plot <- trees_formodel %>%
  ggplot(aes(x = longitude, y = latitude, color = legal_status)) +
  geom_point(alpha = 0.6, size = 0.25) +
  labs(color = NULL, x = NULL, y = NULL) +
  theme(panel.border = element_blank(),
        legend.position = c(0.1, 0.9), legend.justification = c(0, .5)) +
  scale_color_viridis_d(option = "D", begin = 0.1, end = 0.7)
tree_loc_plot
```

![](random_forest_hyperparameters_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
ggsave("tree_loc_plot.png", tree_loc_plot, width = 5, height = 4, dpi = 320)
```

## Build Model

``` r
set.seed(123)
trees_split <-initial_split(trees_formodel, strata = legal_status)
trees_train <- training(trees_split)
trees_test <- testing(trees_split)

nrow(trees_train); nrow(trees_test)
```

    ## [1] 17881

    ## [1] 5961

## Feature engineering for the date

``` r
tree_rec <- recipe(legal_status ~ ., data = trees_train) %>%
  update_role(tree_id, new_role = "ID") %>%
  step_other(species, caretaker, threshold = .01) %>%
  step_other(site_info, threshold = .005) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_date(date, features = c("year")) %>%
  step_rm(date) %>%
  step_downsample(legal_status)

tree_prep <- prep(tree_rec)

juiced <- juice(tree_prep)
```

### Review data preprocessing results

``` r
juiced %>% count(legal_status)
```

<div class="kable-table">

| legal_status   |    n |
|:---------------|-----:|
| DPW Maintained | 4308 |
| Other          | 4308 |

</div>

# Set up model hyperparameters

``` r
tune_spec <- rand_forest(
  mtry = tune(), # 
  trees = 1000, # number of trees to start with
  min_n = tune() # how many data points in a node to keep splitting further
) %>%
  set_mode("classification") %>%
  set_engine("ranger")
```

## Set up workflow

convenience functions

``` r
tune_wf <- workflow() %>%
  add_recipe(tree_rec) %>%
  add_model(tune_spec)
```

## Train-test some model hyperparameters

``` r
set.seed(234)
trees_folds <- vfold_cv(trees_train)

set.seed(345)
doParallel::registerDoParallel()
tune_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = 20)
```

    ## i Creating pre-processing data to finalize unknown parameter: mtry

### view results

``` r
tune_res %>% select_best("accuracy")
```

<div class="kable-table">

| mtry | min_n | .config               |
|-----:|------:|:----------------------|
|   35 |     5 | Preprocessor1_Model18 |

</div>

``` r
tune_res %>% select_best("roc_auc")
```

<div class="kable-table">

| mtry | min_n | .config               |
|-----:|------:|:----------------------|
|   14 |     3 | Preprocessor1_Model14 |

</div>

``` r
side_facet_n_mtry_plot <- tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
    values_to = "value",
    names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")
side_facet_n_mtry_plot
```

![](random_forest_hyperparameters_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
nonortho_gid_n_mtry_plot <- tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  ggplot(aes(x = min_n, y = mtry, color = mean)) +
  geom_point(size = 6) +
  geom_hline(yintercept = 10, linetype = "dotted") +
  geom_hline(yintercept = 30, linetype = "dotted") +
  geom_vline(xintercept = 2, linetype = "dotted") +
  geom_vline(xintercept = 8, linetype = "dotted") +
  scale_color_viridis_c(option = "D") +
  labs(color = "roc_auc")
nonortho_gid_n_mtry_plot
```

![](random_forest_hyperparameters_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

While it’s not a regular grid (of orthogonal combinations that would
allow for *ceteris paribus* testing) of `min_n` and `mtry`, but we can
get an idea of what is going on. It looks like higher values of mtry are
good (above about 10) and lower values of min_n are good (below about
10). We can get a better handle on the hyperparameters by tuning one
more time, this time using regular_grid(). Let’s set ranges of
hyperparameters we want to try, (inside of the dotted line box displayed
on the 2D plot above) based on the results from our initial tune.

## Train-test some model hyperparameters

``` r
set.seed(456)

rf_grid <- grid_regular(mtry(range = c(10, 30)),
                        min_n(range = c(2, 8)),
                        levels = 5)

nrow(rf_grid)
```

    ## [1] 25

``` r
set.seed(456)
doParallel::registerDoParallel()
tune_reg_res <- tune_grid(tune_wf,
                          resamples = trees_folds,
                          grid = rf_grid)
```

### view results

``` r
tune_reg_res %>% select_best("accuracy")
```

<div class="kable-table">

| mtry | min_n | .config               |
|-----:|------:|:----------------------|
|   30 |     2 | Preprocessor1_Model05 |

</div>

``` r
tune_reg_res %>% select_best("roc_auc")
```

<div class="kable-table">

| mtry | min_n | .config               |
|-----:|------:|:----------------------|
|   15 |     2 | Preprocessor1_Model02 |

</div>

``` r
grid_n_mtry_plot <- tune_reg_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, linewidth = 1.5) +
  geom_point() +
  labs(title = "Tune AUC by min_n and mtry",
       y = "AUC")
grid_n_mtry_plot
```

![](random_forest_hyperparameters_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
nonortho_gid_n_mtry_plot <- tune_reg_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  ggplot(aes(x = min_n, y = mtry, color = mean)) +
  geom_point(size = 6) +
  scale_color_viridis_c(option = "D") +
  labs(color = "roc_auc")
nonortho_gid_n_mtry_plot
```

![](random_forest_hyperparameters_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

Both 2D plots show that the mtry = 15 and min_n = 2 hyperperamater
maximize the AUC for this random forest model.

# Finalize the model

``` r
best_auc <- tune_reg_res %>% select_best("roc_auc")
```

``` r
final_rf <- finalize_model(tune_spec, best_auc)

final_rf
```

    ## Random Forest Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   mtry = 15
    ##   trees = 1000
    ##   min_n = 2
    ## 
    ## Computational engine: ranger

## Understand final model

``` r
final_rf_vip_plot<- final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(legal_status ~ ., data = select(juiced, -tree_id)) %>%
  vip(geom = "point")
final_rf_vip_plot
```

![](random_forest_hyperparameters_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

Satisfyingly, whether the caretaker is private makes a large difference,
and latitude and longitide each make a large (and approximately equal)
contribution.

# Apply the final model

``` r
final_wf <- workflow() %>%
  add_recipe(tree_rec) %>%
  add_model(final_rf)

final_result <- final_wf %>% last_fit(trees_split)
```

``` r
final_result %>% collect_metrics()
```

<div class="kable-table">

| .metric  | .estimator | .estimate | .config              |
|:---------|:-----------|----------:|:---------------------|
| accuracy | binary     | 0.8491864 | Preprocessor1_Model1 |
| roc_auc  | binary     | 0.9460006 | Preprocessor1_Model1 |

</div>

This is a great result, because it means we did not over fit to the
training data set. This is the AUC we can expect for new San Francisco
Trees.

## Make predictions

``` r
final_result_ano <- final_result %>%
  collect_predictions() %>%
  mutate("correct_prediction" = if_else(legal_status == `.pred_class`, "Correct", "Incorrect")) %>%
  bind_cols(trees_test)
```

    ## New names:
    ## • `legal_status` -> `legal_status...6`
    ## • `legal_status` -> `legal_status...10`

``` r
tree_correct_loc_plot <- final_result_ano %>%
  ggplot(aes(x = longitude, y = latitude, color = correct_prediction)) +
  geom_point(alpha = 0.6, size = 0.25) +
  labs(color = NULL, x = NULL, y = NULL) +
  theme(panel.border = element_blank(),
        legend.position = c(0.1, 0.9), legend.justification = c(0, .5)) +
  scale_color_viridis_d(option = "C", begin = 0.1, end = 0.7)
tree_correct_loc_plot
```

![](random_forest_hyperparameters_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

There is some degree of spatial bias in the incorrect assignment of
legal status of the SF Trees.
