
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `envClean`: an R package to help clean large unstructured data set(s)

<!-- badges: start -->

<!-- badges: end -->

The goal of `envClean` is to help clean large amounts of unstructured,
biological data (for further analysis elsewhere).

Not all functions will be relevant to all projects.

If a typical species list from a typical observer is required, then
`make_effort_mod()` may be useful to filter out excessively rich or
depauperate lists.

If many data sources are included in the incoming data, taxonomic
alignment via `make_taxonomy()` is likely to be required. If those data
sources are likely to contain duplicates, using taxonomic, geographic
and temporal bins may be the easiest way to ensure duplicates are
removed.

Some functions could be considered ‘experimental’. `add_cover()` uses
principal components analysis on environmental variables to generate a
best guess for percentage cover where some records are missing that
attribute.

## Installation

`envClean` is not on [CRAN](https://CRAN.R-project.org).

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Acanthiza/envClean")
```

Load `envClean`

``` r
library("envClean")
```

## Other packages/resources

These are unrelated to `envClean` and are possibly much better
documented:

- The book [Cleaning Biodiversity Data in
  R](https://cleaning-data-r.ala.org.au/)
- R packages:
  - [CoordinateCleaner](https://cran.r-project.org/web/packages/CoordinateCleaner/vignettes/Cleaning_GBIF_data_with_CoordinateCleaner.html)
  - [bdc](https://cran.r-project.org/web/packages/bdc/index.html)

## What is in `envClean`

The following functions and data sets are provided in `envClean`. See
<https://acanthiza.github.io/envClean/> for more examples.

| object | class | description |
|:---|:---|:---|
| `envClean::add_cover()` | function | Generate best guess of cover for each taxa \* context |
| `envClean::add_height()` | function | Generate best guess of height for each taxa\*context |
| `envClean::add_lifeform()` | function | Generate best guess of lifeform for each taxa\*context |
| `envClean::aoi` | sf and data.frame | Simple feature to define a geographic area of interest. |
| `envClean::bin_date()` | function | Add temporal bins to a dataframe |
| `envClean::bin_geo_rel()` | function | Add a spatial reliability column, binned to contexts |
| `envClean::bin_taxa()` | function | Add code{taxa} column |
| `envClean::clean_quotes()` | function | Remove any ’ or ” from specified columns in a dataframe |
| `envClean::cleaning_summary()` | function | Describte change in taxa, records, visits and sites between cleaning steps |
| `envClean::cleaning_text()` | function | Write a sentence describing change in taxa, records, visits and sites between |
| `envClean::filter_counts()` | function | Filter any context with less instances than a threshold value |
| `envClean::filter_geo_range()` | function | Filter a dataframe with e/n or lat/long to an area of interest polygon (sf) |
| `envClean::filter_prop()` | function | Filter taxa recorded in less than x proportion of contexts |
| `envClean::filter_taxa()` | function | Clean/Tidy to one row per taxa\*Visit |
| `envClean::filter_text_col()` | function | Filter a dataframe column on character string(s) |
| `envClean::find_outliers()` | function | Find local outliers |
| `envClean::find_taxa()` | function | Find how taxa changed through the cleaning/filtering/tidying process |
| `envClean::flag_local_outliers()` | function | Flag local outliers using code{dbscan} package |
| `envClean::flag_rjack_outliers()` | function | Flag reverse jackknife outliers |
| `envClean::flor_all` | data.frame | Example of data combined from several data sources. |
| `envClean::get_taxonomy()` | function | Get GBIF backbone taxonomy |
| `envClean::luclean` | tbl_df, tbl and data.frame | Dataframe of cleaning steps |
| `envClean::lurank` | tbl_df, tbl and data.frame | Dataframe of taxonomic ranks |
| `envClean::make_attribute()` | function | Title |
| `envClean::make_con_status()` | function | Make conservation status from existing status codes |
| `envClean::make_cover()` | function | Make a single (numeric, proportion) cover column from different sorts of |
| `envClean::make_effort_mod()` | function | Distribution of credible values for taxa richness. |
| `envClean::make_effort_mod_pca()` | function | Model the effect of principal components on taxa richness. |
| `envClean::make_env_pca()` | function | Principal components analysis and various outputs from environmental data |
| `envClean::make_gbif_taxonomy()` | function | Make taxonomy lookups |
| `envClean::make_ind_status()` | function | Make indigenous status lookup |
| `envClean::make_lifeform()` | function | Get unique lifeform across taxa, perhaps including further context |
| `envClean::make_subspecies_col()` | function | Make a subspecies column |
| `envClean::make_taxonomy()` | function | Get taxonomy via code{galah::taxa_search()} |
| `envClean::make_unmatched_overrides()` | function | Attempt to find a taxa for names with no match in code{galah::search_taxa()} |
| `envClean::rec_vis_sit_tax()` | function | How many records, visits, sites and taxa in a dataframe |
| `envClean::reduce_geo_rel()` | function | Reduce data frame to a single spatial reliability within a context |
| `envClean::rjack()` | function | Reverse jackknife |
| `envClean::taxonomy_overrides` | tbl_df, tbl and data.frame | Manual taxonomic overrides |
| `envClean::try_name_via_gbif()` | function | Attempt to find an unmatched scientific name using GBIF Backbone Taxonomy |
