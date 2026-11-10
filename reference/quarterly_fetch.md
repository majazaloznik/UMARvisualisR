# Wrap a fetch function so a whole chart comes back quarterly

Wrap a fetch function so a whole chart comes back quarterly

## Usage

``` r
quarterly_fetch(fetch_fn, aggr_func = mean, min_obs = 3L)
```

## Arguments

- fetch_fn:

  the underlying fetch function, e.g. UMARaccessR::get_series_table

- aggr_func:

  aggregation function, e.g. mean or sum

- min_obs:

  months required before a quarter is reported; 3 (the default) allows
  only complete quarters

## Value

a function with fetch_fn's signature
