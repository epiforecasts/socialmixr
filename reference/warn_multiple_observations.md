# Warn if survey has multiple observations per participant

Issues a warning when a survey contains multiple observations per
participant (more rows than unique part_id values).

## Usage

``` r
warn_multiple_observations(
  participants,
  observation_key = NULL,
  filter_hint = c("pipeline", "legacy")
)
```

## Arguments

- participants:

  participant data.table

- observation_key:

  optional column name(s) identifying observations

- filter_hint:

  character; "pipeline" for pipeline-style hint or "legacy" for
  contact_matrix-style hint

## Value

NULL invisibly
