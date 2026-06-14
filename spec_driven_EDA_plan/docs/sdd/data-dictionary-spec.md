# Data dictionary specification

## Purpose

The data dictionary is the core contract for specification-first EDA.

It defines expected variables before data are available and validates observed data once real data arrive.

## Required columns

| Column | Type | Description |
|---|---:|---|
| `name` | character | Expected variable name in the dataset. |
| `label` | character | Human-readable variable label. |
| `type` | character | Expected variable type. |
| `role` | character | Analytical role, such as exposure, outcome, covariate, ID or date. |

## Recommended columns

| Column | Type | Description |
|---|---:|---|
| `units` | character | Units, where applicable. |
| `levels` | character | Allowed categorical levels separated by semicolons. |
| `min` | numeric/date | Minimum plausible value. |
| `max` | numeric/date | Maximum plausible value. |
| `missing_codes` | character | Non-standard missing-value codes separated by semicolons. |
| `required` | logical | Whether the variable must be present. |
| `derivation` | character | Notes on derived variables. |
| `description` | character | Longer description or provenance note. |
| `group` | character | Optional grouping such as demographics, labs, outcomes. |
| `time_varying` | logical | Whether the variable may vary over time. |

## Allowed variable types

Initial MVP types:

```text
numeric
integer
categorical
binary
date
datetime
text
```

Future types may include:

```text
id
survival_time
site
geography
code
ordered_categorical
```

## Example

```csv
name,label,type,role,units,levels,min,max,missing_codes,required,group
person_id,Person identifier,text,id,,,,,TRUE,identifiers
age,Age at baseline,numeric,covariate,years,,18,110,"-9;-99",TRUE,demographics
sex,Sex at birth,categorical,covariate,,"Female;Male;Unknown",,,TRUE,demographics
bmi,Body mass index,numeric,covariate,kg/m2,,10,80,"-9",FALSE,clinical
admission_date,Admission date,date,index_date,,,,,FALSE,dates
death,Death during follow-up,binary,outcome,,"0;1",,,TRUE,outcomes
```

## Validation rules

`validate_eda_spec()` should check:

- required columns exist;
- variable names are non-empty;
- variable names are unique;
- variable types are valid;
- `levels` are present for categorical and binary variables where possible;
- `min` is not greater than `max`;
- `required` can be parsed as logical;
- missing-value codes can be parsed;
- unsupported types raise a clear error.

## Missing-value codes

`missing_codes` should support values such as:

```text
-9
-99
-999
.
NULL
Not known
Refused
Missing
```

These should be standardised to `NA` during EDA, with a record of what was changed.

## Synthetic-data use

The data dictionary should drive `generate_synthetic_data()`.

For MVP:

- numeric: random normal or uniform values inside plausible range;
- integer: integer values inside plausible range;
- categorical: random sample from allowed levels;
- binary: random sample from allowed levels or 0/1;
- date: random dates within a plausible interval;
- datetime: random datetimes;
- text: deterministic synthetic strings.

Future versions may support:

- marginal distributions;
- correlations;
- site-level clustering;
- calendar effects;
- outcome prevalence;
- longitudinal rows per person;
- missing-data mechanisms;
- plausibility constraints.
