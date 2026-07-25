# Software Design

Spec ID: `006-synthetic-integer-generation`
Status: Implemented

## Scope

Harden the internal integer generator used by `epi_eda_generate_synthetic_data()` without changing its public interface.

## Integer Domain Contract

1. Convert the validated numeric bounds to `ceiling(min)` and `floor(max)`.
2. Error when the resulting lower bound exceeds the upper bound because the interval contains no integer.
3. Build the inclusive integer candidate vector.
4. Return an empty vector immediately when `n = 0`.
5. Sample candidate indices with `sample.int()` and index the candidate vector. This avoids `sample()` interpreting a singleton positive value as `1:x`.

## Public API

No public arguments, return columns or dependencies change.

## Errors

An interval containing no integer errors with the variable name and explains that its synthetic bounds contain no integer values.

## Compatibility

Repeated calls with the same specification, `n` and seed remain identical. No exact historical random sequence is guaranteed.
