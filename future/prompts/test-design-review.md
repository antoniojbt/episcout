# Test Design Review Prompt

Review the test plan before implementation.

Check:

- tests define desired behaviour rather than current accidental behaviour;
- expected outputs are independently computed where fixtures are used;
- routine tests avoid internet access and optional source packages;
- edge cases and invalid inputs are covered;
- tests are placed under `tests/testthat/`;
- future spec files remain planning documents only.
