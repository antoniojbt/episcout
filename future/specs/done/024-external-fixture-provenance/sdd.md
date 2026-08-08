# Software Design

Spec ID: `024-external-fixture-provenance`
Status: Implemented

## Scope

Download the two pinned CRAN source archives only during explicit manual
regeneration, verify SHA-256, install them into an isolated temporary library,
load the exact dataset objects, verify serialized bytes before replacement, and
write checksum manifests for every committed fixture-family file.

## Public API

None. Package runtime and analytical interfaces are unchanged.

## Data Flow

1. Download exact versioned CRAN source archives to a temporary directory.
2. Verify archive SHA-256 and install without dependencies into a temporary
   library.
3. Load the named dataset objects and serialize to temporary CSV candidates.
4. Verify candidates against pinned fixture SHA-256 before replacement.
5. Regenerate transparent expectations and provenance records.
6. Write complete per-family checksum manifests.
7. Routine offline tests verify every manifest entry and required provenance.

## Redistribution

`penguins_raw` retains its documented CC0 basis. `blood_storage` is retained as
an exact dataset serialization from the MIT-licensed `medicaldata` source
package; the package's exact `LICENSE` notice is committed beside the fixture.
This records the package-level redistribution basis without claiming that the
fixture independently validates every clinical semantic label.

## Dependencies

No new dependency. Manual regeneration and offline checksum tests use the
already imported `openssl` package.
