# AXIS Agent Notes

AXIS is private research infrastructure. Work from the committed synthetic fixtures unless the user explicitly provides another safe test file.

Rules for future agents:

- Never commit PHI, real specimen accession IDs, real isolate IDs, real patient data, or real VITEK2/OpenSpecimen exports.
- Keep importer functions small and deterministic.
- Add comments where a parsing, linking, or MDRO-category assumption is made.
- Do not add dependencies unless the gain is clear and the dependency is needed by app code, not just convenience.
- Keep tests focused on behavior using `tests/fixtures/`.
- Store generated app data, databases, and exports outside git-tracked paths.
