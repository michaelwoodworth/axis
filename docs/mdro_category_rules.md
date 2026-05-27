# AXIS MDRO Category Rules

AXIS starts with simple deterministic rules for synthetic data. These rules are placeholders for internal review and are not clinical decision support.

## Current Rule Order

1. Use `mdro_hint` when it is one of `CRE`, `ESBL`, `VRE`, `MRSA`, or `MDR-PA`.
2. Infer `VRE` for Enterococcus with vancomycin resistance.
3. Infer `MRSA` for Staphylococcus aureus with oxacillin resistance.
4. Infer `MDR-PA` for Pseudomonas aeruginosa with a resistant call.
5. Infer `CRE` for Enterobacterales with carbapenem resistance.
6. Infer `ESBL` for Escherichia coli or Klebsiella with extended-spectrum cephalosporin resistance.
7. Return `Other` when no rule matches.

## Assumptions

- Synthetic `mdro_hint` values are trusted when present.
- Species matching is text based and intentionally conservative.
- The scaffold uses one row per susceptibility result; future imports may need isolate-level rollups.
