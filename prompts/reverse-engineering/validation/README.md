# CardDemo Validation Prompt Suite

## Overview

This validation suite (VL-000 through VL-012) rigorously validates the output of the reverse engineering prompt suite (RE-000 through RE-011). Each validation prompt cross-references generated documentation against the actual CardDemo source code to detect hallucinations, factual errors, completeness gaps, and quality issues.

## Design Principles

1. **Trust nothing** — Every claim in generated documentation must be verified against source code
2. **Ground truth is embedded** — Each prompt contains the authoritative file inventory so the validator has canonical reference
3. **Source-first verification** — For every `file:line` reference, read the actual source and verify the claim
4. **Quantitative rigor** — File counts, PIC clauses, field names, and record lengths must match exactly
5. **Hallucination detection** — Actively search for references to files, fields, or values that do not exist
6. **Structured scoring** — Every validation produces a numeric score with weighted categories
7. **Actionable output** — Findings are categorized by severity with specific remediation guidance
8. **Reproducible** — Running the same validation twice on unchanged docs produces the same result

## Ground Truth Inventory

These counts are verified directly from the source tree and serve as the canonical reference for all validation prompts.

| Asset | Location | Count | Notes |
|-------|----------|-------|-------|
| COBOL programs | `app/cbl/` | **31** | 29 `.cbl` + 2 `.CBL` (CBSTM03A.CBL, CBSTM03B.CBL) |
| Copybooks | `app/cpy/` | **30** | 29 `.cpy` + 1 `.CPY` (COSTM01.CPY) |
| BMS copybooks | `app/cpy-bms/` | **17** | All `.CPY` uppercase extension |
| BMS mapsets | `app/bms/` | **17** | All `.bms` lowercase extension |
| JCL files | `app/jcl/` | **38** | 33 `.jcl` + 5 `.JCL` |
| Data files (ASCII) | `app/data/ASCII/` | **9** | Text-format reference data |
| Data files (EBCDIC) | `app/data/EBCDIC/` | **13** | Mainframe-format data (includes .gitkeep) |
| Extension dirs | `app/app-*` | **3** | ims-db2-mq, transaction-type-db2, vsam-mq |

### Known Hallucinations in RE Prompts

These are errors in the RE prompts themselves that validators must flag when they appear in generated documentation:

| Hallucination | Reality | Affected RE Prompts |
|--------------|---------|-------------------|
| `CVCAR00Y.cpy` referenced as card copybook | Actual file is `CVCRD01Y.cpy` | RE-001, RE-002 |
| `COUSR00Y.cpy` referenced as user copybook | Actual file is `CSUSR01Y.cpy` | RE-001, RE-002, RE-007 |
| `S9(7)V99 COMP-3` cited for monetary fields | Actual PIC is `S9(10)V99` (see `CVACT01Y.cpy:7`) | RE-001, RE-002, RE-009 |
| 39 COBOL programs claimed | Actual count is 31 | RE-001 (success criteria), CLAUDE.md |
| 41 copybooks claimed | Actual count is 30 | RE-001 (success criteria), CLAUDE.md |
| 21 BMS mapsets claimed | Actual count is 17 | CLAUDE.md |

## Prompt Inventory

| Prompt | Validates | Target Documentation |
|--------|-----------|---------------------|
| [VL-001](VL-001-domain-model-validation.md) | RE-001 | `docs/reverse-engineering/01-domain-model/` |
| [VL-002](VL-002-data-model-validation.md) | RE-002 | `docs/reverse-engineering/02-data-model/` |
| [VL-003](VL-003-context-model-validation.md) | RE-003 | `docs/reverse-engineering/03-context-model/` |
| [VL-004](VL-004-c4-architecture-validation.md) | RE-004 | `docs/reverse-engineering/04-architecture/` |
| [VL-005](VL-005-screen-flow-validation.md) | RE-005 | `docs/reverse-engineering/05-specialized/SCREEN-FLOWS.md` |
| [VL-006](VL-006-batch-workflow-validation.md) | RE-006 | `docs/reverse-engineering/05-specialized/BATCH-WORKFLOWS.md` |
| [VL-007](VL-007-security-model-validation.md) | RE-007 | `docs/reverse-engineering/05-specialized/SECURITY-MODEL.md` |
| [VL-008](VL-008-integration-patterns-validation.md) | RE-008 | `docs/reverse-engineering/05-specialized/INTEGRATION-PATTERNS.md` |
| [VL-009](VL-009-test-coverage-validation.md) | RE-009 | `docs/reverse-engineering/06-quality/` |
| [VL-010](VL-010-modernization-readiness-validation.md) | RE-010 | `docs/reverse-engineering/07-modernization/` |
| [VL-011](VL-011-api-candidates-validation.md) | RE-011 | `docs/reverse-engineering/07-modernization/` |
| [VL-012](VL-012-master-index-validation.md) | RE-000 | `docs/reverse-engineering/index.md` |
| [VL-000](VL-000-cross-document-consistency.md) | ALL | All `docs/reverse-engineering/` files |

## Execution Order

```
Phase 1: Foundation Validation (run in parallel)
├── VL-001-domain-model-validation.md      ─┐
└── VL-002-data-model-validation.md        ─┴─► Run in parallel

Phase 2: Context Validation (depends on Phase 1)
└── VL-003-context-model-validation.md

Phase 3: Architecture Validation (depends on Phase 2)
└── VL-004-c4-architecture-validation.md

Phase 4: Specialized Validation (run in parallel, depends on Phase 1)
├── VL-005-screen-flow-validation.md         ─┐
├── VL-006-batch-workflow-validation.md      ─┤
├── VL-007-security-model-validation.md      ─┼─► Run in parallel
└── VL-008-integration-patterns-validation.md─┘

Phase 5: Quality Validation (depends on Phase 1)
└── VL-009-test-coverage-validation.md

Phase 6: Modernization Validation (run in parallel, depends on Phases 1-4)
├── VL-010-modernization-readiness-validation.md ─┐
└── VL-011-api-candidates-validation.md          ─┴─► Run in parallel

Phase 7: Master Index Validation (depends on Phases 1-6)
└── VL-012-master-index-validation.md

Phase 8: Cross-Document Consistency (run LAST, depends on ALL)
└── VL-000-cross-document-consistency.md
```

## Scoring Rubric

All validation prompts use the same weighted scoring system:

| Category | Weight | Severity | Description |
|----------|--------|----------|-------------|
| Source Reference Accuracy | 35% | Critical | Every `file:line` citation verified against actual source |
| Factual Accuracy | 25% | Critical | PIC clauses, field names, logic descriptions match reality |
| Completeness | 20% | Major | All expected items covered (programs, copybooks, files) |
| Quantitative Accuracy | 10% | Major | File counts, record lengths, byte calculations correct |
| Documentation Quality | 10% | Minor | Mermaid syntax valid, markdown well-formed, clarity |

### Verdict Thresholds

| Score | Verdict | Meaning |
|-------|---------|---------|
| 100 | **PASS** | Documentation is fully accurate and complete — zero defects |
| < 100 | **FAIL** | Documentation contains errors and MUST be remediated before use |

**Zero-tolerance policy**: Any hallucination, factual error, missing item, or quality defect constitutes a FAIL. There is no partial credit. Documentation either perfectly reflects the source code or it requires remediation.

### Finding Severity Levels

- **Critical**: Hallucinated file references, fabricated code excerpts, wrong PIC clauses — causes incorrect modernization decisions
- **Major**: Missing programs/copybooks in analysis, incomplete coverage — causes gaps in modernization planning
- **Minor**: Formatting issues, broken Mermaid syntax, unclear descriptions — causes friction but not errors

## Output Format

Each validation prompt produces a report in `docs/reverse-engineering/validation/` following this structure:

```markdown
# Validation Report: [Document Name]

## Verdict: [PASS|FAIL] — Score: [NN]/100

## Score Breakdown

| Category | Weight | Score | Weighted |
|----------|--------|-------|----------|
| Source Reference Accuracy | 35% | NN/100 | NN.N |
| Factual Accuracy | 25% | NN/100 | NN.N |
| Completeness | 20% | NN/100 | NN.N |
| Quantitative Accuracy | 10% | NN/100 | NN.N |
| Documentation Quality | 10% | NN/100 | NN.N |
| **Total** | **100%** | | **NN.N** |

## Critical Findings
[Findings that must be fixed before documentation can be trusted]

## Major Findings
[Findings that should be fixed to improve documentation reliability]

## Minor Findings
[Findings that improve quality but are not blocking]

## Hallucination Inventory

| # | Claim in Documentation | Reality (from source) | Source File:Line | Severity |
|---|----------------------|----------------------|-----------------|----------|
| 1 | ... | ... | ... | Critical |

## Completeness Gaps

| # | Expected Item | Status | Notes |
|---|--------------|--------|-------|
| 1 | ... | Missing/Partial/Present | ... |

## Recommendations
[Specific, actionable recommendations for remediation]

## Remediation Manifest
Machine-parseable remediation data for downstream remediation prompts.

| ID | Finding | Target File | Location | Current (Wrong) | Required (Correct) | Remediation Action | RE Prompt to Re-run |
|----|---------|-------------|----------|-----------------|--------------------|--------------------|---------------------|
| R-001 | ... | docs/reverse-engineering/...md | Section/Line | "S9(7)V99" | "S9(10)V99" | Replace incorrect PIC clause | RE-002 |

### Remediation Instructions
For each finding above, a remediation prompt should:
1. Read the target file at the specified location
2. Verify the current (wrong) value is still present
3. Replace with the required (correct) value, using the source file:line as authoritative reference
4. Re-validate after remediation to confirm the fix

### Affected RE Prompts
List of RE prompts that should be re-executed (if regeneration is preferred over surgical fix):
- [RE-NNN] — reason for re-execution
```

## Output Directory

```
docs/reverse-engineering/validation/
├── VL-001-domain-model-report.md
├── VL-002-data-model-report.md
├── VL-003-context-model-report.md
├── VL-004-c4-architecture-report.md
├── VL-005-screen-flow-report.md
├── VL-006-batch-workflow-report.md
├── VL-007-security-model-report.md
├── VL-008-integration-patterns-report.md
├── VL-009-test-coverage-report.md
├── VL-010-modernization-readiness-report.md
├── VL-011-api-candidates-report.md
├── VL-012-master-index-report.md
└── VL-000-cross-document-consistency-report.md
```

## Working Directory

Progress tracking and intermediate artifacts:

```
.work/reverse-engineering/validation/
├── progress.yaml
├── vl-001/
├── vl-002/
├── ...
└── vl-000/
```

## Prerequisites

Before running validation prompts, ensure the corresponding RE prompt has been executed and its output exists in `docs/reverse-engineering/`. Validators read the generated documentation and cross-reference it against the actual source code in `app/`.
