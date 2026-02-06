# VL-000: Cross-Document Consistency Validation Report

**Validation ID**: VL-000
**Date**: 2026-02-06
**Validator**: Claude Opus 4.6
**Target**: All RE documentation (36 original + 2 new documents)
**Status**: Complete

---

## 1. Verdict

**FAIL** — Score: **71.0 / 100**

Cross-document consistency is the weakest aspect of the documentation suite. While individual documents are generally well-written and technically accurate, they were produced independently and contain numerous contradictions in program counts, COMMAREA specifications, JCL inventories, and scope boundaries. The lack of a shared "source of truth" manifest means each document established its own baseline, leading to divergent numbers that undermine the reliability of aggregate claims.

---

## 2. Consistency Findings

### CON-001: Program Count Inconsistencies (Critical)

The same metric — "how many COBOL programs are in this system" — yields different answers depending on which document you read:

| Document | Count | What's Counted | Correct? |
|----------|-------|---------------|----------|
| index.md (v1.0) | 29 | Core .cbl only | Partial — misses 2 .CBL + 13 extensions |
| CLAUDE.md (v1.0) | 39 | All .cbl across all dirs | Partial — misses 5 .CBL files |
| PROGRAM-INVENTORY.md (v1.0) | 29 | Core .cbl only | Partial |
| BATCH-WORKFLOWS.md | 10 batch | Core .cbl batch | Partial — actual is 12 core batch |
| C4-L3-COMPONENT.md | "29 programs" | Core .cbl only | Partial |
| MODERNIZATION-READINESS.md | "29 programs" | Core .cbl only | Partial |
| **Actual (all directories)** | **44** | **All .cbl + .CBL across all directories** | **Correct** |

**Status**: Fully remediated. CLAUDE.md, index.md, PROGRAM-INVENTORY.md, BATCH-WORKFLOWS.md, C4-L3-COMPONENT.md, and MODERNIZATION-READINESS.md have all been corrected. See [CODEBASE-MANIFEST.md](../appendices/CODEBASE-MANIFEST.md) for canonical counts.

### CON-002: COMMAREA Size Inconsistencies (High)

| Document | Stated Size | Correct? |
|----------|-------------|----------|
| SECURITY-MODEL.md (Section 4.1) | ~130 bytes | **WRONG** — corrected to 160 bytes |
| COMMAREA-SPECIFICATION.md | 160 bytes | **Corrected** |
| **Actual (computed from COCOM01Y.cpy)** | **160 bytes** | **Correct** |

**Status**: Fully remediated. SECURITY-MODEL.md and COMMAREA-SPECIFICATION.md both corrected to 160 bytes.

### CON-003: JCL Count Inconsistencies (High)

| Document | Count | What's Counted | Correct? |
|----------|-------|---------------|----------|
| BATCH-WORKFLOWS.md (original) | 33 | Documented lowercase .jcl | Partial |
| VL-006 Report | 38 | All in app/jcl/ | Core correct |
| index.md (v1.0) | 33 | From BATCH-WORKFLOWS | Partial |
| **Actual (app/jcl/)** | **38** (33 .jcl + 5 .JCL) | **Core correct** |
| **Actual (all directories)** | **46** (38 core + 8 extension) | **Total correct** |

**Status**: BATCH-WORKFLOWS.md and index.md corrected. VL-006 core count was already accurate.

### CON-004: Copybook Count Inconsistencies (Medium)

| Document | Count | What's Counted |
|----------|-------|---------------|
| index.md (v1.0) | 29 | app/cpy/ .cpy only |
| CLAUDE.md (v1.0) | 41 | All .cpy across all dirs (excluding .CPY BMS) |
| COPYBOOK-INVENTORY.md | 29 | Core .cpy only |
| **Actual** | **58** | **All .cpy + .CPY across all directories** |

### CON-005: Bounded Context Count (Low)

All documents consistently state "7 bounded contexts." This appears correct and consistent across documents.

### CON-006: Business Rules Count (Low)

The Domain Model states "59+ business rules." This number is referenced in the Modernization Assessment and index.md. The count has not been independently verified but is consistent across documents.

### CON-007: BMS Screen Count (Medium)

| Document | Count | What's Counted |
|----------|-------|---------------|
| index.md (v1.0) | 17 | Core BMS screens |
| SCREEN-FLOWS.md | 17 | Core BMS screens |
| **Actual (core)** | **19** | app/bms/*.bms |
| **Actual (total)** | **23** | All directories |

The core BMS directory contains 19 .bms files, not 17. The discrepancy may reflect programs-with-screens vs total BMS files, but this should be clarified.

### CON-008: Transaction ID Coverage (Medium)

| Document | TransIDs Listed | Complete? |
|----------|----------------|-----------|
| index.md | CC00, CA00, CM00 | 3 of 17+ |
| SECURITY-MODEL.md | CC00 only (in auth flow) | 1 |
| CBADMCDJ.jcl CSD | CC00, CCT1, CCT2, CCT3, CCT4, CCDM | 6 |
| Extension CSD files | Additional TransIDs | Not inventoried |

No single document provides a complete TransID-to-program mapping.

---

## 3. Consistency Matrix

### Documents that reference program counts

| Document | Stated | After Correction | Status |
|----------|--------|-----------------|--------|
| CLAUDE.md | ~~39~~ → 31 core | 44 total | Corrected (directory structure) |
| index.md | ~~29~~ | 44 | Corrected |
| PROGRAM-INVENTORY.md | ~~29~~ | 44 | Corrected to v2.0 |
| BATCH-WORKFLOWS.md | ~~33 JCL, 10 batch~~ | 38 JCL, 12 batch | Corrected |
| VL-006 Report | 38 JCL, 12 batch | Accurate for core | Already correct |
| SECURITY-MODEL.md | ~~~130 byte COMMAREA~~ | 160 bytes | Corrected |
| C4-L3-COMPONENT.md | ~~29 programs~~ | Corrected (prior remediation) | Corrected |
| MODERNIZATION-READINESS.md | ~~29 programs, 29 copybooks, 33 JCL~~ | 31 core / 44 total | Corrected (RM-000) |
| API-CANDIDATES.md | 18 programs scored | Core only; correct for scope | Acceptable |
| CONTEXT-MAP.md | 7 bounded contexts | Consistent | OK |

---

## 4. Score Breakdown

| # | Category | Weight | Raw Score | Weighted Score |
|---|----------|--------|-----------|----------------|
| 1 | Program Count Consistency | 25% | 50 | 12.5 |
| 2 | Data Structure Consistency (COMMAREA, VSAM) | 20% | 70 | 14.0 |
| 3 | JCL/Batch Consistency | 15% | 65 | 9.75 |
| 4 | Cross-Reference Accuracy | 15% | 85 | 12.75 |
| 5 | Terminology Consistency | 10% | 90 | 9.0 |
| 6 | Scope Boundary Clarity | 15% | 85 | 12.75 |
| | **TOTAL** | **100%** | | **71.0** (rounded from 70.75) |

---

## 5. Remediation Status

| ID | Finding | Status | Documents Affected |
|----|---------|--------|-------------------|
| CON-001 | Program counts | **Remediated** | CLAUDE.md, index.md, PROGRAM-INVENTORY.md, C4-L3-COMPONENT.md, MODERNIZATION-READINESS.md all corrected |
| CON-002 | COMMAREA size | **Remediated** | SECURITY-MODEL.md and COMMAREA-SPECIFICATION.md corrected to 160 bytes |
| CON-003 | JCL counts | **Remediated** | BATCH-WORKFLOWS.md, index.md, MODERNIZATION-READINESS.md corrected |
| CON-004 | Copybook counts | **Remediated** | index.md corrected to 62 total; MODERNIZATION-READINESS.md references manifest |
| CON-005 | Bounded contexts | **Consistent** | No action needed |
| CON-006 | Business rules count | **Consistent but unverified** | Low priority |
| CON-007 | BMS screen count | **Not remediated** | index.md partially updated; SCREEN-FLOWS needs verification |
| CON-008 | Transaction IDs | **Not remediated** | No complete TransID mapping exists |

---

## 6. Recommendations

1. **Create a shared manifest file** -- **DONE**: [CODEBASE-MANIFEST.md](../appendices/CODEBASE-MANIFEST.md) serves as the single source of truth for all counts and metrics.

2. **Define scope conventions**: Each document should explicitly state whether its counts include extensions, uppercase-extension files, or only core lowercase files.

3. **Complete the TransID inventory**: Create or update `docs/reverse-engineering/appendices/TRANSACTION-INVENTORY.md` with all CICS transaction IDs from CSD files across all directories.

4. **Update remaining stale documents** -- **DONE**: C4-L3-COMPONENT.md and MODERNIZATION-READINESS.md have been corrected. index.md updated with manifest reference.

---

*This validation report was generated as part of the CardDemo reverse engineering validation suite.*
