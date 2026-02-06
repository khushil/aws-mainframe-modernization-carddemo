# VL-004: C4 Architecture Validation Report

**Validation ID**: VL-004
**Date**: 2026-02-06
**Validator**: Claude Opus 4.6
**Target**: RE-004 C4 Architecture Documentation
**Documents Validated**: C4-L1-SYSTEM-CONTEXT.md, C4-L2-CONTAINER.md, C4-L3-COMPONENT.md, C4-L4-CODE-PATTERNS.md, plus 5 diagram files
**Status**: Complete

---

## 1. Verdict

**FAIL** — Score: **90.0 / 100**

The RE-004 C4 Architecture documentation demonstrates excellent source reference accuracy — every code excerpt in L4 verified as an exact match at the cited line numbers — and contains **zero hallucinations**. The architecture patterns are well-documented with accurate COBOL examples, BMS counts are correct (17 mapsets), and all 6 VSAM KSDS files are properly listed. Mermaid diagrams use valid syntax throughout with both C4 and fallback flowchart variants.

However, the documentation has completeness and quantitative gaps: 3 programs are entirely missing from L2 and L3 (CSUTLDTC.cbl, CBSTM03A.CBL, CBSTM03B.CBL), 1 program (COBSWAIT.cbl) is documented in L2 but absent from all L3 component groups, and the numeric tallies at L2 and L3 are consequently incorrect (CICS shows 18 instead of 19, Batch shows 10 instead of 12, total shows 37 instead of 31 core or 40 with extensions).

---

## 2. Score Breakdown

| # | Category | Weight | Raw Score | Weighted Score | Key Evidence |
|---|----------|--------|-----------|----------------|--------------|
| 1 | Source Reference Accuracy | 35% | 100 | 35.0 | All 19 code excerpts verified exact match at cited line numbers |
| 2 | Factual Accuracy | 25% | 100 | 25.0 | All factual claims about listed programs, patterns, and data files correct |
| 3 | Completeness | 20% | 80 | 16.0 | 3 programs completely missing, 1 partially missing (28/31 = 90% core coverage) |
| 4 | Quantitative Accuracy | 10% | 60 | 6.0 | CICS count off by 1 (18→19), Batch off by 2 (10→12), total derivation wrong |
| 5 | Documentation Quality | 10% | 80 | 8.0 | Valid Mermaid, good structure, minor L2/L3 boundary inconsistency |
| | **TOTAL** | **100%** | | **90.0** | |

**Verdict: FAIL** (threshold: 100 = PASS; score 90.0 indicates specific gaps requiring remediation)

---

## 3. Critical Findings

**None.**

No hallucinated file names, fabricated copybooks, invented COBOL constructs, or non-existent programs were found. All referenced source files, line numbers, and code patterns exist and are accurate.

---

## 4. Major Findings

### MAJ-001: CSUTLDTC.cbl Missing from All C4 Levels

**Affected Documents**: `C4-L2-CONTAINER.md`, `C4-L3-COMPONENT.md`
**Source Evidence**: `app/cbl/CSUTLDTC.cbl` exists in the source tree as a CICS date utility program.

The program CSUTLDTC.cbl (Date Utility) is absent from:
- L2 Container diagram's CICS program table (claims 18 core programs, actual is 19)
- All L3 Component groups (not assigned to any functional group)

CSUTLDTC is a utility program invoked by other CICS programs for date conversion. Its omission means any migration plan derived from this architecture documentation would miss a shared dependency.

**Impact**: Downstream migration tooling relying on C4 documentation would not include this utility, potentially breaking date-dependent programs.

### MAJ-002: CBSTM03A.CBL and CBSTM03B.CBL Missing from All C4 Levels

**Affected Documents**: `C4-L2-CONTAINER.md`, `C4-L3-COMPONENT.md`
**Source Evidence**: Both files exist in `app/cbl/` as batch statement processing programs.

| Missing Program | Type | Description |
|----------------|------|-------------|
| CBSTM03A.CBL | Batch | Statement Processing A |
| CBSTM03B.CBL | Batch | Statement Processing B |

The L2 batch program table lists 10 programs; with these two additions the actual count is 12. Neither program appears in any L3 batch component group.

**Impact**: Batch processing architecture is incomplete. Statement generation workflows are undocumented.

### MAJ-003: COBSWAIT.cbl Present in L2 but Missing from L3 Component Groups

**Affected Documents**: `C4-L3-COMPONENT.md`
**Source Evidence**: `app/cbl/COBSWAIT.cbl` exists and is listed in the L2 CICS program table.

COBSWAIT.cbl appears in the L2 Container-level program inventory but is not assigned to any component group at L3. This creates an inconsistency between architecture levels — a program acknowledged to exist at L2 vanishes at L3.

**Impact**: L3 component groups do not fully reconcile with the L2 inventory, undermining the C4 level-to-level traceability.

---

## 5. Minor Findings

### MIN-001: Total Program Count Incorrect at L3

**Document**: `C4-L3-COMPONENT.md`
**Claim**: "37 programs" (18 CICS + 9 extension + 10 batch)
**Actual**: 31 core programs (19 CICS + 12 batch) or 40 total with 9 extension programs
**Issue**: The arithmetic is wrong regardless of interpretation. 18+9+10 = 37, but the correct base counts are 19 CICS + 12 batch = 31 core. With 9 extensions: 31+9 = 40.

### MIN-002: L1 System Context Program Counts Imprecise

**Document**: `C4-L1-SYSTEM-CONTEXT.md`
**Claims**: References to "18+" CICS programs, "10" batch programs, "28+" total
**Actual**: 19 CICS programs, 12 batch programs, 31 core total
**Issue**: While L1 is expected to be high-level, the specific numbers cited are inaccurate. Either use exact counts or omit numbers entirely at L1.

---

## 6. Hallucination Inventory

| Suspect Pattern | Search Scope | Result | Verdict |
|----------------|-------------|--------|---------|
| CVCAR00Y (fabricated copybook) | All 4 C4 docs + 5 diagrams | 0 matches | CLEAN |
| COUSR00Y (fabricated copybook) | All 4 C4 docs + 5 diagrams | 0 matches | CLEAN |
| S9(7)V99 (fabricated PIC clause) | All 4 C4 docs + 5 diagrams | 0 matches | CLEAN |
| Non-existent programs | All C4 docs | All referenced programs verified to exist | CLEAN |
| Fabricated VSAM files | C4-L2/L3 | All 6 KSDS files confirmed (ACCTDAT, CARDDAT, CUSTDAT, TRANSACT, CCXREF, USRSEC) | CLEAN |
| Invented BMS mapsets | C4-L2/L3 | All 17 referenced mapsets exist | CLEAN |
| Fabricated architecture patterns | C4-L4 | All 7 patterns verified in source | CLEAN |

**Hallucination count: 0**

---

## 7. Completeness Gaps

### Program Coverage by C4 Level

| Category | Documented | Actual | Coverage |
|----------|-----------|--------|----------|
| L2 CICS Programs | 18 | 19 | 94.7% |
| L2 Batch Programs | 10 | 12 | 83.3% |
| L2 Extension Programs | 9 | 9 | 100% |
| L3 Component Groups (CICS) | 17 (COBSWAIT missing) | 19 | 89.5% |
| L3 Component Groups (Batch) | 10 | 12 | 83.3% |

### Missing Programs Detail

| Program | L1 | L2 | L3 | L4 |
|---------|-----|-----|-----|-----|
| CSUTLDTC.cbl | ✗ | ✗ | ✗ | N/A |
| CBSTM03A.CBL | ✗ | ✗ | ✗ | N/A |
| CBSTM03B.CBL | ✗ | ✗ | ✗ | N/A |
| COBSWAIT.cbl | N/A | ✓ | ✗ | N/A |

### Other Asset Coverage

| Asset Type | Documented | Actual | Coverage |
|-----------|-----------|--------|----------|
| BMS Mapsets | 17 | 17 | 100% |
| VSAM KSDS Files | 6 | 6 | 100% |
| Copybooks Referenced | Multiple | Correct | Verified |
| Extension Directories | 3 | 3 | 100% |
| Architecture Patterns (L4) | 7 | 7 | 100% |

---

## 8. Source Citation Verification Results

All 19 source citations in the L4 Code Patterns document were verified against actual source files. Every excerpt matches the source at the cited line numbers.

| # | Pattern | File | Lines | Status |
|---|---------|------|-------|--------|
| 1 | Pseudo-conversational | COSGN00C.cbl | 80-102 | EXACT MATCH |
| 2 | Send Map | COSGN00C.cbl | 151-157 | EXACT MATCH |
| 3 | Receive Map | COSGN00C.cbl | 110-115 | EXACT MATCH |
| 4 | VSAM Read | COBIL00C.cbl | 345-372 | EXACT MATCH |
| 5 | VSAM Rewrite | COBIL00C.cbl | 377-403 | EXACT MATCH |
| 6 | Menu Navigation | COADM01C.cbl | 140-158 | EXACT MATCH |
| 7 | Input Validation | COSGN00C.cbl | 117-130 | EXACT MATCH |
| 8 | Confirm Validation | COBIL00C.cbl | 173-191 | EXACT MATCH |
| 9 | Error Handling | COSGN00C.cbl | 221-257 | EXACT MATCH |
| 10 | Batch File Open | CBTRN02C.cbl | 236-252 | EXACT MATCH |
| 11 | IO Status Display | CBTRN02C.cbl | 714-727 | EXACT MATCH |
| 12 | Abend Pattern | CBTRN02C.cbl | 707-711 | EXACT MATCH |
| 13 | Batch Sequential | CBTRN02C.cbl | 193-234 | EXACT MATCH |
| 14 | Context Flag | COCOM01Y.cpy | 29-31 | EXACT MATCH |
| 15 | COMMAREA | COCOM01Y.cpy | 26-28 | EXACT MATCH |
| 16 | Credit Limit | CBTRN02C.cbl | 403-413 | EXACT MATCH |
| 17 | Timestamp | CBTRN02C.cbl | 692-705 | EXACT MATCH |
| 18 | Menu Options | COADM02Y.cpy | 22-59 | MATCH |
| 19 | PIC Clause | CVACT01Y.cpy | 7 | S9(10)V99 — correct, no hallucination |

**Source Reference Accuracy Rate**: 100% (19/19)

---

## 9. Positive Findings

| ID | Finding | Evidence |
|----|---------|----------|
| POS-001 | All 19 L4 code excerpts are exact matches at cited line numbers | See Section 8 — every citation verified |
| POS-002 | Zero hallucinations across all 9 C4 documents | No fabricated copybooks, programs, VSAM files, or PIC clauses |
| POS-003 | BMS mapset count correct (17) | All 17 mapsets verified to exist in `app/bms/` |
| POS-004 | VSAM KSDS file inventory correct (6 files) | ACCTDAT, CARDDAT, CUSTDAT, TRANSACT, CCXREF, USRSEC confirmed |
| POS-005 | All 7 L4 architecture patterns well-documented | Pseudo-conversational, BMS I/O, VSAM CRUD, Navigation, Validation, Error Handling, Batch Processing |
| POS-006 | Mermaid diagrams use valid syntax throughout | Both C4 notation and flowchart fallback versions are syntactically correct |
| POS-007 | Extension programs correctly identified | All 9 programs across 3 extension directories accurately listed |
| POS-008 | L4 patterns use real programs as examples | COSGN00C, COBIL00C, COADM01C, CBTRN02C — all verified |
| POS-009 | Cross-reference between levels is generally consistent | L1→L2→L3→L4 drill-down structure maintained (except noted gaps) |
| POS-010 | PIC clause S9(10)V99 in CVACT01Y.cpy correctly cited | No hallucinated S9(7)V99 variant |

---

## 10. Remediation Manifest

| ID | Severity | File | Current (Wrong) | Required (Correct) | Source Evidence | Action |
|----|----------|------|-----------------|--------------------|-----------------|----|
| REM-001 | MAJOR | C4-L2-CONTAINER.md | 18 core CICS programs | 19 core CICS programs | `app/cbl/CSUTLDTC.cbl` exists | Add CSUTLDTC row to CICS program table, update count |
| REM-002 | MAJOR | C4-L2-CONTAINER.md | 10 batch programs | 12 batch programs | `app/cbl/CBSTM03A.CBL`, `app/cbl/CBSTM03B.CBL` exist | Add CBSTM03A.CBL and CBSTM03B.CBL rows to batch table, update count |
| REM-003 | MAJOR | C4-L3-COMPONENT.md | CSUTLDTC not listed | Add to Utilities group | `app/cbl/CSUTLDTC.cbl` — date utility program | Add CSUTLDTC.cbl as utility component in L3 |
| REM-004 | MAJOR | C4-L3-COMPONENT.md | COBSWAIT in L2 only | Add to Utilities group | `app/cbl/COBSWAIT.cbl` — in L2 table but no L3 group | Add COBSWAIT.cbl to appropriate L3 component group |
| REM-005 | MAJOR | C4-L3-COMPONENT.md | 10 batch programs | 12 batch programs | `app/cbl/CBSTM03A.CBL`, `app/cbl/CBSTM03B.CBL` | Add statement processing component group with both programs |
| REM-006 | MAJOR | C4-L3-COMPONENT.md | 37 programs total | 31 core + 9 ext = 40 | Corrected arithmetic | Update summary table with correct total |
| REM-007 | MINOR | C4-L1-SYSTEM-CONTEXT.md | "18+" CICS, "10" batch, "28+" total | 19 CICS, 12 batch, 31 core | Corrected counts | Update all count references to use exact numbers |
| REM-008 | MINOR | C4-L2-CONTAINER.md | Counts reflect 18/10 | Counts reflect 19/12 | After adding missing programs | Update any summary/count text in L2 |

---

## 11. Cross-References to Prior Validations

### VL-001 (Domain Model) — Score: 88/100

| VL-001 Finding | VL-004 Relevance |
|----------------|-----------------|
| MAJ-002: CBACT02C-04C batch programs missing from domain | Same pattern: batch programs underrepresented; CBSTM03A/B also missing in VL-004 |
| MIN-005: Program counts discrepancy (29 vs 39) | VL-004 also has program count errors at multiple C4 levels |

### VL-002 (Data Model) — Score: 96.8/100

| VL-002 Finding | VL-004 Relevance |
|----------------|-----------------|
| All 6 VSAM files correct | Consistent with VL-004: C4 architecture correctly lists all 6 VSAM KSDS files |

### VL-003 (Context Model) — Score: 80.6/100

| VL-003 Finding | VL-004 Relevance |
|----------------|-----------------|
| MAJ-003: CORPT00C, CBSTM03A/B missing from bounded contexts | VL-004 confirms CBSTM03A/B pattern — consistently omitted across RE outputs |
| MAJ-001: BMS mapset name errors | VL-004 BMS count correct (17); no name errors found in C4 docs |

### Recurring Pattern Across Validations

CBSTM03A.CBL and CBSTM03B.CBL are consistently missing across RE-001, RE-003, and RE-004 outputs. This suggests the original reverse engineering prompts may not have fully inventoried `.CBL` (uppercase extension) files in `app/cbl/`.

---

## 12. Verification Methodology

### Source Assets Inventoried

| Asset Type | Count | Method |
|-----------|-------|--------|
| COBOL Programs (.cbl) | 29 | `ls app/cbl/*.cbl` |
| COBOL Programs (.CBL) | 2 | `ls app/cbl/*.CBL` |
| Copybooks (.cpy) | 29 | `ls app/cpy/*.cpy` |
| Copybooks (.CPY) | 1 | `ls app/cpy/*.CPY` |
| BMS Mapsets (.bms) | 17 | `ls app/bms/*.bms` |
| JCL Files (.jcl + .JCL) | 38 | `ls app/jcl/` |
| Extension Directories | 3 | `ls app/app-*/` |
| VSAM Data Files | 6 | Documented in CLAUDE.md and verified against source references |

### Source Files Verified

**L4 Code Pattern Citations (19 citations across 5 files)**:
- `app/cbl/COSGN00C.cbl` — lines 80-102, 110-115, 117-130, 151-157, 221-257
- `app/cbl/COBIL00C.cbl` — lines 173-191, 345-372, 377-403
- `app/cbl/COADM01C.cbl` — lines 140-158
- `app/cbl/CBTRN02C.cbl` — lines 193-234, 236-252, 403-413, 692-705, 707-711, 714-727
- `app/cpy/COCOM01Y.cpy` — lines 26-28, 29-31
- `app/cpy/COADM02Y.cpy` — lines 22-59
- `app/cpy/CVACT01Y.cpy` — line 7

**Program Existence Verification**:
- All 31 core programs (29 `.cbl` + 2 `.CBL`) confirmed via directory listing
- All 9 extension programs confirmed in their respective `app/app-*/cbl/` directories

### Verification Approach

1. **Source Reference Accuracy** (35% weight): Read each cited source file and line range; compared code excerpts character-by-character against L4 documentation. All 19 citations matched exactly.
2. **Factual Accuracy** (25% weight): Verified all stated program purposes, VSAM file descriptions, BMS mapset counts, architecture pattern descriptions, and extension module categorizations against source code.
3. **Completeness** (20% weight): Enumerated all 31 COBOL programs (19 CICS + 12 batch), 30 copybooks, 17 BMS mapsets, 6 VSAM files, and 3 extension directories; compared against C4 documentation inventories at each level.
4. **Quantitative Accuracy** (10% weight): Verified all numeric claims (program counts, file counts, pattern counts) against actual filesystem contents.
5. **Documentation Quality** (10% weight): Validated Mermaid diagram syntax, C4 level-to-level consistency, markdown formatting, and cross-level traceability.

### Hallucination Vectors Tested
- Fabricated copybook names (CVCAR00Y, COUSR00Y — not referenced)
- Fabricated PIC clause S9(7)V99 — not referenced (actual is S9(10)V99)
- Non-existent programs in any C4 table — none found
- Invented VSAM files or data structures — none found
- Fabricated BMS mapset names — none found

---

## 13. Work Artifacts

| Artifact | Path |
|----------|------|
| Progress Tracking | `.work/reverse-engineering/validation/vl-004/progress.yaml` |

---

## 14. Conclusion

The RE-004 C4 Architecture documentation is strong in its core technical content. The L4 Code Patterns document is particularly noteworthy — all 19 source citations are exact matches, and the 7 architecture patterns provide accurate, useful documentation of CardDemo's COBOL/CICS idioms. Zero hallucinations were found across all 9 documents.

The primary weaknesses are:

1. **Missing programs** (CSUTLDTC.cbl, CBSTM03A.CBL, CBSTM03B.CBL) — omitted from both L2 and L3 levels, creating gaps in the architecture inventory
2. **L2/L3 inconsistency** (COBSWAIT.cbl) — present at L2 but absent from all L3 component groups
3. **Incorrect counts** — CICS (18→19), Batch (10→12), and total (37→40) program counts are wrong at multiple levels

These are remediable issues. The documentation's architectural insights (C4 level decomposition, pattern documentation, VSAM data architecture, extension module identification) are sound and valuable. With the 8 remediation items applied, the score would likely rise to 97-99.

**Recommendation**: Apply remediation manifest before using these documents for migration planning. Priority items are REM-001 through REM-006 (all MAJOR). The recurring absence of `.CBL` (uppercase extension) files across multiple RE outputs (RE-001, RE-003, RE-004) suggests the reverse engineering prompts should be updated to explicitly inventory both `.cbl` and `.CBL` file extensions.
