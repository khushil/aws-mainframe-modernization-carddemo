# VL-003: Context Model Validation Report

**Validation ID**: VL-003
**Date**: 2026-02-06
**Validator**: Claude Opus 4.6
**Target**: RE-003 Context Model Extraction
**Documents Validated**: COMMAREA-SPECIFICATION.md, CONTEXT-MAP.md, NAVIGATION-FLOWS.md
**Status**: Complete

---

## 1. Verdict

**FAIL** — Score: **80.6 / 100**

The RE-003 Context Model documentation is architecturally sound and demonstrates strong understanding of the CardDemo application's bounded contexts, COMMAREA state contract, and navigation patterns. **Zero hallucinations** were detected — all referenced programs, copybooks, and extension modules exist in the source tree.

However, the documentation has several factual accuracy gaps: 5 of 17 BMS mapset/map names are incorrect, the Transaction ID mapping table covers only 3 of 17 TransIDs, and 3 online programs plus 2 batch programs are missing from bounded context assignments. The COMMAREA specification is near-perfect with all 14 fields, 5 groups, and 4 level-88 conditions correctly documented.

---

## 2. Score Breakdown

| # | Category | Weight | Raw Score | Weighted Score | Key Evidence |
|---|----------|--------|-----------|----------------|--------------|
| 1 | Source Reference Accuracy | 35% | 85 | 29.8 | 6/6 COMMAREA refs exact; XCTL line refs correct; 5/17 BMS map names wrong |
| 2 | Factual Accuracy | 25% | 82 | 20.5 | 14/14 fields correct; 3/3 TransIDs correct; 5 BMS errors; data ownership nuances |
| 3 | Completeness | 20% | 62 | 12.4 | 3/17 TransIDs in table; CORPT00C+CBSTM03A/B missing from contexts; 5 PF3 paths missing |
| 4 | Quantitative Accuracy | 10% | 88 | 8.8 | Context count correct; COMMAREA ~155 vs actual 160 bytes; PIC X(07) vs X(7) |
| 5 | Documentation Quality | 10% | 92 | 9.2 | Mermaid diagrams valid; DDD terminology correct; well-structured markdown |
| | **TOTAL** | **100%** | | **80.6** | |

**Verdict: FAIL** (threshold: 100 = PASS; score 80.6 indicates specific gaps requiring remediation)

---

## 3. Critical Findings

**None.**

No hallucinated file names, fabricated copybooks, invented COBOL constructs, or non-existent programs were found. All 3 extension programs (COTRTLIC, COTRTUPC, COPAUS0C) exist in the source tree under `app/app-*/cbl/` directories.

---

## 4. Major Findings

### MAJ-001: BMS Mapset/Map Name Errors (5 of 17 entries)

**Affected Document**: `NAVIGATION-FLOWS.md` lines 377-395 (Screen Flow Mapping table)

Five entries in the BMS Mapset to Program Mapping table contain incorrect mapset and/or map names. All errors occur in programs that use the CCARD-prefixed naming convention rather than the CO-prefixed convention.

| # | Program | Doc Mapset | Doc Map | Actual Mapset | Actual Map | Source |
|---|---------|-----------|---------|---------------|------------|--------|
| 1 | COACTVWC | COACTVW | COACTVWA | COACTVW | **CACTVWA** | `COACTVW.bms:20,25` |
| 2 | COACTUPC | **COACTU1** | **COACTU1A** | **COACTUP** | **CACTUPA** | `COACTUP.bms:20,25` |
| 3 | COCRDLIC | **COCRDL1** | **COCRDL1A** | **COCRDLI** | **CCRDLIA** | `COCRDLI.bms:20,25` |
| 4 | COCRDSLC | **COCRDS1** | **COCRDS1A** | **COCRDSL** | **CCRDSLA** | `COCRDSL.bms:20,25` |
| 5 | COCRDUPC | COCRDUP | **COCRDU1A** | COCRDUP | **CCRDUPA** | `COCRDUP.bms:20,25` |

**Pattern**: The errors consistently substitute numeric suffixes (1) for alphabetic ones (I/L) and use CO-prefixed map names instead of the actual C-prefixed short form (CACTVWA, CACTUPA, CCRDLIA, CCRDSLA, CCRDUPA).

**Impact**: Incorrect BMS names would cause errors in any tooling or migration that relies on these mappings.

### MAJ-002: Transaction ID Table Completeness Gap (3/17 = 17.6%)

**Affected Document**: `NAVIGATION-FLOWS.md` lines 11-15

The Transaction ID Mapping table documents only 3 of 17 verified transaction IDs:

| Documented | Missing |
|-----------|---------|
| CC00 → COSGN00C ✓ | CU00 → COUSR00C |
| CA00 → COADM01C ✓ | CU01 → COUSR01C |
| CM00 → COMEN01C ✓ | CU02 → COUSR02C |
| | CU03 → COUSR03C |
| | CAVW → COACTVWC |
| | CAUP → COACTUPC |
| | CCLI → COCRDLIC |
| | CCDL → COCRDSLC |
| | CCUP → COCRDUPC |
| | CT00 → COTRN00C |
| | CT01 → COTRN01C |
| | CT02 → COTRN02C |
| | CR00 → CORPT00C |
| | CB00 → COBIL00C |

**Source Evidence**: Each TransID is defined in the program's working storage as `WS-TRANID` or `LIT-THISTRANID` VALUE literals. All 14 missing IDs were verified from source.

**Impact**: The table is the primary reference for Transaction ID to program mapping. 82.4% of mappings are absent.

### MAJ-003: Programs Missing from Bounded Context Assignments

**Affected Document**: `CONTEXT-MAP.md`

| Missing Program | Type | Expected Context | Evidence |
|----------------|------|-----------------|----------|
| CORPT00C | Online | Transaction Processing or Reports | Has TransID CR00, is user menu option 9, uses WRITEQ TD |
| CBSTM03A.CBL | Batch | Batch Processing | Exists in `app/cbl/`, batch statement program |
| CBSTM03B.CBL | Batch | Batch Processing | Exists in `app/cbl/`, batch statement program |

**Note**: COBSWAIT.cbl and CSUTLDTC.cbl are correctly absent — they are utility programs with no CICS transactions or VSAM I/O.

### MAJ-004: PF3 Return Paths Missing from Navigation Graph

**Affected Document**: `NAVIGATION-FLOWS.md` lines 34-103

Five programs have PF3 return paths to COMEN01C that are not shown in the Mermaid navigation graph:

| Program | Source Evidence |
|---------|----------------|
| COCRDSLC | `COCRDSLC.cbl:331` — EXEC CICS XCTL PROGRAM(CDEMO-TO-PROGRAM) |
| COCRDUPC | `COCRDUPC.cbl:473` — EXEC CICS XCTL PROGRAM(CDEMO-TO-PROGRAM) |
| COTRN01C | `COTRN01C.cbl:206` — XCTL PROGRAM(CDEMO-TO-PROGRAM) |
| COTRN02C | `COTRN02C.cbl:509` — XCTL PROGRAM(CDEMO-TO-PROGRAM) |
| CORPT00C | `CORPT00C.cbl:549` — XCTL PROGRAM(CDEMO-TO-PROGRAM) |

---

## 5. Minor Findings

### MIN-001: PIC X(7) vs X(07) Formatting Difference

**Document**: `COMMAREA-SPECIFICATION.md` lines 279, 289
**Source**: `COCOM01Y.cpy:43-44`
**Issue**: Documentation uses `PIC X(07)` for CDEMO-LAST-MAP and CDEMO-LAST-MAPSET; source uses `PIC X(7)`. Semantically identical, trivial formatting difference.

### MIN-002: COMMAREA Byte Size Approximation

**Document**: `COMMAREA-SPECIFICATION.md` line 8
**Claim**: "Record Length: ~155 bytes"
**Actual**: 4+8+4+8+8+1+1 + 9+25+25+25 + 11+1 + 16 + 7+7 = **160 bytes**
**Impact**: The tilde (~) indicates approximation, but the gap is 5 bytes (3.1%). The calculation is straightforward enough to be exact.

### MIN-003: USRSEC Ownership Ambiguity in Data Ownership Matrix

**Document**: `CONTEXT-MAP.md` line 299
**Issue**: USRSEC "Owner Context" is listed as "Authentication", but Authentication only READs. User Administration performs full CRUD (READ, WRITE, REWRITE, DELETE).
**Note**: The table's Reader/Writer columns do correctly show User Admin as both reader and writer. The "Owner" designation is debatable — Authentication establishes the security context, but User Administration manages the data.

### MIN-004: CUSTDAT Writer Column Incomplete

**Document**: `CONTEXT-MAP.md` line 302
**Issue**: CUSTDAT Writer column shows only "Batch", but COACTUPC also REWRITEs CUSTDAT (`COACTUPC.cbl:4086`). Account Management should appear in the Writer column.

### MIN-005: Account Management Claims CARDDAT READ

**Document**: `CONTEXT-MAP.md` line 109
**Issue**: Account Management data access table shows `CARDDAT | READ | Associated card info`, but neither COACTVWC nor COACTUPC contains a direct READ on CARDDAT. They read CCXREF and ACCTDAT/CUSTDAT, but card details come through the cross-reference path, not direct CARDDAT access.

### MIN-006: Card Management "READ/WRITE" vs READ/REWRITE

**Document**: `CONTEXT-MAP.md` line 136
**Issue**: Card Management data access shows `CARDDAT | READ/WRITE`, but COCRDUPC performs REWRITE (update existing), not WRITE (new record). The distinction matters for VSAM: WRITE inserts new records; REWRITE updates in place.

---

## 6. Hallucination Inventory

| Suspect Pattern | Search Scope | Result | Verdict |
|----------------|-------------|--------|---------|
| COTRTLIC (extension program) | Source tree | EXISTS at `app/app-transaction-type-db2/cbl/COTRTLIC.cbl` | NOT hallucinated |
| COTRTUPC (extension program) | Source tree | EXISTS at `app/app-transaction-type-db2/cbl/COTRTUPC.cbl` | NOT hallucinated |
| COPAUS0C (extension program) | Source tree | EXISTS at `app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl` | NOT hallucinated |
| CVCAR00Y (fabricated copybook) | All 3 docs | 0 matches | CLEAN |
| COUSR00Y (fabricated copybook) | All 3 docs | 0 matches | CLEAN |
| CDEMO-PGM-NAME (fabricated field) | COMMAREA-SPECIFICATION.md | 0 matches | CLEAN |
| CDEMO-SCR-NAME (fabricated field) | COMMAREA-SPECIFICATION.md | 0 matches | CLEAN |
| CDEMO-MSG-TEXT (fabricated field) | COMMAREA-SPECIFICATION.md | 0 matches | CLEAN |
| Non-existent programs | All 3 docs | All referenced programs exist | CLEAN |

**Hallucination count: 0**

---

## 7. Completeness Gaps

### COMMAREA Specification Completeness

| Element | Documented | Total | Coverage |
|---------|-----------|-------|----------|
| Level-01 records | 1 | 1 | 100% |
| Level-05 groups | 5 | 5 | 100% |
| Level-10 fields | 14 | 14 | 100% |
| Level-88 conditions | 4 | 4 | 100% |
| Source references | 6 | 6 | 100% |

### Transaction ID Completeness

| Category | Count | Coverage |
|----------|-------|----------|
| In explicit table | 3 | 17.6% |
| In navigation graph labels | 3 | 17.6% |
| Total verified from source | 17 | — |
| Missing entirely | 14 | 82.4% |

### Bounded Context Program Coverage

| Category | Count | Coverage |
|----------|-------|----------|
| Online programs assigned to contexts | 14 | 82.4% (14/17) |
| Online programs missing from contexts | 1 (CORPT00C) | — |
| Menu programs (correctly excluded) | 2 (COADM01C, COMEN01C) | — |
| Batch programs in context | 10 | 83.3% (10/12) |
| Batch programs missing | 2 (CBSTM03A/B) | — |
| Utility programs (correctly excluded) | 2 (COBSWAIT, CSUTLDTC) | — |

### BMS Mapset Coverage

| Category | Count | Accuracy |
|----------|-------|----------|
| Correct entries | 12 | 70.6% |
| Incorrect entries | 5 | 29.4% |
| Total entries | 17 | — |

### Navigation Path Coverage

| Category | Documented | Verified in Source |
|----------|-----------|-------------------|
| Menu → Program paths | 17 | 17 (100%) |
| PF3 return paths shown | 10 | 15 verified |
| PF3 return paths missing from graph | 5 | — |

---

## 8. Source Reference Verification Summary

| Source File | Refs Checked | Correct | Minor Issue | Incorrect |
|-------------|-------------|---------|-------------|-----------|
| COCOM01Y.cpy | 6 | 6 | 0 | 0 |
| COSGN00C.cbl | 4 | 4 | 0 | 0 |
| COADM02Y.cpy | 2 | 2 | 0 | 0 |
| COMEN02Y.cpy | 2 | 2 | 0 | 0 |
| COADM01C.cbl | 2 | 2 | 0 | 0 |
| COMEN01C.cbl | 2 | 2 | 0 | 0 |
| BMS mapsets | 17 | 12 | 0 | 5 |
| **Total** | **35** | **30** | **0** | **5** |

**Source Reference Accuracy Rate**: 85.7% (30/35)

---

## 9. Positive Findings

| ID | Finding | Evidence |
|----|---------|----------|
| POS-001 | All 14 COMMAREA fields exactly match source PIC clauses | Verified against `COCOM01Y.cpy:19-45` |
| POS-002 | All 4 level-88 conditions correctly documented with VALUES | `COCOM01Y.cpy:27-31` |
| POS-003 | Zero fabricated fields in COMMAREA specification | Checked for CDEMO-PGM-NAME, CDEMO-SCR-NAME, CDEMO-MSG-TEXT — none found |
| POS-004 | All 3 documented TransIDs are factually correct | CC00/CA00/CM00 verified from source |
| POS-005 | Zero hallucinations across all documents | All programs, copybooks, extensions confirmed to exist |
| POS-006 | 7 bounded contexts are architecturally sound | DDD relationship types correctly applied |
| POS-007 | Extension programs correctly identified (not hallucinated) | COTRTLIC, COTRTUPC, COPAUS0C all exist in extension directories |
| POS-008 | Mermaid diagrams syntactically valid | Context map, navigation graph, state machines all valid |
| POS-009 | Sign-on routing exactly matches source | `COSGN00C.cbl:230-240` — CDEMO-USRTYP-ADMIN → COADM01C, ELSE → COMEN01C |
| POS-010 | Menu structures fully verified | Admin menu (6 options, `COADM02Y.cpy`) and User menu (11 options, `COMEN02Y.cpy`) match |
| POS-011 | XCTL/RETURN TRANSID patterns correctly described | Code examples match actual COBOL patterns |
| POS-012 | Cross-context data access (Bill Payment) correctly identified | COBIL00C writes ACCTDAT + TRANSACT across context boundaries |

---

## 10. Remediation Manifest

| ID | Severity | File | Location | Current | Correct | Source Evidence | Action |
|----|----------|------|----------|---------|---------|----------------|--------|
| REM-001 | MAJOR | NAVIGATION-FLOWS.md | Line 386 | `COACTVW \| COACTVWA` | `COACTVW \| CACTVWA` | `COACTVW.bms:25` | Fix map name |
| REM-002 | MAJOR | NAVIGATION-FLOWS.md | Line 387 | `COACTU1 \| COACTU1A` | `COACTUP \| CACTUPA` | `COACTUP.bms:20,25` | Fix mapset and map names |
| REM-003 | MAJOR | NAVIGATION-FLOWS.md | Line 388 | `COCRDL1 \| COCRDL1A` | `COCRDLI \| CCRDLIA` | `COCRDLI.bms:20,25` | Fix mapset and map names |
| REM-004 | MAJOR | NAVIGATION-FLOWS.md | Line 389 | `COCRDS1 \| COCRDS1A` | `COCRDSL \| CCRDSLA` | `COCRDSL.bms:20,25` | Fix mapset and map names |
| REM-005 | MAJOR | NAVIGATION-FLOWS.md | Line 390 | `COCRDUP \| COCRDU1A` | `COCRDUP \| CCRDUPA` | `COCRDUP.bms:20,25` | Fix map name |
| REM-006 | MAJOR | NAVIGATION-FLOWS.md | Lines 11-15 | 3 TransIDs | 17 TransIDs | All `WS-TRANID`/`LIT-THISTRANID` values | Add 14 missing TransID rows |
| REM-007 | MAJOR | CONTEXT-MAP.md | Section 7 | 10 batch programs | 12 batch programs | `ls app/cbl/CB*.CBL` | Add CBSTM03A.CBL, CBSTM03B.CBL |
| REM-008 | MAJOR | CONTEXT-MAP.md | All contexts | CORPT00C absent | Assign to context | `CORPT00C.cbl` TransID CR00 | Add to Transaction Processing or new Reports context |
| REM-009 | MAJOR | NAVIGATION-FLOWS.md | Lines 34-103 | 10 PF3 paths | 15 PF3 paths | XCTL source in COCRDSLC/COCRDUPC/COTRN01C/COTRN02C/CORPT00C | Add 5 missing PF3 return edges |
| REM-010 | MINOR | COMMAREA-SPECIFICATION.md | Lines 279, 289 | `PIC X(07)` | `PIC X(7)` | `COCOM01Y.cpy:43-44` | Match source format |
| REM-011 | MINOR | COMMAREA-SPECIFICATION.md | Line 8 | `~155 bytes` | `160 bytes` | Sum of all field lengths | Correct byte count |
| REM-012 | MINOR | CONTEXT-MAP.md | Line 299 | Owner: Authentication | Owner: User Administration | USRSEC CRUD by COUSR00C-03C | Update owner designation |
| REM-013 | MINOR | CONTEXT-MAP.md | Line 302 | Writer: Batch | Writer: Account Mgmt, Batch | `COACTUPC.cbl:4086` REWRITE CUSTDAT | Add Account Mgmt to writer |
| REM-014 | MINOR | CONTEXT-MAP.md | Line 109 | CARDDAT READ | Remove CARDDAT row | No direct CARDDAT access in COACTVWC/COACTUPC | Remove or annotate |
| REM-015 | MINOR | CONTEXT-MAP.md | Line 136 | CARDDAT READ/WRITE | CARDDAT READ/REWRITE | `COCRDUPC.cbl:1478` REWRITE not WRITE | Fix VSAM operation |

---

## 11. Cross-References to Prior Validations

### VL-001 (Domain Model) — Score: 88/100

| VL-001 Finding | VL-003 Relevance |
|----------------|-----------------|
| MAJ-002: CBACT02C-04C batch programs missing | Same gap: CBSTM03A/B also missing from VL-003 Batch context |
| MAJ-003: COPAUS0C/Pending Auth missing | VL-003 correctly includes COPAUS0C in navigation graph |
| MIN-005: Program counts discrepancy (29 vs 39) | VL-003 correctly works with actual 29+2=31 programs |

### VL-002 (Data Model) — Score: 96.8/100

| VL-002 Finding | VL-003 Relevance |
|----------------|-----------------|
| MINOR-003: DISCLOSURE_GROUP→TRAN_CAT_BAL indirect relationship | Not relevant to VL-003 — Context Map doesn't reference this relationship |
| INFO-001: EXPIRAION typo preserved | Not relevant to VL-003 — COMMAREA does not contain date fields with this typo |

---

## 12. Verification Methodology

### Source Files Read

**Copybooks (3 files)**:
- `app/cpy/COCOM01Y.cpy` — COMMAREA definition (field-by-field)
- `app/cpy/COADM02Y.cpy` — Admin menu options (full read)
- `app/cpy/COMEN02Y.cpy` — User menu options (full read)

**COBOL Programs (17 files verified)**:
- All 17 online programs grep'd for WS-TRANID/LIT-THISTRANID VALUE definitions
- All 17 online programs grep'd for EXEC CICS READ/WRITE/REWRITE/DELETE/STARTBR patterns
- COSGN00C.cbl lines 220-249 — sign-on routing verification
- COADM01C.cbl — admin menu XCTL routing
- COMEN01C.cbl — user menu XCTL routing
- All PF3 return XCTL statements verified

**BMS Mapsets (17 files)**:
- All 17 `.bms` files verified for DFHMSD mapset names and DFHMDI map names

### Verification Approach

1. **Source Reference Accuracy** (35% weight): Read each cited source file/line; compared COMMAREA fields, XCTL patterns, menu arrays, and BMS names against documentation claims
2. **Factual Accuracy** (25% weight): Verified all PIC clauses, 88-level VALUES, TransID mappings, data access operations (READ/WRITE/REWRITE/DELETE), and BMS mapset/map names
3. **Completeness** (20% weight): Enumerated all 17 TransIDs, all 31 COBOL programs (29 `.cbl` + 2 `.CBL`), all 17 BMS mapsets, and all PF3 return paths; checked coverage against documentation
4. **Quantitative Accuracy** (10% weight): Verified context count (7), COMMAREA byte size (160 vs ~155), PIC formatting (X(7) vs X(07))
5. **Documentation Quality** (10% weight): Validated Mermaid diagram syntax, DDD terminology usage, markdown formatting, cross-document consistency

### Hallucination Vectors Tested
- Non-existent copybook names (CVCAR00Y, COUSR00Y)
- Fabricated COMMAREA fields (CDEMO-PGM-NAME, CDEMO-SCR-NAME, CDEMO-MSG-TEXT)
- Extension programs as potential hallucinations (COTRTLIC, COTRTUPC, COPAUS0C — all confirmed to exist)
- BMS mapset names (5 found incorrect — factual errors, not hallucinations)

---

## 13. Work Artifacts

| Artifact | Path |
|----------|------|
| Progress Tracking | `.work/reverse-engineering/validation/vl-003/progress.yaml` |
| Document Inventory | `.work/reverse-engineering/validation/vl-003/document-inventory.yaml` |
| COMMAREA Verification | `.work/reverse-engineering/validation/vl-003/commarea-verification.yaml` |
| Transaction ID Verification | `.work/reverse-engineering/validation/vl-003/transaction-verification.yaml` |
| Navigation Verification | `.work/reverse-engineering/validation/vl-003/navigation-verification.yaml` |
| Context Verification | `.work/reverse-engineering/validation/vl-003/context-verification.yaml` |

---

## 14. Conclusion

The RE-003 Context Model documentation demonstrates strong architectural analysis with zero hallucinations and correctly identified bounded contexts, DDD relationships, and COMMAREA state contract. The COMMAREA specification is particularly strong with 100% field accuracy.

The primary weaknesses are:
1. **BMS mapset errors** (5 incorrect names) — factual inaccuracies that would impact downstream tooling
2. **TransID table incompleteness** (3/17) — renders the table unreliable as a reference
3. **Missing program assignments** (CORPT00C, CBSTM03A/B) — gaps in bounded context coverage

These are remediable issues. The documentation's architectural insights (context boundaries, DDD relationships, modernization recommendations) are sound and valuable. With the 15 remediation items applied, the score would likely rise to 92-95.

**Recommendation**: Apply remediation manifest before using these documents for migration planning. Priority items are REM-001 through REM-009 (all MAJOR).
