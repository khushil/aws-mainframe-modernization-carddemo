# VL-006: Batch Workflow Validation Report

**Validation ID**: VL-006
**Date**: 2026-02-06
**Validator**: Claude Opus 4.6
**Target**: RE-006 Batch Workflow Analysis
**Document Validated**: `docs/reverse-engineering/05-specialized/BATCH-WORKFLOWS.md`
**Status**: Complete

---

## 1. Verdict

**FAIL** — Score: **76.6 / 100** (revised from 73.8 after MNTTRDB2 reclassification)

The RE-006 Batch Workflow documentation demonstrates strong factual accuracy for the jobs it covers — EXEC PGM= values, VSAM key specifications, record sizes, DD statement DSN values, and CICS file mappings are overwhelmingly correct when verified against actual JCL source. The program-to-job mapping in Appendix A is accurate for all 10 documented programs, and the run_full_batch.sh workflow in section 3.3 matches the actual script exactly.

However, the documentation has significant completeness gaps. Six of 38 JCL files (15.8%) are entirely missing, including the CREASTMT.JCL statement generation workflow which uses two undocumented COBOL programs (CBSTM03A.CBL, CBSTM03B.CBL). The headline counts are wrong — the document claims 33 JCL files and 10 batch programs when the actual counts are 38 and 12. Two of three shell scripts lack dedicated documentation. A phantom node (MNTTRDB2) appears in the WEEKLY Control-M workflow with no corresponding JCL in the repository.

---

## 2. Score Breakdown

| # | Category | Weight | Raw Score | Weighted Score | Key Evidence |
|---|----------|--------|-----------|----------------|--------------|
| 1 | Source Reference Accuracy | 35% | 90 | 31.5 | All 15 documented EXEC PGM= correct; MNTTRDB2 reclassified as valid (was -8); WAITSTEP PARM vs SYSIN minor inaccuracy |
| 2 | Factual Accuracy | 25% | 85 | 21.3 | VSAM specs, DD statements, CICS mappings all verified correct for documented jobs |
| 3 | Completeness | 20% | 55 | 11.0 | 6/38 JCL files missing (15.8%); 2/12 batch programs missing; 2/3 scripts undocumented |
| 4 | Quantitative Accuracy | 10% | 40 | 4.0 | JCL count 33 vs 38 (wrong); batch count 10 vs 12 (wrong) |
| 5 | Documentation Quality | 10% | 88 | 8.8 | Well-structured; valid Mermaid diagrams; good appendices; minor formatting issues |
| | **TOTAL** | **100%** | | **76.6** | |

**Verdict: FAIL** (threshold: 100 = PASS; score 76.6 indicates significant gaps requiring remediation)

**Score Revision (2026-02-06):** Original score was 73.8. After reclassifying HAL-001 (MNTTRDB2) from phantom to valid, Source Reference Accuracy improved from 82 to 90 (+8 points × 35% weight = +2.8 weighted). Revised total: 76.6.

---

## 3. Critical Findings

### CRT-001: Six JCL Files Completely Missing from Documentation

**Affected Section**: All sections (inventory, DAG, appendices)
**Source Evidence**: 38 files in `app/jcl/`, only 32 documented

Six JCL files are entirely absent from the documentation:

| Missing File | Extension | Purpose | Programs Used |
|-------------|-----------|---------|---------------|
| CREASTMT.JCL | .JCL (uppercase) | Statement generation per card from XREF | IDCAMS, SORT, IEFBR14, CBSTM03A |
| FTPJCL.JCL | .JCL (uppercase) | FTP file transfer to/from mainframe | FTP |
| INTRDRJ1.JCL | .JCL (uppercase) | Internal reader trigger — copies FTP file, triggers INTRDRJ2 | IDCAMS, IEBGENER |
| INTRDRJ2.JCL | .JCL (uppercase) | Internal reader chained job — secondary copy | IDCAMS |
| TXT2PDF1.JCL | .JCL (uppercase) | Convert text statement to PDF via TXT2PDF REXX | IKJEFT1B |
| DEFCUST.jcl | .jcl (lowercase) | Define customer VSAM with alternate schema (AWS.CCDA) | IDCAMS |

All five uppercase-extension `.JCL` files are missing. CREASTMT.JCL is the most critical omission because it represents an entire business workflow (statement generation) that chains into TXT2PDF1.JCL for PDF output.

DEFCUST.jcl uses a different DSN schema (`AWS.CCDA.CUSTDATA` vs `AWS.M2.CARDDEMO.CUSTDATA`) and different key specification (KEYS(10 0) vs KEYS(9 0) in CUSTFILE.jcl), suggesting an alternate environment configuration that is entirely undocumented.

**Impact**: Modernization planning would miss the statement generation pipeline and FTP integration patterns.

### CRT-002: Two Batch COBOL Programs Completely Missing

**Affected Section**: Appendix A (Program-to-Job Mapping), Section 8 (Metadata)
**Source Evidence**: `app/cbl/CBSTM03A.CBL` and `app/cbl/CBSTM03B.CBL` exist in repository

| Missing Program | Extension | Called From | Purpose |
|----------------|-----------|-------------|---------|
| CBSTM03A.CBL | .CBL (uppercase) | CREASTMT.JCL STEP040 | Statement generation main program |
| CBSTM03B.CBL | .CBL (uppercase) | Called by CBSTM03A | Statement generation subroutine |

These programs implement a complete statement generation workflow that produces both text (`STATEMNT.PS`) and HTML (`STATEMNT.HTML`) output by reading TRANSACT, CARDXREF, ACCTDATA, and CUSTDATA files. CBSTM03B is a subroutine called by CBSTM03A (the CREASTMT.JCL comment says "DEMONSTRATES CALLED SUBROUTINE").

Additionally, CBTRN01C.cbl exists in `app/cbl/` but is not referenced by any JCL job and is absent from Appendix A's program-to-job mapping table.

**Impact**: The documentation claims 10 batch COBOL programs; the actual count is 12. Migration scope is underestimated.

### CRT-003: Headline Counts Are Wrong

**Affected Sections**: Executive Summary (lines 12, 18, 19), Metadata (line 893)
**Source Evidence**: `ls app/jcl/ | wc -l` = 38; `ls app/cbl/CB* | wc -l` = 12

| Metric | Documented Value | Actual Value | Delta |
|--------|-----------------|--------------|-------|
| Total JCL Jobs | 33 | 38 | -5 (13.2% undercount) |
| Batch COBOL Programs | 10 | 12 | -2 (16.7% undercount) |

These counts appear in three locations:
- Line 12: "33 JCL batch workflows"
- Line 18: "Total JCL Jobs = 33"
- Line 19: "Batch COBOL Programs = 10"
- Line 893: "33 JCL, 10 COBOL"

**Impact**: Any capacity planning or migration scoping based on these counts would be materially underestimated.

---

## 4. Major Findings

### MAJ-001: ~~Phantom Node~~ MNTTRDB2 in WEEKLY Control-M Workflow — RECLASSIFIED

**Affected Section**: 3.1 (WEEKLY-DisclosureGroupsRefresh, line 458)
**Source Evidence**: `app/app-transaction-type-db2/jcl/MNTTRDB2.jcl` **exists** in extension directory

**CORRECTION (2026-02-06):** The original validation searched only `app/jcl/` and did not check extension directories. MNTTRDB2.jcl exists at `app/app-transaction-type-db2/jcl/MNTTRDB2.jcl` and runs `COBTUPDT` to maintain the DB2 transaction type table.

The WEEKLY-DisclosureGroupsRefresh Control-M workflow diagram includes a node `MNTTRDB2` as the first step:
```
MNTTRDB2 -> CLOSEFIL -> DISCGRP -> WAITSTEP -> OPENFIL
```

This is a **valid reference** to a real JCL job in the extension directory. The batch documentation was **more correct** than the validation gave it credit for.

**Status**: Reclassified from "phantom hallucination" to "valid — found in extension directory"
**Impact on score**: This was a Major finding worth -8 points on Source Reference Accuracy. Removing this penalty improves the score.

### MAJ-002: MONTHLY Control-M Workflow Missing TRANBKP and TRANIDX Steps

**Affected Section**: 3.1 (MONTHLY-InterestCalculation, lines 464-472)
**Source Evidence**: `scripts/run_interest_calc.sh` shows the actual sequence

| Step | Doc Control-M | Actual Script |
|------|--------------|---------------|
| 1 | CLOSEFIL | CLOSEFIL |
| 2 | INTCALC | INTCALC |
| 3 | — | **TRANBKP** (missing from doc) |
| 4 | COMBTRAN | COMBTRAN |
| 5 | — | **TRANIDX** (missing from doc) |
| 6 | WAITSTEP | — |
| 7 | OPENFIL | OPENFIL |

COMBTRAN reads `TRANSACT.BKUP(0)` which is produced by TRANBKP. Without TRANBKP, COMBTRAN would read stale or nonexistent backup data. TRANIDX rebuilds the AIX after COMBTRAN reloads the VSAM. Both are functionally necessary.

**Impact**: The documented Control-M workflow for interest calculation is incomplete and would fail if executed as-is.

### MAJ-003: run_posting.sh and run_interest_calc.sh Undocumented

**Affected Section**: 3.3 (only documents run_full_batch.sh)
**Source Evidence**: `scripts/run_posting.sh` and `scripts/run_interest_calc.sh`

Only `run_full_batch.sh` has a dedicated documentation section (3.3). The other two scripts are referenced in the document metadata ("3 shell scripts") but have no workflow tables or descriptions.

**run_posting.sh** sequence: CLOSEFIL → ACCTFILE → TCATBALF → TRANBKP → POSTTRAN → TRANIDX → OPENFIL

**run_interest_calc.sh** sequence: CLOSEFIL → INTCALC → TRANBKP → COMBTRAN → TRANIDX → OPENFIL

**Impact**: Operators cannot reconstruct posting-only or interest-only workflows from the documentation alone.

### MAJ-004: DEFCUST.jcl Uses Alternate DSN Schema

**Affected Section**: Not documented anywhere
**Source Evidence**: `app/jcl/DEFCUST.jcl` lines 25, 35

DEFCUST.jcl defines a customer VSAM cluster using `AWS.CCDA.CUSTDATA.CLUSTER` (and later `AWS.CUSTDATA.CLUSTER`) instead of the standard `AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS` used by CUSTFILE.jcl. It also uses KEYS(10 0) instead of KEYS(9 0).

This suggests an alternate environment configuration or a different version of the customer file layout. The documentation makes no mention of this alternate schema or the key length discrepancy.

**Impact**: Data migration planning would not account for the alternate customer file schema.

---

## 5. Minor Findings

### MIN-001: WAITSTEP PARM Delivery Mechanism

**Affected Section**: 2.8 (WAITSTEP, line 431)
**Source Evidence**: `app/jcl/WAITSTEP.jcl` — SYSIN DD with inline value

The documentation says `PARM: Centiseconds (00003600 = 36 seconds)` implying a JCL PARM= on the EXEC statement. The actual JCL delivers the value via `SYSIN DD *` with inline data `00003600`, not via EXEC PARM=. The value and interpretation are correct; only the delivery mechanism description is inaccurate.

### MIN-002: TRANCATG.jcl in DAG Data Refresh

**Affected Section**: 3.2 (Complete Job Dependency DAG)

TRANCATG.jcl is a VSAM definition job listed in section 2.1 but is not shown as a node in the "Data Refresh" subgraph of the complete DAG (section 3.2), even though similar VSAM definition jobs (TRANTYPE, DISCGRP, TCATBALF) are included. TRANCATG is also not in the run_full_batch.sh script, so its omission from the DAG is arguably correct for the daily workflow but inconsistent with other reference data jobs.

### MIN-003: Extension Case Sensitivity Not Documented

**Affected Section**: None (no documentation exists)

Five JCL files use uppercase `.JCL` extensions and two batch programs use uppercase `.CBL` extensions. The documentation makes no mention of this variance. All uppercase-extension files are also the ones missing from documentation, suggesting the inventory process may have filtered on `.jcl` lowercase only.

### MIN-004: REPTFILE Duplicate GDG Definition

**Affected Section**: 2.2 (REPTFILE)

REPTFILE.jcl defines GDG `AWS.M2.CARDDEMO.TRANREPT` with LIMIT(10). DEFGDGB.jcl also defines the same GDG with LIMIT(5). The documentation notes REPTFILE with limit 10 and DEFGDGB with limit 5, but does not call out the conflict. Whichever job runs last would set the effective limit.

### MIN-005: CBADMCDJ Transaction Count

**Affected Section**: 2.8 (CBADMCDJ, line 423)

The documentation says "5 Transactions" are defined. The actual JCL defines: CCDM, CCT1, CCT2, CCT3, CCT4 = 5 transactions. However, the CSD also defines CC00 via `DEFINE PROGRAM(COSGN00C) ... TRANSID(CC00)`, which is a TRANSID attribute on a program rather than a standalone DEFINE TRANSACTION. The count of 5 standalone transactions is technically correct.

### MIN-006: COND=(4,LT) Interpretation

**Affected Section**: Appendix D (line 882-883)

The document states: "Skip STEP10 if any prior step has RC > 4". The JCL condition `COND=(4,LT)` means "skip this step if 4 is less than the return code of any prior step", which is equivalent to "skip if any prior RC > 4". The interpretation is correct.

---

## 6. Hallucination Inventory

| ID | Type | Location | Hallucinated Content | Reality |
|----|------|----------|---------------------|---------|
| ~~HAL-001~~ | ~~Phantom node~~ **RECLASSIFIED** | Section 3.1, line 458 | MNTTRDB2 in WEEKLY workflow | **VALID**: `app/app-transaction-type-db2/jcl/MNTTRDB2.jcl` exists in extension directory |
| HAL-002 | Inflated count | Lines 12, 18 | "33 JCL batch workflows" | Actual: 38 JCL files |
| HAL-003 | Inflated count | Lines 19, 893 | "10 Batch COBOL Programs" | Actual: 12 batch programs |

**Total hallucinations: 2** (~~1 phantom reference reclassified as valid~~, 2 quantitative fabrications)

**Note (2026-02-06):** HAL-001 was reclassified after discovering the validator did not search extension directories (`app/app-*/`). MNTTRDB2.jcl exists at `app/app-transaction-type-db2/jcl/MNTTRDB2.jcl`. The original hallucination count of 3 has been reduced to 2.

---

## 7. Completeness Tables

### 7.1 JCL File Coverage

| # | JCL File | Extension | In Doc? | Section | Notes |
|---|----------|-----------|---------|---------|-------|
| 1 | ACCTFILE.jcl | .jcl | Yes | 2.1 | Correctly documented |
| 2 | CARDFILE.jcl | .jcl | Yes | 2.1 | Steps and AIX correct |
| 3 | CUSTFILE.jcl | .jcl | Yes | 2.1 | Key and record size correct |
| 4 | XREFFILE.jcl | .jcl | Yes | 2.1 | AIX spec correct |
| 5 | TRANFILE.jcl | .jcl | Yes | 2.1 | AIX spec correct |
| 6 | TRANTYPE.jcl | .jcl | Yes | 2.1 | Key and record size correct |
| 7 | TRANCATG.jcl | .jcl | Yes | 2.1 | Key and record size correct |
| 8 | TCATBALF.jcl | .jcl | Yes | 2.1 | Key and record size correct |
| 9 | DISCGRP.jcl | .jcl | Yes | 2.1 | Key and record size correct |
| 10 | DUSRSECJ.jcl | .jcl | Yes | 2.1 | Inline users correct |
| 11 | DEFGDGB.jcl | .jcl | Yes | 2.2 | 6 GDG bases correct |
| 12 | DEFGDGD.jcl | .jcl | Yes | 2.2 | 3 GDG bases correct |
| 13 | REPTFILE.jcl | .jcl | Yes | 2.2 | GDG limit=10 correct |
| 14 | DALYREJS.jcl | .jcl | Yes | 2.2 | GDG limit=5 correct |
| 15 | POSTTRAN.jcl | .jcl | Yes | 2.3 | PGM=CBTRN02C correct |
| 16 | INTCALC.jcl | .jcl | Yes | 2.3 | PGM=CBACT04C correct |
| 17 | COMBTRAN.jcl | .jcl | Yes | 2.3 | SORT+IDCAMS correct |
| 18 | TRANBKP.jcl | .jcl | Yes | 2.3 | REPROC proc correct |
| 19 | TRANIDX.jcl | .jcl | Yes | 2.3 | Steps 20/25/30 correct |
| 20 | TRANREPT.jcl | .jcl | Yes | 2.4 | REPROC+SORT+CBTRN03C correct |
| 21 | PRTCATBL.jcl | .jcl | Yes | 2.4 | REPROC+SORT correct |
| 22 | CLOSEFIL.jcl | .jcl | Yes | 2.5 | 5 CICS files correct |
| 23 | OPENFIL.jcl | .jcl | Yes | 2.5 | 5 CICS files correct |
| 24 | READACCT.jcl | .jcl | Yes | 2.6 | 3 output formats correct |
| 25 | READCARD.jcl | .jcl | Yes | 2.6 | PGM=CBACT02C correct |
| 26 | READCUST.jcl | .jcl | Yes | 2.6 | PGM=CBCUS01C correct |
| 27 | READXREF.jcl | .jcl | Yes | 2.6 | PGM=CBACT03C correct |
| 28 | CBEXPORT.jcl | .jcl | Yes | 2.7 | 5 source files correct |
| 29 | CBIMPORT.jcl | .jcl | Yes | 2.7 | 5 targets correct |
| 30 | CBADMCDJ.jcl | .jcl | Yes | 2.8 | PGM=DFHCSDUP correct |
| 31 | WAITSTEP.jcl | .jcl | Yes | 2.8 | PGM=COBSWAIT correct |
| 32 | ESDSRRDS.jcl | .jcl | Yes | 2.8 | ESDS+RRDS correct |
| 33 | CREASTMT.JCL | **.JCL** | **No** | — | **MISSING: Statement generation** |
| 34 | FTPJCL.JCL | **.JCL** | **No** | — | **MISSING: FTP file transfer** |
| 35 | INTRDRJ1.JCL | **.JCL** | **No** | — | **MISSING: Internal reader trigger** |
| 36 | INTRDRJ2.JCL | **.JCL** | **No** | — | **MISSING: Internal reader chain** |
| 37 | TXT2PDF1.JCL | **.JCL** | **No** | — | **MISSING: Text to PDF conversion** |
| 38 | DEFCUST.jcl | .jcl | **No** | — | **MISSING: Alternate customer VSAM** |

**Coverage**: 32/38 = 84.2%

### 7.2 Batch Program Coverage

| # | Program | Extension | In Doc? | Job Mapping | Notes |
|---|---------|-----------|---------|-------------|-------|
| 1 | CBACT01C.cbl | .cbl | Yes | READACCT | Correct |
| 2 | CBACT02C.cbl | .cbl | Yes | READCARD | Correct |
| 3 | CBACT03C.cbl | .cbl | Yes | READXREF | Correct |
| 4 | CBACT04C.cbl | .cbl | Yes | INTCALC | Correct |
| 5 | CBCUS01C.cbl | .cbl | Yes | READCUST | Correct |
| 6 | CBTRN01C.cbl | .cbl | Partial | No JCL ref | Not in Appendix A |
| 7 | CBTRN02C.cbl | .cbl | Yes | POSTTRAN | Correct |
| 8 | CBTRN03C.cbl | .cbl | Yes | TRANREPT | Correct |
| 9 | CBEXPORT.cbl | .cbl | Yes | CBEXPORT | Correct |
| 10 | CBIMPORT.cbl | .cbl | Yes | CBIMPORT | Correct |
| 11 | CBSTM03A.CBL | **.CBL** | **No** | CREASTMT | **MISSING** |
| 12 | CBSTM03B.CBL | **.CBL** | **No** | (subroutine) | **MISSING** |

**Coverage**: 10/12 = 83.3%

### 7.3 Script Coverage

| # | Script | In Doc? | Section | Notes |
|---|--------|---------|---------|-------|
| 1 | run_full_batch.sh | Yes | 3.3 | All phases match exactly |
| 2 | run_interest_calc.sh | **No** | — | **MISSING: Interest-only workflow** |
| 3 | run_posting.sh | **No** | — | **MISSING: Posting-only workflow** |

**Coverage**: 1/3 = 33.3%

---

## 8. Verification Evidence

### 8.1 EXEC PGM= Verification (All 15 Documented Programs)

| Job | Documented PGM | Actual PGM | Step | Result |
|-----|---------------|------------|------|--------|
| POSTTRAN | CBTRN02C | CBTRN02C | STEP15 | PASS |
| INTCALC | CBACT04C | CBACT04C | STEP15 | PASS |
| COMBTRAN | SORT | SORT | STEP05R | PASS |
| COMBTRAN | IDCAMS | IDCAMS | STEP10 | PASS |
| TRANBKP | REPROC (proc) | PROC=REPROC | STEP05R | PASS |
| TRANBKP | IDCAMS | IDCAMS | STEP05, STEP10 | PASS |
| TRANIDX | IDCAMS | IDCAMS | STEP20-30 | PASS |
| TRANREPT | REPROC | PROC=REPROC | STEP05R | PASS |
| TRANREPT | SORT | SORT | STEP05R | PASS |
| TRANREPT | CBTRN03C | CBTRN03C | STEP10R | PASS |
| PRTCATBL | REPROC | PROC=REPROC | STEP05R | PASS |
| PRTCATBL | SORT | SORT | STEP10R | PASS |
| READACCT | CBACT01C | CBACT01C | STEP05 | PASS |
| READCARD | CBACT02C | CBACT02C | STEP05 | PASS |
| READCUST | CBCUS01C | CBCUS01C | STEP05 | PASS |
| READXREF | CBACT03C | CBACT03C | STEP05 | PASS |
| CBEXPORT | IDCAMS, CBEXPORT | IDCAMS, CBEXPORT | STEP01, STEP02 | PASS |
| CBIMPORT | CBIMPORT | CBIMPORT | STEP01 | PASS |
| CBADMCDJ | DFHCSDUP | DFHCSDUP | STEP1 | PASS |
| WAITSTEP | COBSWAIT | COBSWAIT | WAIT | PASS |
| CLOSEFIL | SDSF | SDSF | CLCIFIL | PASS |
| OPENFIL | SDSF | SDSF | OPCIFIL | PASS |

**Result: 22/22 = 100% accuracy for documented programs**

### 8.2 VSAM Specification Verification (Appendix B)

| Dataset Pattern | Doc Key | Actual Key | Doc RecSize | Actual RecSize | Result |
|----------------|---------|------------|-------------|----------------|--------|
| *.ACCTDATA.VSAM.KSDS | 11,0 | 11,0 | 300 | 300 | PASS |
| *.CARDDATA.VSAM.KSDS | 16,0 / 11,16 | 16,0 / 11,16 | 150 | 150 | PASS |
| *.CUSTDATA.VSAM.KSDS | 9,0 | 9,0 | 500 | 500 | PASS |
| *.CARDXREF.VSAM.KSDS | 16,0 / 11,25 | 16,0 / 11,25 | 50 | 50 | PASS |
| *.TRANSACT.VSAM.KSDS | 16,0 / 26,304 | 16,0 / 26,304 | 350 | 350 | PASS |
| *.TRANTYPE.VSAM.KSDS | 2,0 | 2,0 | 60 | 60 | PASS |
| *.TRANCATG.VSAM.KSDS | 6,0 | 6,0 | 60 | 60 | PASS |
| *.TCATBALF.VSAM.KSDS | 17,0 | 17,0 | 50 | 50 | PASS |
| *.DISCGRP.VSAM.KSDS | 16,0 | 16,0 | 50 | 50 | PASS |
| *.USRSEC.VSAM.KSDS | 8,0 | 8,0 | 80 | 80 | PASS |
| *.EXPORT.DATA | 4,28 | 4,28 | 500 | 500 | PASS |

**Result: 11/11 = 100% accuracy**

### 8.3 CICS File Mapping Verification (Appendix C)

| CICS Name | Documented DSN | Verified Via | Result |
|-----------|---------------|--------------|--------|
| TRANSACT | AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS | CLOSEFIL/OPENFIL | PASS |
| CCXREF | AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS | CLOSEFIL/OPENFIL | PASS |
| ACCTDAT | AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS | CLOSEFIL/OPENFIL | PASS |
| CXACAIX | AWS.M2.CARDDEMO.CARDXREF.VSAM.AIX.PATH | CLOSEFIL/OPENFIL | PASS |
| USRSEC | AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS | CLOSEFIL/OPENFIL | PASS |
| CARDDAT | AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS | CARDFILE CLCIFIL/OPCIFIL | PASS |
| CARDAIX | AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX.PATH | CARDFILE CLCIFIL/OPCIFIL | PASS |
| CUSTDAT | AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS | CUSTFILE CLCIFIL/OPCIFIL | PASS |

**Result: 8/8 = 100% accuracy**

---

## 9. Remediation Manifest

| Priority | ID | Action | Affected Section(s) |
|----------|----|--------|-------------------|
| P0 | CRT-001 | Add documentation for 6 missing JCL files (CREASTMT, FTPJCL, INTRDRJ1, INTRDRJ2, TXT2PDF1, DEFCUST) | 2.1, 2.8, new sections |
| P0 | CRT-002 | Add CBSTM03A.CBL and CBSTM03B.CBL to program inventory and Appendix A | Appendix A, new section |
| P0 | CRT-003 | Correct JCL count from 33 to 38 and batch program count from 10 to 12 | 1 (Executive Summary), 8 (Metadata) |
| ~~P1~~ | ~~MAJ-001~~ | ~~Remove phantom MNTTRDB2 node~~ **RESOLVED**: MNTTRDB2.jcl exists in extension directory | 3.1 |
| P1 | MAJ-002 | Add TRANBKP and TRANIDX to MONTHLY-InterestCalculation Control-M workflow | 3.1 |
| P1 | MAJ-003 | Add dedicated sections for run_posting.sh and run_interest_calc.sh workflows | 3.3 (expand) |
| P1 | MAJ-004 | Document DEFCUST.jcl alternate DSN schema and key spec difference | New subsection |
| P2 | MIN-001 | Correct WAITSTEP parameter delivery from "PARM" to "SYSIN DD" | 2.8 |
| P2 | MIN-002 | Add TRANCATG to DAG Data Refresh subgraph or note its exclusion | 3.2 |
| P2 | MIN-003 | Add note about .JCL/.CBL uppercase extension files | 1 or 2 |
| P2 | MIN-004 | Note REPTFILE/DEFGDGB GDG limit conflict (10 vs 5) | 2.2 |
| P2 | MIN-005 | Add CBTRN01C.cbl to Appendix A with "(no JCL reference)" notation | Appendix A |

---

## 10. Methodology

### Source Files Analyzed

- **JCL files**: All 38 files in `app/jcl/` (both .jcl and .JCL extensions)
- **Batch programs**: All 12 files matching `app/cbl/CB*` (both .cbl and .CBL extensions)
- **Scripts**: All 3 files: `scripts/run_full_batch.sh`, `scripts/run_interest_calc.sh`, `scripts/run_posting.sh`
- **Target document**: `docs/reverse-engineering/05-specialized/BATCH-WORKFLOWS.md` (900 lines)

### Verification Procedures

1. **Inventory completeness**: Compared glob of `app/jcl/*` (38 files) against all job names mentioned in documentation
2. **EXEC PGM= verification**: For each documented program reference, read the actual JCL and confirmed the PGM= value on the EXEC statement
3. **DD statement verification**: For 22 JCL files, verified DD names, DSN values, DISP parameters, and DCB attributes against documentation claims
4. **VSAM specification verification**: Cross-referenced all KEYS() and RECORDSIZE() values in DEFINE CLUSTER statements against Appendix B
5. **CICS mapping verification**: Verified all 8 CICS file-to-DSN mappings in Appendix C against CLOSEFIL/OPENFIL and embedded CLCIFIL/OPCIFIL steps
6. **DAG node verification**: Confirmed each node in section 3.2 DAG exists as a real JCL file; identified phantom nodes
7. **DAG edge verification**: For each edge, confirmed the data dependency exists (output of source is input of target)
8. **Script verification**: Compared documented phases in section 3.3 against actual script content line-by-line

### Working Artifacts

| Artifact | Location |
|----------|----------|
| Progress tracking | `.work/reverse-engineering/validation/vl-006/progress.yaml` |
| Inventory check | `.work/reverse-engineering/validation/vl-006/inventory-check.yaml` |
| EXEC PGM check | `.work/reverse-engineering/validation/vl-006/exec-pgm-check.yaml` |
| DD statement check | `.work/reverse-engineering/validation/vl-006/dd-statement-check.yaml` |
| Dependency DAG check | `.work/reverse-engineering/validation/vl-006/dependency-dag-check.yaml` |
| Script check | `.work/reverse-engineering/validation/vl-006/script-check.yaml` |

---

## 11. Score Calculation Detail

### Source Reference Accuracy (Weight: 35%, Score: 82)

**Positive factors**:
- All 22 documented EXEC PGM= values verified correct (100%)
- All VSAM DEFINE CLUSTER specs match (100%)
- All CICS file mappings match (100%)
- SORT control statements verified for COMBTRAN, TRANREPT, PRTCATBL

**Negative factors**:
- ~~Phantom MNTTRDB2 node in Control-M workflow (-8 points)~~ RECLASSIFIED: valid reference to extension directory
- MONTHLY workflow missing TRANBKP/TRANIDX steps (-5 points)
- WAITSTEP PARM vs SYSIN delivery mechanism (-2 points)
- CARDFILE/CUSTFILE embedded CICS steps not fully detailed (-3 points)

### Factual Accuracy (Weight: 25%, Score: 85)

**Positive factors**:
- Job purposes accurately described for all documented jobs
- DD statement DSN values correct across all 22 verified files
- Recovery procedures are plausible and technically sound
- Modernization recommendations are reasonable
- COND=(4,LT) interpretation is correct

**Negative factors**:
- MONTHLY Control-M workflow would fail without TRANBKP (-8 points)
- DEFCUST alternate schema not mentioned (-4 points)
- WAITSTEP PARM description slightly wrong (-3 points)

### Completeness (Weight: 20%, Score: 55)

- JCL coverage: 32/38 = 84.2%, but 6 missing files represent entire workflows
- Batch program coverage: 10/12 = 83.3%, with statement generation completely absent
- Script coverage: 1/3 = 33.3% (only run_full_batch.sh documented)
- Missing CREASTMT workflow is a significant business function gap
- CBTRN01C.cbl omitted from program mapping

### Quantitative Accuracy (Weight: 10%, Score: 40)

- JCL count: 33 stated vs 38 actual (13.2% error)
- Batch program count: 10 stated vs 12 actual (16.7% error)
- VSAM cluster count: 15 stated — count in doc appears reasonable but unverified in detail
- GDG base count: 11 stated — DEFGDGB(6) + DEFGDGD(3) + REPTFILE(1) + DALYREJS(1) = 11, correct
- Category counts in section 1 table sum to 33, matching stated total but not actual

### Documentation Quality (Weight: 10%, Score: 88)

**Positive factors**:
- Well-structured with clear section hierarchy
- Valid Mermaid diagrams throughout (no syntax errors)
- Comprehensive appendices (A through D)
- Recovery procedures are actionable
- Data flow diagrams are clear and useful

**Negative factors**:
- No dedicated sections for 2 of 3 scripts (-6 points)
- Duplicate GDG definition not flagged (-3 points)
- Case sensitivity variance not mentioned (-3 points)

### Weighted Total

| Category | Weight | Raw | Weighted |
|----------|--------|-----|----------|
| Source Reference Accuracy | 35% | 90 | 31.50 |
| Factual Accuracy | 25% | 85 | 21.25 |
| Completeness | 20% | 55 | 11.00 |
| Quantitative Accuracy | 10% | 40 | 4.00 |
| Documentation Quality | 10% | 88 | 8.80 |
| **Total** | **100%** | | **76.55** |

**Rounded: 76.6 / 100 → FAIL**

**Revision note (2026-02-06):** Source Reference Accuracy increased from 82 to 90 after MNTTRDB2 was reclassified as a valid reference (found in `app/app-transaction-type-db2/jcl/`). Original total was 73.75.

---

*This validation report was generated as part of the CardDemo reverse engineering validation suite.*
