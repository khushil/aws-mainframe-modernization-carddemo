# Confidence Assessment: CardDemo Reverse Engineering Documentation Suite

**Document Version:** 1.0
**Date:** 2026-02-06
**Classification:** Internal — Quality Assurance
**Author:** Validation Review Team

---

## Executive Summary

This document provides a candid, evidence-based assessment of confidence levels across the CardDemo reverse engineering documentation suite. It identifies blind spots in the validation process itself, quantifies knowledge gaps in extension directory coverage, and prioritizes remediation actions.

**Key findings:**

- The validation infrastructure has a **systematic blind spot**: extension directories (`app/app-*`) were not searched during validation, producing at least one false "hallucination" verdict (MNTTRDB2 in VL-006).
- **6 of 13 validation reports** remain unexecuted, including VL-008 (Integration Patterns) which covers the most complex technology in the system.
- The codebase contains **44 COBOL programs** (not 29 or 39), **62 copybooks** (not 29 or 41), and **46 JCL files** (not 33 or 38) when extension directories are included. Multiple documents cite different wrong counts.
- **4 COBOL programs** in the authorization extension are entirely undocumented: COPAUS1C, PAUDBUNL, DBUNLDGS, PAUDBLOD.
- The COMMAREA size is **160 bytes** — stated as "~130 bytes" in the Security Model and "~155 bytes" elsewhere.
- COACTUPC.cbl at **4,236 lines** is the highest-risk modernization artifact with GO TO spaghetti, multi-file VSAM writes, and an undocumented privilege-reset pattern.

---

## 1. The Validation Process Has Blind Spots (Meta-Concern)

**Severity: Critical**

### 1.1 Extension Directory Blind Spot

VL-006 declared MNTTRDB2 a "phantom hallucination" (HAL-001), but `app/app-transaction-type-db2/jcl/MNTTRDB2.jcl` **exists and is real code**. It runs `COBTUPDT` under DB2 to maintain the transaction type table.

- The validator searched only `app/jcl/` and never checked `app/app-*/jcl/`.
- **Implication:** "Hallucination: confirmed" verdicts cannot be trusted without verifying the validator searched the full source tree.
- The same blind spot likely affects VL-004 (C4 Architecture) which also missed extension programs.

**Evidence:** `app/app-transaction-type-db2/jcl/MNTTRDB2.jcl` exists at the repository root.

### 1.2 Missing Validation Reports

| Status | Validation | Target Document |
|--------|-----------|-----------------|
| Complete | VL-001 (Domain Model) | 01-domain-model/DOMAIN-MODEL.md |
| Complete | VL-002 (Data Model) | 02-data-model/DATA-MODEL.md |
| Complete | VL-003 (Context Model) | 03-context-model/CONTEXT-MAP.md |
| Complete | VL-004 (C4 Architecture) | 04-architecture/ |
| Complete | VL-005 (Screen Flows) | 05-specialized/SCREEN-FLOWS.md |
| Complete | VL-006 (Batch Workflows) | 05-specialized/BATCH-WORKFLOWS.md |
| Complete | VL-007 (Security Model) | 05-specialized/SECURITY-MODEL.md |
| **NOT RUN** | VL-008 (Integration Patterns) | 05-specialized/INTEGRATION-PATTERNS.md |
| **NOT RUN** | VL-009 (Test Coverage) | 06-quality/TEST-COVERAGE.md |
| **NOT RUN** | VL-010 (Modernization) | 07-modernization/MODERNIZATION-READINESS.md |
| **NOT RUN** | VL-011 (API Candidates) | 07-modernization/API-CANDIDATES.md |
| **NOT RUN** | VL-012 (Master Index) | index.md |
| **NOT RUN** | VL-000 (Cross-Document Consistency) | All documents |

The Integration Patterns document (RE-008) covers IMS, DB2, and MQ — the **most complex technology** in the system — and has received **zero validation**.

### 1.3 CLAUDE.md Propagates Incorrect Counts

`CLAUDE.md` stated "39 COBOL programs" and "41 copybooks." Actual counts by directory:

| Directory | .cbl | .CBL | Total |
|-----------|------|------|-------|
| `app/cbl/` | 29 | 2 | 31 |
| `app/app-authorization-ims-db2-mq/cbl/` | 5 | 3 | 8 |
| `app/app-transaction-type-db2/cbl/` | 3 | 0 | 3 |
| `app/app-vsam-mq/cbl/` | 2 | 0 | 2 |
| **Total** | **39** | **5** | **44** |

| Directory | .cpy | .CPY | Total |
|-----------|------|------|-------|
| `app/cpy/` | 29 | 1 | 30 |
| `app/cpy-bms/` | 0 | 17 | 17 |
| `app/app-authorization-ims-db2-mq/cpy/` | 6 | 3 | 9 |
| `app/app-authorization-ims-db2-mq/cpy-bms/` | 2 | 0 | 2 |
| `app/app-transaction-type-db2/cpy/` | 2 | 0 | 2 |
| `app/app-transaction-type-db2/cpy-bms/` | 2 | 0 | 2 |
| **Total** | **41** | **21** | **62** |

Note: The .CPY files include BMS-generated copybooks and IMS PCB mask copybooks (PADFLPCB.CPY, PASFLPCB.CPY, PAUTBPCB.CPY in the auth extension, and COSTM01.CPY in core).

The "39" and "41" figures match lowercase-extension files only across all directories — suggesting the original count included extensions but excluded uppercase-extension files.

---

## 2. Extension Directories: The Largest Knowledge Gap

**Severity: Critical**

### 2.1 Undercounted Programs

| Extension | RE-008 Documents | Actually Contains |
|-----------|-----------------|-------------------|
| `app-authorization-ims-db2-mq/` | 3 programs (COPAUA0C, COPAUS2C, CBPAUP0C) | **8 COBOL programs** (also COPAUS0C, COPAUS1C, PAUDBUNL.CBL, DBUNLDGS.CBL, PAUDBLOD.CBL) |
| `app-transaction-type-db2/` | 3 programs (COTRTLIC, COTRTUPC, COBTUPDT) | 3 programs + 7 CTL files + DDL + JCL |
| `app-vsam-mq/` | 2 programs (COACCT01, CODATE01) | 2 programs + CSD |

### 2.2 Undocumented Programs

| Program | LOC | Type | Purpose | Key Operations |
|---------|-----|------|---------|----------------|
| COPAUS0C.cbl | 1,032 | CICS/IMS | Pending authorization summary list (browse screen) | IMS GU/GNP on PAUTSUM0/PAUTDTL1; BMS COPAU00 screen |
| COPAUS1C.cbl | 604 | CICS/IMS | Authorization detail view with fraud toggle | IMS GU/GNP/REPL on PAUTSUM0/PAUTDTL1; LINK to COPAUS2C; SYNCPOINT/ROLLBACK |
| PAUDBUNL.CBL | 317 | Batch/IMS | IMS database unload to sequential files | CBLTDLI GN/GNP on PAUTSUM0/PAUTDTL1; writes 2 sequential output files |
| DBUNLDGS.CBL | 366 | Batch/IMS+GSAM | IMS database unload to GSAM files | CBLTDLI GN/GNP on PAUTBPCB; ISRT to GSAM via PASFLPCB/PADFLPCB |
| PAUDBLOD.CBL | 369 | Batch/IMS | IMS database load from sequential files | Reads 2 sequential input files; CBLTDLI ISRT/GU on PAUTSUM0/PAUTDTL1 |

### 2.3 Undocumented Non-COBOL Artifacts

| Category | Files | Location |
|----------|-------|----------|
| IMS DBD files | PASFLDBD.DBD, PADFLDBD.DBD, DBPAUTX0.dbd, DBPAUTP0.dbd | app/app-authorization-ims-db2-mq/ims/ |
| IMS PSB files | PSBPAUTB.psb, PSBPAUTL.psb, PAUTBUNL.PSB, DLIGSAMP.PSB | app/app-authorization-ims-db2-mq/ims/ |
| DB2 DDL files | AUTHFRDS.ddl, XAUTHFRD.ddl, TRNTYPE.ddl, XTRNTYPE.ddl, XTRNTYCAT.ddl, TRNTYCAT.ddl | app/app-*/ddl/ |
| CTL control files | DB2CREAT.ctl, DB2FREE.ctl, DB2LTTYP.ctl, DB2LTCAT.ctl, DB2TEP41.ctl, DB2TIAD1.ctl, REPROCT.ctl | app/app-transaction-type-db2/ctl/ |
| CSD definitions | CRDDEMO2.csd, CRDDEMOD.csd, CRDDEMOM.csd | app/app-*/csd/ |
| Extension JCL | CBPAUP0J.jcl, DBPAUTP0.jcl, MNTTRDB2.jcl, TRANEXTR.jcl, CREADB21.jcl, LOADPADB.JCL, UNLDPADB.JCL, UNLDGSAM.JCL | app/app-*/jcl/ |

### 2.4 Why This Matters

- The extensions contain the **only IMS DL/I code** in the system — hierarchical database patterns notoriously difficult to modernize correctly.
- The **MQ request/reply pattern** in COPAUA0C is the closest thing to a real-time API in the legacy system — critical for strangler fig approach.
- IMS PSB/DBD definitions define database structures with **no validation coverage**.
- The IMS unload/load utilities (PAUDBUNL, DBUNLDGS, PAUDBLOD) are **critical for data migration planning**.

---

## 3. Batch Workflows: The Failed Validation (73.8/100)

**Severity: High**

This is the lowest-scoring validation. See [VL-006 Report](validation/VL-006-batch-workflow-report.md) for full details.

### 3.1 Key Concerns

| Issue | Impact |
|-------|--------|
| All 5 uppercase `.JCL` files were missed | Entire statement generation workflow absent |
| 2 uppercase `.CBL` programs were missed | CBSTM03A.CBL (924 LOC) and CBSTM03B.CBL (230 LOC) undocumented |
| MONTHLY workflow missing TRANBKP/TRANIDX | Would produce corrupt results if executed as documented |
| JCL count 33 vs 38 actual (core only) | 13% undercount in core; 46 total with extensions |
| Batch program count 10 vs 12 actual (core only) | 17% undercount; does not include extension batch programs |
| MNTTRDB2 falsely classified as hallucination | VL-006 HAL-001 was incorrect — the JCL exists in extensions |

### 3.2 MNTTRDB2 Reclassification

VL-006 classified MNTTRDB2 as a "phantom node" (HAL-001). This has been corrected:

- **File:** `app/app-transaction-type-db2/jcl/MNTTRDB2.jcl`
- **Purpose:** Maintains the DB2 transaction type table by running COBTUPDT
- **Status:** Reclassified from "phantom hallucination" to "valid — found in extension directory"
- **Impact on score:** HAL-001 was a Major finding worth -8 points. Removing it improves the Source Reference Accuracy score and reduces the hallucination count from 3 to 2.

---

## 4. COACTUPC.cbl: The 4,236-Line Monolith

**Severity: High**

See [COACTUPC Deep-Dive Analysis](deep-dives/COACTUPC-ANALYSIS.md) for the full line-by-line analysis.

### 4.1 Risk Summary

| Metric | Value | Risk |
|--------|-------|------|
| Lines of code | 4,236 | Next largest is 1,560 (COCRDUPC) — 2.7x larger |
| Bounded contexts mixed | 3 | Account update, customer update, card operations |
| GO TO statements | Present | Creates spaghetti control flow |
| VSAM files accessed | 4 | ACCTDAT, CUSTDAT, CARDDAT (via CCXREF), CCXREF |
| Nesting depth | >5 levels | Deep conditional nesting |
| Privilege reset pattern | `SET CDEMO-USRTYP-USER TO TRUE` | Silently demotes admin users |

### 4.2 Why This Is the Highest-Risk Modernization Artifact

- A program this size with GO TO and multi-file VSAM writes almost certainly contains **implicit behaviors** that high-level documentation misses.
- The privilege-reset pattern (`SET CDEMO-USRTYP-USER TO TRUE`) means an admin navigating through this program loses admin rights — either a bug or an undocumented security feature.
- Decomposing this for microservices requires understanding every shared-state dependency, every COMMAREA field flow, and every cross-file VSAM operation sequence.

---

## 5. Security Architecture: Worse Than Documented

**Severity: High**

While the Security Model scored 90/100 in VL-007, several concerns are understated.

### 5.1 The Privilege Reset Pattern

Five programs silently execute `SET CDEMO-USRTYP-USER TO TRUE`:
- COACTVWC, COACTUPC, COCRDLIC, COCRDSLC, COCRDUPC

This resets admin users to regular user privileges in the COMMAREA. On returning to the menu, they see the user menu instead of admin menu.

**Source:** VL-007 finding M-003.

### 5.2 Authorization Model is Theater

The menu check in `COMEN01C.cbl:136-143` is the **only** access control gate. Direct CICS transaction invocation bypasses it entirely.

### 5.3 PCI-DSS at 15% on a Credit Card System

The modernization readiness score of 3.53/5 may be optimistic given that security remediation must happen **before** any API exposure, and the effort is substantial.

### 5.4 COMMAREA Size Error

The Security Model states "~130 bytes" (`SECURITY-MODEL.md:281`). The actual size computed from `COCOM01Y.cpy` fields:

| Field | Size |
|-------|------|
| CDEMO-FROM-TRANID | 4 |
| CDEMO-FROM-PROGRAM | 8 |
| CDEMO-TO-TRANID | 4 |
| CDEMO-TO-PROGRAM | 8 |
| CDEMO-USER-ID | 8 |
| CDEMO-USER-TYPE | 1 |
| CDEMO-PGM-CONTEXT | 1 |
| CDEMO-CUST-ID | 9 |
| CDEMO-CUST-FNAME | 25 |
| CDEMO-CUST-MNAME | 25 |
| CDEMO-CUST-LNAME | 25 |
| CDEMO-ACCT-ID | 11 |
| CDEMO-ACCT-STATUS | 1 |
| CDEMO-CARD-NUM | 16 |
| CDEMO-LAST-MAP | 7 |
| CDEMO-LAST-MAPSET | 7 |
| **Total** | **160 bytes** |

---

## 6. Context Model: Structural Inaccuracies (80.6/100)

**Severity: Medium-High**

### 6.1 BMS Map Name Errors

5 of 17 BMS map names are incorrect in the navigation flows documentation. Programs using `CCARD-` prefixed map names (COACTVWC, COACTUPC, COCRDLIC, COCRDSLC, COCRDUPC) were documented with `CO-` prefixed names.

### 6.2 Transaction ID Coverage

Only 3 of 17+ transaction IDs are documented: CC00, CCT1, CCT2. Extension programs add at least CPVD (COPAUS1C) and others. For CICS migration planning, the complete TransID-to-program mapping is essential.

---

## 7. VSAM Data Integrity: Implicit and Unenforced

**Severity: Medium**

- No referential integrity between VSAM files — orphan records possible after deletes.
- The `ACCT-EXPIRAION-DATE` typo (missing 'T') in production field names signals limited code review.
- FILLER fields are large: ACCTDAT has 178 bytes of FILLER in a 300-byte record (59%).
- The DEFCUST.jcl alternate schema (`AWS.CCDA.CUSTDATA` with KEYS(10 0) vs standard `AWS.M2.CARDDEMO.CUSTDATA` with KEYS(9 0)) suggests an environment variant. DEFCUST uses a different DSN naming convention (`AWS.CCDA.*` and `AWS.CUSTDATA.*`) and a 10-byte key (vs 9-byte in CUSTFILE.jcl), likely representing a different environment or version of the customer file layout.

---

## 8. Modernization Readiness Assessment: Potentially Optimistic

**Severity: Medium**

The 3.53/5 composite score has **not been validated** (VL-010 never ran).

| Concern | Evidence |
|---------|----------|
| Security weight too low | Security scored 1.5/5 but weighted at only 15%. For a credit card system, 25-30% would be more appropriate, pulling the composite below 3.0. |
| API candidate count unverified | "15 of 18 programs suitable for API exposure" — several programs have implicit dependencies (CSUTLDTC, shared COMMAREA state) that complicate API extraction. |
| Batch assessment based on incomplete data | Batch modernization scored 2.4/5 (lowest context score) using documentation that scored 73.8/100 (our worst validation). |
| Program count wrong | Assessment counted 10 batch programs; actual is 12+ core plus extension programs. |

---

## 9. COUSR01 PF12: A Runtime Bug

**Severity: Low-Medium**

The BMS screen for User Add (COUSR01) displays "F12=Exit" in the footer, but the COBOL program has no PF12 handler. Pressing PF12 hits the `WHEN OTHER` branch and displays "Invalid key pressed."

**Decision needed for modernization:** Implement F12=Exit (matching the UI promise) or remove it from the screen (matching the code behavior).

---

## 10. Cross-Document Consistency: Untested

**Severity: Medium**

VL-000 (Cross-Document Consistency) was never executed. Observed inconsistencies:

| Item | Document A | Document B | Actual |
|------|-----------|-----------|--------|
| COMMAREA size | ~130 bytes (Security Model) | ~155 bytes (Context Model) | **160 bytes** |
| Program count | 29 (index.md) | 39 (CLAUDE.md prior) | **44 (all dirs)** or **31 (core only)** |
| JCL count | 33 (BATCH-WORKFLOWS.md) | 38 (VL-006) | **38 core; 46 total** |
| Business rules | 59+ (Domain Model) | Referenced in Modernization Assessment | **Unvalidated count** |

---

## Confidence Heat Map

| Area | Confidence | Validation Score | Key Risk |
|------|-----------|-----------------|----------|
| Data Model | **HIGH** | 96.8 | Minor ER interpretation issue |
| Security Model | **MEDIUM-HIGH** | 90.0 | Privilege reset pattern; COMMAREA size wrong |
| Domain Model | **MEDIUM-HIGH** | 88.0 | Completeness gaps in program/copybook coverage |
| C4 Architecture | **MEDIUM** | 90.0 | Missing extension programs; incorrect counts |
| Screen Flows | **MEDIUM** | 90.6 | Missing BMS copybook references; journey gaps |
| Context Model | **MEDIUM-LOW** | 80.6 | 5 wrong BMS names; incomplete TransID mapping |
| Batch Workflows | **LOW** | 73.8 | Missing files; wrong counts; broken workflow doc |
| Integration Patterns | **UNVALIDATED** | — | Largest knowledge gap; IMS/MQ complexity |
| Modernization Assessment | **UNVALIDATED** | — | Potentially optimistic; based on incomplete data |
| API Candidates | **UNVALIDATED** | — | Not independently verified |
| Extension Directories | **LOW** | — | 5+ undocumented programs; IMS DB tooling unknown |
| COACTUPC Decomposition | **LOW** | — | 4,236-line monolith; GO TO spaghetti |

---

## Remediation Priority Matrix

| Priority | Item | Effort | Impact |
|----------|------|--------|--------|
| **P0** | Fix CLAUDE.md counts | Low | Prevents count propagation errors |
| **P0** | Fix VL-006 MNTTRDB2 false positive | Low | Corrects validation credibility |
| **P0** | Complete program inventory (all dirs) | Medium | Foundation for all other assessments |
| **P1** | Document missing extension programs | Medium | Closes largest knowledge gap |
| **P1** | Fix SECURITY-MODEL.md COMMAREA size | Low | Corrects factual error |
| **P1** | Fix BATCH-WORKFLOWS.md counts | Low | Corrects factual errors |
| **P1** | Deep-dive COACTUPC.cbl | High | De-risks highest-risk artifact |
| **P2** | Run VL-008 (Integration Patterns) | High | Validates most complex technology |
| **P2** | Run VL-000 (Cross-Document Consistency) | Medium | Catches cross-doc errors |
| **P2** | Document DEFCUST.jcl alternate schema | Low | Explains environment variant |
| **P3** | Run VL-009 through VL-012 | High | Completes validation coverage |
| **P3** | Fix Context Model BMS names | Medium | Corrects navigation flow documentation |

---

## Cross-References

| Document | Relevance |
|----------|-----------|
| [VL-001 Domain Model Report](validation/VL-001-domain-model-report.md) | Score: 88.0 |
| [VL-002 Data Model Report](validation/VL-002-data-model-report.md) | Score: 96.8 |
| [VL-003 Context Model Report](validation/VL-003-context-model-report.md) | Score: 80.6 |
| [VL-004 C4 Architecture Report](validation/VL-004-c4-architecture-report.md) | Score: 90.0 |
| [VL-005 Screen Flow Report](validation/VL-005-screen-flow-report.md) | Score: 90.6 |
| [VL-006 Batch Workflow Report](validation/VL-006-batch-workflow-report.md) | Score: 73.8 |
| [VL-007 Security Model Report](validation/VL-007-security-model-report.md) | Score: 90.0 |
| [COACTUPC Deep-Dive](deep-dives/COACTUPC-ANALYSIS.md) | Monolith analysis |
| [Program Inventory](appendices/PROGRAM-INVENTORY.md) | Complete program listing |
| [Integration Patterns](05-specialized/INTEGRATION-PATTERNS.md) | Extension documentation |

---

## Document Control

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-02-06 | Validation Review Team | Initial confidence assessment |

---

*This document was generated as part of the CardDemo reverse engineering quality assurance process.*
