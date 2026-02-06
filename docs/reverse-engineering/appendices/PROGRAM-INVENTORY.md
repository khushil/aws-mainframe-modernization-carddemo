# CardDemo Program Inventory

**Document Version:** 2.0
**Updated:** 2026-02-06
**Total Programs:** 44 (31 core + 13 extension)
**Total Lines of Code:** 30,175 (20,650 core + 9,525 extension)

> **Revision Note:** Version 1.0 documented 29 programs (19,496 LOC) from `app/cbl/` lowercase .cbl files only. Version 2.0 adds 2 uppercase .CBL programs from `app/cbl/` and 13 programs from extension directories.

---

## Summary

| Category | Count | LOC | % of Total |
|----------|-------|-----|-----------|
| **Core Programs** | | | |
| Online CICS Programs (CO*) | 17 | 14,067 | 46.6% |
| Batch Programs (CB*) | 12 | 5,690 | 18.9% |
| Shared Utilities (CS*) | 1 | 157 | 0.5% |
| Other (COBSWAIT) | 1 | 41 | 0.1% |
| **Core Subtotal** | **31** | **20,650** | **68.4%** |
| **Extension Programs** | | | |
| Auth IMS-DB2-MQ Extension | 8 | 5,926 | 19.6% |
| Transaction Type DB2 Extension | 3 | 4,037 | 13.4% |
| VSAM-MQ Extension | 2 | 1,144 | 3.8% |
| **Extension Subtotal** | **13** | **9,525** | **31.6%** |
| **Grand Total** | **44** | **30,175** | **100%** |

---

## Core Online CICS Programs

These programs execute within the CICS transaction processing environment and interact with users via BMS screens.

| # | Program | Ext | Function | LOC | BMS Screen | Transaction | Bounded Context | Complexity |
|---|---------|-----|----------|-----|------------|-------------|-----------------|------------|
| 1 | COSGN00C | .cbl | User sign-on and authentication | 260 | COSGN00 | CC00 | Authentication | Low |
| 2 | COADM01C | .cbl | Admin menu navigation | 288 | COADM01 | CA00 | Navigation | Low |
| 3 | COMEN01C | .cbl | User menu navigation | 308 | COMEN01 | CM00 | Navigation | Low |
| 4 | COACTVWC | .cbl | Account view (read-only display) | 941 | COACTVW | CM00 | Account Management | Moderate |
| 5 | COACTUPC | .cbl | Account/customer update | 4,236 | COACTUP | CM00 | Account Management | Very High |
| 6 | COCRDLIC | .cbl | Credit card list (paginated browse) | 1,459 | COCRDLI | CM00 | Card Management | High |
| 7 | COCRDSLC | .cbl | Credit card detail view | 887 | COCRDSL | CM00 | Card Management | Moderate |
| 8 | COCRDUPC | .cbl | Credit card update | 1,560 | COCRDUP | CM00 | Card Management | High |
| 9 | COTRN00C | .cbl | Transaction list (paginated browse) | 699 | COTRN00 | CM00 | Transaction Processing | Moderate |
| 10 | COTRN01C | .cbl | Transaction detail view | 330 | COTRN01 | CM00 | Transaction Processing | Low |
| 11 | COTRN02C | .cbl | Transaction add (manual entry) | 783 | COTRN02 | CM00 | Transaction Processing | Moderate |
| 12 | COBIL00C | .cbl | Bill payment processing | 572 | COBIL00 | CM00 | Bill Payment | Moderate |
| 13 | CORPT00C | .cbl | Transaction report submission | 649 | CORPT00 | CM00 | Reporting | Moderate |
| 14 | COUSR00C | .cbl | User list (admin only, browse) | 695 | COUSR00 | CA00 | User Administration | Moderate |
| 15 | COUSR01C | .cbl | User add (admin only) | 299 | COUSR01 | CA00 | User Administration | Low |
| 16 | COUSR02C | .cbl | User update (admin only) | 414 | COUSR02 | CA00 | User Administration | Low |
| 17 | COUSR03C | .cbl | User delete (admin only) | 359 | COUSR03 | CA00 | User Administration | Low |

---

## Core Batch Programs

These programs execute in batch mode via JCL job submissions.

| # | Program | Ext | Function | LOC | JCL Job(s) | Input Files | Output Files | Complexity |
|---|---------|-----|----------|-----|------------|-------------|--------------|------------|
| 18 | CBACT01C | .cbl | Account data extract | 430 | READACCT | ACCTDAT | 3 output formats | Low |
| 19 | CBACT02C | .cbl | Card data read | 178 | READCARD | CARDDAT | SYSOUT | Low |
| 20 | CBACT03C | .cbl | Cross-reference read | 178 | READXREF | CCXREF | SYSOUT | Low |
| 21 | CBACT04C | .cbl | Interest calculation | 652 | INTCALC | TCATBALF, CCXREF, ACCTDAT, DISCGRP | SYSTRAN GDG | Moderate |
| 22 | CBCUS01C | .cbl | Customer data read | 178 | READCUST | CUSTDAT | SYSOUT | Low |
| 23 | CBTRN01C | .cbl | Transaction processing | 494 | (none) | TRANSACT | — | Moderate |
| 24 | CBTRN02C | .cbl | Transaction posting | 731 | POSTTRAN | DALYTRAN, TRANSACT, CCXREF, ACCTDAT, TCATBALF | DALYREJS GDG | Moderate |
| 25 | CBTRN03C | .cbl | Transaction reporting | 649 | TRANREPT | TRANSACT (sorted) | TRANREPT GDG | Moderate |
| 26 | CBEXPORT | .cbl | Multi-file export | 582 | CBEXPORT | 5 VSAM files | EXPORT.DATA | Moderate |
| 27 | CBIMPORT | .cbl | Import and split | 487 | CBIMPORT | EXPORT.DATA | 4 import files + errors | Moderate |
| 28 | CBSTM03A | **.CBL** | Statement generation main | 924 | CREASTMT.JCL | TRANSACT, CARDXREF, ACCTDATA, CUSTDATA | STATEMNT.PS, STATEMNT.HTML | Moderate |
| 29 | CBSTM03B | **.CBL** | Statement generation subroutine | 230 | (called by CBSTM03A) | — | — | Low |

---

## Core Shared Utilities

| # | Program | Ext | Function | LOC | Called By | Complexity |
|---|---------|-----|----------|-----|----------|------------|
| 30 | CSUTLDTC | .cbl | Date validation utility | 157 | COACTUPC, COCRDUPC | Low |
| 31 | COBSWAIT | .cbl | Batch wait/delay | 41 | WAITSTEP JCL | Trivial |

---

## Extension: Authorization IMS-DB2-MQ (`app/app-authorization-ims-db2-mq/`)

Online programs for pending authorization management with IMS database and MQ messaging.

| # | Program | Ext | Function | LOC | Technology | Complexity |
|---|---------|-----|----------|-----|------------|------------|
| 32 | COPAUA0C | .cbl | Authorization decision engine — MQ request/reply, IMS read, VSAM lookup | 1,026 | CICS + IMS DL/I + MQ | High |
| 33 | COPAUS0C | .cbl | Pending authorization summary list (browse screen) | 1,032 | CICS + IMS DL/I | High |
| 34 | COPAUS1C | .cbl | Authorization detail view with fraud toggle | 604 | CICS + IMS DL/I | Moderate |
| 35 | COPAUS2C | .cbl | Fraud report/remove — DB2 insert/update | 244 | CICS + DB2 | Low |
| 36 | CBPAUP0C | .cbl | Purge expired authorization records (batch) | 386 | Batch + IMS DL/I | Low |
| 37 | PAUDBUNL | **.CBL** | IMS database unload to sequential files | 317 | Batch + IMS DL/I | Moderate |
| 38 | DBUNLDGS | **.CBL** | IMS database unload to GSAM files | 366 | Batch + IMS DL/I + GSAM | Moderate |
| 39 | PAUDBLOD | **.CBL** | IMS database load from sequential files | 369 | Batch + IMS DL/I | Moderate |

### Extension JCL (Authorization)

| JCL File | Ext | Purpose | Programs Used |
|----------|-----|---------|---------------|
| CBPAUP0J.jcl | .jcl | Run purge batch | CBPAUP0C |
| DBPAUTP0.jcl | .jcl | IMS database maintenance | IMS utilities |
| LOADPADB.JCL | .JCL | Load IMS database from sequential files | PAUDBLOD |
| UNLDPADB.JCL | .JCL | Unload IMS database to sequential files | PAUDBUNL |
| UNLDGSAM.JCL | .JCL | Unload IMS database to GSAM | DBUNLDGS |

### Extension IMS Artifacts

| Artifact | Type | Purpose |
|----------|------|---------|
| PSBPAUTB.psb | PSB | PCB for online auth browsing |
| PSBPAUTL.psb | PSB | PCB for auth list operations |
| PAUTBUNL.PSB | PSB | PCB for batch unload |
| DLIGSAMP.PSB | PSB | PCB for GSAM operations |
| DBPAUTP0.dbd | DBD | Primary auth database definition |
| DBPAUTX0.dbd | DBD | Auth index database definition |
| PASFLDBD.DBD | DBD | GSAM summary flat file definition |
| PADFLDBD.DBD | DBD | GSAM detail flat file definition |

---

## Extension: Transaction Type DB2 (`app/app-transaction-type-db2/`)

Programs for managing transaction type reference data via DB2.

| # | Program | Ext | Function | LOC | Technology | Complexity |
|---|---------|-----|----------|-----|------------|------------|
| 40 | COTRTLIC | .cbl | Transaction type list (paginated browse via DB2 cursor) | 2,098 | CICS + DB2 | Very High |
| 41 | COTRTUPC | .cbl | Transaction type update form | 1,702 | CICS + DB2 | High |
| 42 | COBTUPDT | .cbl | Batch transaction type update | 237 | Batch + DB2 | Low |

### Extension JCL (Transaction Type)

| JCL File | Ext | Purpose | Programs Used |
|----------|-----|---------|---------------|
| MNTTRDB2.jcl | .jcl | Maintain transaction type table | COBTUPDT |
| TRANEXTR.jcl | .jcl | Extract transaction types from DB2 | DB2 utilities |
| CREADB21.jcl | .jcl | Create DB2 tables and indexes | DB2 DDL |

### Extension DB2 Artifacts

| Artifact | Purpose |
|----------|---------|
| TRNTYPE.ddl | Transaction type table DDL |
| XTRNTYPE.ddl | Transaction type index DDL |
| TRNTYCAT.ddl | Transaction type category table DDL |
| XTRNTYCAT.ddl | Transaction type category index DDL |

### Extension CTL Files

| File | Purpose |
|------|---------|
| DB2CREAT.ctl | DB2 object creation control |
| DB2FREE.ctl | DB2 resource free control |
| DB2LTTYP.ctl | DB2 type table load control |
| DB2LTCAT.ctl | DB2 category table load control |
| DB2TEP41.ctl | DB2 test environment control |
| DB2TIAD1.ctl | DB2 test data control |
| REPROCT.ctl | Reprocessing control |

---

## Extension: VSAM-MQ (`app/app-vsam-mq/`)

Programs for account data inquiry via MQ messaging over VSAM data.

| # | Program | Ext | Function | LOC | Technology | Complexity |
|---|---------|-----|----------|-----|------------|------------|
| 43 | COACCT01 | .cbl | Account inquiry via MQ request/reply | 620 | CICS + VSAM + MQ | Moderate |
| 44 | CODATE01 | .cbl | Date service via MQ request/reply | 524 | CICS + MQ | Moderate |

---

## Complexity Distribution (All 44 Programs)

```
Very High (>2000 LOC)  ████████                           3 programs (6.8%) — COACTUPC, COTRTLIC, COTRTUPC (ext)
High (1000-2000 LOC)   ████████████                       4 programs (9.1%) — COCRDLIC, COCRDUPC, COPAUA0C (ext), COPAUS0C (ext)
Moderate (500-999 LOC)  ████████████████████████           14 programs (31.8%)
Low (200-499 LOC)       ██████████████████████████████     16 programs (36.4%)
Trivial (<200 LOC)      ██████████████                     7 programs (15.9%)
```

---

## API Suitability Scores

*Scores for core programs only. Extension programs not yet assessed.*

| Program | API Score | Priority | Proposed Endpoint |
|---------|-----------|----------|-------------------|
| COCRDSLC | 4.75 | High | `GET /cards/{cardNumber}` |
| COACTVWC | 4.55 | High | `GET /accounts/{accountId}` |
| COTRN01C | 4.50 | High | `GET /transactions/{transactionId}` |
| COSGN00C | 4.45 | High | `POST /auth/token` |
| COCRDLIC | 4.00 | High | `GET /accounts/{accountId}/cards` |
| COTRN00C | 3.75 | High | `GET /transactions` |
| COBIL00C | 3.75 | High | `POST /accounts/{accountId}/payments` |
| COUSR00C | 3.50 | Medium | `GET /admin/users` |
| COUSR03C | 3.40 | Medium | `DELETE /admin/users/{userId}` |
| COCRDUPC | 3.25 | Medium | `PUT /cards/{cardNumber}` |
| COUSR01C | 3.20 | Medium | `POST /admin/users` |
| COUSR02C | 3.20 | Medium | `PUT /admin/users/{userId}` |
| COTRN02C | 2.70 | Low | `POST /transactions` |
| COACTUPC | 2.45 | Low | `PUT /accounts/{accountId}` |
| CORPT00C | 2.25 | Low | `POST /reports` |
| COADM01C | 2.15 | Not Suitable | — |
| COMEN01C | 2.15 | Not Suitable | — |
| COBSWAIT | 1.80 | Not Suitable | — |

---

## File Access Matrix

### Core Programs

| Program | ACCTDAT | CARDDAT | CUSTDAT | TRANSACT | CCXREF | USRSEC | TCATBALF |
|---------|---------|---------|---------|----------|--------|--------|----------|
| COSGN00C | | | | | | R | |
| COACTVWC | R | R | R | | R(AIX) | | |
| COACTUPC | RW | R | RW | | R(AIX) | | |
| COCRDLIC | | R(AIX) | | | | | |
| COCRDSLC | | R | | | | | |
| COCRDUPC | | RW | | | R | | |
| COTRN00C | | | | R | | | |
| COTRN01C | | | | R | | | |
| COTRN02C | RW | | | RW | R | | |
| COBIL00C | RW | | | RW | R(AIX) | | |
| CORPT00C | | | | | | | |
| COUSR00C | | | | | | R | |
| COUSR01C | | | | | | RW | |
| COUSR02C | | | | | | RW | |
| COUSR03C | | | | | | RW | |
| CBTRN02C | RW | | | RW | R | | RW |
| CBACT04C | R | | | | R(AIX) | | R |
| CBEXPORT | R | R | R | R | R | | |
| CBSTM03A | | | R | R | R | | |

### Extension Programs

| Program | VSAM | IMS DB | DB2 | MQ |
|---------|------|--------|-----|-----|
| COPAUA0C | ACCTDAT(R), CCXREF(R) | PAUTSUM0(RW), PAUTDTL1(RW) | — | Request/Reply |
| COPAUS0C | — | PAUTSUM0(R), PAUTDTL1(R) | — | — |
| COPAUS1C | — | PAUTSUM0(R), PAUTDTL1(RW) | — | — |
| COPAUS2C | — | — | AUTHFRDS(RW) | — |
| CBPAUP0C | — | PAUTSUM0(RD), PAUTDTL1(RD) | — | — |
| PAUDBUNL | — | PAUTSUM0(R), PAUTDTL1(R) | — | — |
| DBUNLDGS | — | PAUTSUM0(R), PAUTDTL1(R), GSAM(W) | — | — |
| PAUDBLOD | — | PAUTSUM0(W), PAUTDTL1(W) | — | — |
| COTRTLIC | — | — | TRANSACTION_TYPE(R), TRANSACTION_TYPE_CATEGORY(R) | — |
| COTRTUPC | — | — | TRANSACTION_TYPE(RW) | — |
| COBTUPDT | — | — | TRANSACTION_TYPE(RW) | — |
| COACCT01 | ACCTDAT(R) | — | — | Request/Reply |
| CODATE01 | — | — | — | Request/Reply |

Legend: R = Read, W = Write, RW = Read/Write, RD = Read/Delete, AIX = Via Alternate Index

---

## Cross-References

| Document | Relevance |
|----------|-----------|
| [CONFIDENCE-ASSESSMENT.md](../CONFIDENCE-ASSESSMENT.md) | Confidence levels for this inventory |
| [COACTUPC Deep-Dive](../deep-dives/COACTUPC-ANALYSIS.md) | Line-by-line analysis of largest program |
| [API-CANDIDATES.md](../07-modernization/API-CANDIDATES.md) | API scoring details |
| [CONTEXT-MAP.md](../03-context-model/CONTEXT-MAP.md) | Bounded context assignments |
| [INTEGRATION-PATTERNS.md](../05-specialized/INTEGRATION-PATTERNS.md) | Extension program documentation |
| [BATCH-WORKFLOWS.md](../05-specialized/BATCH-WORKFLOWS.md) | JCL job mappings |
| [SCREEN-FLOWS.md](../05-specialized/SCREEN-FLOWS.md) | BMS screen specifications |

---

## Document Control

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-02-05 | Claude Code | Initial 29-program inventory (core .cbl only) |
| 2.0 | 2026-02-06 | Validation Review | Added 2 .CBL core programs + 13 extension programs; total 44 |

---

*Generated as part of the CardDemo reverse engineering project. Updated during confidence assessment remediation.*
