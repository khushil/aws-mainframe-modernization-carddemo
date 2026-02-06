# CardDemo Program Inventory

**Document Version:** 1.0
**Generated:** 2026-02-05
**Total Programs:** 29 (19 online + 10 batch)
**Total Lines of Code:** 19,496

---

## Summary

| Category | Count | LOC | % of Total |
|----------|-------|-----|-----------|
| Online CICS Programs (CO*) | 17 | 14,067 | 72.2% |
| Batch Programs (CB*) | 10 | 4,236 | 21.7% |
| Shared Utilities (CS*) | 1 | 157 | 0.8% |
| Other (COBSWAIT) | 1 | 41 | 0.2% |
| **Total** | **29** | **19,496** | **100%** |

---

## Online CICS Programs

These programs execute within the CICS transaction processing environment and interact with users via BMS screens.

| # | Program | Function | LOC | BMS Screen | Transaction | Bounded Context | Complexity |
|---|---------|----------|-----|------------|-------------|-----------------|------------|
| 1 | COSGN00C | User sign-on and authentication | 260 | COSGN00 | CC00 | Authentication | Low |
| 2 | COADM01C | Admin menu navigation | 288 | COADM01 | CA00 | Navigation | Low |
| 3 | COMEN01C | User menu navigation | 308 | COMEN01 | CM00 | Navigation | Low |
| 4 | COACTVWC | Account view (read-only display) | 941 | COACTVW | CM00 | Account Management | Moderate |
| 5 | COACTUPC | Account/customer update | 4,236 | COACTUP | CM00 | Account Management | Very High |
| 6 | COCRDLIC | Credit card list (paginated browse) | 1,459 | COCRDLI | CM00 | Card Management | High |
| 7 | COCRDSLC | Credit card detail view | 887 | COCRDSL | CM00 | Card Management | Moderate |
| 8 | COCRDUPC | Credit card update | 1,560 | COCRDUP | CM00 | Card Management | High |
| 9 | COTRN00C | Transaction list (paginated browse) | 699 | COTRN00 | CM00 | Transaction Processing | Moderate |
| 10 | COTRN01C | Transaction detail view | 330 | COTRN01 | CM00 | Transaction Processing | Low |
| 11 | COTRN02C | Transaction add (manual entry) | 783 | COTRN02 | CM00 | Transaction Processing | Moderate |
| 12 | COBIL00C | Bill payment processing | 572 | COBIL00 | CM00 | Bill Payment | Moderate |
| 13 | CORPT00C | Transaction report submission | 649 | CORPT00 | CM00 | Reporting | Moderate |
| 14 | COUSR00C | User list (admin only, browse) | 695 | COUSR00 | CA00 | User Administration | Moderate |
| 15 | COUSR01C | User add (admin only) | 299 | COUSR01 | CA00 | User Administration | Low |
| 16 | COUSR02C | User update (admin only) | 414 | COUSR02 | CA00 | User Administration | Low |
| 17 | COUSR03C | User delete (admin only) | 359 | COUSR03 | CA00 | User Administration | Low |

---

## Batch Programs

These programs execute in batch mode via JCL job submissions and process data without user interaction.

| # | Program | Function | LOC | JCL Job(s) | Input Files | Output Files | Complexity |
|---|---------|----------|-----|------------|-------------|--------------|------------|
| 18 | CBACT01C | Account data extract | 430 | READACCT | ACCTDAT | 3 output formats | Low |
| 19 | CBACT02C | Card data read | 178 | READCARD | CARDDAT | SYSOUT | Low |
| 20 | CBACT03C | Cross-reference read | 178 | READXREF | CCXREF | SYSOUT | Low |
| 21 | CBACT04C | Interest calculation | 652 | INTCALC | TCATBALF, CCXREF, ACCTDAT, DISCGRP | SYSTRAN GDG | Moderate |
| 22 | CBCUS01C | Customer data read | 178 | READCUST | CUSTDAT | SYSOUT | Low |
| 23 | CBTRN01C | Transaction processing | 494 | - | TRANSACT | - | Moderate |
| 24 | CBTRN02C | Transaction posting | 731 | POSTTRAN | DALYTRAN, TRANSACT, CCXREF, ACCTDAT, TCATBALF | DALYREJS GDG | Moderate |
| 25 | CBTRN03C | Transaction reporting | 649 | TRANREPT | TRANSACT (sorted) | TRANREPT GDG | Moderate |
| 26 | CBEXPORT | Multi-file export | 582 | CBEXPORT | 5 VSAM files | EXPORT.DATA | Moderate |
| 27 | CBIMPORT | Import and split | 487 | CBIMPORT | EXPORT.DATA | 4 import files + errors | Moderate |

---

## Shared Utilities

| # | Program | Function | LOC | Called By | Complexity |
|---|---------|----------|-----|----------|------------|
| 28 | CSUTLDTC | Date validation utility | 157 | COACTUPC, COCRDUPC | Low |
| 29 | COBSWAIT | Batch wait/delay | 41 | WAITSTEP JCL | Trivial |

---

## Complexity Distribution

```
Very High (>2000 LOC)  ████                               1 program  (3.4%)
High (1000-2000 LOC)   ████████                           2 programs (6.9%)
Moderate (500-999 LOC)  ████████████████                   8 programs (27.6%)
Low (200-499 LOC)       ██████████████████████             11 programs (37.9%)
Trivial (<200 LOC)      ██████████████                     7 programs (24.1%)
```

---

## API Suitability Scores

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
| COADM01C | 2.15 | Not Suitable | - |
| COMEN01C | 2.15 | Not Suitable | - |
| COBSWAIT | 1.80 | Not Suitable | - |

---

## File Access Matrix

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

Legend: R = Read, W = Write, RW = Read/Write, AIX = Via Alternate Index

---

## Cross-References

| Document | Relevance |
|----------|-----------|
| [API-CANDIDATES.md](../07-modernization/API-CANDIDATES.md) | API scoring details |
| [CONTEXT-MAP.md](../03-context-model/CONTEXT-MAP.md) | Bounded context assignments |
| [C4-L4-CODE-PATTERNS.md](../04-architecture/C4-L4-CODE-PATTERNS.md) | Coding patterns used |
| [BATCH-WORKFLOWS.md](../05-specialized/BATCH-WORKFLOWS.md) | JCL job mappings |
| [SCREEN-FLOWS.md](../05-specialized/SCREEN-FLOWS.md) | BMS screen specifications |

---

*Generated as part of the RE-000 Master Index for the CardDemo reverse engineering project.*
