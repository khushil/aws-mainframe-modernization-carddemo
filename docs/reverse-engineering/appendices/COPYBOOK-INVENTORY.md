# CardDemo Copybook Inventory

**Document Version:** 1.1
**Updated:** 2026-02-06
**Total Copybooks:** 30 (29 .cpy + 1 .CPY)
**Total Lines of Code:** 2,748+

---

## Summary

| Category | Count | LOC | Description |
|----------|-------|-----|-------------|
| Data Record Layouts (CV*) | 13 | 694 | VSAM file record definitions |
| Communication/Session (CO*) | 3 | 210 | COMMAREA and menu definitions |
| Common Services (CS*) | 8 | 1,608 | Shared utilities, validation, messages |
| Statement Processing (CO*) | 1 | 39 | Statement report record layout |
| Program-Specific (CU*, CV*) | 3 | 88 | Individual program support |
| Deprecated | 1 | 10 | Unused artifacts |
| Other | 1 | 138 | Export record structure |
| **Total** | **30** | **2,787** | |

---

## Data Record Layouts

These copybooks define the VSAM file record structures used across programs.

| # | Copybook | LOC | Purpose | Key Fields | Used By |
|---|----------|-----|---------|------------|---------|
| 1 | CVACT01Y | 20 | Account master record | ACCT-ID (9(11)), ACCT-CURR-BAL, ACCT-CREDIT-LIMIT | COACTVWC, COACTUPC, COBIL00C, CBTRN02C, CBACT04C |
| 2 | CVACT02Y | 14 | Card master record | CARD-NUM (X(16)), CARD-ACCT-ID, CARD-CVV-CD | COCRDSLC, COCRDUPC, COCRDLIC |
| 3 | CVACT03Y | 11 | Card-account cross-reference | XREF-CARD-NUM, XREF-CUST-ID, XREF-ACCT-ID | COBIL00C, COTRN02C, CBTRN02C |
| 4 | CVCRD01Y | 46 | Card program work area | AID key definitions, navigation control | COCRDUPC |
| 5 | CVCUS01Y | 26 | Customer record structure | CUST-ID (9(09)), CUST-FIRST-NAME, CUST-SSN | COACTVWC, COACTUPC |
| 6 | CVTRA01Y | 13 | Transaction category balance | TRAN-CAT-KEY, TRAN-CAT-BAL | CBTRN02C, CBACT04C |
| 7 | CVTRA02Y | 13 | Disclosure group record | DIS-GROUP-KEY, DIS-INT-RATE | CBACT04C |
| 8 | CVTRA03Y | 10 | Transaction type master | TRAN-TYPE-CD, TRAN-TYPE-DESC | Reference data |
| 9 | CVTRA04Y | 12 | Transaction category type | TRAN-CAT-CD, TRAN-CAT-TYPE-DESC | Reference data |
| 10 | CVTRA05Y | 21 | Transaction detail record | TRAN-ID (X(16)), TRAN-AMT, TRAN-CARD-NUM | COTRN00C, COTRN01C, COBIL00C, CBTRN02C |
| 11 | CVTRA06Y | 21 | Daily transaction record | DALYTRAN-ID, DALYTRAN-AMT, DALYTRAN-CARD-NUM | CBTRN02C (batch input) |
| 12 | CVTRA07Y | 73 | Transaction report structure | Report headers, detail lines, totals | CBTRN03C, CORPT00C |
| 13 | CUSTREC | 26 | Customer master record (alternate) | CUST-ID, personal/address fields, FICO score | CBCUS01C |

---

## Communication and Session Copybooks

| # | Copybook | LOC | Purpose | Key Fields | Used By |
|---|----------|-----|---------|------------|---------|
| 14 | COCOM01Y | 47 | COMMAREA session structure | CDEMO-USER-ID, CDEMO-USER-TYPE, CDEMO-ACCT-ID, CDEMO-CARD-NUM | All online programs |
| 15 | COADM02Y | 62 | Admin menu option definitions | 6 options: User CRUD, Transaction Type management | COADM01C |
| 16 | COMEN02Y | 101 | User menu option definitions | 11 options: Account, Card, Transaction, Bill Payment, Reports | COMEN01C |

---

## Common Service Copybooks

| # | Copybook | LOC | Purpose | Key Definitions | Used By |
|---|----------|-----|---------|-----------------|---------|
| 17 | CSLKPCDY | 1,318 | Lookup code repository | Valid US phone area codes, state codes, state-zip validation | COACTUPC |
| 18 | CSUTLDPY | 375 | Date validation working storage | CCYYMMDD parsing, leap year flags | CSUTLDTC, COACTUPC, COCRDUPC |
| 19 | CSUTLDWY | 89 | Date validation procedure division | Year/month/day validation paragraphs | CSUTLDTC |
| 20 | CSSTRPFY | 85 | AID key processing | Maps PF1-PF24 and Enter to boolean flags | All online programs |
| 21 | CSDAT01Y | 58 | Date/time working storage | Timestamp, formatted date/time fields | Multiple programs |
| 22 | CODATECN | 52 | Date conversion utilities | Input/output date format handling | Date-related programs |
| 23 | CSMSG02Y | 35 | Abend error handling | Error codes, messages, diagnostic info | All programs (error handling) |
| 24 | CSMSG01Y | 24 | Common message constants | Thank you message, invalid key message | COSGN00C, menu programs |

---

## Other Copybooks

| # | Copybook | Ext | LOC | Purpose | Used By |
|---|----------|-----|-----|---------|---------|
| 25 | CVEXPORT | .cpy | 103 | Multi-record export layout | CBEXPORT, CBIMPORT |
| 26 | COTTL01Y | .cpy | 27 | Screen title constants | All online programs (header display) |
| 27 | CSSETATY | .cpy | 30 | Screen attribute setting template | Online programs (field highlighting) |
| 28 | CSUSR01Y | .cpy | 26 | User security record layout | COSGN00C, COUSR00C-03C |
| 29 | COSTM01 | **.CPY** | 39 | Transaction record layout for statement reporting (TRNX-RECORD) | CBSTM03A.CBL |

---

## Deprecated Copybooks

| # | Copybook | LOC | Purpose | Status |
|---|----------|-----|---------|--------|
| 30 | UNUSED1Y | 10 | Placeholder data structure (ID, name, password, type) | Unused - candidate for removal |

---

## Copybook Dependency Map

```
COCOM01Y (Session)
├── Used by ALL 17 online programs
├── Defines: User ID, Type, Account, Card context
└── Size: 47 lines (160 bytes COMMAREA)

CSLKPCDY (Lookups)
├── Used by COACTUPC only
├── Contains: 500+ area codes, 50+ state codes, 200+ zip combos
└── Size: 1,318 lines (largest copybook - 48% of total)

CVACT01Y + CVACT02Y + CVACT03Y (Data Records)
├── Account + Card + CrossRef records
├── Used by 10+ programs
└── Core data model definitions

CSSTRPFY (AID Keys)
├── Used by all online programs
├── PF key to boolean flag mapping
└── Standard CICS interaction pattern

CVTRA05Y + CVTRA06Y (Transaction Records)
├── Online + Batch transaction layouts
├── Mirror structures (same fields)
└── Used by 5+ programs
```

---

## Size Distribution

```
LOC Range          Copybooks  % of Total LOC
────────────────   ─────────  ─────────────
> 100 LOC          4          69.8%  (CSLKPCDY, CSUTLDPY, CVEXPORT, COMEN02Y)
50-100 LOC         5          12.5%  (CSUTLDWY, CSSTRPFY, COADM02Y, CSDAT01Y, CODATECN)
20-49 LOC          11         12.4%  (COCOM01Y, CVTRA07Y, CVCRD01Y, etc.)
< 20 LOC           9           5.3%  (CVACT01Y, CVTRA01Y-04Y, etc.)
```

---

## Cross-References

| Document | Relevance |
|----------|-----------|
| [DATA-DICTIONARY.md](../02-data-model/DATA-DICTIONARY.md) | Field-level definitions from copybooks |
| [DATA-MODEL.md](../02-data-model/DATA-MODEL.md) | VSAM record specifications |
| [PROGRAM-INVENTORY.md](./PROGRAM-INVENTORY.md) | Program-to-copybook usage |
| [COMMAREA-SPECIFICATION.md](../03-context-model/COMMAREA-SPECIFICATION.md) | COCOM01Y detailed analysis |
| [DOMAIN-MODEL.md](../01-domain-model/DOMAIN-MODEL.md) | Entity definitions from copybooks |

---

*Generated as part of the RE-000 Master Index for the CardDemo reverse engineering project.*
