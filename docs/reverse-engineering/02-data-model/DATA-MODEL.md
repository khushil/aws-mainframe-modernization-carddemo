# CardDemo Data Model

## Document Information

| Attribute | Value |
|-----------|-------|
| **Version** | 1.0 |
| **Last Updated** | 2026-02-05 |
| **Source Analysis** | RE-002 Data Model Extraction |
| **Input Artifacts** | 10 copybooks, 10 JCL definitions, 3 batch programs analyzed in detail |

---

## 1. Physical Data Model (VSAM Layer)

The CardDemo application uses VSAM (Virtual Storage Access Method) KSDS (Key-Sequenced Data Sets) for all persistent storage. This section documents the physical file layouts extracted from JCL definitions and copybook analysis.

### 1.1 Core Entity Files

These are the primary data stores representing the core business entities.

#### ACCTDAT - Account Master File

| Property | Value | Source |
|----------|-------|--------|
| **VSAM Type** | KSDS | ACCTFILE.jcl:44 |
| **Record Size** | 300 bytes (fixed) | CVACT01Y.cpy:2 |
| **Primary Key** | ACCT-ID (positions 1-11) | KEYS(11 0) |
| **Key Type** | PIC 9(11) - Numeric display | CVACT01Y.cpy:5 |
| **Dataset Name** | AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS | ACCTFILE.jcl:36 |
| **Alternate Index** | None | - |
| **Copybook** | CVACT01Y.cpy | app/cpy/ |

**Physical Layout:**
```
Offset  Length  Field                    PIC Clause
------  ------  ----------------------   ----------
0       11      ACCT-ID                  9(11)
11      1       ACCT-ACTIVE-STATUS       X(01)
12      12      ACCT-CURR-BAL            S9(10)V99
24      12      ACCT-CREDIT-LIMIT        S9(10)V99
36      12      ACCT-CASH-CREDIT-LIMIT   S9(10)V99
48      10      ACCT-OPEN-DATE           X(10)
58      10      ACCT-EXPIRAION-DATE      X(10)
68      10      ACCT-REISSUE-DATE        X(10)
78      12      ACCT-CURR-CYC-CREDIT     S9(10)V99
90      12      ACCT-CURR-CYC-DEBIT      S9(10)V99
102     10      ACCT-ADDR-ZIP            X(10)
112     10      ACCT-GROUP-ID            X(10)
122     178     FILLER                   X(178)
------  ------
TOTAL   300 bytes
```

#### CARDDAT - Card Master File

| Property | Value | Source |
|----------|-------|--------|
| **VSAM Type** | KSDS with AIX | CARDFILE.jcl:50-92 |
| **Record Size** | 150 bytes (fixed) | CVACT02Y.cpy:2 |
| **Primary Key** | CARD-NUM (positions 1-16) | KEYS(16 0) |
| **Key Type** | PIC X(16) - Alphanumeric | CVACT02Y.cpy:5 |
| **Dataset Name** | AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS | CARDFILE.jcl:50 |
| **Alternate Index** | CARD-ACCT-ID (positions 17-27, KEYS(11 16)) | CARDFILE.jcl:85 |
| **AIX Uniqueness** | NONUNIQUEKEY | CARDFILE.jcl:86 |
| **Copybook** | CVACT02Y.cpy | app/cpy/ |

**Physical Layout:**
```
Offset  Length  Field                    PIC Clause
------  ------  ----------------------   ----------
0       16      CARD-NUM                 X(16)
16      11      CARD-ACCT-ID             9(11)
27      3       CARD-CVV-CD              9(03)
30      50      CARD-EMBOSSED-NAME       X(50)
80      10      CARD-EXPIRAION-DATE      X(10)
90      1       CARD-ACTIVE-STATUS       X(01)
91      59      FILLER                   X(59)
------  ------
TOTAL   150 bytes
```

#### CUSTDAT - Customer Master File

| Property | Value | Source |
|----------|-------|--------|
| **VSAM Type** | KSDS | CUSTFILE.jcl:46 |
| **Record Size** | 500 bytes (fixed) | CVCUS01Y.cpy:2 |
| **Primary Key** | CUST-ID (positions 1-9) | KEYS(9 0) |
| **Key Type** | PIC 9(09) - Numeric display | CVCUS01Y.cpy:5 |
| **Dataset Name** | AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS | CUSTFILE.jcl:46 |
| **Alternate Index** | None | - |
| **Copybook** | CVCUS01Y.cpy | app/cpy/ |

> **Alternate Schema Note (2026-02-06):** A second customer VSAM definition exists in `app/jcl/DEFCUST.jcl` using different parameters:
> - **DSN:** `AWS.CCDA.CUSTDATA.CLUSTER` and `AWS.CUSTDATA.CLUSTER` (vs `AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS`)
> - **Key:** `KEYS(10 0)` (10-byte key at offset 0, vs 9-byte in CUSTFILE.jcl)
> - **SHAREOPTIONS:** `(1 4)` (vs `(2 3)` in CUSTFILE.jcl)
>
> This appears to represent an alternate environment configuration or an earlier version of the file layout. The 10-byte vs 9-byte key difference is significant — it suggests a different record format. CUSTFILE.jcl also includes CICS file close/open steps and data load from flat file, while DEFCUST.jcl does not.

**Physical Layout:**
```
Offset  Length  Field                    PIC Clause
------  ------  ----------------------   ----------
0       9       CUST-ID                  9(09)
9       25      CUST-FIRST-NAME          X(25)
34      25      CUST-MIDDLE-NAME         X(25)
59      25      CUST-LAST-NAME           X(25)
84      50      CUST-ADDR-LINE-1         X(50)
134     50      CUST-ADDR-LINE-2         X(50)
184     50      CUST-ADDR-LINE-3         X(50)
234     2       CUST-ADDR-STATE-CD       X(02)
236     3       CUST-ADDR-COUNTRY-CD     X(03)
239     10      CUST-ADDR-ZIP            X(10)
249     15      CUST-PHONE-NUM-1         X(15)
264     15      CUST-PHONE-NUM-2         X(15)
279     9       CUST-SSN                 9(09)
288     20      CUST-GOVT-ISSUED-ID      X(20)
308     10      CUST-DOB-YYYY-MM-DD      X(10)
318     10      CUST-EFT-ACCOUNT-ID      X(10)
328     1       CUST-PRI-CARD-HOLDER-IND X(01)
329     3       CUST-FICO-CREDIT-SCORE   9(03)
332     168     FILLER                   X(168)
------  ------
TOTAL   500 bytes
```

#### CCXREF - Card Cross-Reference File

| Property | Value | Source |
|----------|-------|--------|
| **VSAM Type** | KSDS with AIX | XREFFILE.jcl:39-102 |
| **Record Size** | 50 bytes (fixed) | CVACT03Y.cpy:2 |
| **Primary Key** | XREF-CARD-NUM (positions 1-16) | KEYS(16 0) |
| **Key Type** | PIC X(16) - Alphanumeric | CVACT03Y.cpy:5 |
| **Dataset Name** | AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS | XREFFILE.jcl:39 |
| **Alternate Index** | XREF-ACCT-ID (positions 26-36, KEYS(11,25)) | XREFFILE.jcl:74 |
| **AIX Uniqueness** | NONUNIQUEKEY | XREFFILE.jcl:75 |
| **Copybook** | CVACT03Y.cpy | app/cpy/ |

**Physical Layout:**
```
Offset  Length  Field                    PIC Clause
------  ------  ----------------------   ----------
0       16      XREF-CARD-NUM            X(16)
16      9       XREF-CUST-ID             9(09)
25      11      XREF-ACCT-ID             9(11)
36      14      FILLER                   X(14)
------  ------
TOTAL   50 bytes
```

#### TRANSACT - Transaction Master File

| Property | Value | Source |
|----------|-------|--------|
| **VSAM Type** | KSDS with AIX | TRANFILE.jcl:49-111 |
| **Record Size** | 350 bytes (fixed) | CVTRA05Y.cpy:2 |
| **Primary Key** | TRAN-ID (positions 1-16) | KEYS(16 0) |
| **Key Type** | PIC X(16) - Alphanumeric | CVTRA05Y.cpy:5 |
| **Dataset Name** | AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS | TRANFILE.jcl:49 |
| **Alternate Index** | TRAN-PROC-TS (positions 305-330, KEYS(26 304)) | TRANFILE.jcl:84 |
| **AIX Uniqueness** | NONUNIQUEKEY | TRANFILE.jcl:85 |
| **Copybook** | CVTRA05Y.cpy | app/cpy/ |

**Physical Layout:**
```
Offset  Length  Field                    PIC Clause
------  ------  ----------------------   ----------
0       16      TRAN-ID                  X(16)
16      2       TRAN-TYPE-CD             X(02)
18      4       TRAN-CAT-CD              9(04)
22      10      TRAN-SOURCE              X(10)
32      100     TRAN-DESC                X(100)
132     11      TRAN-AMT                 S9(09)V99
143     9       TRAN-MERCHANT-ID         9(09)
152     50      TRAN-MERCHANT-NAME       X(50)
202     50      TRAN-MERCHANT-CITY       X(50)
252     10      TRAN-MERCHANT-ZIP        X(10)
262     16      TRAN-CARD-NUM            X(16)
278     26      TRAN-ORIG-TS             X(26)
304     26      TRAN-PROC-TS             X(26)
330     20      FILLER                   X(20)
------  ------
TOTAL   350 bytes
```

#### USRSEC - User Security File

| Property | Value | Source |
|----------|-------|--------|
| **VSAM Type** | KSDS (CICS-managed) | DUSRSECJ.jcl |
| **Record Size** | 80 bytes (fixed) | CSUSR01Y.cpy:17-23 |
| **Primary Key** | SEC-USR-ID (positions 1-8) | - |
| **Key Type** | PIC X(08) - Alphanumeric | CSUSR01Y.cpy:18 |
| **Copybook** | CSUSR01Y.cpy | app/cpy/ |

**Physical Layout:**
```
Offset  Length  Field                    PIC Clause
------  ------  ----------------------   ----------
0       8       SEC-USR-ID               X(08)
8       20      SEC-USR-FNAME            X(20)
28      20      SEC-USR-LNAME            X(20)
48      8       SEC-USR-PWD              X(08)
56      1       SEC-USR-TYPE             X(01)
57      23      SEC-USR-FILLER           X(23)
------  ------
TOTAL   80 bytes
```

---

### 1.2 Lookup/Reference Files

These files contain static reference data for transaction processing.

#### TCATBALF - Transaction Category Balance File

| Property | Value | Source |
|----------|-------|--------|
| **VSAM Type** | KSDS | TCATBALF.jcl:36 |
| **Record Size** | 50 bytes (fixed) | CVTRA01Y.cpy:2 |
| **Primary Key** | Composite: ACCT-ID + TYPE-CD + CAT-CD (17 bytes) | KEYS(17 0) |
| **Dataset Name** | AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS | TCATBALF.jcl:36 |
| **Copybook** | CVTRA01Y.cpy | app/cpy/ |

**Physical Layout:**
```
Offset  Length  Field                    PIC Clause
------  ------  ----------------------   ----------
0       11      TRANCAT-ACCT-ID          9(11)
11      2       TRANCAT-TYPE-CD          X(02)
13      4       TRANCAT-CD               9(04)
17      11      TRAN-CAT-BAL             S9(09)V99
28      22      FILLER                   X(22)
------  ------
TOTAL   50 bytes
```

**Key Structure (TRAN-CAT-KEY):**
```
05 TRAN-CAT-KEY.
   10 TRANCAT-ACCT-ID    PIC 9(11).   -- Account identifier
   10 TRANCAT-TYPE-CD    PIC X(02).   -- Transaction type code
   10 TRANCAT-CD         PIC 9(04).   -- Transaction category code
```

#### DISCGRP - Disclosure Group File (Interest Rates)

| Property | Value | Source |
|----------|-------|--------|
| **VSAM Type** | KSDS | DISCGRP.jcl:36 |
| **Record Size** | 50 bytes (fixed) | CVTRA02Y.cpy:2 |
| **Primary Key** | Composite: GROUP-ID + TYPE-CD + CAT-CD (16 bytes) | KEYS(16 0) |
| **Dataset Name** | AWS.M2.CARDDEMO.DISCGRP.VSAM.KSDS | DISCGRP.jcl:36 |
| **Copybook** | CVTRA02Y.cpy | app/cpy/ |

**Physical Layout:**
```
Offset  Length  Field                    PIC Clause
------  ------  ----------------------   ----------
0       10      DIS-ACCT-GROUP-ID        X(10)
10      2       DIS-TRAN-TYPE-CD         X(02)
12      4       DIS-TRAN-CAT-CD          9(04)
16      6       DIS-INT-RATE             S9(04)V99
22      28      FILLER                   X(28)
------  ------
TOTAL   50 bytes
```

**Key Structure (DIS-GROUP-KEY):**
```
05 DIS-GROUP-KEY.
   10 DIS-ACCT-GROUP-ID  PIC X(10).   -- Account group (e.g., 'DEFAULT')
   10 DIS-TRAN-TYPE-CD   PIC X(02).   -- Transaction type
   10 DIS-TRAN-CAT-CD    PIC 9(04).   -- Transaction category
```

#### TRANTYPE - Transaction Type File

| Property | Value | Source |
|----------|-------|--------|
| **VSAM Type** | KSDS | TRANTYPE.jcl:36 |
| **Record Size** | 60 bytes (fixed) | CVTRA03Y.cpy:2 |
| **Primary Key** | TRAN-TYPE (2 bytes) | KEYS(2 0) |
| **Dataset Name** | AWS.M2.CARDDEMO.TRANTYPE.VSAM.KSDS | TRANTYPE.jcl:36 |
| **Copybook** | CVTRA03Y.cpy | app/cpy/ |

**Physical Layout:**
```
Offset  Length  Field                    PIC Clause
------  ------  ----------------------   ----------
0       2       TRAN-TYPE                X(02)
2       50      TRAN-TYPE-DESC           X(50)
52      8       FILLER                   X(08)
------  ------
TOTAL   60 bytes
```

#### TRANCATG - Transaction Category File

| Property | Value | Source |
|----------|-------|--------|
| **VSAM Type** | KSDS | TRANCATG.jcl:36 |
| **Record Size** | 60 bytes (fixed) | CVTRA04Y.cpy:2 |
| **Primary Key** | Composite: TYPE-CD + CAT-CD (6 bytes) | KEYS(6 0) |
| **Dataset Name** | AWS.M2.CARDDEMO.TRANCATG.VSAM.KSDS | TRANCATG.jcl:36 |
| **Copybook** | CVTRA04Y.cpy | app/cpy/ |

**Physical Layout:**
```
Offset  Length  Field                    PIC Clause
------  ------  ----------------------   ----------
0       2       TRAN-TYPE-CD             X(02)
2       4       TRAN-CAT-CD              9(04)
6       50      TRAN-CAT-TYPE-DESC       X(50)
56      4       FILLER                   X(04)
------  ------
TOTAL   60 bytes
```

---

### 1.3 Transient/Staging Files

These files are used for batch processing workflows.

#### DALYTRAN - Daily Transaction Input File

| Property | Value |
|----------|-------|
| **File Type** | Sequential (PS) |
| **Record Size** | 350 bytes |
| **Purpose** | Daily transaction input for posting |
| **Copybook** | CVTRA06Y.cpy |
| **Written By** | External transaction capture systems |
| **Read By** | CBTRN02C.cbl (Transaction Posting) |

**Note:** Same layout as TRANSACT (CVTRA05Y.cpy) but uses DALYTRAN- prefix in field names.

#### DALYREJS - Daily Rejects File

| Property | Value |
|----------|-------|
| **File Type** | Sequential (PS) |
| **Record Size** | 430 bytes (350 + 80 trailer) |
| **Purpose** | Rejected transactions with validation error messages |
| **Written By** | CBTRN02C.cbl |

**Layout:**
```
Offset  Length  Field                    Description
------  ------  ----------------------   -----------
0       350     REJECT-TRAN-DATA         Original transaction record
350     4       WS-VALIDATION-FAIL-REASON Error code (9(04))
354     76      WS-VALIDATION-FAIL-REASON-DESC Error description
```

---

## 2. Logical Data Model

### 2.1 Entity-Relationship Overview

The CardDemo application manages credit card accounts through the following entity relationships:

```
┌─────────────────────────────────────────────────────────────────────┐
│                        CUSTOMER (CUSTDAT)                           │
│  PK: CUST-ID                                                        │
│  Name, Address, Contact, SSN, DOB, Credit Score                     │
└───────────────────────────────┬─────────────────────────────────────┘
                                │ 1
                                │
                                │ N (via CCXREF)
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│                      CARD_XREF (CCXREF)                             │
│  PK: XREF-CARD-NUM                                                  │
│  FK: XREF-CUST-ID → CUSTOMER                                        │
│  FK: XREF-ACCT-ID → ACCOUNT                                         │
│  (Junction table linking cards to customers and accounts)           │
└───────────────────────────────┬─────────────────────────────────────┘
                                │
              ┌─────────────────┴─────────────────┐
              │                                   │
              │ N                                 │ 1
              ▼                                   ▼
┌─────────────────────────────────┐   ┌───────────────────────────────┐
│         CARD (CARDDAT)          │   │       ACCOUNT (ACCTDAT)       │
│  PK: CARD-NUM                   │   │  PK: ACCT-ID                  │
│  FK: CARD-ACCT-ID → ACCOUNT     │   │  Balance, Limits, Dates       │
│  CVV, Name, Expiry, Status      │   │  FK: ACCT-GROUP-ID → DISCGRP  │
└────────────────┬────────────────┘   └───────────────┬───────────────┘
                 │                                    │
                 │ 1                                  │ 1
                 │                                    │
                 │ N                                  │ N
                 ▼                                    ▼
┌─────────────────────────────────┐   ┌───────────────────────────────┐
│     TRANSACTION (TRANSACT)      │   │    TRAN_CAT_BAL (TCATBALF)    │
│  PK: TRAN-ID                    │   │  PK: ACCT-ID + TYPE + CAT     │
│  FK: TRAN-CARD-NUM → CARD       │   │  Running balance by category  │
│  Amount, Merchant, Timestamps   │   └───────────────────────────────┘
└─────────────────────────────────┘
```

### 2.2 Relationship Cardinalities

| Relationship | Type | Description |
|--------------|------|-------------|
| CUSTOMER → CCXREF | 1:N | One customer can have multiple cards |
| ACCOUNT → CCXREF | 1:N | One account can have multiple cards (joint/authorized users) |
| ACCOUNT → CARD | 1:N | One account can have multiple cards |
| CARD → TRANSACTION | 1:N | One card generates many transactions |
| ACCOUNT → TCATBALF | 1:N | One account has balances in multiple categories |
| DISCGRP → ACCOUNT | N:1 | Many accounts share same disclosure group |
| TRANTYPE → TRANSACTION | 1:N | One type code used by many transactions |
| TRANCATG → TRANSACTION | 1:N | One category used by many transactions |

### 2.3 Denormalization Analysis

#### CCXREF Junction Table

The CCXREF file is a **denormalized junction table** that stores:
- Card Number (primary key)
- Customer ID (foreign key)
- Account ID (foreign key)

**Current Structure Issues:**
1. Redundant storage of relationship data
2. No separate CUSTOMER_ACCOUNT association
3. Forces Card-centric access pattern

**Modernization Recommendation:**
In a normalized relational schema, this should be split into:
```sql
-- Normalized schema recommendation
CREATE TABLE CUSTOMER_ACCOUNT (
    customer_id     BIGINT NOT NULL,
    account_id      BIGINT NOT NULL,
    relationship_type VARCHAR(20),  -- PRIMARY, AUTHORIZED_USER, JOINT
    PRIMARY KEY (customer_id, account_id),
    FOREIGN KEY (customer_id) REFERENCES CUSTOMER(cust_id),
    FOREIGN KEY (account_id) REFERENCES ACCOUNT(acct_id)
);

CREATE TABLE CARD (
    card_num        VARCHAR(16) PRIMARY KEY,
    account_id      BIGINT NOT NULL,
    -- other card fields...
    FOREIGN KEY (account_id) REFERENCES ACCOUNT(acct_id)
);
```

---

## 3. Alternate Index Summary

| Base File | AIX Name | Key Position | Key Length | Uniqueness | Purpose |
|-----------|----------|--------------|------------|------------|---------|
| CARDDAT | CARDDATA.VSAM.AIX | 16 (CARD-ACCT-ID) | 11 bytes | Non-unique | Find cards by account |
| CCXREF | CARDXREF.VSAM.AIX | 25 (XREF-ACCT-ID) | 11 bytes | Non-unique | Find xrefs by account |
| TRANSACT | TRANSACT.VSAM.AIX | 304 (TRAN-PROC-TS) | 26 bytes | Non-unique | Date range queries |

---

## 4. File Access Patterns

### 4.1 Online (CICS) Access

| File | DD Name | Access Mode | Operations |
|------|---------|-------------|------------|
| ACCTDAT | ACCTFILE | Random/Sequential | READ, WRITE, REWRITE, DELETE |
| CARDDAT | CARDFILE | Random | READ, WRITE, REWRITE |
| CARDAIX | CARDAIX | Random (via path) | READ |
| CUSTDAT | CUSTFILE | Random/Sequential | READ, WRITE, REWRITE, DELETE |
| CCXREF | CARDXREF | Random | READ, WRITE, REWRITE |
| TRANSACT | TRANFILE | Random/Sequential | READ, WRITE |
| USRSEC | USRSEC | Random | READ |

### 4.2 Batch Access

| Program | Input Files | Output Files | Update Files |
|---------|-------------|--------------|--------------|
| CBTRN02C | DALYTRAN (seq), CCXREF, ACCTFILE | TRANSACT, DALYREJS | ACCTFILE, TCATBALF |
| CBTRN03C | TRANSACT (seq), CCXREF, TRANTYPE, TRANCATG | TRANREPT | - |
| CBACT04C | TCATBALF (seq), CCXREF, DISCGRP | TRANSACT | ACCTFILE |

---

## 5. Data Integrity Constraints

### 5.1 Referential Integrity (Application-Enforced)

| Parent Entity | Child Entity | Enforced By |
|---------------|--------------|-------------|
| ACCOUNT | CARD | CARD-ACCT-ID validation in online programs |
| CARD | TRANSACTION | XREF lookup in CBTRN02C validation |
| CUSTOMER | CCXREF | Not explicitly enforced |
| TRANTYPE | TRANSACTION | Lookup in reporting programs |
| TRANCATG | TRANSACTION | Lookup in reporting programs |

### 5.2 Business Rules (Embedded in Code)

| Rule | Location | Description |
|------|----------|-------------|
| Credit Limit Check | CBTRN02C:403-413 | ACCT-CREDIT-LIMIT >= computed balance |
| Account Expiration | CBTRN02C:414-420 | ACCT-EXPIRAION-DATE >= transaction date |
| Card Validity | CBTRN02C:380-392 | Card must exist in CCXREF |

---

## 6. Storage Estimates

| File | Record Size | Est. Records | Est. Storage |
|------|-------------|--------------|--------------|
| ACCTDAT | 300 bytes | 10,000 | ~3 MB |
| CARDDAT | 150 bytes | 25,000 | ~4 MB |
| CUSTDAT | 500 bytes | 8,000 | ~4 MB |
| CCXREF | 50 bytes | 25,000 | ~1.25 MB |
| TRANSACT | 350 bytes | 1,000,000 | ~350 MB |
| TCATBALF | 50 bytes | 100,000 | ~5 MB |
| DISCGRP | 50 bytes | 500 | ~25 KB |
| TRANTYPE | 60 bytes | 20 | ~1.2 KB |
| TRANCATG | 60 bytes | 200 | ~12 KB |
| USRSEC | 80 bytes | 100 | ~8 KB |

---

## 7. Technical Notes

### 7.1 Numeric Storage Format

CardDemo uses **DISPLAY format** (not COMP-3) for all signed decimal fields:

```cobol
05  ACCT-CURR-BAL     PIC S9(10)V99.   -- 12 bytes character storage
```

**Important:** In DISPLAY format:
- `S9(10)V99` uses 12 bytes (10 + 2 decimal positions)
- The sign is stored in the zone nibble of the last byte
- This is NOT the same as COMP-3/packed decimal

### 7.2 Date/Timestamp Formats

| Format | Example | Usage |
|--------|---------|-------|
| Date | `YYYY-MM-DD` | ACCT-OPEN-DATE, etc. (10 bytes) |
| Timestamp | `YYYY-MM-DD-HH.MM.SS.nnnnnn` | TRAN-ORIG-TS, TRAN-PROC-TS (26 bytes) |

### 7.3 Key Generation

Transaction IDs are generated by concatenating:
```cobol
STRING PARM-DATE,           -- 10 bytes (YYYY-MM-DD)
       WS-TRANID-SUFFIX     -- 6 bytes (sequential counter)
  DELIMITED BY SIZE
  INTO TRAN-ID              -- 16 bytes total
```

---

## 8. Data File Inventory

The following data files are included in the repository for initial data load and testing.

### 8.1 ASCII Data Files (`app/data/ASCII/`)

These are human-readable text-format equivalents of the VSAM data, used for reference and EBCDIC conversion.

| File | Corresponding Entity | Description |
|------|---------------------|-------------|
| acctdata.txt | ACCOUNT (ACCTDAT) | Account master records |
| carddata.txt | CARD (CARDDAT) | Card master records |
| cardxref.txt | CARD_XREF (CCXREF) | Card-to-account cross-references |
| custdata.txt | CUSTOMER (CUSTDAT) | Customer master records |
| dailytran.txt | DALYTRAN | Daily transaction input |
| discgrp.txt | DISCLOSURE_GROUP (DISCGRP) | Interest rate disclosure groups |
| tcatbal.txt | TRAN_CAT_BAL (TCATBALF) | Transaction category balances |
| trancatg.txt | TRAN_CATEGORY (TRANCATG) | Transaction category definitions |
| trantype.txt | TRAN_TYPE (TRANTYPE) | Transaction type definitions |

**Total: 9 files**

### 8.2 EBCDIC Data Files (`app/data/EBCDIC/`)

These are mainframe-format flat files used by JCL REPRO jobs to load VSAM clusters.

| File | Corresponding Entity | Description |
|------|---------------------|-------------|
| AWS.M2.CARDDEMO.ACCDATA.PS | ACCOUNT (ACCTDAT) | Account data (alternate name) |
| AWS.M2.CARDDEMO.ACCTDATA.PS | ACCOUNT (ACCTDAT) | Account master data |
| AWS.M2.CARDDEMO.CARDDATA.PS | CARD (CARDDAT) | Card master data |
| AWS.M2.CARDDEMO.CARDXREF.PS | CARD_XREF (CCXREF) | Card cross-reference data |
| AWS.M2.CARDDEMO.CUSTDATA.PS | CUSTOMER (CUSTDAT) | Customer master data |
| AWS.M2.CARDDEMO.DALYTRAN.PS | DALYTRAN | Daily transaction input |
| AWS.M2.CARDDEMO.DALYTRAN.PS.INIT | DALYTRAN | Empty daily transaction (reset) |
| AWS.M2.CARDDEMO.DISCGRP.PS | DISCLOSURE_GROUP (DISCGRP) | Disclosure group data |
| AWS.M2.CARDDEMO.EXPORT.DATA.PS | N/A | Exported data (batch output) |
| AWS.M2.CARDDEMO.TCATBALF.PS | TRAN_CAT_BAL (TCATBALF) | Transaction category balances |
| AWS.M2.CARDDEMO.TRANCATG.PS | TRAN_CATEGORY (TRANCATG) | Transaction category definitions |
| AWS.M2.CARDDEMO.TRANTYPE.PS | TRAN_TYPE (TRANTYPE) | Transaction type definitions |
| AWS.M2.CARDDEMO.USRSEC.PS | USER_SECURITY (USRSEC) | User security records |

**Total: 13 files**

---

## References

| Source | Purpose |
|--------|---------|
| app/cpy/*.cpy | Copybook record layouts |
| app/jcl/*.jcl | VSAM cluster definitions |
| app/cbl/CBTRN02C.cbl | Transaction posting batch |
| app/cbl/CBTRN03C.cbl | Transaction reporting |
| app/cbl/CBACT04C.cbl | Interest calculation |
