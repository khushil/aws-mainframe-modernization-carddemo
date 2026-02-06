# CardDemo File Inventory

**Document Version:** 1.0
**Generated:** 2026-02-05
**Total VSAM Clusters:** 10 (6 core + 4 reference)
**Total Alternate Indexes:** 3
**Total GDG Bases:** 11

---

## Summary

| Category | Count | Description |
|----------|-------|-------------|
| Core VSAM KSDS | 6 | Primary business data (Account, Card, Customer, Transaction, CrossRef, UserSec) |
| Reference VSAM KSDS | 4 | Configuration/reference data (TranType, TranCategory, CategoryBalance, DisclosureGroup) |
| Alternate Indexes | 3 | Secondary access paths (Card→Account, CrossRef→Account, Transaction→Timestamp) |
| GDG Bases | 11 | Generation data groups for backup, reporting, and daily processing |
| Export VSAM | 1 | Multi-record export file |

---

## Core VSAM Files

### ACCTDAT - Account Master

| Property | Value |
|----------|-------|
| **Dataset Name** | `AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS` |
| **CICS File Name** | ACCTDAT |
| **Type** | KSDS |
| **Record Size** | 300 bytes (fixed) |
| **Primary Key** | ACCT-ID, 11 bytes at offset 0 |
| **Key Type** | PIC 9(11) - Numeric display |
| **Alternate Index** | None |
| **Copybook** | CVACT01Y |
| **JCL Definition** | ACCTFILE.jcl |
| **Access Programs** | COACTVWC (R), COACTUPC (RW), COBIL00C (RW), COTRN02C (RW), CBTRN02C (RW), CBACT04C (R), CBACT01C (R) |

**Key Fields:**

| Offset | Length | Field | Type | Description |
|--------|--------|-------|------|-------------|
| 0 | 11 | ACCT-ID | 9(11) | Account identifier (PK) |
| 11 | 1 | ACCT-ACTIVE-STATUS | X(01) | Y=Active, N=Inactive |
| 12 | 12 | ACCT-CURR-BAL | S9(10)V99 | Current balance |
| 24 | 12 | ACCT-CREDIT-LIMIT | S9(10)V99 | Credit limit |
| 36 | 12 | ACCT-CASH-CREDIT-LIMIT | S9(10)V99 | Cash advance limit |
| 48 | 10 | ACCT-OPEN-DATE | X(10) | Account opening date |
| 58 | 10 | ACCT-EXPIRAION-DATE | X(10) | Account expiration |
| 68 | 10 | ACCT-REISSUE-DATE | X(10) | Reissue date |
| 78 | 12 | ACCT-CURR-CYC-CREDIT | S9(10)V99 | Current cycle credits |
| 90 | 12 | ACCT-CURR-CYC-DEBIT | S9(10)V99 | Current cycle debits |
| 102 | 10 | ACCT-ADDR-ZIP | X(10) | Address ZIP code |
| 112 | 10 | ACCT-GROUP-ID | X(10) | Disclosure group ID |
| 122 | 178 | FILLER | X(178) | Reserved |

---

### CARDDAT - Card Master

| Property | Value |
|----------|-------|
| **Dataset Name** | `AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS` |
| **CICS File Name** | CARDDAT |
| **Type** | KSDS with Alternate Index |
| **Record Size** | 150 bytes (fixed) |
| **Primary Key** | CARD-NUM, 16 bytes at offset 0 |
| **Key Type** | PIC X(16) - Alphanumeric |
| **Alternate Index** | CARDAIX: CARD-ACCT-ID, 11 bytes at offset 16 (NONUNIQUEKEY) |
| **AIX Path** | `AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX.PATH` |
| **Copybook** | CVACT02Y |
| **JCL Definition** | CARDFILE.jcl |
| **Access Programs** | COCRDSLC (R), COCRDUPC (RW), COCRDLIC (R via AIX), COACTVWC (R) |

**Key Fields:**

| Offset | Length | Field | Type | Description |
|--------|--------|-------|------|-------------|
| 0 | 16 | CARD-NUM | X(16) | Card number (PK) |
| 16 | 11 | CARD-ACCT-ID | 9(11) | Account ID (AIX key) |
| 27 | 3 | CARD-CVV-CD | 9(03) | CVV code (security concern) |
| 30 | 50 | CARD-EMBOSSED-NAME | X(50) | Name on card |
| 80 | 10 | CARD-EXPIRAION-DATE | X(10) | Expiration date |
| 90 | 1 | CARD-ACTIVE-STATUS | X(01) | Y=Active, N=Inactive |
| 91 | 59 | FILLER | X(59) | Reserved |

---

### CUSTDAT - Customer Master

| Property | Value |
|----------|-------|
| **Dataset Name** | `AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS` |
| **CICS File Name** | CUSTDAT |
| **Type** | KSDS |
| **Record Size** | 500 bytes (fixed) |
| **Primary Key** | CUST-ID, 9 bytes at offset 0 |
| **Key Type** | PIC 9(09) - Numeric display |
| **Alternate Index** | None |
| **Copybook** | CVCUS01Y / CUSTREC |
| **JCL Definition** | CUSTFILE.jcl |
| **Access Programs** | COACTVWC (R), COACTUPC (RW), CBCUS01C (R) |

**Key Fields:**

| Offset | Length | Field | Type | Description |
|--------|--------|-------|------|-------------|
| 0 | 9 | CUST-ID | 9(09) | Customer ID (PK) |
| 9 | 25 | CUST-FIRST-NAME | X(25) | First name |
| 34 | 25 | CUST-MIDDLE-NAME | X(25) | Middle name |
| 59 | 25 | CUST-LAST-NAME | X(25) | Last name |
| 84 | 50 | CUST-ADDR-LINE-1 | X(50) | Address line 1 |
| 134 | 50 | CUST-ADDR-LINE-2 | X(50) | Address line 2 |
| 184 | 50 | CUST-ADDR-LINE-3 | X(50) | Address line 3 |
| 234 | 2 | CUST-ADDR-STATE-CD | X(02) | State code |
| 236 | 3 | CUST-ADDR-COUNTRY-CD | X(03) | Country code |
| 239 | 10 | CUST-ADDR-ZIP | X(10) | ZIP code |
| 249 | 15 | CUST-PHONE-NUM-1 | X(15) | Primary phone |
| 264 | 15 | CUST-PHONE-NUM-2 | X(15) | Secondary phone |
| 279 | 9 | CUST-SSN | 9(09) | Social Security Number |
| 288 | 20 | CUST-GOVT-ISSUED-ID | X(20) | Government ID |
| 308 | 10 | CUST-DOB-YYYYMMDD | X(10) | Date of birth |
| 318 | 10 | CUST-EFT-ACCOUNT-ID | X(10) | EFT account reference |
| 328 | 1 | CUST-PRI-CARD-HLDR-IND | X(01) | Primary cardholder flag |
| 329 | 3 | CUST-FICO-CREDIT-SCORE | 9(03) | FICO score (300-850) |

---

### TRANSACT - Transaction Master

| Property | Value |
|----------|-------|
| **Dataset Name** | `AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS` |
| **CICS File Name** | TRANSACT |
| **Type** | KSDS with Alternate Index |
| **Record Size** | 350 bytes (fixed) |
| **Primary Key** | TRAN-ID, 16 bytes at offset 0 |
| **Key Type** | PIC X(16) - Alphanumeric |
| **Alternate Index** | TRAN-PROC-TS, 26 bytes at offset 304 (Processed Timestamp) |
| **AIX Path** | `AWS.M2.CARDDEMO.TRANSACT.VSAM.AIX.PATH` |
| **Copybook** | CVTRA05Y |
| **JCL Definition** | TRANFILE.jcl |
| **Access Programs** | COTRN00C (R), COTRN01C (R), COTRN02C (RW), COBIL00C (RW), CBTRN02C (RW) |

---

### CCXREF - Card Cross-Reference

| Property | Value |
|----------|-------|
| **Dataset Name** | `AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS` |
| **CICS File Name** | CCXREF |
| **Type** | KSDS with Alternate Index |
| **Record Size** | 50 bytes (fixed) |
| **Primary Key** | XREF-CARD-NUM, 16 bytes at offset 0 |
| **Alternate Index** | CXACAIX: XREF-ACCT-ID, 11 bytes at offset 25 (NONUNIQUEKEY) |
| **AIX CICS Name** | CXACAIX |
| **Copybook** | CVACT03Y |
| **JCL Definition** | XREFFILE.jcl |
| **Access Programs** | COBIL00C (R via AIX), COTRN02C (R), CBTRN02C (R), CBACT04C (R via AIX) |

**Key Fields:**

| Offset | Length | Field | Type | Description |
|--------|--------|-------|------|-------------|
| 0 | 16 | XREF-CARD-NUM | X(16) | Card number (PK) |
| 16 | 9 | XREF-CUST-ID | 9(09) | Customer ID (FK) |
| 25 | 11 | XREF-ACCT-ID | 9(11) | Account ID (AIX key, FK) |

---

### USRSEC - User Security

| Property | Value |
|----------|-------|
| **Dataset Name** | `AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS` |
| **CICS File Name** | USRSEC |
| **Type** | KSDS |
| **Record Size** | 80 bytes (fixed) |
| **Primary Key** | SEC-USR-ID, 8 bytes at offset 0 |
| **Key Type** | PIC X(08) - Alphanumeric |
| **Copybook** | CSUSR01Y |
| **JCL Definition** | DUSRSECJ.jcl |
| **Access Programs** | COSGN00C (R), COUSR00C (R), COUSR01C (RW), COUSR02C (RW), COUSR03C (RW) |
| **Default Records** | 10 users (5 admin + 5 regular), all with password "PASSWORD" |

---

## Reference VSAM Files

| # | CICS Name | Dataset Pattern | Key | Record Size | Copybook | Purpose |
|---|-----------|-----------------|-----|-------------|----------|---------|
| 7 | TRANTYPE | `*.TRANTYPE.VSAM.KSDS` | 2 bytes at offset 0 | 60 bytes | CVTRA03Y | Transaction type codes |
| 8 | TRANCATG | `*.TRANCATG.VSAM.KSDS` | 6 bytes at offset 0 | 60 bytes | CVTRA04Y | Transaction category codes |
| 9 | TCATBALF | `*.TCATBALF.VSAM.KSDS` | 17 bytes at offset 0 | 50 bytes | CVTRA01Y | Transaction category balances |
| 10 | DISCGRP | `*.DISCGRP.VSAM.KSDS` | 16 bytes at offset 0 | 50 bytes | CVTRA02Y | Disclosure group interest rates |

---

## Alternate Index Summary

| AIX Name | Base File | AIX Key | Length | Offset | Unique | CICS Path Name |
|----------|-----------|---------|--------|--------|--------|---------------|
| CARDAIX | CARDDAT | CARD-ACCT-ID | 11 | 16 | No | CARDAIX |
| CXACAIX | CCXREF | XREF-ACCT-ID | 11 | 25 | No | CXACAIX |
| TRANSACT AIX | TRANSACT | TRAN-PROC-TS | 26 | 304 | No | (batch only) |

---

## GDG Bases

| GDG Base | Limit | Purpose | Created By |
|----------|-------|---------|-----------|
| `*.TRANSACT.BKUP` | 5 | Transaction backup | TRANBKP |
| `*.TRANSACT.DALY` | 5 | Daily transaction extract | POSTTRAN |
| `*.TRANREPT` | 10 | Transaction reports | TRANREPT |
| `*.TCATBALF.BKUP` | 5 | Category balance backup | DEFGDGB |
| `*.SYSTRAN` | 5 | System-generated transactions | INTCALC |
| `*.TRANSACT.COMBINED` | 5 | Combined transactions | COMBTRAN |
| `*.TRANTYPE.BKUP` | 5 | Transaction type backup | DEFGDGD |
| `*.TRANCATG.PS.BKUP` | 5 | Transaction category backup | DEFGDGD |
| `*.DISCGRP.BKUP` | 5 | Disclosure group backup | DEFGDGD |
| `*.DALYREJS` | 5 | Daily rejects | POSTTRAN |
| `*.TRANREPT` | 10 | Report output | REPTFILE |

---

## CICS File-to-Dataset Mapping

| CICS Name | Physical Dataset | Type |
|-----------|------------------|------|
| ACCTDAT | `AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS` | KSDS |
| CARDDAT | `AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS` | KSDS |
| CARDAIX | `AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX.PATH` | AIX Path |
| CUSTDAT | `AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS` | KSDS |
| TRANSACT | `AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS` | KSDS |
| CCXREF | `AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS` | KSDS |
| CXACAIX | `AWS.M2.CARDDEMO.CARDXREF.VSAM.AIX.PATH` | AIX Path |
| USRSEC | `AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS` | KSDS |

---

## Cross-References

| Document | Relevance |
|----------|-----------|
| [DATA-MODEL.md](../02-data-model/DATA-MODEL.md) | Complete VSAM schema documentation |
| [DATA-DICTIONARY.md](../02-data-model/DATA-DICTIONARY.md) | Field-level definitions |
| [DATA-LINEAGE.md](../02-data-model/DATA-LINEAGE.md) | Data flow between files |
| [BATCH-WORKFLOWS.md](../05-specialized/BATCH-WORKFLOWS.md) | JCL job definitions for each file |
| [PROGRAM-INVENTORY.md](./PROGRAM-INVENTORY.md) | Program-to-file access matrix |

---

*Generated as part of the RE-000 Master Index for the CardDemo reverse engineering project.*
