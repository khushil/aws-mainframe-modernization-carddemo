# CardDemo Test Coverage Analysis

## Document Information

| Attribute | Value |
|-----------|-------|
| **Version** | 1.0 |
| **Last Updated** | 2026-02-05 |
| **Source Analysis** | RE-009 Test Coverage Analysis |
| **Input Artifacts** | 9 test data files, 39 COBOL programs, 41 copybooks |

---

## 1. Executive Summary

This document provides a comprehensive analysis of the test data assets available in the CardDemo application. The analysis reveals **626 total test records** across 9 data files, with significant coverage of core entities but notable gaps in edge case and exception scenarios.

### Key Findings

| Metric | Value | Assessment |
|--------|-------|------------|
| Total Test Records | 626 | Adequate for basic testing |
| Entity Coverage | 5/5 core entities | Complete |
| Transaction Type Coverage | 3/7 types | **Partial** |
| Edge Case Coverage | Limited | **Needs expansion** |
| Authentication Test Data | Missing | **Critical gap** |

### Coverage Summary

```
Coverage Heat Map:
+-------------------+--------+----------+------------+
| Area              | Have   | Gap      | Severity   |
+-------------------+--------+----------+------------+
| Accounts          | 50     | Inactive | CRITICAL   |
| Cards             | 50     | Expired  | MEDIUM     |
| Customers         | 50     | Intl     | MEDIUM     |
| Transactions      | 300    | Declined | HIGH       |
| Authentication    | 0      | All      | CRITICAL   |
+-------------------+--------+----------+------------+
```

---

## 2. Test Data Inventory

### 2.1 Source File Summary

| File | Entity | Records | Key Field | Format | Quality |
|------|--------|---------|-----------|--------|---------|
| acctdata.txt | Account | 50 | ACCT-ID (11 digits) | Fixed 300 bytes | Good |
| carddata.txt | Card | 50 | CARD-NUM (16 digits) | Fixed 150 bytes | Good |
| custdata.txt | Customer | 50 | CUST-ID (9 digits) | Fixed 500 bytes | Good |
| cardxref.txt | Cross-Reference | 50 | XREF-CARD-NUM | Fixed 50 bytes | Good |
| dailytran.txt | Daily Transaction | 300 | TRAN-ID (16 digits) | Fixed 350 bytes | Good |
| trantype.txt | Transaction Type | 7 | TYPE-CD (2 digits) | Fixed 60 bytes | Complete |
| trancatg.txt | Transaction Category | 18 | CAT-CD (6 digits) | Fixed 60 bytes | Complete |
| discgrp.txt | Discount Group | 51 | GROUP-KEY | Fixed 50 bytes | Good |
| tcatbal.txt | Category Balance | 50 | ACCT+TYPE+CAT | Fixed 50 bytes | Good |

**Total: 626 records across 9 files**

---

### 2.2 Detailed File Analysis

#### 2.2.1 Account Data (acctdata.txt)

**Record Layout:** CVACT01Y.cpy (300 bytes)

| Field | Position | PIC | Test Data Range | Quality |
|-------|----------|-----|-----------------|---------|
| ACCT-ID | 1-11 | 9(11) | 00000000001 - 00000000050 | Sequential, no gaps |
| ACCT-ACTIVE-STATUS | 12 | X(01) | 'Y' only | **Gap: No 'N' values** |
| ACCT-CURR-BAL | 13-24 | S9(10)V99 | 0.20 - 8,430.00 | Good variety |
| ACCT-CREDIT-LIMIT | 25-36 | S9(10)V99 | 1,200.00 - 97,500.00 | Good variety |
| ACCT-CASH-CREDIT-LIMIT | 37-48 | S9(10)V99 | 93.00 - 65,820.00 | Good variety |
| ACCT-OPEN-DATE | 49-58 | X(10) | 2009-04-20 - 2019-04-06 | Historical range |
| ACCT-EXPIRAION-DATE | 59-68 | X(10) | 2023-01-06 - 2025-12-28 | Some expired |
| ACCT-REISSUE-DATE | 69-78 | X(10) | Same as expiration | Matches |
| ACCT-CURR-CYC-CREDIT | 79-90 | S9(10)V99 | 0.00 | All zeros |
| ACCT-CURR-CYC-DEBIT | 91-102 | S9(10)V99 | 0.00 | All zeros |
| ACCT-GROUP-ID | 113-122 | X(10) | 'A000000000' | Single value |

**Observations:**
- All 50 accounts are ACTIVE (status='Y')
- Account IDs are sequential 1-50
- Cycle credit/debit fields are all zeros
- Single group ID value limits APR testing scenarios

**Recommendations:**
- Add inactive accounts (status='N') for deactivation testing
- Add accounts with non-zero cycle balances
- Add varied group IDs for APR lookup testing

---

#### 2.2.2 Card Data (carddata.txt)

**Record Layout:** CVACT02Y.cpy (150 bytes)

| Field | Position | PIC | Test Data Range | Quality |
|-------|----------|-----|-----------------|---------|
| CARD-NUM | 1-16 | X(16) | 16-digit strings | Realistic format |
| CARD-ACCT-ID | 17-27 | 9(11) | 00000000001-00000000050 | Links to accounts |
| CARD-CVV-CD | 28-30 | 9(03) | 3-digit codes | Within 000-999 |
| CARD-EMBOSSED-NAME | 31-80 | X(50) | Names like "Aniya Von" | Good variety |
| CARD-EXPIRAION-DATE | 81-90 | X(10) | 2023-01-06 - 2025-12-28 | Some expired |
| CARD-ACTIVE-STATUS | 91 | X(01) | 'Y' only | **Gap: No 'N' values** |

**Sample Card Numbers:**
```
0500024453765740  - Linked to Account 50
0683586198171516  - Linked to Account 27
0923877193247330  - Linked to Account 2
```

**Observations:**
- All 50 cards are ACTIVE (status='Y')
- Card numbers appear realistic (16 digits)
- Embossed names are properly formatted
- CVV codes within valid range

**Recommendations:**
- Add inactive cards for card blocking tests
- Add cards with past expiration dates marked inactive
- Add cards with special characters in names (edge case)

---

#### 2.2.3 Customer Data (custdata.txt)

**Record Layout:** CVCUS01Y.cpy (500 bytes)

| Field | Position | PIC | Test Data Range | Quality |
|-------|----------|-----|-----------------|---------|
| CUST-ID | 1-9 | 9(09) | 000000001 - 000000050 | Sequential |
| CUST-FIRST-NAME | 10-34 | X(25) | Various names | Good variety |
| CUST-MIDDLE-NAME | 35-59 | X(25) | Various names | Good variety |
| CUST-LAST-NAME | 60-84 | X(25) | Various names | Good variety |
| CUST-ADDR-LINE-1 | 85-134 | X(50) | Street addresses | Good variety |
| CUST-ADDR-LINE-2 | 135-184 | X(50) | "Apt.", "Suite" | Good variety |
| CUST-ADDR-LINE-3 | 185-234 | X(50) | City names | Good variety |
| CUST-ADDR-STATE-CD | 235-236 | X(02) | US state codes | Valid codes |
| CUST-ADDR-COUNTRY-CD | 237-239 | X(03) | 'USA' only | **Gap: No intl** |
| CUST-ADDR-ZIP | 240-249 | X(10) | 5-digit + ext | Valid format |
| CUST-PHONE-NUM-1 | 250-264 | X(15) | (xxx)xxx-xxxx | Valid format |
| CUST-PHONE-NUM-2 | 265-279 | X(15) | (xxx)xxx-xxxx | Valid format |
| CUST-SSN | 280-288 | 9(09) | 9-digit numbers | Valid format |
| CUST-DOB-YYYY-MM-DD | 309-318 | X(10) | 1960-2001 | Good range |
| CUST-FICO-CREDIT-SCORE | 330-332 | 9(03) | Various scores | Good variety |

**State Code Distribution:**
```
AL, AK, AR, AS, AP, CA, CT, DE, FM, GA, GU, IA, ID, IL, IN, KS, LA,
MD, ME, MH, MI, MN, MS, NC, NH, NV, OK, OR, PW, RI, SC, TX, VA, VT, WA
```

**Observations:**
- All customers are US-based (country='USA')
- Good distribution of US state codes
- Phone numbers follow NANPA format
- SSN values appear realistic (no 000, 666, or 900+ prefixes)
- FICO scores span reasonable range

**Recommendations:**
- Add international customers for address validation testing
- Add customers with edge case SSNs (near boundaries)
- Add customers with special characters in names

---

#### 2.2.4 Cross-Reference Data (cardxref.txt)

**Record Layout:** CVACT03Y.cpy (50 bytes)

| Field | Position | PIC | Test Data Range | Quality |
|-------|----------|-----|-----------------|---------|
| XREF-CARD-NUM | 1-16 | X(16) | Matches carddata.txt | Complete linkage |
| XREF-CUST-ID | 17-25 | 9(09) | 00000001-00000050 | Links to customers |
| XREF-ACCT-ID | 26-36 | 9(11) | 00000000001-50 | Links to accounts |

**Linkage Analysis:**
```
Card → Account → Customer (1:1:1 mapping in test data)
Example: Card 0500024453765740 → Account 50 → Customer 50
```

**Observations:**
- Perfect 1:1:1 mapping (no multiple cards per account)
- All cards linked to valid accounts and customers
- No orphan records

**Recommendations:**
- Add scenarios with multiple cards per account
- Add scenarios with multiple accounts per customer
- Test orphan handling (card without account)

---

#### 2.2.5 Daily Transaction Data (dailytran.txt)

**Record Layout:** CVTRA05Y.cpy (350 bytes)

| Field | Position | PIC | Test Data Range | Quality |
|-------|----------|-----|-----------------|---------|
| TRAN-ID | 1-16 | X(16) | Unique 16-digit IDs | Sequential |
| TRAN-TYPE-CD | 17-18 | X(02) | '01', '02', '03' only | **Gap: Missing 04-07** |
| TRAN-CAT-CD | 19-22 | 9(04) | 0001 | Single category |
| TRAN-SOURCE | 23-32 | X(10) | 'POS TERM', 'OPERATOR' | Two sources |
| TRAN-DESC | 33-132 | X(100) | Purchase descriptions | Good variety |
| TRAN-AMT | 133-143 | S9(09)V99 | Various amounts | Signed values |
| TRAN-MERCHANT-ID | 144-152 | 9(09) | 800000000 | Single value |
| TRAN-MERCHANT-NAME | 153-202 | X(50) | Business names | Good variety |
| TRAN-MERCHANT-CITY | 203-252 | X(50) | City names | Good variety |
| TRAN-MERCHANT-ZIP | 253-262 | X(10) | ZIP codes | Valid format |
| TRAN-CARD-NUM | 263-278 | X(16) | Links to cards | Valid linkage |
| TRAN-ORIG-TS | 279-304 | X(26) | 2022-06-10 | Single date |
| TRAN-PROC-TS | 305-330 | X(26) | Blank | Not populated |

**Transaction Type Distribution:**

| Type | Code | Count | Description |
|------|------|-------|-------------|
| Purchase | 01 | ~200 | Sales drafts, POS transactions |
| Payment | 02 | ~50 | Refunds, credits |
| Credit | 03 | ~50 | Adjustments |
| Authorization | 04 | 0 | **Not represented** |
| Refund | 05 | 0 | **Not represented** |
| Reversal | 06 | 0 | **Not represented** |
| Adjustment | 07 | 0 | **Not represented** |

**Observations:**
- All transactions dated 2022-06-10 (single day)
- Only 3 of 7 transaction types represented
- No declined or rejected transactions
- Single merchant ID value

**Recommendations:**
- Add transactions spanning multiple dates
- Add type 04-07 transactions
- Add declined/rejected transaction scenarios
- Vary merchant IDs for better coverage

---

#### 2.2.6 Reference Data Files

##### Transaction Types (trantype.txt)

| Code | Description | Test Coverage |
|------|-------------|---------------|
| 01 | Purchase | Covered |
| 02 | Payment | Covered |
| 03 | Credit | Covered |
| 04 | Authorization | **Not covered** |
| 05 | Refund | **Not covered** |
| 06 | Reversal | **Not covered** |
| 07 | Adjustment | **Not covered** |

##### Transaction Categories (trancatg.txt)

18 categories defined covering:
- Purchase subcategories (010001-010005)
- Payment subcategories (020001-020003)
- Credit subcategories (030001-030003)
- Authorization subcategories (040001-040003)
- Refund subcategory (050001)
- Reversal subcategories (060001-060002)
- Adjustment subcategory (070001)

##### Discount Groups (discgrp.txt)

| Group | Entries | APR Range |
|-------|---------|-----------|
| A00000000001-A00000000007 | 17 | 0.00% - 2.50% |
| DEFAULT | 17 | 0.00% - 2.50% |
| ZEROAPR | 17 | 0.00% only |

---

## 3. Coverage Assessment Matrix

### 3.1 Entity Coverage

| Entity | Records | Key Range | Status Variety | Edge Cases |
|--------|---------|-----------|----------------|------------|
| Account | 50 | 1-50 | Active only | **Missing: Inactive, Expired** |
| Card | 50 | 50 unique | Active only | **Missing: Blocked, Expired** |
| Customer | 50 | 1-50 | Active | **Missing: International** |
| Transaction | 300 | Unique IDs | Types 01-03 | **Missing: 04-07, Declined** |
| Cross-Ref | 50 | 1:1:1 | Valid links | **Missing: Multi-card** |

### 3.2 Functional Coverage by Program

| Program | Function | Test Data Coverage | Gap |
|---------|----------|-------------------|-----|
| COSGN00C | Sign-on | **0%** | No USRSEC.txt data |
| COADM01C | Admin Menu | Partial | Admin user data missing |
| COBIL00C | Bill Payment | 80% | Balance edge cases |
| COCRDSLC | Card Search | 90% | Inactive card cases |
| COCRDUPC | Card Update | 70% | Status change cases |
| COACTVWC | Account View | 90% | Inactive account cases |
| COTRN00C | Transaction List | 60% | Multi-page pagination |
| CBTRN01C | Interest Calc | 70% | Group variety needed |
| CBTRN02C | Transaction Post | 60% | Rejection scenarios |

### 3.3 Transaction Type Coverage

| Type Code | Type Name | Daily Trans | Categories | Coverage |
|-----------|-----------|-------------|------------|----------|
| 01 | Purchase | Yes | 5 defined | 80% |
| 02 | Payment | Yes | 3 defined | 80% |
| 03 | Credit | Yes | 3 defined | 60% |
| 04 | Authorization | **No** | 3 defined | **0%** |
| 05 | Refund | **No** | 1 defined | **0%** |
| 06 | Reversal | **No** | 2 defined | **0%** |
| 07 | Adjustment | **No** | 1 defined | **0%** |

---

## 4. Coverage Gap Analysis

### 4.1 Critical Gaps

#### GAP-001: No Inactive Accounts

| Attribute | Value |
|-----------|-------|
| **Severity** | CRITICAL |
| **Impact** | Cannot test account deactivation flow, status validation, or account reactivation |
| **Affected Programs** | COACTVWC, COACTUP, CBTRN02C |
| **Affected Rules** | BR-S001, BR-S002, BR-V007 |
| **Recommendation** | Add 5-10 accounts with ACCT-ACTIVE-STATUS = 'N' |

```cobol
* Current: All accounts have status 'Y'
* Needed: Add records with ACCT-ACTIVE-STATUS = 'N'
05 ACCT-ACTIVE-STATUS    PIC X(01).
   88 ACCT-STATUS-ACTIVE  VALUE 'Y'.
   88 ACCT-STATUS-INACTIVE VALUE 'N'.
```

#### GAP-002: Missing Authentication Test Data

| Attribute | Value |
|-----------|-------|
| **Severity** | CRITICAL |
| **Impact** | Cannot test sign-on functionality, password validation, or user type routing |
| **Affected Programs** | COSGN00C |
| **Affected Rules** | BR-V001, BR-V002, BR-V003, BR-V004, BR-A001, BR-A002 |
| **Recommendation** | Create USRSEC.txt with admin and regular user records |

```
Required USRSEC.txt structure (per CSUSR01Y.cpy):
- SEC-USR-ID        PIC X(08)  - User ID
- SEC-USR-PWD       PIC X(08)  - Password
- SEC-USR-TYPE      PIC X(01)  - 'A' (Admin) or 'U' (User)
```

### 4.2 High Severity Gaps

#### GAP-003: No Declined/Rejected Transactions

| Attribute | Value |
|-----------|-------|
| **Severity** | HIGH |
| **Impact** | Cannot test rejection handling, error message display, or reject file processing |
| **Affected Programs** | CBTRN02C |
| **Affected Rules** | BR-R001, BR-R002, BR-R003, BR-R004, BR-R005 |
| **Recommendation** | Add transactions that trigger rejection codes 100-109 |

**Rejection Scenarios Needed:**

| Rejection Code | Scenario | Test Data Needed |
|----------------|----------|------------------|
| 100 | Invalid card number | Transaction with non-existent card |
| 101 | Account not found | Transaction with orphan card |
| 102 | Over credit limit | Transaction exceeding limit |
| 103 | Expired account | Transaction for expired account |
| 109 | Account update fail | N/A (system error) |

### 4.3 Medium Severity Gaps

#### GAP-004: No International Customers

| Attribute | Value |
|-----------|-------|
| **Severity** | MEDIUM |
| **Impact** | Cannot test international address handling, country code validation |
| **Affected Programs** | COACTUPC |
| **Affected Rules** | BR-V021, BR-V022 |
| **Recommendation** | Add customers with non-US country codes |

#### GAP-005: No Fraud Test Cases

| Attribute | Value |
|-----------|-------|
| **Severity** | MEDIUM |
| **Impact** | Cannot test fraud detection scenarios if fraud rules are added |
| **Affected Programs** | None currently, but relevant for modernization |
| **Recommendation** | Add suspicious transaction patterns for future fraud detection testing |

---

## 5. Data Quality Observations

### 5.1 Completeness Analysis

| File | Null/Empty Fields | Population Rate |
|------|-------------------|-----------------|
| acctdata.txt | ACCT-CURR-CYC-CREDIT/DEBIT always 0 | 95% |
| carddata.txt | FILLER (expected) | 100% |
| custdata.txt | Some optional fields blank | 98% |
| cardxref.txt | None | 100% |
| dailytran.txt | TRAN-PROC-TS always blank | 90% |

### 5.2 Referential Integrity Verification

```
Integrity Check Results:
+-----------------------------+--------+
| Relationship                | Status |
+-----------------------------+--------+
| Card → Account              | PASS   |
| Card → Customer (via XREF)  | PASS   |
| Transaction → Card          | PASS   |
| Account → Group (discgrp)   | PARTIAL|
| Transaction → Type          | PASS   |
+-----------------------------+--------+
```

**Partial Integrity Note:** Account GROUP-ID values ('A000000000') only partially match discount group keys.

### 5.3 Data Format Consistency

| Field Type | Format | Consistent |
|------------|--------|------------|
| Dates | YYYY-MM-DD | Yes |
| Amounts | Signed COBOL (trailing sign overpunch) | Yes |
| Phone Numbers | (xxx)xxx-xxxx | Yes |
| ZIP Codes | 5-digit or 5+4 | Yes |
| Card Numbers | 16-digit numeric string | Yes |

---

## 6. Migration Testing Recommendations

### 6.1 Data Conversion Test Cases

| Test ID | Source Field | Target Type | Test Focus |
|---------|--------------|-------------|------------|
| MIG-001 | ACCT-CURR-BAL | DECIMAL(12,2) | Signed decimal conversion |
| MIG-002 | Date fields | DATE/TIMESTAMP | ISO 8601 parsing |
| MIG-003 | Phone numbers | VARCHAR(15) | Format preservation |
| MIG-004 | All FILLER | N/A | FILLER removal |
| MIG-005 | TRAN-AMT | DECIMAL(11,2) | Trailing sign overpunch |

### 6.2 Recommended Test Data Additions

| Priority | Category | Records to Add |
|----------|----------|----------------|
| P0 | Authentication | 5 user records (2 admin, 3 regular) |
| P0 | Inactive accounts | 5 accounts with status='N' |
| P1 | Expired cards | 5 cards with past expiration |
| P1 | Rejection scenarios | 10 transactions triggering codes 100-103 |
| P2 | International customers | 3 non-US customers |
| P2 | Multi-card accounts | 3 accounts with 2-3 cards each |

### 6.3 Regression Test Baseline

**Recommended baseline for each migration phase:**

1. **Data Migration Phase**
   - All 626 records migrate without loss
   - Key field values preserved
   - Relationships maintained

2. **Business Logic Migration Phase**
   - All 50+ validation rules produce same results
   - Calculation outputs match within precision tolerance
   - State transitions work identically

3. **Integration Phase**
   - End-to-end transaction flow works
   - Multi-file operations maintain consistency
   - Error handling behaves identically

---

## 7. Gap Remediation Roadmap

> **Note:** This section documents gaps that require domain expert input for resolution. These gaps are intentionally left unresolved to avoid introducing incorrect assumptions into the test data. Each gap includes templates and requirements for future remediation.

### 7.1 Remediation Priority Matrix

| Gap ID | Severity | Migration Blocker? | Domain Expert Required | Remediation Phase |
|--------|----------|-------------------|------------------------|-------------------|
| GAP-001 | CRITICAL | Yes - blocks deactivation testing | Yes - account lifecycle rules | Pre-UAT |
| GAP-002 | CRITICAL | Yes - blocks authentication testing | Yes - security policies | Pre-SIT |
| GAP-003 | HIGH | Partial - limits batch testing | Yes - rejection business rules | Pre-UAT |
| GAP-004 | MEDIUM | No - US-only may be acceptable | Yes - international address rules | Post-Go-Live |
| GAP-005 | MEDIUM | No - fraud detection may be new feature | Yes - fraud detection rules | Post-Go-Live |

### 7.2 GAP-001: Inactive Accounts

**Impact:** Cannot test account deactivation flow, status validation, or reactivation scenarios.

**Blocked Test Scenarios:**
- TC-ACCT-006: View Inactive Account
- TC-BTCH-005: Transaction Rejected - Account Expired (partial)
- All account status transition tests

**Domain Expert Questions:**
1. What triggers account deactivation? (Non-payment? Customer request? Fraud?)
2. Can inactive accounts be reactivated? What is the process?
3. Should transactions be rejected for inactive accounts, or just flagged?
4. Are there different inactive statuses (suspended vs. closed)?

**Template for Remediation:**
```
ACCT-ID:             000000000xx (assign IDs 51-60)
ACCT-ACTIVE-STATUS:  N
ACCT-CURR-BAL:       [Domain expert: What balance state for inactive?]
ACCT-OPEN-DATE:      [Historical date]
ACCT-EXPIRAION-DATE: [Domain expert: Past date or N/A for inactive?]
```

**Acceptance Criteria:**
- [ ] Domain expert confirms deactivation rules
- [ ] 5-10 inactive account records created
- [ ] At least 2 different inactive scenarios represented
- [ ] TC-ACCT-006 passes with inactive account data

---

### 7.3 GAP-002: Authentication Test Data

**Impact:** Cannot test sign-on functionality, password validation, user type routing, or admin vs. user access control.

**Blocked Test Scenarios:**
- TC-AUTH-001: Successful Admin Login
- TC-AUTH-002: Successful Regular User Login
- TC-AUTH-003 through TC-AUTH-006: All authentication scenarios
- TC-INT-002: Admin to User Flow

**Domain Expert Questions:**
1. What is the password policy? (Length, complexity, expiration)
2. Are there account lockout rules after failed attempts?
3. What user types exist beyond Admin (A) and User (U)?
4. Is there a password hashing/encryption requirement?
5. Are there service accounts or system users?

**Template for Remediation (USRSEC.txt structure per CSUSR01Y.cpy):**
```
Record Layout: SEC-USER-DATA
Position  Length  Field           Sample Values
--------  ------  --------------  -------------
1-8       8       SEC-USR-ID      ADMIN001, USER0001
9-16      8       SEC-USR-PWD     [Domain expert: test passwords]
17        1       SEC-USR-TYPE    A=Admin, U=User
18-25     8       SEC-USR-FNAME   [First name]
26-45     20      SEC-USR-LNAME   [Last name]
```

**Recommended Test Users (pending domain expert approval):**
| User ID | Type | Purpose |
|---------|------|---------|
| ADMIN001 | A | Primary admin testing |
| ADMIN002 | A | Secondary admin (concurrent access) |
| USER0001 | U | Primary user testing |
| USER0002 | U | Secondary user testing |
| USER0003 | U | Edge case testing (long name, etc.) |

**Security Considerations:**
- [ ] Test passwords must follow production policy format
- [ ] Domain expert must confirm no production credentials used
- [ ] Password complexity must match validation rules in COSGN00C

**Acceptance Criteria:**
- [ ] Domain expert approves user list and password policy
- [ ] USRSEC.txt created with approved records
- [ ] TC-AUTH-001 and TC-AUTH-002 pass
- [ ] Admin routing to COADM01C verified
- [ ] User routing to COMEN01C verified

---

### 7.4 GAP-003: Rejected Transaction Scenarios

**Impact:** Cannot test batch rejection handling, error message accuracy, or reject file processing.

**Blocked Test Scenarios:**
- TC-BTCH-002: Transaction Rejected - Invalid Card
- TC-BTCH-003: Transaction Rejected - Account Not Found
- TC-BTCH-004: Transaction Rejected - Over Credit Limit
- TC-BTCH-005: Transaction Rejected - Account Expired

**Domain Expert Questions:**
1. Are rejection codes 100-109 the complete set, or are there others?
2. What happens to rejected transactions in production? (Reprocessing? Manual review?)
3. Are there partial rejection scenarios (e.g., split transactions)?
4. What is the business process when a customer disputes a rejection?

**Template for Remediation (dailytran.txt rejection scenarios):**

| Rejection Code | Scenario | Test Data Requirement |
|----------------|----------|----------------------|
| 100 | Invalid card | Transaction with card number not in CARDXREF |
| 101 | Account not found | CARDXREF entry pointing to non-existent account |
| 102 | Over credit limit | Transaction amount > (ACCT-CREDIT-LIMIT - ACCT-CURR-BAL) |
| 103 | Expired account | Transaction date > ACCT-EXPIRAION-DATE |

**Sample Rejection Transaction Template:**
```
TRAN-ID:        9999999999999xxx (high IDs for rejection tests)
TRAN-TYPE-CD:   01 (Purchase)
TRAN-CAT-CD:    0001
TRAN-AMT:       [Varies by scenario]
TRAN-CARD-NUM:  [Invalid or linked to problem account]
TRAN-ORIG-TS:   [Current date for most; future for expiry test]
```

**Acceptance Criteria:**
- [ ] Domain expert confirms rejection code completeness
- [ ] 10+ rejection scenario transactions created
- [ ] Each rejection code (100-103) has at least 2 test cases
- [ ] DALYREJS output file format validated
- [ ] RETURN-CODE = 4 when rejections occur

---

### 7.5 GAP-004: International Customer Data

**Impact:** Cannot test international address handling, non-US country codes, or international phone formats.

**Blocked Test Scenarios:**
- International address validation (if implemented)
- Country-specific business rules (if any)

**Domain Expert Questions:**
1. Does CardDemo support international customers in production?
2. What countries are supported? (List of valid country codes)
3. Are there different validation rules for international addresses?
4. How are international phone numbers formatted?
5. Are there currency considerations for international accounts?

**Template for Remediation (custdata.txt international records):**
```
CUST-ADDR-STATE-CD:    [Domain expert: Valid for non-US?]
CUST-ADDR-COUNTRY-CD:  CAN, GBR, MEX (examples pending approval)
CUST-ADDR-ZIP:         [Country-specific format]
CUST-PHONE-NUM-1:      [International format: +1-xxx-xxx-xxxx?]
```

**Acceptance Criteria:**
- [ ] Domain expert confirms international support scope
- [ ] If supported: 3-5 international customer records
- [ ] If not supported: Document as out-of-scope

---

### 7.6 GAP-005: Fraud Detection Test Cases

**Impact:** Cannot test fraud detection scenarios if/when fraud rules are implemented.

**Blocked Test Scenarios:**
- Future fraud detection testing
- Suspicious transaction pattern identification

**Domain Expert Questions:**
1. Does the legacy system have fraud detection? (Not evident in code analysis)
2. If modernizing, what fraud patterns should be detected?
3. What is the fraud alert/response workflow?
4. Are there velocity checks (e.g., max transactions per hour)?

**Assessment:** This gap may not require remediation if:
- Legacy system has no fraud detection
- Fraud detection is a new feature for modernization (separate test suite)

**Acceptance Criteria:**
- [ ] Domain expert confirms fraud detection scope
- [ ] If legacy fraud exists: Document rules and create test data
- [ ] If new feature: Mark as out-of-scope for migration testing

---

### 7.7 Remediation Tracking

| Gap | Status | Domain Expert Assigned | Target Date | Sign-Off |
|-----|--------|------------------------|-------------|----------|
| GAP-001 | PENDING | TBD | TBD | [ ] |
| GAP-002 | PENDING | TBD | TBD | [ ] |
| GAP-003 | PENDING | TBD | TBD | TBD | [ ] |
| GAP-004 | PENDING | TBD | TBD | [ ] |
| GAP-005 | PENDING | TBD | TBD | [ ] |

**Remediation Workflow:**
1. Assign domain expert to each gap
2. Schedule gap review sessions
3. Domain expert provides answers to questions
4. Test data created based on approved templates
5. Test scenarios executed to validate remediation
6. Sign-off recorded in tracking table above

---

## 8. Cross-References

| Document | Relationship |
|----------|--------------|
| [BUSINESS-RULES.md](../01-domain-model/BUSINESS-RULES.md) | BR-Vxxx rule IDs referenced in coverage mapping |
| [DATA-MODEL.md](../02-data-model/DATA-MODEL.md) | Physical layouts and PIC clauses |
| [DATA-DICTIONARY.md](../02-data-model/DATA-DICTIONARY.md) | Field definitions and valid values |
| [TEST-SCENARIOS.md](./TEST-SCENARIOS.md) | Test cases derived from this coverage analysis |
| [EDGE-CASES.md](./EDGE-CASES.md) | Boundary conditions from PIC clause analysis |

---

## Appendix A: Test Data File Record Counts

```
$ wc -l app/data/ASCII/*.txt
   51 acctdata.txt    (50 records + blank line)
   51 carddata.txt    (50 records + blank line)
   51 custdata.txt    (50 records + blank line)
   51 cardxref.txt    (50 records + blank line)
  301 dailytran.txt   (300 records + blank line)
    8 trantype.txt    (7 records + blank line)
   19 trancatg.txt    (18 records + blank line)
   52 discgrp.txt     (51 records + blank line)
   51 tcatbal.txt     (50 records + blank line)
  ---
  635 total lines (626 data records)
```

## Appendix B: EBCDIC Equivalents

All test data files have EBCDIC equivalents in `app/data/EBCDIC/` with `.VSAM.KSDS.` naming convention. These are binary files suitable for mainframe upload.

---

*Document generated as part of RE-009 Test Coverage Analysis*
