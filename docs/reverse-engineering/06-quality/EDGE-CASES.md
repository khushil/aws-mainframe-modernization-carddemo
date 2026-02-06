# CardDemo Edge Cases and Boundary Conditions Catalog

## Document Information

| Attribute | Value |
|-----------|-------|
| **Version** | 1.0 |
| **Last Updated** | 2026-02-05 |
| **Source Analysis** | RE-009 Test Coverage Analysis |
| **Total Boundary Conditions** | 45+ fields |
| **Total 88-Level Conditions** | 25+ conditions |

---

## 1. Overview

This document catalogs boundary conditions derived from COBOL PIC clauses and 88-level conditions found in the CardDemo application. These edge cases are critical for:

- **Migration Testing**: Ensuring data type conversions handle extreme values
- **Regression Testing**: Validating business logic at field limits
- **Security Testing**: Identifying potential overflow/underflow vulnerabilities

---

## 2. Boundary Conditions Catalog

### 2.1 Numeric Field Boundaries

#### Primary Key Fields

| BC-ID | Field | Copybook | PIC Clause | Min Value | Max Value | Test Cases |
|-------|-------|----------|------------|-----------|-----------|------------|
| BC-001 | ACCT-ID | CVACT01Y.cpy:5 | 9(11) | 0 | 99,999,999,999 | 0, 1, max, max+1 |
| BC-002 | CUST-ID | CVCUS01Y.cpy:5 | 9(09) | 0 | 999,999,999 | 0, 1, max, max+1 |
| BC-003 | TRAN-ID | CVTRA05Y.cpy:5 | X(16) | 0...0 (16 digits) | 9...9 (16 digits) | min, max, alphanumeric |
| BC-004 | CARD-NUM | CVACT02Y.cpy:5 | X(16) | 16 zeros | 16 nines | min, max, short, long |

**Test Case Details for BC-001 (Account ID):**

```
BC-001-MIN:   00000000000 (11 zeros) - Should be rejected (BR-V006)
BC-001-LMIN:  00000000001 (minimum valid) - Should be accepted
BC-001-MAX:   99999999999 (11 nines) - Should be accepted
BC-001-OVER:  100000000000 (12 digits) - Should be truncated or error
```

---

#### Monetary Fields

| BC-ID | Field | Copybook | PIC Clause | Min Value | Max Value | Precision |
|-------|-------|----------|------------|-----------|-----------|-----------|
| BC-005 | ACCT-CURR-BAL | CVACT01Y.cpy:7 | S9(10)V99 | -9,999,999,999.99 | +9,999,999,999.99 | 2 decimal |
| BC-006 | ACCT-CREDIT-LIMIT | CVACT01Y.cpy:8 | S9(10)V99 | -9,999,999,999.99 | +9,999,999,999.99 | 2 decimal |
| BC-007 | ACCT-CASH-CREDIT-LIMIT | CVACT01Y.cpy:9 | S9(10)V99 | -9,999,999,999.99 | +9,999,999,999.99 | 2 decimal |
| BC-008 | ACCT-CURR-CYC-CREDIT | CVACT01Y.cpy:13 | S9(10)V99 | -9,999,999,999.99 | +9,999,999,999.99 | 2 decimal |
| BC-009 | ACCT-CURR-CYC-DEBIT | CVACT01Y.cpy:14 | S9(10)V99 | -9,999,999,999.99 | +9,999,999,999.99 | 2 decimal |
| BC-010 | TRAN-AMT | CVTRA05Y.cpy:10 | S9(09)V99 | -999,999,999.99 | +999,999,999.99 | 2 decimal |

**Test Case Details for BC-005 (Account Balance):**

```
BC-005-MAX-POS:  +9,999,999,999.99 - Maximum positive balance
BC-005-MAX-NEG:  -9,999,999,999.99 - Maximum negative (credit)
BC-005-ZERO:     +0.00 - Zero balance
BC-005-MIN-POS:  +0.01 - Minimum positive amount
BC-005-MIN-NEG:  -0.01 - Minimum negative (credit)
BC-005-OVER:     +10,000,000,000.00 - Overflow condition
BC-005-PREC:     +1234.567 - Precision truncation test
```

**Critical Calculation Boundary Test:**

```cobol
* From CBTRN02C.cbl:403-405
COMPUTE WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT
                    - ACCT-CURR-CYC-DEBIT
                    + DALYTRAN-AMT

* Overflow test case:
*   ACCT-CURR-CYC-CREDIT = +9,999,999,999.99
*   ACCT-CURR-CYC-DEBIT  = -9,999,999,999.99 (credit)
*   DALYTRAN-AMT         = +999,999,999.99
* Result exceeds S9(09)V99 capacity - verify overflow handling
```

---

#### Security/Verification Fields

| BC-ID | Field | Copybook | PIC Clause | Min Value | Max Value | Notes |
|-------|-------|----------|------------|-----------|-----------|-------|
| BC-011 | CARD-CVV-CD | CVACT02Y.cpy:7 | 9(03) | 0 | 999 | 3-digit CVV |
| BC-012 | CUST-SSN | CVCUS01Y.cpy:17 | 9(09) | 0 | 999,999,999 | SSN validation rules |
| BC-013 | CUST-FICO-CREDIT-SCORE | CVCUS01Y.cpy:22 | 9(03) | 0 | 999 | Business range 300-850 |

**Test Case Details for BC-011 (CVV):**

```
BC-011-MIN:    000 - Valid but unusual
BC-011-VALID:  123 - Typical valid CVV
BC-011-MAX:    999 - Maximum value
BC-011-OVER:   1000 - Should truncate to 000 or error
BC-011-SHORT:  12 - Should be padded or rejected
```

**Test Case Details for BC-013 (FICO Score):**

```
BC-013-PIC-MIN:    000 - PIC minimum (below business range)
BC-013-BIZ-MIN:    300 - Business minimum (lowest valid FICO)
BC-013-BIZ-MAX:    850 - Business maximum (highest valid FICO)
BC-013-PIC-MAX:    999 - PIC maximum (above business range)
BC-013-TYPICAL:    720 - Typical good score
```

---

### 2.2 String Field Length Boundaries

| BC-ID | Field | Copybook | PIC Clause | Max Length | Test Cases |
|-------|-------|----------|------------|------------|------------|
| BC-014 | CUST-FIRST-NAME | CVCUS01Y.cpy:6 | X(25) | 25 chars | empty, 25 chars, 26+ chars |
| BC-015 | CUST-MIDDLE-NAME | CVCUS01Y.cpy:7 | X(25) | 25 chars | empty, 25 chars, 26+ chars |
| BC-016 | CUST-LAST-NAME | CVCUS01Y.cpy:8 | X(25) | 25 chars | empty, 25 chars, 26+ chars |
| BC-017 | CUST-ADDR-LINE-1 | CVCUS01Y.cpy:9 | X(50) | 50 chars | empty, 50 chars, 51+ chars |
| BC-018 | CUST-ADDR-LINE-2 | CVCUS01Y.cpy:10 | X(50) | 50 chars | empty, 50 chars |
| BC-019 | CUST-ADDR-LINE-3 | CVCUS01Y.cpy:11 | X(50) | 50 chars | empty, 50 chars |
| BC-020 | CARD-EMBOSSED-NAME | CVACT02Y.cpy:8 | X(50) | 50 chars | empty, max, special chars |
| BC-021 | TRAN-DESC | CVTRA05Y.cpy:9 | X(100) | 100 chars | empty, max |
| BC-022 | TRAN-MERCHANT-NAME | CVTRA05Y.cpy:12 | X(50) | 50 chars | empty, max |
| BC-023 | TRAN-MERCHANT-CITY | CVTRA05Y.cpy:13 | X(50) | 50 chars | empty, max |

**Test Case Details for BC-020 (Card Embossed Name):**

```
BC-020-EMPTY:   '' (all spaces) - Should trigger BR-V011 error
BC-020-MIN:     'A' (1 character) - Minimum valid
BC-020-MAX:     'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWX' (50 chars)
BC-020-OVER:    'ABCD...51 chars' - Should be truncated
BC-020-SPECIAL: 'John O''Brien Jr.' - Apostrophe handling
BC-020-NUMERIC: 'John Smith 123' - Should fail validation (BR-V011)
```

---

### 2.3 Date Field Boundaries

| BC-ID | Field | Copybook | PIC Clause | Format | Test Cases |
|-------|-------|----------|------------|--------|------------|
| BC-024 | ACCT-OPEN-DATE | CVACT01Y.cpy:10 | X(10) | YYYY-MM-DD | min date, max date, invalid |
| BC-025 | ACCT-EXPIRAION-DATE | CVACT01Y.cpy:11 | X(10) | YYYY-MM-DD | past, future, boundary |
| BC-026 | ACCT-REISSUE-DATE | CVACT01Y.cpy:12 | X(10) | YYYY-MM-DD | various dates |
| BC-027 | CARD-EXPIRAION-DATE | CVACT02Y.cpy:9 | X(10) | YYYY-MM-DD | past, future, boundary |
| BC-028 | CUST-DOB-YYYY-MM-DD | CVCUS01Y.cpy:19 | X(10) | YYYY-MM-DD | historical, recent |
| BC-029 | TRAN-ORIG-TS | CVTRA05Y.cpy:16 | X(26) | Timestamp | various timestamps |

**Test Case Details for BC-025 (Account Expiration):**

```
BC-025-PAST:       '2020-01-01' - Expired (triggers BR-T001 rejection)
BC-025-TODAY:      Current date - Edge case
BC-025-FUTURE:     '2030-12-31' - Valid future date
BC-025-LEAP:       '2024-02-29' - Leap year date
BC-025-NOLEAP:     '2023-02-29' - Invalid non-leap year
BC-025-MIN-MONTH:  '2025-01-01' - January
BC-025-MAX-MONTH:  '2025-12-31' - December
BC-025-INVALID:    '2025-13-01' - Invalid month
BC-025-INVALID2:   '2025-00-15' - Invalid zero month
BC-025-DAY31:      '2025-04-31' - Invalid (April has 30 days)
```

---

### 2.4 Code/Status Field Boundaries

| BC-ID | Field | Copybook | PIC Clause | Valid Values | Test Cases |
|-------|-------|----------|------------|--------------|------------|
| BC-030 | ACCT-ACTIVE-STATUS | CVACT01Y.cpy:6 | X(01) | 'Y', 'N' | Y, N, X, space, lowercase |
| BC-031 | CARD-ACTIVE-STATUS | CVACT02Y.cpy:10 | X(01) | 'Y', 'N' | Y, N, X, space, lowercase |
| BC-032 | TRAN-TYPE-CD | CVTRA05Y.cpy:6 | X(02) | '01'-'07' | 00, 01-07, 08, 99, AA |
| BC-033 | TRAN-CAT-CD | CVTRA05Y.cpy:7 | 9(04) | 0001-9999 | 0000, 0001, 9999, above |
| BC-034 | CUST-ADDR-STATE-CD | CVCUS01Y.cpy:12 | X(02) | US state codes | valid, invalid, lowercase |
| BC-035 | CUST-ADDR-COUNTRY-CD | CVCUS01Y.cpy:13 | X(03) | 'USA', etc. | valid, invalid |
| BC-036 | SEC-USR-TYPE | CSUSR01Y.cpy | X(01) | 'A', 'U' | A, U, X, space |

**Test Case Details for BC-032 (Transaction Type):**

```
BC-032-VALID:
  '01' - Purchase (valid)
  '02' - Payment (valid)
  '03' - Credit (valid)
  '04' - Authorization (valid)
  '05' - Refund (valid)
  '06' - Reversal (valid)
  '07' - Adjustment (valid)

BC-032-INVALID:
  '00' - Below minimum (invalid)
  '08' - Above maximum (invalid)
  '99' - Way above maximum (invalid)
  'AA' - Non-numeric (invalid)
  ' 1' - Space-padded (may be invalid)
```

---

## 3. 88-Level Condition Inventory

### 3.1 User Type Conditions (COCOM01Y.cpy)

```cobol
10 CDEMO-USER-TYPE               PIC X(01).
   88 CDEMO-USRTYP-ADMIN         VALUE 'A'.
   88 CDEMO-USRTYP-USER          VALUE 'U'.
```

| Condition | Value | Test Scenarios |
|-----------|-------|----------------|
| CDEMO-USRTYP-ADMIN | 'A' | Admin login, admin menu access |
| CDEMO-USRTYP-USER | 'U' | User login, user menu access |
| Invalid | 'X', ' ', lowercase | Should fail or default |

---

### 3.2 Program Context Conditions (COCOM01Y.cpy)

```cobol
10 CDEMO-PGM-CONTEXT             PIC 9(01).
   88 CDEMO-PGM-ENTER            VALUE 0.
   88 CDEMO-PGM-REENTER          VALUE 1.
```

| Condition | Value | Test Scenarios |
|-----------|-------|----------------|
| CDEMO-PGM-ENTER | 0 | First entry to program |
| CDEMO-PGM-REENTER | 1 | Subsequent screen interaction |
| Invalid | 2-9 | Undefined behavior |

---

### 3.3 Input Validation Flags (COCRDUPC.cbl)

```cobol
05  WS-INPUT-FLAG                         PIC X(1).
  88  INPUT-OK                            VALUE '0'.
  88  INPUT-ERROR                         VALUE '1'.
  88  INPUT-PENDING                       VALUE LOW-VALUES.
```

| Condition | Value | Description |
|-----------|-------|-------------|
| INPUT-OK | '0' | All validations passed |
| INPUT-ERROR | '1' | Validation failed |
| INPUT-PENDING | LOW-VALUES | Not yet validated |

---

### 3.4 Card Status Validation (COCRDUPC.cbl)

```cobol
05  FLG-YES-NO-CHECK                      PIC X(1) VALUE 'N'.
  88 FLG-YES-NO-VALID                     VALUES 'Y', 'N'.
```

| Condition | Value | Test Case |
|-----------|-------|-----------|
| FLG-YES-NO-VALID | 'Y' or 'N' | Accept these values |
| Invalid | Other | Reject with BR-V012 error |

---

### 3.5 Month Validation (COCRDUPC.cbl)

```cobol
05  CARD-MONTH-CHECK                      PIC X(2).
05  CARD-MONTH-CHECK-N REDEFINES
    CARD-MONTH-CHECK                      PIC 9(2).
    88 VALID-MONTH                        VALUES 1 THRU 12.
```

| Condition | Values | Test Cases |
|-----------|--------|------------|
| VALID-MONTH | 1-12 | 01, 06, 12 (valid) |
| Invalid | 0, 13-99 | 00, 13, 99 (invalid) |

---

### 3.6 Year Validation (COCRDUPC.cbl)

```cobol
05  CARD-YEAR-CHECK                      PIC X(4).
05  CARD-YEAR-CHECK-N REDEFINES
    CARD-YEAR-CHECK                      PIC 9(4).
    88 VALID-YEAR                        VALUES 1950 THRU 2099.
```

| Condition | Values | Test Cases |
|-----------|--------|------------|
| VALID-YEAR | 1950-2099 | 1950, 2000, 2025, 2099 (valid) |
| Invalid Below | 0-1949 | 0000, 1949 (invalid) |
| Invalid Above | 2100+ | 2100, 9999 (invalid) |

---

### 3.7 Batch Processing Flags (CBTRN02C.cbl)

```cobol
01  APPL-RESULT             PIC S9(9)   COMP.
    88  APPL-AOK            VALUE 0.
    88  APPL-EOF            VALUE 16.
```

| Condition | Value | Description |
|-----------|-------|-------------|
| APPL-AOK | 0 | Operation successful |
| APPL-EOF | 16 | End of file reached |
| Error | 8, 12 | Various error conditions |

---

### 3.8 Card Update State Machine (COCRDUPC.cbl)

```cobol
05 CARD-UPDATE-SCREEN-DATA.
   10 CCUP-CHANGE-ACTION                 PIC X(1) VALUE LOW-VALUES.
      88 CCUP-DETAILS-NOT-FETCHED        VALUES LOW-VALUES, SPACES.
      88 CCUP-SHOW-DETAILS               VALUE 'S'.
      88 CCUP-CHANGES-MADE               VALUES 'E', 'N', 'C', 'L', 'F'.
      88 CCUP-CHANGES-NOT-OK             VALUE 'E'.
      88 CCUP-CHANGES-OK-NOT-CONFIRMED   VALUE 'N'.
      88 CCUP-CHANGES-OKAYED-AND-DONE    VALUE 'C'.
      88 CCUP-CHANGES-FAILED             VALUES 'L', 'F'.
      88 CCUP-CHANGES-OKAYED-LOCK-ERROR  VALUE 'L'.
      88 CCUP-CHANGES-OKAYED-BUT-FAILED  VALUE 'F'.
```

**State Transition Test Cases:**

| State | Condition | Valid Transitions |
|-------|-----------|-------------------|
| Initial | CCUP-DETAILS-NOT-FETCHED | → 'S' (show details) |
| Show | CCUP-SHOW-DETAILS | → 'E' or 'N' (edit or validate) |
| Error | CCUP-CHANGES-NOT-OK | → 'S' (re-show) or 'N' (re-validate) |
| Validated | CCUP-CHANGES-OK-NOT-CONFIRMED | → 'C' (commit) or 'L'/'F' (fail) |
| Complete | CCUP-CHANGES-OKAYED-AND-DONE | → Exit |
| Failed | CCUP-CHANGES-FAILED | → 'S' (retry) or Exit |

---

### 3.9 Bill Payment Confirmation (COBIL00C.cbl)

```cobol
05 WS-CONF-PAY-FLG            PIC X(01) VALUE 'N'.
   88 CONF-PAY-YES                       VALUE 'Y'.
   88 CONF-PAY-NO                        VALUE 'N'.
```

| Condition | Value | Test Case |
|-----------|-------|-----------|
| CONF-PAY-YES | 'Y' | Process payment |
| CONF-PAY-NO | 'N' | Cancel payment |
| Invalid | Other | Trigger BR-V018 error |

---

### 3.10 Error Flag (COBIL00C.cbl)

```cobol
05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
   88 ERR-FLG-ON                         VALUE 'Y'.
   88 ERR-FLG-OFF                        VALUE 'N'.
```

| Condition | Value | Description |
|-----------|-------|-------------|
| ERR-FLG-ON | 'Y' | Error occurred, halt processing |
| ERR-FLG-OFF | 'N' | No error, continue |

---

## 4. Error Condition Catalog

### 4.1 Authentication Errors (COSGN00C)

| Error ID | Condition | Message | Source |
|----------|-----------|---------|--------|
| ERR-AUTH-001 | User ID empty | "Please enter User ID ..." | COSGN00C.cbl:120 |
| ERR-AUTH-002 | Password empty | "Please enter Password ..." | COSGN00C.cbl:125 |
| ERR-AUTH-003 | Password mismatch | "Wrong Password. Try again ..." | COSGN00C.cbl:242-243 |
| ERR-AUTH-004 | User not found | "User not found. Try again ..." | COSGN00C.cbl:249 |
| ERR-AUTH-005 | File error | "Unable to verify the User ..." | COSGN00C.cbl:254 |

---

### 4.2 Account Errors (COACTVWC, COBIL00C)

| Error ID | Condition | Message | Source |
|----------|-----------|---------|--------|
| ERR-ACCT-001 | Account empty | "Account number not provided" | COACTVWC.cbl:121-122 |
| ERR-ACCT-002 | Account non-numeric | "Account number must be a non zero 11 digit number" | COACTVWC.cbl:127-128 |
| ERR-ACCT-003 | Account all zeros | "Account number must be a non zero 11 digit number" | COACTVWC.cbl:125-126 |
| ERR-ACCT-004 | Account not found | "Did not find this account in account master file" | COACTVWC.cbl:131-132 |
| ERR-ACCT-005 | Account not in XREF | "Did not find this account in account card xref file" | COACTVWC.cbl:129-130 |
| ERR-ACCT-006 | Zero balance | "You have nothing to pay..." | COBIL00C.cbl:201-202 |

---

### 4.3 Card Errors (COCRDSLC, COCRDUPC)

| Error ID | Condition | Message | Source |
|----------|-----------|---------|--------|
| ERR-CARD-001 | Card empty | "Card number not provided" | COCRDSLC.cbl:140-141 |
| ERR-CARD-002 | Card non-numeric | "Card number if supplied must be a 16 digit number" | COCRDSLC.cbl:148-149 |
| ERR-CARD-003 | Card not found | "Did not find cards for this search condition" | COCRDSLC.cbl:153-154 |
| ERR-CARD-004 | Name not alpha | "Card name can only contain alphabets and spaces" | COCRDUPC.cbl:183-184 |
| ERR-CARD-005 | Invalid status | "Card Active Status must be Y or N" | COCRDUPC.cbl:195-196 |
| ERR-CARD-006 | Invalid month | "Card expiry month must be between 1 and 12" | COCRDUPC.cbl:197-198 |
| ERR-CARD-007 | Invalid year | "Invalid card expiry year" | COCRDUPC.cbl:199-200 |
| ERR-CARD-008 | Lock failed | "Could not lock record for update" | COCRDUPC.cbl:205-206 |
| ERR-CARD-009 | Concurrent update | "Record changed by some one else. Please review" | COCRDUPC.cbl:207-208 |
| ERR-CARD-010 | Update failed | "Update of record failed" | COCRDUPC.cbl:209-210 |

---

### 4.4 Batch Rejection Codes (CBTRN02C)

| Code | Error ID | Condition | Description | Source |
|------|----------|-----------|-------------|--------|
| 100 | ERR-BTCH-001 | Invalid card | Card not in cross-reference | CBTRN02C.cbl:385-387 |
| 101 | ERR-BTCH-002 | Account missing | Account linked to card not found | CBTRN02C.cbl:397-399 |
| 102 | ERR-BTCH-003 | Overlimit | Transaction exceeds credit limit | CBTRN02C.cbl:410-412 |
| 103 | ERR-BTCH-004 | Expired | Transaction after account expiration | CBTRN02C.cbl:417-419 |
| 109 | ERR-BTCH-005 | Update fail | Account record update failed | CBTRN02C.cbl:556-558 |

---

### 4.5 File Status Error Handling

Common VSAM file status codes handled:

| Status | Meaning | Programs |
|--------|---------|----------|
| 00 | Success | All |
| 02 | Duplicate alternate key | CBTRN02C |
| 10 | End of file | CBTRN02C |
| 13 | Record not found | COSGN00C, COBIL00C |
| 21 | Sequence error | Batch programs |
| 22 | Duplicate key | COBIL00C |
| 23 | Record not found | Multiple |
| 35 | File not found | Batch programs |
| 39 | File attributes mismatch | Batch programs |
| 47 | File not open | Batch programs |

---

## 5. Precision and Rounding Edge Cases

### 5.1 Decimal Arithmetic Scenarios

| Test ID | Scenario | Calculation | Expected Behavior |
|---------|----------|-------------|-------------------|
| PREC-001 | Balance + Small Amount | 9,999,999,999.98 + 0.01 | Should not overflow |
| PREC-002 | Balance - Small Amount | 0.01 - 0.01 | Result = 0.00 |
| PREC-003 | Large Subtraction | 0.00 - 9,999,999,999.99 | Negative balance |
| PREC-004 | Half-cent Rounding | N/A | COBOL truncates, no half-adjust |
| PREC-005 | Multiple Decimals | Sum of 0.01 * 100 | Should equal 1.00 exactly |

### 5.2 Interest Calculation Edge Cases

| Test ID | Scenario | Input | Expected |
|---------|----------|-------|----------|
| INT-001 | Zero APR | 1000.00 balance, 0.00% APR | No interest |
| INT-002 | Maximum APR | 1000.00 balance, 2.50% APR | 25.00 interest |
| INT-003 | Minimum balance | 0.01 balance, 1.50% APR | ~0.00 interest |
| INT-004 | Maximum balance | 9,999,999,999.99 @ 2.50% | ~249,999,999.99 |

### 5.3 Signed Number Edge Cases

COBOL signed numbers use trailing sign overpunch in ASCII:

| Value | Last Digit | Overpunch |
|-------|------------|-----------|
| +0 | 0 | { |
| +1 | 1 | A |
| +9 | 9 | I |
| -0 | 0 | } |
| -1 | 1 | J |
| -9 | 9 | R |

**Test Cases:**
```
Positive amounts: Look for {, A-I in last position
Negative amounts: Look for }, J-R in last position
Zero: Could be { (positive zero) or } (negative zero)
```

---

## 6. Migration-Specific Edge Cases

### 6.1 Data Type Conversion Risks

| Risk ID | Source Type | Target Type | Risk | Mitigation |
|---------|-------------|-------------|------|------------|
| MIG-RISK-001 | PIC S9(10)V99 | DECIMAL(12,2) | Sign handling | Verify negative values |
| MIG-RISK-002 | PIC 9(11) | BIGINT | Leading zeros | Verify numeric conversion |
| MIG-RISK-003 | PIC X(16) | VARCHAR(16) | Space padding | Trim or preserve? |
| MIG-RISK-004 | PIC X(10) date | DATE | Format parsing | Validate all formats |
| MIG-RISK-005 | PIC X(26) timestamp | TIMESTAMP | Precision loss | 6 decimal places |

### 6.2 EBCDIC to UTF-8 Conversion Issues

| Issue ID | Character | EBCDIC | Risk |
|----------|-----------|--------|------|
| CONV-001 | Apostrophe | x'7D' | O'Brien → O'Brien |
| CONV-002 | Ampersand | x'50' | Smith & Jones |
| CONV-003 | Accented chars | Various | Not in test data |
| CONV-004 | Control chars | x'00'-x'3F' | Should not appear |
| CONV-005 | Currency symbols | x'5B' ($) | Verify rendering |

### 6.3 Packed Decimal Conversion

Transaction amounts use signed overpunch. Test cases for conversion:

```
Test: +1234.56
COBOL display: 000001234F (where F = positive sign)
ASCII representation: 0000012345{ (trailing overpunch)
Target DECIMAL: 1234.56

Test: -1234.56
COBOL display: 000001234D (where D = negative sign)
ASCII representation: 0000012345R (trailing overpunch)
Target DECIMAL: -1234.56
```

---

## 7. Boundary Test Case Matrix

### 7.1 Account ID Boundaries

| Test Case | Input | Expected Result | Rule |
|-----------|-------|-----------------|------|
| BC-ACCT-001 | 00000000000 | Error: zeros not allowed | BR-V006 |
| BC-ACCT-002 | 00000000001 | Valid - minimum | BR-V005 |
| BC-ACCT-003 | 99999999999 | Valid - maximum | BR-V005 |
| BC-ACCT-004 | 100000000000 | Error: 12 digits | BR-V005 |
| BC-ACCT-005 | ABCDEFGHIJK | Error: not numeric | BR-V005 |
| BC-ACCT-006 | (empty) | Error: required | BR-V008 |

### 7.2 Card Number Boundaries

| Test Case | Input | Expected Result | Rule |
|-----------|-------|-----------------|------|
| BC-CARD-001 | 0000000000000000 | Valid but may fail lookup | BR-V009 |
| BC-CARD-002 | 9999999999999999 | Valid but may fail lookup | BR-V009 |
| BC-CARD-003 | 123456789012345 | Error: 15 digits | BR-V009 |
| BC-CARD-004 | 12345678901234567 | Error: 17 digits | BR-V009 |
| BC-CARD-005 | ABCD1234EFGH5678 | Error: non-numeric | BR-V009 |
| BC-CARD-006 | (empty) | Error: required | - |

### 7.3 Amount Boundaries

| Test Case | Amount | Expected Result | Context |
|-----------|--------|-----------------|---------|
| BC-AMT-001 | 0.00 | Valid zero | Bill payment: "nothing to pay" |
| BC-AMT-002 | 0.01 | Valid minimum | Smallest transaction |
| BC-AMT-003 | 999999999.99 | Valid maximum | Largest transaction |
| BC-AMT-004 | -999999999.99 | Valid credit | Credit/refund |
| BC-AMT-005 | 1000000000.00 | Overflow | Exceeds PIC clause |

### 7.4 Date Boundaries

| Test Case | Date | Expected Result | Notes |
|-----------|------|-----------------|-------|
| BC-DATE-001 | 1950-01-01 | Valid minimum | Earliest allowed year |
| BC-DATE-002 | 2099-12-31 | Valid maximum | Latest allowed year |
| BC-DATE-003 | 1949-12-31 | Invalid | Below minimum year |
| BC-DATE-004 | 2100-01-01 | Invalid | Above maximum year |
| BC-DATE-005 | 2024-02-29 | Valid | Leap year |
| BC-DATE-006 | 2023-02-29 | Invalid | Not a leap year |
| BC-DATE-007 | 2025-04-31 | Invalid | April has 30 days |

---

## 8. Cross-References

| Document | Relationship |
|----------|--------------|
| [BUSINESS-RULES.md](../01-domain-model/BUSINESS-RULES.md) | BR-xxx rules define validation logic |
| [DATA-MODEL.md](../02-data-model/DATA-MODEL.md) | PIC clauses from copybook analysis |
| [DATA-DICTIONARY.md](../02-data-model/DATA-DICTIONARY.md) | Field definitions and constraints |
| [TEST-COVERAGE.md](./TEST-COVERAGE.md) | Test data inventory gaps |
| [TEST-SCENARIOS.md](./TEST-SCENARIOS.md) | Test cases using these boundaries |

---

## Appendix A: Complete 88-Level Condition Summary

| Copybook/Program | Level | Condition | Values |
|------------------|-------|-----------|--------|
| COCOM01Y.cpy | 10 | CDEMO-USRTYP-ADMIN | 'A' |
| COCOM01Y.cpy | 10 | CDEMO-USRTYP-USER | 'U' |
| COCOM01Y.cpy | 10 | CDEMO-PGM-ENTER | 0 |
| COCOM01Y.cpy | 10 | CDEMO-PGM-REENTER | 1 |
| COCRDUPC.cbl | 05 | INPUT-OK | '0' |
| COCRDUPC.cbl | 05 | INPUT-ERROR | '1' |
| COCRDUPC.cbl | 05 | INPUT-PENDING | LOW-VALUES |
| COCRDUPC.cbl | 88 | FLG-YES-NO-VALID | 'Y', 'N' |
| COCRDUPC.cbl | 88 | VALID-MONTH | 1 THRU 12 |
| COCRDUPC.cbl | 88 | VALID-YEAR | 1950 THRU 2099 |
| COCRDUPC.cbl | 88 | CCUP-DETAILS-NOT-FETCHED | LOW-VALUES, SPACES |
| COCRDUPC.cbl | 88 | CCUP-SHOW-DETAILS | 'S' |
| COCRDUPC.cbl | 88 | CCUP-CHANGES-MADE | 'E', 'N', 'C', 'L', 'F' |
| COBIL00C.cbl | 88 | ERR-FLG-ON | 'Y' |
| COBIL00C.cbl | 88 | ERR-FLG-OFF | 'N' |
| COBIL00C.cbl | 88 | CONF-PAY-YES | 'Y' |
| COBIL00C.cbl | 88 | CONF-PAY-NO | 'N' |
| CBTRN02C.cbl | 88 | APPL-AOK | 0 |
| CBTRN02C.cbl | 88 | APPL-EOF | 16 |

---

*Document generated as part of RE-009 Test Coverage Analysis*
