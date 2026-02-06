# CardDemo Test Scenarios Catalog

## Document Information

| Attribute | Value |
|-----------|-------|
| **Version** | 1.0 |
| **Last Updated** | 2026-02-05 |
| **Source Analysis** | RE-009 Test Coverage Analysis |
| **Total Scenarios** | 78 |

---

## 1. Test Scenario Naming Convention

| Prefix | Category | Description |
|--------|----------|-------------|
| TC-AUTH-nnn | Authentication | Sign-on and user verification scenarios |
| TC-ACCT-nnn | Account Management | Account inquiry and update scenarios |
| TC-CARD-nnn | Card Management | Card search and update scenarios |
| TC-TRAN-nnn | Transaction | Transaction inquiry and processing scenarios |
| TC-BILL-nnn | Bill Payment | Bill payment flow scenarios |
| TC-BTCH-nnn | Batch Processing | Batch job execution scenarios |
| TC-INT-nnn | Integration | Cross-system integration scenarios |
| TC-MIG-nnn | Migration | Migration validation scenarios |

## 2. Priority Levels

| Priority | Description | Execution |
|----------|-------------|-----------|
| P0 | Critical path - must pass | Every build |
| P1 | High priority - core functionality | Daily |
| P2 | Medium priority - important features | Weekly |
| P3 | Low priority - edge cases | Release |

## 3. Test Category Definitions

| Category | Description |
|----------|-------------|
| Positive | Valid input, expected successful outcome |
| Negative | Invalid input, expected error handling |
| Boundary | Values at min/max limits |
| State | Entity state transition testing |
| Integration | Multi-component interaction |

---

## 4. Authentication Scenarios

### TC-AUTH-001: Successful Admin Login

| Attribute | Value |
|-----------|-------|
| **Category** | Positive |
| **Program** | COSGN00C |
| **Priority** | P0 |
| **Preconditions** | User record exists in USRSEC with type='A' |
| **Business Rules** | BR-V001, BR-V002, BR-V003, BR-A001 |
| **Source Reference** | COSGN00C.cbl:230-234 |

**Test Steps:**
1. Navigate to sign-on screen (TRAN CC00)
2. Enter valid admin user ID
3. Enter matching password
4. Press ENTER

**Expected Results:**
- User authenticated successfully
- COMMAREA populated with user ID and type 'A'
- Transfer to admin menu (COADM01C)

---

### TC-AUTH-002: Successful Regular User Login

| Attribute | Value |
|-----------|-------|
| **Category** | Positive |
| **Program** | COSGN00C |
| **Priority** | P0 |
| **Preconditions** | User record exists in USRSEC with type='U' |
| **Business Rules** | BR-V001, BR-V002, BR-V003, BR-A002 |
| **Source Reference** | COSGN00C.cbl:235-239 |

**Test Steps:**
1. Navigate to sign-on screen (TRAN CC00)
2. Enter valid regular user ID
3. Enter matching password
4. Press ENTER

**Expected Results:**
- User authenticated successfully
- COMMAREA populated with user ID and type 'U'
- Transfer to user menu (COMEN01C)

---

### TC-AUTH-003: Missing User ID

| Attribute | Value |
|-----------|-------|
| **Category** | Negative |
| **Program** | COSGN00C |
| **Priority** | P0 |
| **Preconditions** | Sign-on screen displayed |
| **Business Rules** | BR-V001 |
| **Source Reference** | COSGN00C.cbl:118-122 |

**Test Steps:**
1. Navigate to sign-on screen
2. Leave User ID field empty (SPACES or LOW-VALUES)
3. Enter any password
4. Press ENTER

**Expected Results:**
- Error message: "Please enter User ID ..."
- Cursor positioned on User ID field
- No navigation occurs

---

### TC-AUTH-004: Missing Password

| Attribute | Value |
|-----------|-------|
| **Category** | Negative |
| **Program** | COSGN00C |
| **Priority** | P0 |
| **Preconditions** | Sign-on screen displayed |
| **Business Rules** | BR-V002 |
| **Source Reference** | COSGN00C.cbl:123-127 |

**Test Steps:**
1. Navigate to sign-on screen
2. Enter valid User ID
3. Leave Password field empty
4. Press ENTER

**Expected Results:**
- Error message: "Please enter Password ..."
- Cursor positioned on Password field
- No navigation occurs

---

### TC-AUTH-005: Invalid Password

| Attribute | Value |
|-----------|-------|
| **Category** | Negative |
| **Program** | COSGN00C |
| **Priority** | P0 |
| **Preconditions** | Valid user exists in USRSEC |
| **Business Rules** | BR-V003 |
| **Source Reference** | COSGN00C.cbl:241-246 |

**Test Steps:**
1. Navigate to sign-on screen
2. Enter valid User ID
3. Enter incorrect password
4. Press ENTER

**Expected Results:**
- Error message: "Wrong Password. Try again ..."
- Cursor positioned on Password field
- No navigation occurs

---

### TC-AUTH-006: User Not Found

| Attribute | Value |
|-----------|-------|
| **Category** | Negative |
| **Program** | COSGN00C |
| **Priority** | P1 |
| **Preconditions** | User ID does not exist in USRSEC |
| **Business Rules** | BR-V004 |
| **Source Reference** | COSGN00C.cbl:247-251 |

**Test Steps:**
1. Navigate to sign-on screen
2. Enter non-existent User ID
3. Enter any password
4. Press ENTER

**Expected Results:**
- Error message: "User not found. Try again ..."
- Cursor positioned on User ID field
- RESP code 13 (NOTFND) handled

---

## 5. Account Management Scenarios

### TC-ACCT-001: View Account Details (Valid Account)

| Attribute | Value |
|-----------|-------|
| **Category** | Positive |
| **Program** | COACTVWC |
| **Priority** | P0 |
| **Preconditions** | Account exists in ACCTDAT |
| **Business Rules** | BR-V005, BR-V006, BR-V007 |
| **Source Reference** | COACTVWC.cbl:113-116 |

**Test Steps:**
1. Navigate to Account View screen (TRAN CAVW)
2. Enter valid 11-digit account ID (e.g., 00000000001)
3. Press ENTER

**Expected Results:**
- Account details displayed (balance, limit, dates)
- Associated customer information shown
- Message: "Displaying details of given Account"

---

### TC-ACCT-002: Account ID Not Provided

| Attribute | Value |
|-----------|-------|
| **Category** | Negative |
| **Program** | COACTVWC |
| **Priority** | P1 |
| **Preconditions** | Account View screen displayed |
| **Business Rules** | BR-V005 |
| **Source Reference** | COACTVWC.cbl:121-122 |

**Test Steps:**
1. Navigate to Account View screen
2. Leave Account ID field empty
3. Press ENTER

**Expected Results:**
- Error message: "Account number not provided"
- Cursor positioned on Account ID field

---

### TC-ACCT-003: Account ID Non-Numeric

| Attribute | Value |
|-----------|-------|
| **Category** | Negative |
| **Program** | COACTVWC |
| **Priority** | P1 |
| **Preconditions** | Account View screen displayed |
| **Business Rules** | BR-V005 |
| **Source Reference** | COACTVWC.cbl:127-128 |

**Test Steps:**
1. Navigate to Account View screen
2. Enter non-numeric Account ID (e.g., "ABCDEFGHIJK")
3. Press ENTER

**Expected Results:**
- Error message: "Account number must be a non zero 11 digit number"
- Cursor positioned on Account ID field

---

### TC-ACCT-004: Account ID All Zeros

| Attribute | Value |
|-----------|-------|
| **Category** | Negative |
| **Program** | COACTVWC |
| **Priority** | P1 |
| **Preconditions** | Account View screen displayed |
| **Business Rules** | BR-V006 |
| **Source Reference** | COACTVWC.cbl:125-126 |

**Test Steps:**
1. Navigate to Account View screen
2. Enter all-zero Account ID (00000000000)
3. Press ENTER

**Expected Results:**
- Error message: "Account number must be a non zero 11 digit number"
- Cursor positioned on Account ID field

---

### TC-ACCT-005: Account Not Found

| Attribute | Value |
|-----------|-------|
| **Category** | Negative |
| **Program** | COACTVWC |
| **Priority** | P1 |
| **Preconditions** | Account ID does not exist in ACCTDAT |
| **Business Rules** | BR-V007 |
| **Source Reference** | COACTVWC.cbl:131-132 |

**Test Steps:**
1. Navigate to Account View screen
2. Enter non-existent valid Account ID (e.g., 99999999999)
3. Press ENTER

**Expected Results:**
- Error message: "Did not find this account in account master file"
- RESP code NOTFND handled

---

### TC-ACCT-006: View Inactive Account

| Attribute | Value |
|-----------|-------|
| **Category** | State |
| **Program** | COACTVWC |
| **Priority** | P2 |
| **Preconditions** | Account exists with status='N' |
| **Business Rules** | BR-S001 |
| **Test Data Needed** | GAP-001 resolution required |

**Test Steps:**
1. Navigate to Account View screen
2. Enter inactive account ID
3. Press ENTER

**Expected Results:**
- Account details displayed
- Status shows 'N' (Inactive)
- Visual indicator of inactive status

---

### TC-ACCT-007: Account Balance Boundary - Zero

| Attribute | Value |
|-----------|-------|
| **Category** | Boundary |
| **Program** | COACTVWC |
| **Priority** | P2 |
| **Preconditions** | Account exists with zero balance |
| **Business Rules** | BR-C003 |

**Test Steps:**
1. Navigate to Account View screen
2. Enter account with zero balance
3. Press ENTER

**Expected Results:**
- Balance displayed as $0.00
- All other fields populated correctly

---

### TC-ACCT-008: Account Balance Boundary - Maximum

| Attribute | Value |
|-----------|-------|
| **Category** | Boundary |
| **Program** | COACTVWC |
| **Priority** | P3 |
| **Preconditions** | Account with balance near PIC S9(10)V99 maximum |
| **Business Rules** | BR-C003 |
| **Test Data Needed** | Account with balance = 9,999,999,999.99 |

**Test Steps:**
1. Navigate to Account View screen
2. Enter account with maximum balance
3. Press ENTER

**Expected Results:**
- Balance displays correctly without overflow
- Formatting handles large numbers

---

## 6. Card Management Scenarios

### TC-CARD-001: Search Card by Account and Card Number

| Attribute | Value |
|-----------|-------|
| **Category** | Positive |
| **Program** | COCRDSLC |
| **Priority** | P0 |
| **Preconditions** | Card exists in CARDDAT linked to account |
| **Business Rules** | BR-V005, BR-V009 |
| **Source Reference** | COCRDSLC.cbl:129-132 |

**Test Steps:**
1. Navigate to Card Search screen (TRAN CCDL)
2. Enter valid Account ID (e.g., 00000000050)
3. Enter valid Card Number (e.g., 0500024453765740)
4. Press ENTER

**Expected Results:**
- Card details displayed (embossed name, CVV, expiry, status)
- Message: "Displaying requested details"

---

### TC-CARD-002: Card Number Not Provided

| Attribute | Value |
|-----------|-------|
| **Category** | Negative |
| **Program** | COCRDSLC |
| **Priority** | P1 |
| **Preconditions** | Card Search screen displayed |
| **Business Rules** | BR-V009 |
| **Source Reference** | COCRDSLC.cbl:140-141 |

**Test Steps:**
1. Navigate to Card Search screen
2. Enter valid Account ID
3. Leave Card Number empty
4. Press ENTER

**Expected Results:**
- Error message: "Card number not provided"
- Cursor positioned on Card Number field

---

### TC-CARD-003: Card Number Non-Numeric

| Attribute | Value |
|-----------|-------|
| **Category** | Negative |
| **Program** | COCRDSLC |
| **Priority** | P1 |
| **Preconditions** | Card Search screen displayed |
| **Business Rules** | BR-V009 |
| **Source Reference** | COCRDSLC.cbl:148-149 |

**Test Steps:**
1. Navigate to Card Search screen
2. Enter valid Account ID
3. Enter non-numeric Card Number
4. Press ENTER

**Expected Results:**
- Error message: "Card number if supplied must be a 16 digit number"

---

### TC-CARD-004: Card Not Found in Cross-Reference

| Attribute | Value |
|-----------|-------|
| **Category** | Negative |
| **Program** | COCRDSLC |
| **Priority** | P1 |
| **Preconditions** | Card does not exist for account |
| **Business Rules** | BR-V015 |
| **Source Reference** | COCRDSLC.cbl:151-154 |

**Test Steps:**
1. Navigate to Card Search screen
2. Enter valid Account ID
3. Enter non-existent Card Number
4. Press ENTER

**Expected Results:**
- Error message: "Did not find cards for this search condition"

---

### TC-CARD-005: Update Card Status to Inactive

| Attribute | Value |
|-----------|-------|
| **Category** | State |
| **Program** | COCRDUPC |
| **Priority** | P1 |
| **Preconditions** | Card exists with status='Y' |
| **Business Rules** | BR-V012, BR-S003 |
| **Source Reference** | COCRDUPC.cbl:846-872 |

**Test Steps:**
1. Navigate to Card Update screen (TRAN CCUP)
2. Retrieve existing card
3. Change status from 'Y' to 'N'
4. Press F5 to confirm

**Expected Results:**
- Validation passes
- Card status updated in CARDDAT
- Message: "Changes committed to database"

---

### TC-CARD-006: Update Card Status - Invalid Value

| Attribute | Value |
|-----------|-------|
| **Category** | Negative |
| **Program** | COCRDUPC |
| **Priority** | P1 |
| **Preconditions** | Card Update screen displayed |
| **Business Rules** | BR-V012 |
| **Source Reference** | COCRDUPC.cbl:195-196 |

**Test Steps:**
1. Navigate to Card Update screen
2. Retrieve existing card
3. Enter invalid status value (e.g., 'X')
4. Press ENTER

**Expected Results:**
- Error message: "Card Active Status must be Y or N"
- Update not committed

---

### TC-CARD-007: Update Card Name - Valid Alphabetic

| Attribute | Value |
|-----------|-------|
| **Category** | Positive |
| **Program** | COCRDUPC |
| **Priority** | P1 |
| **Preconditions** | Card Update screen with card retrieved |
| **Business Rules** | BR-V011 |
| **Source Reference** | COCRDUPC.cbl:822-837 |

**Test Steps:**
1. Navigate to Card Update screen
2. Retrieve existing card
3. Change embossed name to valid alphabetic value
4. Press F5 to confirm

**Expected Results:**
- Validation passes
- Card name updated
- Message: "Changes committed to database"

---

### TC-CARD-008: Update Card Name - Contains Numbers

| Attribute | Value |
|-----------|-------|
| **Category** | Negative |
| **Program** | COCRDUPC |
| **Priority** | P1 |
| **Preconditions** | Card Update screen displayed |
| **Business Rules** | BR-V011 |
| **Source Reference** | COCRDUPC.cbl:183-184 |

**Test Steps:**
1. Navigate to Card Update screen
2. Retrieve existing card
3. Enter name containing numbers (e.g., "John Smith 123")
4. Press ENTER

**Expected Results:**
- Error message: "Card name can only contain alphabets and spaces"
- Update not committed

---

### TC-CARD-009: Update Expiry Month - Valid Range

| Attribute | Value |
|-----------|-------|
| **Category** | Positive |
| **Program** | COCRDUPC |
| **Priority** | P1 |
| **Preconditions** | Card Update screen with card retrieved |
| **Business Rules** | BR-V013, BR-T002 |
| **Source Reference** | COCRDUPC.cbl:877-907 |

**Test Steps:**
1. Navigate to Card Update screen
2. Retrieve existing card
3. Change expiry month to valid value (01-12)
4. Press F5 to confirm

**Expected Results:**
- Validation passes (VALID-MONTH = VALUES 1 THRU 12)
- Expiry month updated

---

### TC-CARD-010: Update Expiry Month - Invalid Zero

| Attribute | Value |
|-----------|-------|
| **Category** | Boundary |
| **Program** | COCRDUPC |
| **Priority** | P1 |
| **Preconditions** | Card Update screen displayed |
| **Business Rules** | BR-V013 |
| **Source Reference** | COCRDUPC.cbl:197-198 |

**Test Steps:**
1. Navigate to Card Update screen
2. Retrieve existing card
3. Enter expiry month = 00
4. Press ENTER

**Expected Results:**
- Error message: "Card expiry month must be between 1 and 12"

---

### TC-CARD-011: Update Expiry Month - Invalid 13

| Attribute | Value |
|-----------|-------|
| **Category** | Boundary |
| **Program** | COCRDUPC |
| **Priority** | P1 |
| **Preconditions** | Card Update screen displayed |
| **Business Rules** | BR-V013 |

**Test Steps:**
1. Navigate to Card Update screen
2. Retrieve existing card
3. Enter expiry month = 13
4. Press ENTER

**Expected Results:**
- Error message: "Card expiry month must be between 1 and 12"

---

### TC-CARD-012: Update Expiry Year - Valid Range

| Attribute | Value |
|-----------|-------|
| **Category** | Positive |
| **Program** | COCRDUPC |
| **Priority** | P1 |
| **Preconditions** | Card Update screen with card retrieved |
| **Business Rules** | BR-V014, BR-T003 |
| **Source Reference** | COCRDUPC.cbl:913-944 |

**Test Steps:**
1. Navigate to Card Update screen
2. Retrieve existing card
3. Change expiry year to valid value (1950-2099)
4. Press F5 to confirm

**Expected Results:**
- Validation passes (VALID-YEAR = VALUES 1950 THRU 2099)
- Expiry year updated

---

### TC-CARD-013: Update Expiry Year - Below Minimum

| Attribute | Value |
|-----------|-------|
| **Category** | Boundary |
| **Program** | COCRDUPC |
| **Priority** | P2 |
| **Preconditions** | Card Update screen displayed |
| **Business Rules** | BR-V014 |

**Test Steps:**
1. Navigate to Card Update screen
2. Retrieve existing card
3. Enter expiry year = 1949
4. Press ENTER

**Expected Results:**
- Error message: "Invalid card expiry year"

---

### TC-CARD-014: Update Expiry Year - Above Maximum

| Attribute | Value |
|-----------|-------|
| **Category** | Boundary |
| **Program** | COCRDUPC |
| **Priority** | P2 |
| **Preconditions** | Card Update screen displayed |
| **Business Rules** | BR-V014 |

**Test Steps:**
1. Navigate to Card Update screen
2. Retrieve existing card
3. Enter expiry year = 2100
4. Press ENTER

**Expected Results:**
- Error message: "Invalid card expiry year"

---

## 7. Bill Payment Scenarios

### TC-BILL-001: Successful Bill Payment

| Attribute | Value |
|-----------|-------|
| **Category** | Positive |
| **Program** | COBIL00C |
| **Priority** | P0 |
| **Preconditions** | Account exists with positive balance |
| **Business Rules** | BR-V008, BR-C006, BR-C008 |
| **Source Reference** | COBIL00C.cbl:210-234 |

**Test Steps:**
1. Navigate to Bill Payment screen (TRAN CB00)
2. Enter valid Account ID with positive balance
3. Enter confirmation 'Y'
4. Press ENTER

**Expected Results:**
- Transaction created in TRANSACT file
- Account balance reduced by payment amount
- Message: "Payment successful. Your Transaction ID is ..."
- Transaction type = '02' (Payment)

---

### TC-BILL-002: Bill Payment - Account ID Empty

| Attribute | Value |
|-----------|-------|
| **Category** | Negative |
| **Program** | COBIL00C |
| **Priority** | P1 |
| **Preconditions** | Bill Payment screen displayed |
| **Business Rules** | BR-V008 |
| **Source Reference** | COBIL00C.cbl:159-164 |

**Test Steps:**
1. Navigate to Bill Payment screen
2. Leave Account ID empty
3. Press ENTER

**Expected Results:**
- Error message: "Acct ID can NOT be empty..."
- Cursor positioned on Account ID field

---

### TC-BILL-003: Bill Payment - Account Not Found

| Attribute | Value |
|-----------|-------|
| **Category** | Negative |
| **Program** | COBIL00C |
| **Priority** | P1 |
| **Preconditions** | Account ID does not exist |
| **Business Rules** | BR-V007 |
| **Source Reference** | COBIL00C.cbl:359-364 |

**Test Steps:**
1. Navigate to Bill Payment screen
2. Enter non-existent Account ID
3. Press ENTER

**Expected Results:**
- Error message: "Account ID NOT found..."
- RESP code NOTFND handled

---

### TC-BILL-004: Bill Payment - Zero Balance

| Attribute | Value |
|-----------|-------|
| **Category** | Boundary |
| **Program** | COBIL00C |
| **Priority** | P1 |
| **Preconditions** | Account exists with balance = 0 |
| **Business Rules** | BR-L002 |
| **Source Reference** | COBIL00C.cbl:198-205 |

**Test Steps:**
1. Navigate to Bill Payment screen
2. Enter Account ID with zero balance
3. Press ENTER

**Expected Results:**
- Error message: "You have nothing to pay..."
- Payment not processed

---

### TC-BILL-005: Bill Payment - Negative Balance

| Attribute | Value |
|-----------|-------|
| **Category** | Boundary |
| **Program** | COBIL00C |
| **Priority** | P2 |
| **Preconditions** | Account exists with negative balance (credit) |
| **Business Rules** | BR-L002 |

**Test Steps:**
1. Navigate to Bill Payment screen
2. Enter Account ID with negative balance
3. Press ENTER

**Expected Results:**
- Error message: "You have nothing to pay..."
- Payment not processed (balance <= 0)

---

### TC-BILL-006: Bill Payment - Invalid Confirmation Value

| Attribute | Value |
|-----------|-------|
| **Category** | Negative |
| **Program** | COBIL00C |
| **Priority** | P1 |
| **Preconditions** | Bill Payment screen with valid account |
| **Business Rules** | BR-V018 |
| **Source Reference** | COBIL00C.cbl:185-190 |

**Test Steps:**
1. Navigate to Bill Payment screen
2. Enter valid Account ID
3. Enter invalid confirmation (e.g., 'X')
4. Press ENTER

**Expected Results:**
- Error message: "Invalid value. Valid values are (Y/N)..."

---

### TC-BILL-007: Bill Payment - Confirmation 'N' (Cancel)

| Attribute | Value |
|-----------|-------|
| **Category** | Positive |
| **Program** | COBIL00C |
| **Priority** | P1 |
| **Preconditions** | Bill Payment screen with valid account |
| **Business Rules** | BR-V018 |
| **Source Reference** | COBIL00C.cbl:178-181 |

**Test Steps:**
1. Navigate to Bill Payment screen
2. Enter valid Account ID
3. Enter confirmation 'N'
4. Press ENTER

**Expected Results:**
- Screen cleared
- Payment not processed
- No transaction created

---

### TC-BILL-008: Bill Payment - Transaction ID Sequence

| Attribute | Value |
|-----------|-------|
| **Category** | Integration |
| **Program** | COBIL00C |
| **Priority** | P2 |
| **Preconditions** | TRANSACT file has existing records |
| **Business Rules** | BR-Q001, BR-C008 |
| **Source Reference** | COBIL00C.cbl:212-219 |

**Test Steps:**
1. Note highest transaction ID in TRANSACT
2. Process bill payment for valid account
3. Verify new transaction ID

**Expected Results:**
- New transaction ID = previous highest + 1
- Sequential increment maintained

---

### TC-BILL-009: Bill Payment - Duplicate Transaction Prevention

| Attribute | Value |
|-----------|-------|
| **Category** | Negative |
| **Program** | COBIL00C |
| **Priority** | P2 |
| **Preconditions** | Concurrent payment attempts |
| **Business Rules** | BR-Q001 |
| **Source Reference** | COBIL00C.cbl:533-539 |

**Test Steps:**
1. Simulate concurrent payment processing
2. Verify duplicate handling

**Expected Results:**
- DUPKEY/DUPREC handled
- Error message: "Tran ID already exist..."

---

### TC-BILL-010: Bill Payment - Large Balance

| Attribute | Value |
|-----------|-------|
| **Category** | Boundary |
| **Program** | COBIL00C |
| **Priority** | P2 |
| **Preconditions** | Account with large balance near maximum |
| **Business Rules** | BR-C006 |

**Test Steps:**
1. Navigate to Bill Payment screen
2. Enter Account ID with large balance
3. Confirm payment
4. Press ENTER

**Expected Results:**
- Payment processes correctly
- Balance calculation handles large values
- No overflow errors

---

## 8. Batch Processing Scenarios

### TC-BTCH-001: Successful Transaction Posting

| Attribute | Value |
|-----------|-------|
| **Category** | Positive |
| **Program** | CBTRN02C |
| **Priority** | P0 |
| **Preconditions** | Valid daily transaction file, all reference files available |
| **Business Rules** | BR-Q002, BR-Q003, BR-C002, BR-C003 |
| **Source Reference** | CBTRN02C.cbl:202-219 |

**Test Steps:**
1. Prepare daily transaction file with valid records
2. Execute CBTRN02C batch job
3. Verify output files

**Expected Results:**
- All valid transactions written to TRANSACT
- Account balances updated correctly
- Category balances updated
- Display: "TRANSACTIONS PROCESSED: n"

---

### TC-BTCH-002: Transaction Rejected - Invalid Card

| Attribute | Value |
|-----------|-------|
| **Category** | Negative |
| **Program** | CBTRN02C |
| **Priority** | P1 |
| **Preconditions** | Daily transaction with non-existent card number |
| **Business Rules** | BR-R001, BR-V016 |
| **Source Reference** | CBTRN02C.cbl:385-387 |

**Test Steps:**
1. Prepare daily transaction with invalid card number
2. Execute CBTRN02C batch job
3. Verify rejection file

**Expected Results:**
- Transaction written to DALYREJS file
- Rejection code 100
- Message: "INVALID CARD NUMBER FOUND"
- RETURN-CODE = 4 if any rejections

---

### TC-BTCH-003: Transaction Rejected - Account Not Found

| Attribute | Value |
|-----------|-------|
| **Category** | Negative |
| **Program** | CBTRN02C |
| **Priority** | P1 |
| **Preconditions** | Transaction for card with missing account |
| **Business Rules** | BR-R002, BR-V017 |
| **Source Reference** | CBTRN02C.cbl:397-399 |

**Test Steps:**
1. Create card-xref pointing to non-existent account
2. Prepare daily transaction for that card
3. Execute CBTRN02C batch job

**Expected Results:**
- Transaction written to DALYREJS file
- Rejection code 101
- Message: "ACCOUNT RECORD NOT FOUND"

---

### TC-BTCH-004: Transaction Rejected - Over Credit Limit

| Attribute | Value |
|-----------|-------|
| **Category** | Boundary |
| **Program** | CBTRN02C |
| **Priority** | P1 |
| **Preconditions** | Transaction amount exceeds credit limit |
| **Business Rules** | BR-R003, BR-L001 |
| **Source Reference** | CBTRN02C.cbl:407-412 |

**Test Steps:**
1. Find account with low credit limit
2. Prepare transaction exceeding limit
3. Execute CBTRN02C batch job

**Expected Results:**
- Transaction written to DALYREJS file
- Rejection code 102
- Validation: ACCT-CREDIT-LIMIT >= WS-TEMP-BAL fails

---

### TC-BTCH-005: Transaction Rejected - Account Expired

| Attribute | Value |
|-----------|-------|
| **Category** | State |
| **Program** | CBTRN02C |
| **Priority** | P1 |
| **Preconditions** | Transaction date after account expiration |
| **Business Rules** | BR-R004, BR-T001, BR-S002 |
| **Source Reference** | CBTRN02C.cbl:414-420 |

**Test Steps:**
1. Find account with past expiration date
2. Prepare transaction with current date
3. Execute CBTRN02C batch job

**Expected Results:**
- Transaction written to DALYREJS file
- Rejection code 103
- Validation: ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS fails

---

### TC-BTCH-006: Balance Calculation Accuracy

| Attribute | Value |
|-----------|-------|
| **Category** | Integration |
| **Program** | CBTRN02C |
| **Priority** | P1 |
| **Preconditions** | Multiple transactions for same account |
| **Business Rules** | BR-C002, BR-C003, BR-C004, BR-C005 |
| **Source Reference** | CBTRN02C.cbl:527, 547-552 |

**Test Steps:**
1. Note initial account balance
2. Process multiple transactions (debits and credits)
3. Verify final balance calculation

**Expected Results:**
- ACCT-CURR-BAL updated correctly
- ACCT-CURR-CYC-CREDIT incremented for credits
- ACCT-CURR-CYC-DEBIT incremented for debits
- Category balances updated per category

---

### TC-BTCH-007: Empty Daily Transaction File

| Attribute | Value |
|-----------|-------|
| **Category** | Boundary |
| **Program** | CBTRN02C |
| **Priority** | P2 |
| **Preconditions** | Empty daily transaction file |
| **Business Rules** | BR-Q002 |

**Test Steps:**
1. Prepare empty daily transaction file
2. Execute CBTRN02C batch job

**Expected Results:**
- Job completes successfully
- Display: "TRANSACTIONS PROCESSED: 0"
- Display: "TRANSACTIONS REJECTED: 0"
- RETURN-CODE = 0

---

### TC-BTCH-008: File Open Error Handling

| Attribute | Value |
|-----------|-------|
| **Category** | Negative |
| **Program** | CBTRN02C |
| **Priority** | P2 |
| **Preconditions** | Missing or inaccessible input file |
| **Business Rules** | N/A |
| **Source Reference** | CBTRN02C.cbl:236-251 |

**Test Steps:**
1. Remove or rename daily transaction file
2. Execute CBTRN02C batch job

**Expected Results:**
- Error message displayed
- I/O status displayed
- Program abends gracefully

---

### TC-BTCH-009: Validation Sequence Order

| Attribute | Value |
|-----------|-------|
| **Category** | Integration |
| **Program** | CBTRN02C |
| **Priority** | P2 |
| **Preconditions** | Transaction failing multiple validations |
| **Business Rules** | BR-Q003 |
| **Source Reference** | CBTRN02C.cbl:211-216 |

**Test Steps:**
1. Prepare transaction that fails card validation (first check)
2. Same transaction would also fail credit limit
3. Execute CBTRN02C batch job

**Expected Results:**
- Only first validation failure reported
- Rejection code = 100 (invalid card), not 102 (overlimit)
- Validation order: Card → Account → Credit Limit → Expiration

---

### TC-BTCH-010: Category Balance Creation

| Attribute | Value |
|-----------|-------|
| **Category** | Integration |
| **Program** | CBTRN02C |
| **Priority** | P2 |
| **Preconditions** | Transaction for account with no TCATBAL record |
| **Business Rules** | BR-C002 |
| **Source Reference** | CBTRN02C.cbl:502-525 |

**Test Steps:**
1. Find account without existing TCATBAL record
2. Process transaction for that account
3. Verify TCATBAL file

**Expected Results:**
- New TCATBAL record created if not exists
- WS-CREATE-TRANCAT-REC flag = 'Y'
- Balance initialized with transaction amount

---

## 9. Integration Scenarios

### TC-INT-001: End-to-End Card Transaction Flow

| Attribute | Value |
|-----------|-------|
| **Category** | Integration |
| **Program** | Multiple (COCRDSLC, COCRDUPC, CBTRN02C) |
| **Priority** | P0 |
| **Preconditions** | Complete test environment |
| **Business Rules** | Multiple |

**Test Steps:**
1. Look up card via COCRDSLC
2. Update card status via COCRDUPC
3. Generate daily transaction for card
4. Process batch via CBTRN02C
5. Verify all files updated consistently

**Expected Results:**
- Card update reflected in CARDDAT
- Transaction posted to TRANSACT
- Account balance updated in ACCTDAT
- All cross-references maintained

---

### TC-INT-002: Admin to User Flow

| Attribute | Value |
|-----------|-------|
| **Category** | Integration |
| **Program** | COSGN00C, COADM01C, COMEN01C |
| **Priority** | P1 |
| **Preconditions** | Admin and regular user accounts |
| **Business Rules** | BR-A001, BR-A002 |

**Test Steps:**
1. Login as admin user
2. Verify admin menu options
3. Logout
4. Login as regular user
5. Verify user menu options

**Expected Results:**
- Admin sees COADM01C menu
- User sees COMEN01C menu
- Menu options filtered by user type

---

### TC-INT-003: Bill Payment to Batch Integration

| Attribute | Value |
|-----------|-------|
| **Category** | Integration |
| **Program** | COBIL00C, CBTRN02C |
| **Priority** | P1 |
| **Preconditions** | Online bill payment processed |
| **Business Rules** | BR-C006, BR-C008, BR-Q001 |

**Test Steps:**
1. Process online bill payment via COBIL00C
2. Verify transaction in TRANSACT
3. Run batch cycle
4. Verify no reprocessing of online transactions

**Expected Results:**
- Online payment in TRANSACT
- Batch skips already-posted transactions
- Account balance consistent

---

### TC-INT-004: Cross-Reference Consistency

| Attribute | Value |
|-----------|-------|
| **Category** | Integration |
| **Program** | Multiple |
| **Priority** | P1 |
| **Preconditions** | All data files present |

**Test Steps:**
1. Verify every card has XREF entry
2. Verify every XREF points to valid account
3. Verify every XREF points to valid customer
4. Process transactions for sampled cards

**Expected Results:**
- No orphan records
- All foreign keys valid
- Transaction processing succeeds

---

### TC-INT-005: Error Recovery Flow

| Attribute | Value |
|-----------|-------|
| **Category** | Integration |
| **Program** | COCRDUPC |
| **Priority** | P2 |
| **Preconditions** | Card update in progress |
| **Business Rules** | BR-S004 |
| **Source Reference** | COCRDUPC.cbl:205-210 |

**Test Steps:**
1. Start card update
2. Simulate concurrent update (record lock)
3. Verify error handling

**Expected Results:**
- Message: "Could not lock record for update" or
- Message: "Record changed by someone else. Please review"
- Data integrity maintained

---

## 10. Migration Validation Scenarios

### TC-MIG-001: Data Type Conversion - Signed Decimal

| Attribute | Value |
|-----------|-------|
| **Category** | Migration |
| **Priority** | P0 |
| **Focus** | ACCT-CURR-BAL PIC S9(10)V99 conversion |

**Test Steps:**
1. Extract accounts with various balance values
2. Migrate to target DECIMAL(12,2)
3. Compare pre/post values

**Expected Results:**
- Positive balances converted correctly
- Negative balances preserved
- Decimal precision maintained (2 decimal places)
- No data loss

---

### TC-MIG-002: Data Type Conversion - Dates

| Attribute | Value |
|-----------|-------|
| **Category** | Migration |
| **Priority** | P0 |
| **Focus** | Date fields (YYYY-MM-DD format) |

**Test Steps:**
1. Extract all date fields
2. Migrate to target DATE/TIMESTAMP
3. Compare pre/post values

**Expected Results:**
- All dates parsed correctly
- Leap year dates handled
- Edge dates (2000-02-29) converted
- No timezone issues

---

### TC-MIG-003: Record Count Validation

| Attribute | Value |
|-----------|-------|
| **Category** | Migration |
| **Priority** | P0 |
| **Focus** | Record counts match pre/post migration |

**Test Steps:**
1. Count records in each source file
2. Execute migration
3. Count records in target tables
4. Compare counts

**Expected Results:**
```
Expected Counts:
- Accounts: 50
- Cards: 50
- Customers: 50
- Transactions: 300
- Cross-References: 50
- Transaction Types: 7
- Categories: 18
- Discount Groups: 51
- Category Balances: 50
```

---

### TC-MIG-004: Key Value Preservation

| Attribute | Value |
|-----------|-------|
| **Category** | Migration |
| **Priority** | P0 |
| **Focus** | Primary keys unchanged |

**Test Steps:**
1. Extract all primary keys from source
2. Execute migration
3. Extract primary keys from target
4. Compare sets

**Expected Results:**
- All key values identical
- No truncation or padding
- No character set issues

---

### TC-MIG-005: Relationship Integrity

| Attribute | Value |
|-----------|-------|
| **Category** | Migration |
| **Priority** | P0 |
| **Focus** | Foreign key relationships maintained |

**Test Steps:**
1. Document all relationships in source
2. Execute migration
3. Verify foreign keys in target
4. Test join queries

**Expected Results:**
- Card → Account links valid
- XREF → Account, Customer links valid
- Transaction → Card links valid
- No orphan records created

---

### TC-MIG-006: Calculation Equivalence

| Attribute | Value |
|-----------|-------|
| **Category** | Migration |
| **Priority** | P1 |
| **Focus** | Business calculations match |

**Test Steps:**
1. Process sample transactions in legacy
2. Note calculated values
3. Process same transactions in migrated system
4. Compare results

**Expected Results:**
- Balance calculations match
- Interest calculations match
- Cycle totals match
- Rounding behavior consistent

---

### TC-MIG-007: Validation Rule Equivalence

| Attribute | Value |
|-----------|-------|
| **Category** | Migration |
| **Priority** | P1 |
| **Focus** | Validation rules produce same results |

**Test Steps:**
1. Test all BR-Vxxx rules in legacy
2. Test same rules in migrated system
3. Compare acceptance/rejection

**Expected Results:**
- Same inputs produce same validation results
- Error messages equivalent
- Edge cases handled identically

---

### TC-MIG-008: EBCDIC to UTF-8 Conversion

| Attribute | Value |
|-----------|-------|
| **Category** | Migration |
| **Priority** | P1 |
| **Focus** | Character encoding conversion |

**Test Steps:**
1. Extract text fields (names, addresses)
2. Migrate with encoding conversion
3. Verify character fidelity

**Expected Results:**
- All alphabetic characters preserved
- Special characters handled
- No mojibake or replacement characters
- Trailing spaces preserved or trimmed consistently

---

### TC-MIG-009: Timestamp Precision

| Attribute | Value |
|-----------|-------|
| **Category** | Migration |
| **Priority** | P2 |
| **Focus** | TRAN-ORIG-TS X(26) conversion |

**Test Steps:**
1. Extract timestamps from source
2. Migrate to target TIMESTAMP type
3. Compare precision

**Expected Results:**
- Date component preserved
- Time component preserved
- Microseconds handled correctly
- Timezone handling documented

---

### TC-MIG-010: FILLER Field Handling

| Attribute | Value |
|-----------|-------|
| **Category** | Migration |
| **Priority** | P2 |
| **Focus** | FILLER fields excluded from migration |

**Test Steps:**
1. Document all FILLER fields and sizes
2. Execute migration
3. Verify FILLER not migrated

**Expected Results:**
- FILLER fields excluded
- Record sizes appropriately smaller
- No padding bytes in target
- Data alignment correct

---

## 11. Scenario Summary

| Category | Count | P0 | P1 | P2 | P3 |
|----------|-------|----|----|----|----|
| Authentication | 6 | 4 | 2 | 0 | 0 |
| Account Management | 8 | 1 | 4 | 2 | 1 |
| Card Management | 14 | 1 | 10 | 3 | 0 |
| Bill Payment | 10 | 1 | 6 | 3 | 0 |
| Batch Processing | 10 | 1 | 5 | 4 | 0 |
| Integration | 5 | 1 | 3 | 1 | 0 |
| Migration | 10 | 5 | 3 | 2 | 0 |
| **TOTAL** | **78** | **14** | **33** | **15** | **1** |

---

## Cross-References

| Document | Relationship |
|----------|--------------|
| [BUSINESS-RULES.md](../01-domain-model/BUSINESS-RULES.md) | BR-xxx rule IDs referenced in scenarios |
| [TEST-COVERAGE.md](./TEST-COVERAGE.md) | Test data availability for scenarios |
| [EDGE-CASES.md](./EDGE-CASES.md) | Boundary conditions for boundary tests |
| [DATA-MODEL.md](../02-data-model/DATA-MODEL.md) | Record layouts for data scenarios |

---

*Document generated as part of RE-009 Test Coverage Analysis*
