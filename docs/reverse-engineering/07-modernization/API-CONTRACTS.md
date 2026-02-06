# CardDemo API Contracts

## Overview

This document provides the complete OpenAPI 3.0.3 specifications for CardDemo modernization APIs. Each specification maps directly to COBOL program interfaces, preserving business logic while providing RESTful access.

**Contract Files Location:** `.work/reverse-engineering/modernization/api/contracts/`

| API | File | Source Programs |
|-----|------|-----------------|
| Account API | `account-api.yaml` | COACTVWC |
| Card API | `card-api.yaml` | COCRDSLC, COCRDUPC, COCRDLIC |
| Transaction API | `transaction-api.yaml` | COTRN01C, COTRN00C |
| Payment API | `payment-api.yaml` | COBIL00C |
| Auth API | `auth-api.yaml` | COSGN00C |

---

## 1. Authentication API

### POST /auth/token

**Source:** `COSGN00C.cbl`

Authenticates user credentials and returns a JWT access token.

#### Request

```json
{
  "userId": "ADMIN001",
  "password": "PASSWORD"
}
```

**COBOL Mapping:**
| JSON Field | COBOL Field | PIC Clause |
|------------|-------------|------------|
| userId | WS-USER-ID | PIC X(08) |
| password | WS-USER-PWD | PIC X(08) |

#### Response (200 OK)

```json
{
  "accessToken": "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9...",
  "tokenType": "Bearer",
  "expiresIn": 3600,
  "scope": "carddemo:admin accounts:read accounts:write cards:read cards:write transactions:read transactions:write users:read users:write",
  "user": {
    "userId": "ADMIN001",
    "firstName": "SYSTEM",
    "lastName": "ADMINISTRATOR",
    "userType": "admin"
  }
}
```

**COBOL Mapping:**
| JSON Field | COBOL Field | Transformation |
|------------|-------------|----------------|
| user.userId | SEC-USR-ID | Direct |
| user.firstName | SEC-USR-FNAME | Trim spaces |
| user.lastName | SEC-USR-LNAME | Trim spaces |
| user.userType | SEC-USR-TYPE | 'A' → "admin", 'U' → "user" |
| scope | SEC-USR-TYPE | Map to OAuth scopes |

#### Scope Assignment Logic

```cobol
* From COSGN00C.cbl lines 230-239
IF CDEMO-USRTYP-ADMIN
   * Scopes: carddemo:admin + all read/write
ELSE
   * Scopes: carddemo:user + read + transactions:write
END-IF
```

#### Error Responses

| HTTP | Code | COBOL Condition |
|------|------|-----------------|
| 400 | MISSING_USER_ID | `USERIDI = SPACES OR LOW-VALUES` |
| 400 | MISSING_PASSWORD | `PASSWDI = SPACES OR LOW-VALUES` |
| 401 | USER_NOT_FOUND | `DFHRESP = 13 (NOTFND)` |
| 401 | INVALID_CREDENTIALS | `SEC-USR-PWD NOT = WS-USER-PWD` |

---

## 2. Account API

### GET /accounts/{accountId}

**Source:** `COACTVWC.cbl`

Retrieves account information with associated customer details.

#### Path Parameters

| Parameter | Type | Pattern | COBOL Field |
|-----------|------|---------|-------------|
| accountId | string | `^\d{11}$` | CDEMO-ACCT-ID |

#### Response (200 OK)

```json
{
  "accountId": "00000000001",
  "status": "ACTIVE",
  "currentBalance": 1500.00,
  "creditLimit": 5000.00,
  "cashCreditLimit": 1000.00,
  "openDate": "2020-01-15",
  "expirationDate": "2025-01-15",
  "reissueDate": "2023-01-15",
  "currentCycleCredit": 500.00,
  "currentCycleDebit": 200.00,
  "groupId": "GROUP001",
  "customer": {
    "customerId": "000000001",
    "firstName": "JOHN",
    "middleName": "Q",
    "lastName": "PUBLIC",
    "ssn": "XXX-XX-1234",
    "dateOfBirth": "1980-05-15",
    "ficoScore": 750,
    "address": {
      "line1": "123 MAIN STREET",
      "line2": "APT 4B",
      "city": "SPRINGFIELD",
      "state": "IL",
      "zipCode": "62701",
      "country": "USA"
    },
    "primaryPhone": "555-123-4567",
    "secondaryPhone": "555-987-6543"
  }
}
```

**COBOL Field Mappings (Account - CVACT01Y):**

| JSON Path | COBOL Field | PIC | Transformation |
|-----------|-------------|-----|----------------|
| accountId | ACCT-ID | 9(11) | Pad with leading zeros |
| status | ACCT-ACTIVE-STATUS | X(01) | 'Y' → "ACTIVE", 'N' → "INACTIVE" |
| currentBalance | ACCT-CURR-BAL | S9(10)V99 | Decimal |
| creditLimit | ACCT-CREDIT-LIMIT | S9(10)V99 | Decimal |
| cashCreditLimit | ACCT-CASH-CREDIT-LIMIT | S9(10)V99 | Decimal |
| openDate | ACCT-OPEN-DATE | X(10) | ISO date |
| expirationDate | ACCT-EXPIRAION-DATE | X(10) | ISO date |
| reissueDate | ACCT-REISSUE-DATE | X(10) | ISO date |
| currentCycleCredit | ACCT-CURR-CYC-CREDIT | S9(10)V99 | Decimal |
| currentCycleDebit | ACCT-CURR-CYC-DEBIT | S9(10)V99 | Decimal |
| groupId | ACCT-GROUP-ID | X(10) | Trim |

**COBOL Field Mappings (Customer - CVCUS01Y):**

| JSON Path | COBOL Field | PIC | Transformation |
|-----------|-------------|-----|----------------|
| customer.customerId | CUST-ID | 9(09) | Pad zeros |
| customer.firstName | CUST-FIRST-NAME | X(25) | Trim |
| customer.lastName | CUST-LAST-NAME | X(25) | Trim |
| customer.ssn | CUST-SSN | 9(09) | Mask: `XXX-XX-{last4}` |
| customer.dateOfBirth | CUST-DOB-YYYY-MM-DD | X(10) | ISO date |
| customer.ficoScore | CUST-FICO-CREDIT-SCORE | 9(03) | Integer |
| customer.address.line1 | CUST-ADDR-LINE-1 | X(50) | Trim |
| customer.address.state | CUST-ADDR-STATE-CD | X(02) | Direct |
| customer.address.zipCode | CUST-ADDR-ZIP | X(10) | Trim |
| customer.primaryPhone | CUST-PHONE-NUM-1 | X(15) | Trim |

#### Data Flow

```
Request: GET /accounts/00000000001

1. CXACAIX READ (key: ACCT-ID)
   ├─ Success → Get CUST-ID
   └─ NOTFND → 404

2. ACCTDAT READ (key: ACCT-ID)
   ├─ Success → Account record
   └─ NOTFND → 404

3. CUSTDAT READ (key: CUST-ID)
   ├─ Success → Customer record
   └─ NOTFND → 404

Response: Combined account + customer JSON
```

---

## 3. Card API

### GET /cards/{cardNumber}

**Source:** `COCRDSLC.cbl`

#### Path Parameters

| Parameter | Type | Pattern | COBOL Field |
|-----------|------|---------|-------------|
| cardNumber | string | `^\d{16}$` | WS-CARD-RID-CARDNUM |

#### Response (200 OK)

```json
{
  "cardNumber": "4111111111111111",
  "accountId": "00000000001",
  "embossedName": "JOHN Q PUBLIC",
  "expirationDate": "2025-12-31",
  "status": "ACTIVE"
}
```

**COBOL Field Mappings (CVACT02Y):**

| JSON Path | COBOL Field | PIC | Notes |
|-----------|-------------|-----|-------|
| cardNumber | CARD-NUM | X(16) | Direct |
| accountId | CARD-ACCT-ID | 9(11) | Pad zeros |
| embossedName | CARD-EMBOSSED-NAME | X(50) | Trim |
| expirationDate | CARD-EXPIRAION-DATE | X(10) | ISO date |
| status | CARD-ACTIVE-STATUS | X(01) | Y/N to enum |

**Security Note:** `CARD-CVV-CD` (PIC 9(03)) is NEVER included in API responses.

### PUT /cards/{cardNumber}

**Source:** `COCRDUPC.cbl`

#### Request

```json
{
  "embossedName": "JOHN Q PUBLIC JR",
  "status": "INACTIVE"
}
```

### GET /accounts/{accountId}/cards

**Source:** `COCRDLIC.cbl`

#### Response (200 OK)

```json
{
  "items": [
    {
      "cardNumber": "4111111111111111",
      "accountId": "00000000001",
      "embossedName": "JOHN Q PUBLIC",
      "expirationDate": "2025-12-31",
      "status": "ACTIVE"
    }
  ],
  "pagination": {
    "hasMore": false,
    "nextCursor": null,
    "totalCount": 1
  }
}
```

---

## 4. Transaction API

### GET /transactions/{transactionId}

**Source:** `COTRN01C.cbl`

#### Path Parameters

| Parameter | Type | Pattern | COBOL Field |
|-----------|------|---------|-------------|
| transactionId | string | `^\d{16}$` | TRAN-ID |

#### Response (200 OK)

```json
{
  "transactionId": "0000000000000001",
  "typeCode": "01",
  "categoryCode": 5411,
  "source": "POS TERM",
  "description": "AMAZON.COM PURCHASE",
  "amount": 99.99,
  "cardNumber": "XXXX-XXXX-XXXX-1111",
  "originTimestamp": "2024-01-15T14:30:00Z",
  "processedTimestamp": "2024-01-15T14:30:05Z",
  "merchant": {
    "id": "123456789",
    "name": "AMAZON.COM",
    "city": "SEATTLE",
    "zipCode": "98101"
  }
}
```

**COBOL Field Mappings (CVTRA05Y):**

| JSON Path | COBOL Field | PIC | Transformation |
|-----------|-------------|-----|----------------|
| transactionId | TRAN-ID | X(16) | Direct |
| typeCode | TRAN-TYPE-CD | X(02) | Direct |
| categoryCode | TRAN-CAT-CD | 9(04) | Integer |
| source | TRAN-SOURCE | X(10) | Trim |
| description | TRAN-DESC | X(100) | Trim |
| amount | TRAN-AMT | S9(09)V99 | Decimal |
| cardNumber | TRAN-CARD-NUM | X(16) | Mask last 4 |
| originTimestamp | TRAN-ORIG-TS | X(26) | ISO 8601 |
| processedTimestamp | TRAN-PROC-TS | X(26) | ISO 8601 |
| merchant.id | TRAN-MERCHANT-ID | 9(09) | String |
| merchant.name | TRAN-MERCHANT-NAME | X(50) | Trim |
| merchant.city | TRAN-MERCHANT-CITY | X(50) | Trim |
| merchant.zipCode | TRAN-MERCHANT-ZIP | X(10) | Trim |

### GET /transactions

**Source:** `COTRN00C.cbl`

#### Query Parameters

| Parameter | Default | COBOL Mapping |
|-----------|---------|---------------|
| cursor | null | TRAN-ID for STARTBR |
| limit | 10 | Fixed page size in COBOL |
| direction | forward | PF8 (forward) / PF7 (backward) |

#### Response (200 OK)

```json
{
  "items": [
    {
      "transactionId": "0000000000000001",
      "description": "AMAZON.COM PURCHASE",
      "amount": 99.99,
      "date": "2024-01-15"
    }
  ],
  "pagination": {
    "firstId": "0000000000000001",
    "lastId": "0000000000000010",
    "currentPage": 1,
    "hasNextPage": true,
    "hasPrevPage": false
  }
}
```

**Pagination Mapping:**

| JSON | COBOL | Notes |
|------|-------|-------|
| firstId | CDEMO-CT00-TRNID-FIRST | First ID on page |
| lastId | CDEMO-CT00-TRNID-LAST | Last ID on page |
| currentPage | CDEMO-CT00-PAGE-NUM | Page counter |
| hasNextPage | NEXT-PAGE-YES | 88-level condition |

---

## 5. Payment API

### POST /accounts/{accountId}/payments

**Source:** `COBIL00C.cbl`

Creates a bill payment that pays the full account balance.

#### Request

```json
{
  "confirm": true,
  "idempotencyKey": "550e8400-e29b-41d4-a716-446655440000"
}
```

**COBOL Mapping:**
| JSON | COBOL | Values |
|------|-------|--------|
| confirm | CONFIRMI | true='Y', false='N' |

#### Response (201 Created)

```json
{
  "transactionId": "0000000000000123",
  "paymentAmount": 1500.00,
  "newBalance": 0.00,
  "processedTimestamp": "2024-01-15T14:30:05Z",
  "message": "Payment successful"
}
```

**Transaction Logic (from COBIL00C lines 210-243):**

```cobol
* 1. Generate new transaction ID
MOVE HIGH-VALUES TO TRAN-ID
PERFORM STARTBR-TRANSACT-FILE
PERFORM READPREV-TRANSACT-FILE  * Get max ID
ADD 1 TO WS-TRAN-ID-NUM

* 2. Create transaction record
INITIALIZE TRAN-RECORD
MOVE WS-TRAN-ID-NUM TO TRAN-ID
MOVE '02'           TO TRAN-TYPE-CD        * Bill payment type
MOVE 2              TO TRAN-CAT-CD
MOVE 'POS TERM'     TO TRAN-SOURCE
MOVE 'BILL PAYMENT - ONLINE' TO TRAN-DESC
MOVE ACCT-CURR-BAL  TO TRAN-AMT

* 3. Update account balance
COMPUTE ACCT-CURR-BAL = ACCT-CURR-BAL - TRAN-AMT
```

#### Error Responses

| HTTP | Code | COBOL Condition | Message |
|------|------|-----------------|---------|
| 404 | ACCT_NOT_FOUND | DFHRESP(NOTFND) on ACCTDAT | "Account ID NOT found" |
| 409 | DUPLICATE_TRAN | DFHRESP(DUPREC) on TRANSACT | "Tran ID already exist" |
| 422 | NO_BALANCE_DUE | ACCT-CURR-BAL <= 0 | "You have nothing to pay" |

---

## Error Response Format

All APIs use a consistent error response format:

```json
{
  "error": {
    "code": "ERROR_CODE",
    "message": "Human-readable message",
    "details": {
      "field": "fieldName",
      "constraint": "validation rule"
    }
  }
}
```

### HTTP Status Code Mapping

| COBOL Condition | HTTP | Error Code |
|-----------------|------|------------|
| Success | 200/201 | - |
| DFHRESP(NOTFND) | 404 | RESOURCE_NOT_FOUND |
| Empty required field | 400 | VALIDATION_ERROR |
| Non-numeric where required | 400 | VALIDATION_ERROR |
| Wrong password | 401 | INVALID_CREDENTIALS |
| User type restriction | 403 | FORBIDDEN |
| Business rule (balance <= 0) | 422 | BUSINESS_RULE_VIOLATION |
| DFHRESP(DUPREC) | 409 | DUPLICATE_RESOURCE |
| DFHRESP(other) | 500 | INTERNAL_ERROR |

---

## Security

### JWT Token Structure

```json
{
  "header": {
    "alg": "RS256",
    "typ": "JWT"
  },
  "payload": {
    "sub": "ADMIN001",
    "name": "SYSTEM ADMINISTRATOR",
    "type": "admin",
    "scope": "carddemo:admin accounts:read ...",
    "iat": 1704067200,
    "exp": 1704070800
  }
}
```

### Scope Requirements by Endpoint

| Endpoint | Required Scope |
|----------|----------------|
| GET /accounts/* | accounts:read |
| GET /cards/* | cards:read |
| PUT /cards/* | cards:write |
| GET /transactions/* | transactions:read |
| POST /accounts/*/payments | transactions:write |
| GET /admin/users | users:read |
| POST/PUT/DELETE /admin/users/* | users:write |

---

## Data Masking

Sensitive fields are masked in API responses:

| Field | Original | Masked |
|-------|----------|--------|
| SSN | 123456789 | XXX-XX-6789 |
| Card Number | 4111111111111111 | XXXX-XXXX-XXXX-1111 |
| CVV | 123 | Never returned |

---

## Validation Reference

### Common Patterns

| Type | Regex Pattern | Example |
|------|---------------|---------|
| Account ID | `^\d{11}$` | 00000000001 |
| Card Number | `^\d{16}$` | 4111111111111111 |
| Transaction ID | `^\d{16}$` | 0000000000000001 |
| User ID | `^[A-Z0-9]{1,8}$` | ADMIN001 |
| Date | `^\d{4}-\d{2}-\d{2}$` | 2024-01-15 |

---

## Contract Files

Full OpenAPI 3.0.3 specifications are available at:

```
.work/reverse-engineering/modernization/api/contracts/
├── account-api.yaml      # Account operations
├── card-api.yaml         # Card operations
├── transaction-api.yaml  # Transaction operations
├── payment-api.yaml      # Payment operations
└── auth-api.yaml         # Authentication
```

### Validation Command

```bash
# Validate all contracts
redocly lint .work/reverse-engineering/modernization/api/contracts/*.yaml

# Preview API documentation
redocly preview-docs .work/reverse-engineering/modernization/api/contracts/account-api.yaml
```
