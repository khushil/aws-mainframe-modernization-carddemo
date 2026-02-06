# CardDemo API Candidate Analysis

## Executive Summary

This document identifies COBOL programs in the CardDemo application suitable for API exposure. Using a weighted 4-factor scoring framework, we evaluated 18 programs and identified 7 high-priority, 5 medium-priority, and 3 low-priority candidates for API modernization.

**Key Findings:**
- 15 of 18 programs are suitable for API exposure
- 7 programs recommended for immediate API implementation (Wave 1-2)
- Read-only operations score highest due to statelessness and simplicity
- Bill payment (COBIL00C) is the highest-value write operation

## Scoring Methodology

### 4-Factor Framework

| Factor | Weight | Description | Scale |
|--------|--------|-------------|-------|
| **Statelessness** | 30% | Ability to operate without session state | 1-5 (5=fully stateless) |
| **Data Simplicity** | 25% | Complexity of input/output structures | 1-5 (5=simple primitives) |
| **Business Value** | 25% | Frequency of use and business criticality | 1-5 (5=core operation) |
| **Isolation** | 20% | Dependencies on other programs | 1-5 (5=standalone) |

**Formula:** `Score = (S × 0.30) + (D × 0.25) + (B × 0.25) + (I × 0.20)`

### Priority Thresholds

| Priority | Score Range | Recommendation |
|----------|-------------|----------------|
| High | ≥ 3.75 | Immediate API implementation |
| Medium | 3.00 - 3.74 | Second phase implementation |
| Low | 2.50 - 2.99 | Consider for future phases |
| Not Suitable | < 2.50 | Do not expose as API |

---

## High Priority Candidates (Score ≥ 3.75)

### 1. COCRDSLC - Card Details View
**Score: 4.75** | `GET /cards/{cardNumber}`

| Factor | Score | Rationale |
|--------|-------|-----------|
| Statelessness | 5 | Single file read, no session context |
| Data Simplicity | 5 | Simple 6-field card record |
| Business Value | 4 | Common lookup operation |
| Isolation | 5 | Reads only CARDDAT |

**Source:** `app/cbl/COCRDSLC.cbl` (lines 736-777)

**Files:** CARDDAT (read)

**Implementation Notes:**
- CVV must be excluded from API response
- Card number can be used as natural key
- Simplest candidate to implement

---

### 2. COACTVWC - Account View
**Score: 4.55** | `GET /accounts/{accountId}`

| Factor | Score | Rationale |
|--------|-------|-----------|
| Statelessness | 5 | Reads only, no state modifications |
| Data Simplicity | 4 | Complex but structured output (account + customer) |
| Business Value | 5 | Core business operation |
| Isolation | 4 | Reads 3 files but no writes |

**Source:** `app/cbl/COACTVWC.cbl` (lines 687-721)

**Files:** CXACAIX (read), ACCTDAT (read), CUSTDAT (read)

**Implementation Notes:**
- SSN must be masked in response (XXX-XX-1234)
- Join across 3 files requires careful error handling
- Most complete view of customer relationship

---

### 3. COTRN01C - Transaction View
**Score: 4.50** | `GET /transactions/{transactionId}`

| Factor | Score | Rationale |
|--------|-------|-----------|
| Statelessness | 5 | Single read operation |
| Data Simplicity | 4 | Transaction record with merchant details |
| Business Value | 4 | Transaction inquiry is common |
| Isolation | 5 | Reads only TRANSACT |

**Source:** `app/cbl/COTRN01C.cbl` (lines 267-296)

**Files:** TRANSACT (read)

**Implementation Notes:**
- Card number must be masked
- Timestamps need ISO 8601 conversion
- Merchant details nested in response

---

### 4. COSGN00C - User Authentication
**Score: 4.45** | `POST /auth/token`

| Factor | Score | Rationale |
|--------|-------|-----------|
| Statelessness | 4 | Creates session context but stateless request |
| Data Simplicity | 5 | Simple user ID + password input |
| Business Value | 5 | Essential for API security |
| Isolation | 4 | Reads USRSEC, transfers to menu |

**Source:** `app/cbl/COSGN00C.cbl` (lines 108-257)

**Files:** USRSEC (read)

**Implementation Notes:**
- Map SEC-USR-TYPE to OAuth scopes
- Convert to JWT token response
- Admin (A) vs User (U) scope differences

---

### 5. COCRDLIC - List Cards for Account
**Score: 4.00** | `GET /accounts/{accountId}/cards`

| Factor | Score | Rationale |
|--------|-------|-----------|
| Statelessness | 4 | Browse operation with cursor state |
| Data Simplicity | 4 | Array of card records |
| Business Value | 4 | Common account inquiry |
| Isolation | 4 | Uses CARDAIX alternate index |

**Source:** `app/cbl/COCRDLIC.cbl`

**Files:** CARDAIX (browse)

**Implementation Notes:**
- Implement cursor-based pagination
- Account ID as path parameter

---

### 6. COTRN00C - Transaction List
**Score: 3.75** | `GET /transactions`

| Factor | Score | Rationale |
|--------|-------|-----------|
| Statelessness | 4 | Maintains page cursor |
| Data Simplicity | 3 | Returns array with pagination metadata |
| Business Value | 4 | Statement/history view |
| Isolation | 4 | Browses TRANSACT only |

**Source:** `app/cbl/COTRN00C.cbl`

**Files:** TRANSACT (browse)

**Implementation Notes:**
- COBOL uses fixed 10-item pages
- Map PF7/PF8 to cursor parameters
- Consider limit parameter for flexibility

---

### 7. COBIL00C - Bill Payment
**Score: 3.75** | `POST /accounts/{accountId}/payments`

| Factor | Score | Rationale |
|--------|-------|-----------|
| Statelessness | 3 | Multi-step transaction with locks |
| Data Simplicity | 4 | Simple confirmation input, structured output |
| Business Value | 5 | High-value payment operation |
| Isolation | 3 | Reads/writes multiple files |

**Source:** `app/cbl/COBIL00C.cbl` (lines 154-244)

**Files:** ACCTDAT (read/update), CXACAIX (read), TRANSACT (write)

**Implementation Notes:**
- Pays full balance only (per COBOL logic)
- Returns generated transaction ID
- Requires idempotency key for safety

---

## Medium Priority Candidates (Score 3.00 - 3.74)

| Program | Function | Score | API Endpoint |
|---------|----------|-------|--------------|
| COUSR00C | List Users | 3.50 | `GET /admin/users` |
| COUSR03C | Delete User | 3.40 | `DELETE /admin/users/{userId}` |
| COCRDUPC | Update Card | 3.25 | `PUT /cards/{cardNumber}` |
| COUSR01C | Create User | 3.20 | `POST /admin/users` |
| COUSR02C | Update User | 3.20 | `PUT /admin/users/{userId}` |

**Common Characteristics:**
- Admin-only operations (lower business value for general API)
- Write operations requiring more validation
- Suitable for internal admin API

---

## Low Priority Candidates (Score 2.50 - 2.99)

| Program | Function | Score | Reason |
|---------|----------|-------|--------|
| COTRN02C | Add Transaction | 2.70 | Complex validation, multiple file updates |
| COACTUPC | Update Account | 2.45 | Cascading updates across files |
| CORPT00C | Report Submit | 2.25 | Batch-oriented, async processing needed |

---

## Not Suitable for API (Score < 2.50)

| Program | Function | Score | Reason |
|---------|----------|-------|--------|
| COADM01C | Admin Menu | 2.15 | UI navigation only |
| COMEN01C | User Menu | 2.15 | UI navigation only |
| COBSWAIT | Batch Wait | 1.80 | Internal batch coordination |

---

## Implementation Roadmap

### Wave 1: Read-Only APIs (Lowest Risk)

| Order | Program | Endpoint | Risk | Value |
|-------|---------|----------|------|-------|
| 1 | COSGN00C | `POST /auth/token` | Low | High |
| 2 | COCRDSLC | `GET /cards/{cardNumber}` | Low | Medium |
| 3 | COACTVWC | `GET /accounts/{accountId}` | Low | High |
| 4 | COTRN01C | `GET /transactions/{id}` | Low | Medium |

**Rationale:** Establish authentication, then expose read-only data access. No risk of data modification.

### Wave 2: List Operations

| Order | Program | Endpoint | Risk | Value |
|-------|---------|----------|------|-------|
| 5 | COTRN00C | `GET /transactions` | Low | Medium |
| 6 | COCRDLIC | `GET /accounts/{id}/cards` | Low | Medium |

**Rationale:** Add pagination patterns for browsing data.

### Wave 3: Write Operations

| Order | Program | Endpoint | Risk | Value |
|-------|---------|----------|------|-------|
| 7 | COBIL00C | `POST /accounts/{id}/payments` | Medium | High |
| 8 | COCRDUPC | `PUT /cards/{cardNumber}` | Medium | Medium |

**Rationale:** Carefully introduce write operations with proper validation and idempotency.

---

## REST Resource Design

### Resource Hierarchy

```
/auth
  POST /token          - COSGN00C

/accounts
  GET /{accountId}     - COACTVWC
  POST /{accountId}/payments - COBIL00C
  GET /{accountId}/cards     - COCRDLIC

/cards
  GET /{cardNumber}    - COCRDSLC
  PUT /{cardNumber}    - COCRDUPC

/transactions
  GET /                - COTRN00C
  GET /{transactionId} - COTRN01C
```

### HTTP Method Mapping

| COBOL Pattern | HTTP Method |
|---------------|-------------|
| READ | GET |
| BROWSE (STARTBR/READNEXT) | GET (list) |
| WRITE | POST |
| REWRITE | PUT |
| DELETE | DELETE |

---

## COBOL-to-JSON Type Mapping

| COBOL Type | JSON Type | Format | Example |
|------------|-----------|--------|---------|
| `PIC 9(11)` | string | `^\d{11}$` | "00000000001" |
| `PIC 9(16)` | string | `^\d{16}$` | "4111111111111111" |
| `PIC X(n)` | string | maxLength: n | "JOHN DOE" |
| `PIC S9(10)V99` | number | decimal | 1234.56 |
| `PIC X(10)` date | string | date (ISO 8601) | "2024-12-31" |
| `PIC X(26)` timestamp | string | date-time | "2024-12-31T23:59:59Z" |
| 88-level Y/N | string | enum | "ACTIVE" / "INACTIVE" |

---

## Error Code Mapping

| COBOL Condition | HTTP Status | Error Code |
|-----------------|-------------|------------|
| DFHRESP(NORMAL) | 200/201 | - |
| DFHRESP(NOTFND) | 404 | RESOURCE_NOT_FOUND |
| Validation error | 400 | VALIDATION_ERROR |
| Wrong password | 401 | INVALID_CREDENTIALS |
| User type check | 403 | FORBIDDEN |
| Balance <= 0 | 422 | BUSINESS_RULE_VIOLATION |
| DFHRESP(DUPREC) | 409 | DUPLICATE_RESOURCE |
| Other file error | 500 | INTERNAL_ERROR |

---

## Security Scope Matrix

| Scope | Description | Admin | User |
|-------|-------------|-------|------|
| `carddemo:admin` | Admin access | ✓ | |
| `carddemo:user` | User access | ✓ | ✓ |
| `accounts:read` | View accounts | ✓ | ✓ |
| `accounts:write` | Modify accounts | ✓ | |
| `cards:read` | View cards | ✓ | ✓ |
| `cards:write` | Modify cards | ✓ | |
| `transactions:read` | View transactions | ✓ | ✓ |
| `transactions:write` | Create payments | ✓ | ✓ |
| `users:read` | View users | ✓ | |
| `users:write` | Manage users | ✓ | |

**Mapping from COBOL:**
- `CDEMO-USRTYP-ADMIN` (value 'A') → Admin scopes
- `CDEMO-USRTYP-USER` (value 'U') → User scopes

---

## Appendix: Full Scoring Matrix

| # | Program | Function | S | D | B | I | Score | Priority |
|---|---------|----------|---|---|---|---|-------|----------|
| 1 | COCRDSLC | View Card | 5 | 5 | 4 | 5 | **4.75** | High |
| 2 | COACTVWC | View Account | 5 | 4 | 5 | 4 | **4.55** | High |
| 3 | COTRN01C | View Transaction | 5 | 4 | 4 | 5 | **4.50** | High |
| 4 | COSGN00C | Authentication | 4 | 5 | 5 | 4 | **4.45** | High |
| 5 | COCRDLIC | List Cards | 4 | 4 | 4 | 4 | **4.00** | High |
| 6 | COTRN00C | List Transactions | 4 | 3 | 4 | 4 | **3.75** | High |
| 7 | COBIL00C | Bill Payment | 3 | 4 | 5 | 3 | **3.75** | High |
| 8 | COUSR00C | List Users | 4 | 4 | 2 | 4 | **3.50** | Medium |
| 9 | COUSR03C | Delete User | 3 | 5 | 2 | 4 | **3.40** | Medium |
| 10 | COCRDUPC | Update Card | 3 | 3 | 4 | 3 | **3.25** | Medium |
| 11 | COUSR01C | Create User | 3 | 4 | 2 | 4 | **3.20** | Medium |
| 12 | COUSR02C | Update User | 3 | 4 | 2 | 4 | **3.20** | Medium |
| 13 | COTRN02C | Add Transaction | 2 | 2 | 4 | 3 | **2.70** | Low |
| 14 | COACTUPC | Update Account | 2 | 2 | 4 | 2 | **2.45** | Low |
| 15 | CORPT00C | Report Submit | 2 | 3 | 2 | 2 | **2.25** | Low |
| 16 | COADM01C | Admin Menu | 1 | 5 | 1 | 2 | **2.15** | Not Suitable |
| 17 | COMEN01C | User Menu | 1 | 5 | 1 | 2 | **2.15** | Not Suitable |
| 18 | COBSWAIT | Batch Wait | 1 | 5 | 1 | 1 | **1.80** | Not Suitable |

---

## References

- OpenAPI contracts: `.work/reverse-engineering/modernization/api/contracts/`
- Interface analysis: `.work/reverse-engineering/modernization/api/interface-analysis.yaml`
- Scoring details: `.work/reverse-engineering/modernization/api/api-scores.yaml`
