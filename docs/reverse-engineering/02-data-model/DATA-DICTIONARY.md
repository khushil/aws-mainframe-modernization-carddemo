# CardDemo Data Dictionary

## Document Information

| Attribute | Value |
|-----------|-------|
| **Version** | 1.0 |
| **Last Updated** | 2026-02-05 |
| **Source Analysis** | RE-002 Data Model Extraction |
| **Total Entities** | 10 |
| **Total Fields** | 80 leaf-level fields (87 including group-level items) |

---

## 1. COBOL-to-SQL Type Mapping Reference

This mapping table provides guidance for converting COBOL PIC clauses to SQL data types during modernization.

### 1.1 Alphanumeric Types

| COBOL PIC | SQL Type | Storage | Notes |
|-----------|----------|---------|-------|
| `PIC X(n)` | `VARCHAR(n)` or `CHAR(n)` | n bytes | Use CHAR for fixed-length codes |
| `PIC A(n)` | `VARCHAR(n)` | n bytes | Alphabetic only |

### 1.2 Numeric Types (DISPLAY Format)

CardDemo uses DISPLAY format (character storage) for all numeric fields.

| COBOL PIC | SQL Type | COBOL Storage | Notes |
|-----------|----------|---------------|-------|
| `PIC 9(n)` | `NUMERIC(n)` or `BIGINT` | n bytes | Unsigned integer |
| `PIC S9(n)` | `NUMERIC(n)` | n bytes | Signed integer |
| `PIC 9(n)V99` | `DECIMAL(n+2,2)` | n+2 bytes | Implied decimal |
| `PIC S9(n)V99` | `DECIMAL(n+2,2)` | n+2 bytes | Signed with implied decimal |

### 1.3 Packed Decimal (COMP-3) - NOT USED IN CARDDEMO

For reference only - CardDemo does not use COMP-3:

| COBOL PIC | SQL Type | COBOL Storage | Notes |
|-----------|----------|---------------|-------|
| `PIC S9(n)V99 COMP-3` | `DECIMAL(n+2,2)` | ceil((n+3)/2) bytes | Packed decimal |

### 1.4 Binary Types

| COBOL PIC | SQL Type | COBOL Storage | Notes |
|-----------|----------|---------------|-------|
| `PIC S9(4) COMP` | `SMALLINT` | 2 bytes | Half-word binary |
| `PIC S9(9) COMP` | `INTEGER` | 4 bytes | Full-word binary |
| `PIC S9(18) COMP` | `BIGINT` | 8 bytes | Double-word binary |

### 1.5 Date/Time Types

| COBOL Format | SQL Type | Example |
|--------------|----------|---------|
| `PIC X(10)` (YYYY-MM-DD) | `DATE` | 2024-01-15 |
| `PIC X(26)` (DB2 timestamp) | `TIMESTAMP` | 2024-01-15-10.30.45.123456 |

---

## 2. Entity: ACCOUNT (ACCTDAT)

**Source Copybook:** `app/cpy/CVACT01Y.cpy`
**Record Length:** 300 bytes
**Primary Key:** ACCT-ID

| # | Field Name | COBOL PIC | Offset | Length | SQL Type | Description |
|---|------------|-----------|--------|--------|----------|-------------|
| 1 | ACCT-ID | 9(11) | 0 | 11 | BIGINT | Account identifier (primary key) |
| 2 | ACCT-ACTIVE-STATUS | X(01) | 11 | 1 | CHAR(1) | Account status flag |
| 3 | ACCT-CURR-BAL | S9(10)V99 | 12 | 12 | DECIMAL(12,2) | Current account balance |
| 4 | ACCT-CREDIT-LIMIT | S9(10)V99 | 24 | 12 | DECIMAL(12,2) | Credit limit amount |
| 5 | ACCT-CASH-CREDIT-LIMIT | S9(10)V99 | 36 | 12 | DECIMAL(12,2) | Cash advance limit |
| 6 | ACCT-OPEN-DATE | X(10) | 48 | 10 | DATE | Account opening date |
| 7 | ACCT-EXPIRAION-DATE | X(10) | 58 | 10 | DATE | Account expiration date |
| 8 | ACCT-REISSUE-DATE | X(10) | 68 | 10 | DATE | Card reissue date |
| 9 | ACCT-CURR-CYC-CREDIT | S9(10)V99 | 78 | 12 | DECIMAL(12,2) | Current cycle credits |
| 10 | ACCT-CURR-CYC-DEBIT | S9(10)V99 | 90 | 12 | DECIMAL(12,2) | Current cycle debits |
| 11 | ACCT-ADDR-ZIP | X(10) | 102 | 10 | VARCHAR(10) | Account billing ZIP code |
| 12 | ACCT-GROUP-ID | X(10) | 112 | 10 | VARCHAR(10) | Disclosure group reference |
| 13 | FILLER | X(178) | 122 | 178 | - | Reserved for expansion |

### Value Domains (88-Level Conditions)

| Field | Value | Meaning |
|-------|-------|---------|
| ACCT-ACTIVE-STATUS | 'Y' | Active account |
| ACCT-ACTIVE-STATUS | 'N' | Inactive/Closed account |

### SQL DDL

```sql
CREATE TABLE ACCOUNT (
    acct_id                 BIGINT          NOT NULL,
    acct_active_status      CHAR(1)         NOT NULL DEFAULT 'Y',
    acct_curr_bal           DECIMAL(12,2)   NOT NULL DEFAULT 0.00,
    acct_credit_limit       DECIMAL(12,2)   NOT NULL,
    acct_cash_credit_limit  DECIMAL(12,2)   NOT NULL,
    acct_open_date          DATE            NOT NULL,
    acct_expiration_date    DATE            NOT NULL,
    acct_reissue_date       DATE,
    acct_curr_cyc_credit    DECIMAL(12,2)   NOT NULL DEFAULT 0.00,
    acct_curr_cyc_debit     DECIMAL(12,2)   NOT NULL DEFAULT 0.00,
    acct_addr_zip           VARCHAR(10),
    acct_group_id           VARCHAR(10),
    CONSTRAINT pk_account PRIMARY KEY (acct_id),
    CONSTRAINT chk_acct_status CHECK (acct_active_status IN ('Y', 'N'))
);
```

---

## 3. Entity: CARD (CARDDAT)

**Source Copybook:** `app/cpy/CVACT02Y.cpy`
**Record Length:** 150 bytes
**Primary Key:** CARD-NUM
**Alternate Index:** CARD-ACCT-ID (non-unique)

| # | Field Name | COBOL PIC | Offset | Length | SQL Type | Description |
|---|------------|-----------|--------|--------|----------|-------------|
| 1 | CARD-NUM | X(16) | 0 | 16 | VARCHAR(16) | Card number (primary key) |
| 2 | CARD-ACCT-ID | 9(11) | 16 | 11 | BIGINT | Associated account ID (FK) |
| 3 | CARD-CVV-CD | 9(03) | 27 | 3 | CHAR(3) | Card verification value |
| 4 | CARD-EMBOSSED-NAME | X(50) | 30 | 50 | VARCHAR(50) | Name embossed on card |
| 5 | CARD-EXPIRAION-DATE | X(10) | 80 | 10 | DATE | Card expiration date |
| 6 | CARD-ACTIVE-STATUS | X(01) | 90 | 1 | CHAR(1) | Card status flag |
| 7 | FILLER | X(59) | 91 | 59 | - | Reserved for expansion |

### Value Domains

| Field | Value | Meaning |
|-------|-------|---------|
| CARD-ACTIVE-STATUS | 'Y' | Active card |
| CARD-ACTIVE-STATUS | 'N' | Inactive/Cancelled card |

### SQL DDL

```sql
CREATE TABLE CARD (
    card_num                VARCHAR(16)     NOT NULL,
    card_acct_id            BIGINT          NOT NULL,
    card_cvv_cd             CHAR(3)         NOT NULL,
    card_embossed_name      VARCHAR(50)     NOT NULL,
    card_expiration_date    DATE            NOT NULL,
    card_active_status      CHAR(1)         NOT NULL DEFAULT 'Y',
    CONSTRAINT pk_card PRIMARY KEY (card_num),
    CONSTRAINT fk_card_account FOREIGN KEY (card_acct_id) REFERENCES ACCOUNT(acct_id),
    CONSTRAINT chk_card_status CHECK (card_active_status IN ('Y', 'N'))
);

CREATE INDEX idx_card_acct_id ON CARD(card_acct_id);
```

---

## 4. Entity: CUSTOMER (CUSTDAT)

**Source Copybook:** `app/cpy/CVCUS01Y.cpy`
**Record Length:** 500 bytes
**Primary Key:** CUST-ID

| # | Field Name | COBOL PIC | Offset | Length | SQL Type | Description |
|---|------------|-----------|--------|--------|----------|-------------|
| 1 | CUST-ID | 9(09) | 0 | 9 | BIGINT | Customer identifier (PK) |
| 2 | CUST-FIRST-NAME | X(25) | 9 | 25 | VARCHAR(25) | First name |
| 3 | CUST-MIDDLE-NAME | X(25) | 34 | 25 | VARCHAR(25) | Middle name |
| 4 | CUST-LAST-NAME | X(25) | 59 | 25 | VARCHAR(25) | Last name |
| 5 | CUST-ADDR-LINE-1 | X(50) | 84 | 50 | VARCHAR(50) | Address line 1 |
| 6 | CUST-ADDR-LINE-2 | X(50) | 134 | 50 | VARCHAR(50) | Address line 2 |
| 7 | CUST-ADDR-LINE-3 | X(50) | 184 | 50 | VARCHAR(50) | Address line 3 |
| 8 | CUST-ADDR-STATE-CD | X(02) | 234 | 2 | CHAR(2) | State code |
| 9 | CUST-ADDR-COUNTRY-CD | X(03) | 236 | 3 | CHAR(3) | Country code |
| 10 | CUST-ADDR-ZIP | X(10) | 239 | 10 | VARCHAR(10) | ZIP/Postal code |
| 11 | CUST-PHONE-NUM-1 | X(15) | 249 | 15 | VARCHAR(15) | Primary phone |
| 12 | CUST-PHONE-NUM-2 | X(15) | 264 | 15 | VARCHAR(15) | Secondary phone |
| 13 | CUST-SSN | 9(09) | 279 | 9 | CHAR(9) | Social Security Number |
| 14 | CUST-GOVT-ISSUED-ID | X(20) | 288 | 20 | VARCHAR(20) | Government ID |
| 15 | CUST-DOB-YYYY-MM-DD | X(10) | 308 | 10 | DATE | Date of birth |
| 16 | CUST-EFT-ACCOUNT-ID | X(10) | 318 | 10 | VARCHAR(10) | EFT/ACH account ID |
| 17 | CUST-PRI-CARD-HOLDER-IND | X(01) | 328 | 1 | CHAR(1) | Primary cardholder flag |
| 18 | CUST-FICO-CREDIT-SCORE | 9(03) | 329 | 3 | SMALLINT | FICO credit score |
| 19 | FILLER | X(168) | 332 | 168 | - | Reserved for expansion |

### Value Domains

| Field | Value | Meaning |
|-------|-------|---------|
| CUST-PRI-CARD-HOLDER-IND | 'Y' | Primary cardholder |
| CUST-PRI-CARD-HOLDER-IND | 'N' | Authorized user |

### SQL DDL

```sql
CREATE TABLE CUSTOMER (
    cust_id                 BIGINT          NOT NULL,
    cust_first_name         VARCHAR(25)     NOT NULL,
    cust_middle_name        VARCHAR(25),
    cust_last_name          VARCHAR(25)     NOT NULL,
    cust_addr_line_1        VARCHAR(50),
    cust_addr_line_2        VARCHAR(50),
    cust_addr_line_3        VARCHAR(50),
    cust_addr_state_cd      CHAR(2),
    cust_addr_country_cd    CHAR(3),
    cust_addr_zip           VARCHAR(10),
    cust_phone_num_1        VARCHAR(15),
    cust_phone_num_2        VARCHAR(15),
    cust_ssn                CHAR(9),        -- Consider encryption
    cust_govt_issued_id     VARCHAR(20),
    cust_dob                DATE,
    cust_eft_account_id     VARCHAR(10),
    cust_pri_card_holder_ind CHAR(1),
    cust_fico_credit_score  SMALLINT,
    CONSTRAINT pk_customer PRIMARY KEY (cust_id),
    CONSTRAINT chk_fico_score CHECK (cust_fico_credit_score BETWEEN 300 AND 850)
);
```

---

## 5. Entity: CARD_XREF (CCXREF)

**Source Copybook:** `app/cpy/CVACT03Y.cpy`
**Record Length:** 50 bytes
**Primary Key:** XREF-CARD-NUM
**Alternate Index:** XREF-ACCT-ID (non-unique)

| # | Field Name | COBOL PIC | Offset | Length | SQL Type | Description |
|---|------------|-----------|--------|--------|----------|-------------|
| 1 | XREF-CARD-NUM | X(16) | 0 | 16 | VARCHAR(16) | Card number (PK) |
| 2 | XREF-CUST-ID | 9(09) | 16 | 9 | BIGINT | Customer ID (FK) |
| 3 | XREF-ACCT-ID | 9(11) | 25 | 11 | BIGINT | Account ID (FK) |
| 4 | FILLER | X(14) | 36 | 14 | - | Reserved |

### SQL DDL

```sql
-- Note: This is a denormalized junction table
-- Consider normalizing to separate CUSTOMER_ACCOUNT table

CREATE TABLE CARD_XREF (
    xref_card_num           VARCHAR(16)     NOT NULL,
    xref_cust_id            BIGINT          NOT NULL,
    xref_acct_id            BIGINT          NOT NULL,
    CONSTRAINT pk_card_xref PRIMARY KEY (xref_card_num),
    CONSTRAINT fk_xref_customer FOREIGN KEY (xref_cust_id) REFERENCES CUSTOMER(cust_id),
    CONSTRAINT fk_xref_account FOREIGN KEY (xref_acct_id) REFERENCES ACCOUNT(acct_id)
);

CREATE INDEX idx_xref_acct_id ON CARD_XREF(xref_acct_id);
CREATE INDEX idx_xref_cust_id ON CARD_XREF(xref_cust_id);
```

---

## 6. Entity: TRANSACTION (TRANSACT)

**Source Copybook:** `app/cpy/CVTRA05Y.cpy`
**Record Length:** 350 bytes
**Primary Key:** TRAN-ID
**Alternate Index:** TRAN-PROC-TS (non-unique)

| # | Field Name | COBOL PIC | Offset | Length | SQL Type | Description |
|---|------------|-----------|--------|--------|----------|-------------|
| 1 | TRAN-ID | X(16) | 0 | 16 | VARCHAR(16) | Transaction ID (PK) |
| 2 | TRAN-TYPE-CD | X(02) | 16 | 2 | CHAR(2) | Transaction type code |
| 3 | TRAN-CAT-CD | 9(04) | 18 | 4 | SMALLINT | Transaction category |
| 4 | TRAN-SOURCE | X(10) | 22 | 10 | VARCHAR(10) | Transaction source |
| 5 | TRAN-DESC | X(100) | 32 | 100 | VARCHAR(100) | Description |
| 6 | TRAN-AMT | S9(09)V99 | 132 | 11 | DECIMAL(11,2) | Transaction amount |
| 7 | TRAN-MERCHANT-ID | 9(09) | 143 | 9 | BIGINT | Merchant identifier |
| 8 | TRAN-MERCHANT-NAME | X(50) | 152 | 50 | VARCHAR(50) | Merchant name |
| 9 | TRAN-MERCHANT-CITY | X(50) | 202 | 50 | VARCHAR(50) | Merchant city |
| 10 | TRAN-MERCHANT-ZIP | X(10) | 252 | 10 | VARCHAR(10) | Merchant ZIP code |
| 11 | TRAN-CARD-NUM | X(16) | 262 | 16 | VARCHAR(16) | Card used (FK) |
| 12 | TRAN-ORIG-TS | X(26) | 278 | 26 | TIMESTAMP | Original timestamp |
| 13 | TRAN-PROC-TS | X(26) | 304 | 26 | TIMESTAMP | Processing timestamp |
| 14 | FILLER | X(20) | 330 | 20 | - | Reserved |

### Value Domains for TRAN-SOURCE

| Value | Meaning |
|-------|---------|
| 'System' | System-generated (interest, fees) |
| 'Online' | Online transaction |
| 'POS' | Point of sale |
| 'ATM' | ATM withdrawal |

### SQL DDL

```sql
CREATE TABLE TRANSACTION (
    tran_id                 VARCHAR(16)     NOT NULL,
    tran_type_cd            CHAR(2)         NOT NULL,
    tran_cat_cd             SMALLINT        NOT NULL,
    tran_source             VARCHAR(10),
    tran_desc               VARCHAR(100),
    tran_amt                DECIMAL(11,2)   NOT NULL,
    tran_merchant_id        BIGINT,
    tran_merchant_name      VARCHAR(50),
    tran_merchant_city      VARCHAR(50),
    tran_merchant_zip       VARCHAR(10),
    tran_card_num           VARCHAR(16)     NOT NULL,
    tran_orig_ts            TIMESTAMP       NOT NULL,
    tran_proc_ts            TIMESTAMP       NOT NULL,
    CONSTRAINT pk_transaction PRIMARY KEY (tran_id),
    CONSTRAINT fk_tran_card FOREIGN KEY (tran_card_num) REFERENCES CARD(card_num),
    CONSTRAINT fk_tran_type FOREIGN KEY (tran_type_cd) REFERENCES TRAN_TYPE(tran_type),
    CONSTRAINT fk_tran_cat FOREIGN KEY (tran_type_cd, tran_cat_cd)
        REFERENCES TRAN_CATEGORY(tran_type_cd, tran_cat_cd)
);

CREATE INDEX idx_tran_proc_ts ON TRANSACTION(tran_proc_ts);
CREATE INDEX idx_tran_card_num ON TRANSACTION(tran_card_num);
```

---

## 7. Entity: USER_SECURITY (USRSEC)

**Source Copybook:** `app/cpy/CSUSR01Y.cpy`
**Record Length:** 80 bytes
**Primary Key:** SEC-USR-ID

| # | Field Name | COBOL PIC | Offset | Length | SQL Type | Description |
|---|------------|-----------|--------|--------|----------|-------------|
| 1 | SEC-USR-ID | X(08) | 0 | 8 | VARCHAR(8) | User ID (PK) |
| 2 | SEC-USR-FNAME | X(20) | 8 | 20 | VARCHAR(20) | First name |
| 3 | SEC-USR-LNAME | X(20) | 28 | 20 | VARCHAR(20) | Last name |
| 4 | SEC-USR-PWD | X(08) | 48 | 8 | VARCHAR(255) | Password (hash in modern) |
| 5 | SEC-USR-TYPE | X(01) | 56 | 1 | CHAR(1) | User type |
| 6 | SEC-USR-FILLER | X(23) | 57 | 23 | - | Reserved |

### Value Domains

| Field | Value | Meaning |
|-------|-------|---------|
| SEC-USR-TYPE | 'A' | Administrator |
| SEC-USR-TYPE | 'U' | Regular user |

### SQL DDL

```sql
CREATE TABLE USER_SECURITY (
    sec_usr_id              VARCHAR(8)      NOT NULL,
    sec_usr_fname           VARCHAR(20)     NOT NULL,
    sec_usr_lname           VARCHAR(20)     NOT NULL,
    sec_usr_pwd_hash        VARCHAR(255)    NOT NULL,  -- Store hash, not plain
    sec_usr_type            CHAR(1)         NOT NULL DEFAULT 'U',
    created_at              TIMESTAMP       DEFAULT CURRENT_TIMESTAMP,
    updated_at              TIMESTAMP       DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_user_security PRIMARY KEY (sec_usr_id),
    CONSTRAINT chk_usr_type CHECK (sec_usr_type IN ('A', 'U'))
);
```

---

## 8. Entity: TRAN_CAT_BAL (TCATBALF)

**Source Copybook:** `app/cpy/CVTRA01Y.cpy`
**Record Length:** 50 bytes
**Primary Key:** TRAN-CAT-KEY (composite)

| # | Field Name | COBOL PIC | Offset | Length | SQL Type | Description |
|---|------------|-----------|--------|--------|----------|-------------|
| 1 | TRANCAT-ACCT-ID | 9(11) | 0 | 11 | BIGINT | Account ID (PK part) |
| 2 | TRANCAT-TYPE-CD | X(02) | 11 | 2 | CHAR(2) | Type code (PK part) |
| 3 | TRANCAT-CD | 9(04) | 13 | 4 | SMALLINT | Category code (PK part) |
| 4 | TRAN-CAT-BAL | S9(09)V99 | 17 | 11 | DECIMAL(11,2) | Running balance |
| 5 | FILLER | X(22) | 28 | 22 | - | Reserved |

### SQL DDL

```sql
CREATE TABLE TRAN_CAT_BAL (
    trancat_acct_id         BIGINT          NOT NULL,
    trancat_type_cd         CHAR(2)         NOT NULL,
    trancat_cd              SMALLINT        NOT NULL,
    tran_cat_bal            DECIMAL(11,2)   NOT NULL DEFAULT 0.00,
    CONSTRAINT pk_tran_cat_bal PRIMARY KEY (trancat_acct_id, trancat_type_cd, trancat_cd),
    CONSTRAINT fk_tcb_account FOREIGN KEY (trancat_acct_id) REFERENCES ACCOUNT(acct_id),
    CONSTRAINT fk_tcb_category FOREIGN KEY (trancat_type_cd, trancat_cd)
        REFERENCES TRAN_CATEGORY(tran_type_cd, tran_cat_cd)
);
```

---

## 9. Entity: DISCLOSURE_GROUP (DISCGRP)

**Source Copybook:** `app/cpy/CVTRA02Y.cpy`
**Record Length:** 50 bytes
**Primary Key:** DIS-GROUP-KEY (composite)

| # | Field Name | COBOL PIC | Offset | Length | SQL Type | Description |
|---|------------|-----------|--------|--------|----------|-------------|
| 1 | DIS-ACCT-GROUP-ID | X(10) | 0 | 10 | VARCHAR(10) | Group ID (PK part) |
| 2 | DIS-TRAN-TYPE-CD | X(02) | 10 | 2 | CHAR(2) | Type code (PK part) |
| 3 | DIS-TRAN-CAT-CD | 9(04) | 12 | 4 | SMALLINT | Category code (PK part) |
| 4 | DIS-INT-RATE | S9(04)V99 | 16 | 6 | DECIMAL(6,2) | Annual interest rate % |
| 5 | FILLER | X(28) | 22 | 28 | - | Reserved |

### SQL DDL

```sql
CREATE TABLE DISCLOSURE_GROUP (
    dis_acct_group_id       VARCHAR(10)     NOT NULL,
    dis_tran_type_cd        CHAR(2)         NOT NULL,
    dis_tran_cat_cd         SMALLINT        NOT NULL,
    dis_int_rate            DECIMAL(6,2)    NOT NULL,
    CONSTRAINT pk_disclosure_group
        PRIMARY KEY (dis_acct_group_id, dis_tran_type_cd, dis_tran_cat_cd),
    CONSTRAINT fk_dg_category FOREIGN KEY (dis_tran_type_cd, dis_tran_cat_cd)
        REFERENCES TRAN_CATEGORY(tran_type_cd, tran_cat_cd)
);
```

---

## 10. Entity: TRAN_TYPE (TRANTYPE)

**Source Copybook:** `app/cpy/CVTRA03Y.cpy`
**Record Length:** 60 bytes
**Primary Key:** TRAN-TYPE

| # | Field Name | COBOL PIC | Offset | Length | SQL Type | Description |
|---|------------|-----------|--------|--------|----------|-------------|
| 1 | TRAN-TYPE | X(02) | 0 | 2 | CHAR(2) | Type code (PK) |
| 2 | TRAN-TYPE-DESC | X(50) | 2 | 50 | VARCHAR(50) | Type description |
| 3 | FILLER | X(08) | 52 | 8 | - | Reserved |

### Known Values

| Code | Description |
|------|-------------|
| 01 | Interest charge |
| 02 | Purchase |
| 03 | Cash advance |
| 04 | Payment |
| 05 | Fee |

### SQL DDL

```sql
CREATE TABLE TRAN_TYPE (
    tran_type               CHAR(2)         NOT NULL,
    tran_type_desc          VARCHAR(50)     NOT NULL,
    CONSTRAINT pk_tran_type PRIMARY KEY (tran_type)
);
```

---

## 11. Entity: TRAN_CATEGORY (TRANCATG)

**Source Copybook:** `app/cpy/CVTRA04Y.cpy`
**Record Length:** 60 bytes
**Primary Key:** TRAN-CAT-KEY (composite)

| # | Field Name | COBOL PIC | Offset | Length | SQL Type | Description |
|---|------------|-----------|--------|--------|----------|-------------|
| 1 | TRAN-TYPE-CD | X(02) | 0 | 2 | CHAR(2) | Type code (PK part) |
| 2 | TRAN-CAT-CD | 9(04) | 2 | 4 | SMALLINT | Category code (PK part) |
| 3 | TRAN-CAT-TYPE-DESC | X(50) | 6 | 50 | VARCHAR(50) | Category description |
| 4 | FILLER | X(04) | 56 | 4 | - | Reserved |

### SQL DDL

```sql
CREATE TABLE TRAN_CATEGORY (
    tran_type_cd            CHAR(2)         NOT NULL,
    tran_cat_cd             SMALLINT        NOT NULL,
    tran_cat_type_desc      VARCHAR(50)     NOT NULL,
    CONSTRAINT pk_tran_category PRIMARY KEY (tran_type_cd, tran_cat_cd),
    CONSTRAINT fk_tc_type FOREIGN KEY (tran_type_cd) REFERENCES TRAN_TYPE(tran_type)
);
```

---

## 12. Complete SQL Schema Generation Script

```sql
-- CardDemo Modernized Schema
-- Generated from RE-002 Data Model Extraction
-- Compatible with PostgreSQL/MySQL/SQL Server

-- ============================================
-- Reference Tables
-- ============================================

CREATE TABLE TRAN_TYPE (
    tran_type               CHAR(2)         NOT NULL,
    tran_type_desc          VARCHAR(50)     NOT NULL,
    CONSTRAINT pk_tran_type PRIMARY KEY (tran_type)
);

CREATE TABLE TRAN_CATEGORY (
    tran_type_cd            CHAR(2)         NOT NULL,
    tran_cat_cd             SMALLINT        NOT NULL,
    tran_cat_type_desc      VARCHAR(50)     NOT NULL,
    CONSTRAINT pk_tran_category PRIMARY KEY (tran_type_cd, tran_cat_cd),
    CONSTRAINT fk_tc_type FOREIGN KEY (tran_type_cd) REFERENCES TRAN_TYPE(tran_type)
);

CREATE TABLE DISCLOSURE_GROUP (
    dis_acct_group_id       VARCHAR(10)     NOT NULL,
    dis_tran_type_cd        CHAR(2)         NOT NULL,
    dis_tran_cat_cd         SMALLINT        NOT NULL,
    dis_int_rate            DECIMAL(6,2)    NOT NULL,
    CONSTRAINT pk_disclosure_group
        PRIMARY KEY (dis_acct_group_id, dis_tran_type_cd, dis_tran_cat_cd),
    CONSTRAINT fk_dg_category FOREIGN KEY (dis_tran_type_cd, dis_tran_cat_cd)
        REFERENCES TRAN_CATEGORY(tran_type_cd, tran_cat_cd)
);

-- ============================================
-- Core Entity Tables
-- ============================================

CREATE TABLE CUSTOMER (
    cust_id                 BIGINT          NOT NULL,
    cust_first_name         VARCHAR(25)     NOT NULL,
    cust_middle_name        VARCHAR(25),
    cust_last_name          VARCHAR(25)     NOT NULL,
    cust_addr_line_1        VARCHAR(50),
    cust_addr_line_2        VARCHAR(50),
    cust_addr_line_3        VARCHAR(50),
    cust_addr_state_cd      CHAR(2),
    cust_addr_country_cd    CHAR(3),
    cust_addr_zip           VARCHAR(10),
    cust_phone_num_1        VARCHAR(15),
    cust_phone_num_2        VARCHAR(15),
    cust_ssn                CHAR(9),
    cust_govt_issued_id     VARCHAR(20),
    cust_dob                DATE,
    cust_eft_account_id     VARCHAR(10),
    cust_pri_card_holder_ind CHAR(1),
    cust_fico_credit_score  SMALLINT,
    CONSTRAINT pk_customer PRIMARY KEY (cust_id)
);

CREATE TABLE ACCOUNT (
    acct_id                 BIGINT          NOT NULL,
    acct_active_status      CHAR(1)         NOT NULL DEFAULT 'Y',
    acct_curr_bal           DECIMAL(12,2)   NOT NULL DEFAULT 0.00,
    acct_credit_limit       DECIMAL(12,2)   NOT NULL,
    acct_cash_credit_limit  DECIMAL(12,2)   NOT NULL,
    acct_open_date          DATE            NOT NULL,
    acct_expiration_date    DATE            NOT NULL,
    acct_reissue_date       DATE,
    acct_curr_cyc_credit    DECIMAL(12,2)   NOT NULL DEFAULT 0.00,
    acct_curr_cyc_debit     DECIMAL(12,2)   NOT NULL DEFAULT 0.00,
    acct_addr_zip           VARCHAR(10),
    acct_group_id           VARCHAR(10),
    CONSTRAINT pk_account PRIMARY KEY (acct_id)
);

CREATE TABLE CARD (
    card_num                VARCHAR(16)     NOT NULL,
    card_acct_id            BIGINT          NOT NULL,
    card_cvv_cd             CHAR(3)         NOT NULL,
    card_embossed_name      VARCHAR(50)     NOT NULL,
    card_expiration_date    DATE            NOT NULL,
    card_active_status      CHAR(1)         NOT NULL DEFAULT 'Y',
    CONSTRAINT pk_card PRIMARY KEY (card_num),
    CONSTRAINT fk_card_account FOREIGN KEY (card_acct_id) REFERENCES ACCOUNT(acct_id)
);

-- Normalized junction table (replaces CCXREF)
CREATE TABLE CUSTOMER_ACCOUNT (
    cust_id                 BIGINT          NOT NULL,
    acct_id                 BIGINT          NOT NULL,
    relationship_type       VARCHAR(20)     NOT NULL DEFAULT 'PRIMARY',
    CONSTRAINT pk_cust_acct PRIMARY KEY (cust_id, acct_id),
    CONSTRAINT fk_ca_customer FOREIGN KEY (cust_id) REFERENCES CUSTOMER(cust_id),
    CONSTRAINT fk_ca_account FOREIGN KEY (acct_id) REFERENCES ACCOUNT(acct_id)
);

CREATE TABLE TRANSACTION (
    tran_id                 VARCHAR(16)     NOT NULL,
    tran_type_cd            CHAR(2)         NOT NULL,
    tran_cat_cd             SMALLINT        NOT NULL,
    tran_source             VARCHAR(10),
    tran_desc               VARCHAR(100),
    tran_amt                DECIMAL(11,2)   NOT NULL,
    tran_merchant_id        BIGINT,
    tran_merchant_name      VARCHAR(50),
    tran_merchant_city      VARCHAR(50),
    tran_merchant_zip       VARCHAR(10),
    tran_card_num           VARCHAR(16)     NOT NULL,
    tran_orig_ts            TIMESTAMP       NOT NULL,
    tran_proc_ts            TIMESTAMP       NOT NULL,
    CONSTRAINT pk_transaction PRIMARY KEY (tran_id),
    CONSTRAINT fk_tran_card FOREIGN KEY (tran_card_num) REFERENCES CARD(card_num),
    CONSTRAINT fk_tran_category FOREIGN KEY (tran_type_cd, tran_cat_cd)
        REFERENCES TRAN_CATEGORY(tran_type_cd, tran_cat_cd)
);

CREATE TABLE TRAN_CAT_BAL (
    trancat_acct_id         BIGINT          NOT NULL,
    trancat_type_cd         CHAR(2)         NOT NULL,
    trancat_cd              SMALLINT        NOT NULL,
    tran_cat_bal            DECIMAL(11,2)   NOT NULL DEFAULT 0.00,
    CONSTRAINT pk_tran_cat_bal PRIMARY KEY (trancat_acct_id, trancat_type_cd, trancat_cd),
    CONSTRAINT fk_tcb_account FOREIGN KEY (trancat_acct_id) REFERENCES ACCOUNT(acct_id),
    CONSTRAINT fk_tcb_category FOREIGN KEY (trancat_type_cd, trancat_cd)
        REFERENCES TRAN_CATEGORY(tran_type_cd, tran_cat_cd)
);

CREATE TABLE USER_SECURITY (
    sec_usr_id              VARCHAR(8)      NOT NULL,
    sec_usr_fname           VARCHAR(20)     NOT NULL,
    sec_usr_lname           VARCHAR(20)     NOT NULL,
    sec_usr_pwd_hash        VARCHAR(255)    NOT NULL,
    sec_usr_type            CHAR(1)         NOT NULL DEFAULT 'U',
    created_at              TIMESTAMP       DEFAULT CURRENT_TIMESTAMP,
    updated_at              TIMESTAMP       DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_user_security PRIMARY KEY (sec_usr_id)
);

-- ============================================
-- Indexes
-- ============================================

CREATE INDEX idx_card_acct_id ON CARD(card_acct_id);
CREATE INDEX idx_tran_proc_ts ON TRANSACTION(tran_proc_ts);
CREATE INDEX idx_tran_card_num ON TRANSACTION(tran_card_num);
CREATE INDEX idx_cust_ssn ON CUSTOMER(cust_ssn);
CREATE INDEX idx_cust_name ON CUSTOMER(cust_last_name, cust_first_name);
```

---

## 13. Security Considerations for Modernization

### Sensitive Fields Requiring Encryption

| Entity | Field | Reason |
|--------|-------|--------|
| CUSTOMER | cust_ssn | PII - Social Security Number |
| CUSTOMER | cust_dob | PII - Date of Birth |
| CARD | card_cvv_cd | PCI-DSS requirement |
| CARD | card_num | PCI-DSS requirement (tokenize) |
| USER_SECURITY | sec_usr_pwd | Must hash, never store plain |

### PCI-DSS Compliance Notes

1. **Card Numbers:** Consider tokenization instead of direct storage
2. **CVV Codes:** Should not be stored after authorization
3. **Access Logging:** Add audit trail tables
4. **Data Masking:** Implement views for masked card numbers
