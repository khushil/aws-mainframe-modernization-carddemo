# RE-006: CardDemo Batch Workflow Analysis

**Document Version:** 1.0
**Generated:** 2026-02-05
**Prompt ID:** RE-006
**Status:** Complete

---

## 1. Executive Summary

This document provides a comprehensive analysis of the JCL batch workflows in the CardDemo mainframe credit card processing application. The analysis covers job-step specifications, inter-job dependencies, data flow diagrams, restart/recovery procedures, and modernization recommendations.

> **Count Correction (2026-02-06):** Original document stated 33 JCL jobs and 10 batch programs. Actual counts are 38 JCL files in `app/jcl/` (33 .jcl + 5 .JCL) and 12 batch COBOL programs in `app/cbl/` (10 .cbl + 2 .CBL). An additional 8 JCL files exist in extension directories. See [CONFIDENCE-ASSESSMENT.md](../CONFIDENCE-ASSESSMENT.md) for details.

### Key Findings

| Metric | Value |
|--------|-------|
| Total JCL Jobs (core) | 38 (33 lowercase + 5 uppercase extension) |
| Total JCL Jobs (with extensions) | 46 |
| Batch COBOL Programs (core) | 12 (10 .cbl + 2 .CBL) |
| VSAM Clusters Defined | 15 |
| GDG Bases Defined | 11 |
| Scheduler Workflows | 5 (Control-M) |
| Critical Processing Jobs | 5 |

### Job Categories

| Category | Count | Purpose |
|----------|-------|---------|
| VSAM File Definition | 10 | Create/populate VSAM clusters |
| GDG Base Definition | 4 | Define generation data groups |
| Transaction Processing | 5 | Core business processing |
| Reporting | 3 | Generate reports |
| CICS File Management | 2 | Open/close files for batch |
| Data Read/Extract | 4 | Extract data from VSAM |
| Data Import/Export | 2 | Data migration utilities |
| Other/Utility | 3 | CICS resources, timing, etc. |

---

## 2. JCL Job Inventory

### 2.1 VSAM File Definition Jobs (10)

These jobs define, delete, and populate VSAM clusters that form the application's persistent data layer.

#### ACCTFILE - Account Master VSAM

| Attribute | Value |
|-----------|-------|
| **Purpose** | Define and load account master VSAM |
| **Program** | IDCAMS |
| **Steps** | STEP05 (Delete), STEP10 (Define), STEP15 (REPRO) |
| **VSAM Cluster** | `AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS` |
| **Key Specification** | 11 bytes at offset 0 (Account ID) |
| **Record Size** | 300 bytes |
| **Input** | `AWS.M2.CARDDEMO.ACCTDATA.PS` |

```
Step Flow: Delete existing → Define cluster → Load from PS file
```

#### CARDFILE - Card Master VSAM with AIX

| Attribute | Value |
|-----------|-------|
| **Purpose** | Define card master VSAM with alternate index |
| **Program** | IDCAMS, SDSF |
| **Steps** | CLCIFIL, STEP05-60, OPCIFIL |
| **VSAM Cluster** | `AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS` |
| **Key Specification** | 16 bytes at offset 0 (Card Number) |
| **AIX Specification** | 11 bytes at offset 16 (Account ID) |
| **Record Size** | 150 bytes |
| **CICS Files** | CARDDAT, CARDAIX |

Special features:
- Closes CICS files before processing
- Creates alternate index on Account ID
- Reopens CICS files after processing

#### CUSTFILE - Customer Master VSAM

| Attribute | Value |
|-----------|-------|
| **Purpose** | Define customer master VSAM |
| **Key Specification** | 9 bytes at offset 0 (Customer ID) |
| **Record Size** | 500 bytes |
| **CICS Files** | CUSTDAT |

#### XREFFILE - Card Cross-Reference VSAM with AIX

| Attribute | Value |
|-----------|-------|
| **Purpose** | Define card-to-account cross-reference |
| **Key Specification** | 16 bytes at offset 0 (Card Number) |
| **AIX Specification** | 11 bytes at offset 25 (Account ID) |
| **Record Size** | 50 bytes |

#### TRANFILE - Transaction Master VSAM with AIX

| Attribute | Value |
|-----------|-------|
| **Purpose** | Define transaction master VSAM |
| **Key Specification** | 16 bytes at offset 0 (Transaction ID) |
| **AIX Specification** | 26 bytes at offset 304 (Processed Timestamp) |
| **Record Size** | 350 bytes |
| **CICS Files** | TRANSACT, CXACAIX |

#### TRANTYPE - Transaction Type Reference

| Attribute | Value |
|-----------|-------|
| **Key Specification** | 2 bytes at offset 0 (Type Code) |
| **Record Size** | 60 bytes |

#### TRANCATG - Transaction Category Reference

| Attribute | Value |
|-----------|-------|
| **Key Specification** | 6 bytes at offset 0 (Type + Category) |
| **Record Size** | 60 bytes |

#### TCATBALF - Transaction Category Balance

| Attribute | Value |
|-----------|-------|
| **Key Specification** | 17 bytes at offset 0 (Acct+Type+Cat) |
| **Record Size** | 50 bytes |

#### DISCGRP - Disclosure Group Reference

| Attribute | Value |
|-----------|-------|
| **Key Specification** | 16 bytes at offset 0 |
| **Record Size** | 50 bytes |

#### DUSRSECJ - User Security VSAM

| Attribute | Value |
|-----------|-------|
| **Purpose** | Define user security with inline data |
| **Programs** | IEBGENER, IDCAMS |
| **Key Specification** | 8 bytes at offset 0 (User ID) |
| **Record Size** | 80 bytes |
| **Inline Users** | 10 (5 admin, 5 regular) |

### 2.2 GDG Base Definition Jobs (4)

#### DEFGDGB - Transaction Processing GDGs

Defines 6 GDG bases with limit 5:

| GDG Base | Purpose |
|----------|---------|
| `AWS.M2.CARDDEMO.TRANSACT.BKUP` | Transaction backup |
| `AWS.M2.CARDDEMO.TRANSACT.DALY` | Daily transaction extract |
| `AWS.M2.CARDDEMO.TRANREPT` | Transaction reports |
| `AWS.M2.CARDDEMO.TCATBALF.BKUP` | Category balance backup |
| `AWS.M2.CARDDEMO.SYSTRAN` | System-generated transactions |
| `AWS.M2.CARDDEMO.TRANSACT.COMBINED` | Combined transactions |

#### DEFGDGD - Reference Data GDGs

Defines 3 GDG bases and creates initial generations:

| GDG Base | Purpose |
|----------|---------|
| `AWS.M2.CARDDEMO.TRANTYPE.BKUP` | Transaction type backup |
| `AWS.M2.CARDDEMO.TRANCATG.PS.BKUP` | Transaction category backup |
| `AWS.M2.CARDDEMO.DISCGRP.BKUP` | Disclosure group backup |

#### REPTFILE - Transaction Report GDG

| Attribute | Value |
|-----------|-------|
| **GDG Base** | `AWS.M2.CARDDEMO.TRANREPT` |
| **Limit** | 10 generations |

#### DALYREJS - Daily Rejects GDG

| Attribute | Value |
|-----------|-------|
| **GDG Base** | `AWS.M2.CARDDEMO.DALYREJS` |
| **Limit** | 5 generations |

### 2.3 Transaction Processing Jobs (5) - CRITICAL

These are the core business processing jobs that execute the application's primary functions.

#### POSTTRAN - Post Daily Transactions

| Attribute | Value |
|-----------|-------|
| **Program** | CBTRN02C |
| **Criticality** | HIGH |
| **Function** | Post records from daily transaction file |

**Input Files:**
- `AWS.M2.CARDDEMO.DALYTRAN.PS` - Daily transactions
- `AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS` - Transaction master
- `AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS` - Card cross-reference
- `AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS` - Account master
- `AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS` - Category balance

**Output Files:**
- `AWS.M2.CARDDEMO.DALYREJS(+1)` - Rejected transactions

**Processing Logic:**
1. Read daily transaction input file
2. Validate card number via XREF
3. Update transaction master VSAM
4. Update category balance VSAM
5. Write rejected transactions to GDG

#### INTCALC - Interest Calculation

| Attribute | Value |
|-----------|-------|
| **Program** | CBACT04C |
| **Criticality** | HIGH |
| **Function** | Calculate interest and fees |
| **PARM** | YYYYMMDD00 (Date parameter) |

**Input Files:**
- `AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS` - Category balance
- `AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS` - Card cross-reference
- `AWS.M2.CARDDEMO.CARDXREF.VSAM.AIX.PATH` - Cross-ref by account
- `AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS` - Account master
- `AWS.M2.CARDDEMO.DISCGRP.VSAM.KSDS` - Disclosure groups

**Output Files:**
- `AWS.M2.CARDDEMO.SYSTRAN(+1)` - System-generated transactions

**Processing Logic:**
1. Read category balance records sequentially
2. Lookup disclosure group for interest rates
3. Calculate interest and fees
4. Generate interest/fee transactions
5. Write to SYSTRAN GDG

#### COMBTRAN - Combine Transactions

| Attribute | Value |
|-----------|-------|
| **Program** | SORT, IDCAMS |
| **Criticality** | HIGH |
| **Function** | Merge backup + system transactions |

**Step Details:**

| Step | Program | Function |
|------|---------|----------|
| STEP05R | SORT | Sort and combine BKUP(0) + SYSTRAN(0) |
| STEP10 | IDCAMS | REPRO combined file to VSAM |

**SORT Control:**
```
SORT FIELDS=(TRAN-ID,A)
```

#### TRANBKP - Transaction Backup

| Attribute | Value |
|-----------|-------|
| **Program** | REPROC (proc), IDCAMS |
| **Criticality** | HIGH |
| **Function** | Backup and redefine transaction VSAM |

**Step Details:**

| Step | Program | Function |
|------|---------|----------|
| STEP05R | REPROC | REPRO VSAM to sequential backup |
| STEP05 | IDCAMS | DELETE cluster and AIX |
| STEP10 | IDCAMS | DEFINE cluster (COND 4,LT) |

**Key Attribute:** Uses REPROC procedure which invokes IDCAMS with REPROCT control file.

#### TRANIDX - Transaction AIX Rebuild

| Attribute | Value |
|-----------|-------|
| **Program** | IDCAMS |
| **Function** | Create/rebuild AIX on processed timestamp |

**Step Details:**

| Step | Function |
|------|----------|
| STEP20 | DEFINE ALTERNATEINDEX |
| STEP25 | DEFINE PATH |
| STEP30 | BLDINDEX |

### 2.4 Reporting Jobs (3)

#### TRANREPT - Transaction Detail Report

| Attribute | Value |
|-----------|-------|
| **Programs** | REPROC, SORT, CBTRN03C |
| **Function** | Generate date-filtered transaction report |

**Step Details:**

| Step | Program | Function |
|------|---------|----------|
| STEP05R | REPROC | Unload transaction VSAM to GDG |
| STEP05R | SORT | Filter by date, sort by card number |
| STEP10R | CBTRN03C | Generate formatted report |

**SORT Parameters (configurable):**
```
PARM-START-DATE,C'2022-01-01'
PARM-END-DATE,C'2022-07-06'
```

#### PRTCATBL - Category Balance Report

| Attribute | Value |
|-----------|-------|
| **Programs** | REPROC, SORT |
| **Function** | Print transaction category balance report |

**Output Format:** LRECL=40, sorted by Account+Type+Category

#### REPTFILE - (See GDG Definition)

### 2.5 CICS File Management Jobs (2)

#### CLOSEFIL - Close CICS Files

| Attribute | Value |
|-----------|-------|
| **Program** | SDSF |
| **CICS Region** | CICSAWSA |

**CICS Commands Executed:**
```
/F CICSAWSA,'CEMT SET FIL(TRANSACT) CLO'
/F CICSAWSA,'CEMT SET FIL(CCXREF) CLO'
/F CICSAWSA,'CEMT SET FIL(ACCTDAT) CLO'
/F CICSAWSA,'CEMT SET FIL(CXACAIX) CLO'
/F CICSAWSA,'CEMT SET FIL(USRSEC) CLO'
```

#### OPENFIL - Open CICS Files

| Attribute | Value |
|-----------|-------|
| **Program** | SDSF |
| **CICS Region** | CICSAWSA |

**CICS Commands Executed:**
```
/F CICSAWSA,'CEMT SET FIL(TRANSACT) OPE'
/F CICSAWSA,'CEMT SET FIL(CCXREF) OPE'
/F CICSAWSA,'CEMT SET FIL(ACCTDAT) OPE'
/F CICSAWSA,'CEMT SET FIL(CXACAIX) OPE'
/F CICSAWSA,'CEMT SET FIL(USRSEC) OPE'
```

### 2.6 Data Read/Extract Jobs (4)

#### READACCT - Account Data Extract

| Attribute | Value |
|-----------|-------|
| **Program** | CBACT01C |
| **Input** | Account master VSAM |
| **Outputs** | 3 formats: PSCOMP (107), ARRYPS (110), VBPS (84 VB) |

#### READCARD, READCUST, READXREF

Simple read programs for card, customer, and cross-reference VSAM files respectively. Output to SYSOUT only.

### 2.7 Data Import/Export Jobs (2)

#### CBEXPORT - Multi-File Export

| Attribute | Value |
|-----------|-------|
| **Programs** | IDCAMS, CBEXPORT |
| **Function** | Export 5 VSAM files to unified export file |

**Export File Specification:**
- Cluster: `AWS.M2.CARDDEMO.EXPORT.DATA`
- Key: 4 bytes at offset 28
- Record Size: 500 bytes

**Source Files:**
1. Customer master
2. Account master
3. Card cross-reference
4. Transaction master
5. Card master

#### CBIMPORT - Import and Split

| Attribute | Value |
|-----------|-------|
| **Program** | CBIMPORT |
| **Function** | Import from unified file, split to normalized files |

**Target Files:**
- `AWS.M2.CARDDEMO.CUSTDATA.IMPORT` (LRECL=500)
- `AWS.M2.CARDDEMO.ACCTDATA.IMPORT` (LRECL=300)
- `AWS.M2.CARDDEMO.CARDXREF.IMPORT` (LRECL=50)
- `AWS.M2.CARDDEMO.TRANSACT.IMPORT` (LRECL=350)
- `AWS.M2.CARDDEMO.IMPORT.ERRORS` (LRECL=132)

### 2.8 Other/Utility Jobs (3)

#### CBADMCDJ - CICS Resource Definition

| Attribute | Value |
|-----------|-------|
| **Program** | DFHCSDUP |
| **Function** | Define CICS resources for CardDemo |

**Defines:**
- Library: COM2DOLL
- 17 Mapsets
- 16 Programs
- 5 Transactions
- Group: CARDDEMO

#### WAITSTEP - Timing Delay

| Attribute | Value |
|-----------|-------|
| **Program** | COBSWAIT |
| **PARM** | Centiseconds (00003600 = 36 seconds) |

#### ESDSRRDS - Alternate VSAM Types

Defines ESDS and RRDS versions of user security file for demonstration purposes.

---

## 3. Job Dependency Analysis

### 3.1 Control-M Workflow Definitions

The application uses Control-M for job scheduling with the following defined workflows:

#### DAILY-TransactionBackup (All Days)

```mermaid
graph LR
    A[CLOSEFIL] --> B[TRANBKP]
    B --> C[WAITSTEP]
    C --> D[OPENFIL]
```

#### WEEKLY-DisclosureGroupsRefresh (Saturday)

```mermaid
graph LR
    A[MNTTRDB2] --> B[CLOSEFIL]
    B --> C[DISCGRP]
    C --> D[WAITSTEP]
    D --> E[OPENFIL]
```

#### MONTHLY-InterestCalculation

```mermaid
graph LR
    A[CLOSEFIL] --> B[INTCALC]
    B --> C[COMBTRAN]
    C --> D[WAITSTEP]
    D --> E[OPENFIL]
```

### 3.2 Complete Job Dependency DAG

```mermaid
graph TD
    subgraph "Setup Phase"
        GDG[DEFGDGB/DEFGDGD] --> VSAM
        VSAM[VSAM Definitions] --> CLOSEFIL
    end

    subgraph "Data Refresh"
        CLOSEFIL --> ACCTFILE
        CLOSEFIL --> CARDFILE
        CLOSEFIL --> CUSTFILE
        CLOSEFIL --> XREFFILE
        CLOSEFIL --> TRANFILE
        CLOSEFIL --> DISCGRP
        CLOSEFIL --> TCATBALF
        CLOSEFIL --> TRANTYPE
        CLOSEFIL --> DUSRSECJ
    end

    subgraph "Core Processing"
        ACCTFILE --> POSTTRAN
        TCATBALF --> POSTTRAN
        XREFFILE --> POSTTRAN
        POSTTRAN --> INTCALC
        INTCALC --> TRANBKP
        TRANBKP --> COMBTRAN
        COMBTRAN --> TRANIDX
    end

    subgraph "Finalization"
        TRANIDX --> OPENFIL
        DISCGRP --> OPENFIL
    end

    subgraph "Reporting"
        TRANIDX --> TRANREPT
        TCATBALF --> PRTCATBL
    end
```

### 3.3 Shell Script Workflow: run_full_batch.sh

| Phase | Jobs | Wait Time |
|-------|------|-----------|
| 1. Close CICS | CLOSEFIL | 5s |
| 2. Data Refresh | ACCTFILE, CARDFILE, XREFFILE, CUSTFILE, TRANBKP, DISCGRP, TCATBALF, TRANTYPE, DUSRSECJ | 5s |
| 3. Core Processing | POSTTRAN | 10s |
| 4. Interest | INTCALC | 5s |
| 5. Backup/Combine | TRANBKP, COMBTRAN | 5s |
| 6. Index | TRANIDX | 5s |
| 7. Open CICS | OPENFIL | - |

---

## 4. Data Flow Diagrams

### 4.1 Daily Transaction Processing Flow

```mermaid
flowchart TB
    subgraph Input
        DT[DALYTRAN.PS<br>Daily Transactions]
        XR[CARDXREF.VSAM<br>Cross Reference]
        AC[ACCTDATA.VSAM<br>Account Master]
        TC[TCATBALF.VSAM<br>Category Balance]
    end

    subgraph Processing
        PT[POSTTRAN<br>CBTRN02C]
    end

    subgraph Output
        TR[TRANSACT.VSAM<br>Transaction Master]
        RJ[DALYREJS GDG<br>Rejects]
        TCU[TCATBALF.VSAM<br>Updated Balances]
    end

    DT --> PT
    XR --> PT
    AC --> PT
    TC --> PT
    PT --> TR
    PT --> RJ
    PT --> TCU
```

### 4.2 Interest Calculation Flow

```mermaid
flowchart TB
    subgraph Input
        TC[TCATBALF.VSAM<br>Category Balance]
        XR[CARDXREF.VSAM<br>+ AIX Path]
        AC[ACCTDATA.VSAM<br>Account Master]
        DG[DISCGRP.VSAM<br>Disclosure Groups]
    end

    subgraph Processing
        IC[INTCALC<br>CBACT04C]
    end

    subgraph Output
        ST[SYSTRAN GDG<br>System Transactions]
    end

    TC --> IC
    XR --> IC
    AC --> IC
    DG --> IC
    IC --> ST
```

### 4.3 Transaction Consolidation Flow

```mermaid
flowchart TB
    subgraph "Step 1: Backup (TRANBKP)"
        TV[TRANSACT.VSAM] -->|REPRO| TB[TRANSACT.BKUP<br>GDG +1]
        TB --> DEL[DELETE/DEFINE<br>TRANSACT.VSAM]
    end

    subgraph "Step 2: Combine (COMBTRAN)"
        TB0[TRANSACT.BKUP<br>GDG 0] --> SORT
        ST0[SYSTRAN<br>GDG 0] --> SORT
        SORT --> TC[TRANSACT.COMBINED<br>GDG +1]
        TC -->|REPRO| TV2[TRANSACT.VSAM<br>Reloaded]
    end

    subgraph "Step 3: Index (TRANIDX)"
        TV2 --> AIX[TRANSACT.VSAM.AIX<br>Processed Timestamp]
    end
```

### 4.4 Export/Import Flow

```mermaid
flowchart LR
    subgraph "Source System"
        C[CUSTDATA]
        A[ACCTDATA]
        X[CARDXREF]
        T[TRANSACT]
        D[CARDDATA]
    end

    subgraph "Export (CBEXPORT)"
        C --> E[EXPORT.DATA<br>Unified VSAM]
        A --> E
        X --> E
        T --> E
        D --> E
    end

    subgraph "Import (CBIMPORT)"
        E --> CI[CUSTDATA.IMPORT]
        E --> AI[ACCTDATA.IMPORT]
        E --> XI[CARDXREF.IMPORT]
        E --> TI[TRANSACT.IMPORT]
        E --> ER[IMPORT.ERRORS]
    end
```

---

## 5. Restart and Recovery Procedures

### 5.1 Critical Job Recovery Matrix

| Job | Error Point | Recovery Action | Idempotent? |
|-----|-------------|-----------------|-------------|
| POSTTRAN | Any step | Re-run after verifying input | Yes (reprocesses all) |
| INTCALC | Any step | Re-run with same date PARM | Yes |
| TRANBKP | STEP05R | Check backup, re-run | No - verify backup first |
| TRANBKP | STEP05/10 | Re-run from STEP05 | Yes (delete/define) |
| COMBTRAN | STEP05R | Re-run | Yes (recreates output) |
| COMBTRAN | STEP10 | Re-run STEP10 only | Yes (REPRO to empty VSAM) |

### 5.2 POSTTRAN Recovery

**Scenario: Job fails mid-processing**

1. Check DALYREJS GDG for partial rejects
2. Verify TRANSACT.VSAM state
3. If partial update:
   - Restore TRANSACT.VSAM from TRANSACT.BKUP(0)
   - Restore TCATBALF.VSAM from TCATBALF.BKUP(0)
4. Fix input data issue
5. Re-submit POSTTRAN

**Restart JCL:**
```jcl
//POSTTRAN JOB ... RESTART=STEP15
```

### 5.3 INTCALC Recovery

**Scenario: Job abends during calculation**

1. Output SYSTRAN GDG generation may be partial
2. Delete partial generation: `DELETE AWS.M2.CARDDEMO.SYSTRAN(+1)`
3. Re-submit INTCALC with same PARM

### 5.4 TRANBKP Recovery

**Scenario: VSAM delete succeeds but define fails**

1. Transaction VSAM is unavailable
2. Check TRANSACT.BKUP(+1) contains valid data
3. Restart from STEP10 to redefine cluster
4. If backup failed, restore from TRANSACT.BKUP(0)

### 5.5 COMBTRAN Recovery

**Scenario: SORT step fails**

1. Output file may be partial
2. Delete `AWS.M2.CARDDEMO.TRANSACT.COMBINED(+1)`
3. Re-submit job

**Scenario: REPRO fails**

1. VSAM may be empty or partial
2. Re-run STEP10 only
3. If VSAM corrupted, restore from TRANBKP and re-run COMBTRAN

### 5.6 GDG Management

**Rolling back a bad generation:**
```jcl
//ROLLBACK EXEC PGM=IDCAMS
//SYSIN DD *
  DELETE AWS.M2.CARDDEMO.TRANSACT.BKUP(0) PURGE
/*
```

**Skipping a generation (create placeholder):**
```jcl
//SKIP EXEC PGM=IEBGENER
//SYSUT1 DD DUMMY
//SYSUT2 DD DSN=AWS.M2.CARDDEMO.SYSTRAN(+1),DISP=(NEW,CATLG)
```

---

## 6. Modernization Recommendations

### 6.1 Event-Driven Candidates

| Current Job | Issue | Recommended Architecture |
|-------------|-------|-------------------------|
| POSTTRAN | Batch window constraint | Event streaming (Kafka/Kinesis) |
| CLOSEFIL/OPENFIL | CICS contention | Eliminate with concurrent access |

**POSTTRAN Modernization:**

```mermaid
flowchart LR
    subgraph "Current"
        A[Daily File] -->|Batch| B[POSTTRAN]
        B --> C[VSAM]
    end

    subgraph "Modernized"
        D[Real-time Events] -->|Stream| E[Kafka]
        E --> F[Lambda/Container]
        F --> G[Aurora/DynamoDB]
    end
```

**Benefits:**
- Real-time transaction posting
- No batch window required
- Horizontal scalability
- Built-in retry/DLQ

### 6.2 Batch-Appropriate Jobs

| Job | Reason to Keep Batch | Modern Platform |
|-----|---------------------|-----------------|
| INTCALC | End-of-period calculation | AWS Batch + Step Functions |
| TRANREPT | Date-range aggregation | Lambda + S3 + Athena |
| COMBTRAN | Large volume merge | AWS Glue ETL |
| TRANBKP | Point-in-time backup | Native database snapshots |

**INTCALC Modernization with Step Functions:**

```mermaid
flowchart TB
    A[EventBridge Schedule] --> B[Step Function]
    B --> C{Check Prerequisites}
    C -->|Pass| D[AWS Batch Job]
    C -->|Fail| E[SNS Alert]
    D --> F[Calculate Interest]
    F --> G[Write Results]
    G --> H[Update Status]
```

### 6.3 GDG Migration Strategy

| GDG Pattern | S3 Equivalent |
|-------------|---------------|
| GDG(0) - Current | `s3://bucket/prefix/current/` |
| GDG(+1) - New | `s3://bucket/prefix/YYYY-MM-DD-HH-MM/` |
| GDG(-n) - Historical | S3 versioning or timestamp prefixes |
| SCRATCH | S3 lifecycle policy (delete after N days) |

**Example S3 Structure:**
```
s3://carddemo-data/
├── transactions/
│   ├── current/              # Symlink/latest pointer
│   ├── 2026-02-05-00-00/    # Generation 1
│   ├── 2026-02-04-00-00/    # Generation 2
│   └── 2026-02-03-00-00/    # Generation 3
└── backups/
    └── (same pattern)
```

### 6.4 CICS File Contention Elimination

**Current Pattern:**
```
CLOSEFIL → [Batch Jobs] → OPENFIL
```

**Modernized Pattern (Database):**
- Use Aurora PostgreSQL or DynamoDB
- MVCC provides concurrent read/write
- No file locking required
- Point-in-time recovery built-in

### 6.5 Monitoring and Observability

| Current | Modernized |
|---------|-----------|
| JES spool output | CloudWatch Logs |
| COND codes | Step Function execution state |
| Operator intervention | SNS/PagerDuty alerts |
| CA-7/Control-M | EventBridge + Step Functions |

---

## 7. Appendices

### Appendix A: Program-to-Job Mapping

| Program | Job(s) | Function |
|---------|--------|----------|
| CBACT01C | READACCT | Account data extract |
| CBACT02C | READCARD | Card data read |
| CBACT03C | READXREF | Cross-reference read |
| CBACT04C | INTCALC | Interest calculation |
| CBCUS01C | READCUST | Customer data read |
| CBTRN02C | POSTTRAN | Transaction posting |
| CBTRN03C | TRANREPT | Transaction reporting |
| CBEXPORT | CBEXPORT | Multi-file export |
| CBIMPORT | CBIMPORT | Import and split |
| COBSWAIT | WAITSTEP | Timing delay |
| IDCAMS | 25+ jobs | VSAM utilities |
| IEBGENER | Multiple | Data copy |
| DFHCSDUP | CBADMCDJ | CICS resource definition |
| SORT | COMBTRAN, TRANREPT, PRTCATBL | Data sorting |
| SDSF | CLOSEFIL, OPENFIL | CICS file commands |

### Appendix B: Dataset Inventory

| Dataset Pattern | Type | Key | Record |
|-----------------|------|-----|--------|
| `*.ACCTDATA.VSAM.KSDS` | KSDS | 11,0 | 300 |
| `*.CARDDATA.VSAM.KSDS` | KSDS+AIX | 16,0 / 11,16 | 150 |
| `*.CUSTDATA.VSAM.KSDS` | KSDS | 9,0 | 500 |
| `*.CARDXREF.VSAM.KSDS` | KSDS+AIX | 16,0 / 11,25 | 50 |
| `*.TRANSACT.VSAM.KSDS` | KSDS+AIX | 16,0 / 26,304 | 350 |
| `*.TRANTYPE.VSAM.KSDS` | KSDS | 2,0 | 60 |
| `*.TRANCATG.VSAM.KSDS` | KSDS | 6,0 | 60 |
| `*.TCATBALF.VSAM.KSDS` | KSDS | 17,0 | 50 |
| `*.DISCGRP.VSAM.KSDS` | KSDS | 16,0 | 50 |
| `*.USRSEC.VSAM.KSDS` | KSDS | 8,0 | 80 |
| `*.EXPORT.DATA` | KSDS | 4,28 | 500 |

### Appendix C: CICS File Mapping

| CICS Name | Physical Dataset |
|-----------|------------------|
| TRANSACT | `AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS` |
| CCXREF | `AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS` |
| ACCTDAT | `AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS` |
| CXACAIX | `AWS.M2.CARDDEMO.CARDXREF.VSAM.AIX.PATH` |
| USRSEC | `AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS` |
| CARDDAT | `AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS` |
| CARDAIX | `AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX.PATH` |
| CUSTDAT | `AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS` |

### Appendix D: Condition Code Reference

| IDCAMS RC | Meaning | Action |
|-----------|---------|--------|
| 0 | Success | Continue |
| 4 | Warning (e.g., file not found for delete) | Usually OK |
| 8 | Error (non-fatal) | Investigate |
| 12 | Severe error | Job fails |
| 16 | Critical error | Job fails |

**Common COND Usage:**
```jcl
//STEP10 EXEC PGM=IDCAMS,COND=(4,LT)
```
Meaning: Skip STEP10 if any prior step has RC > 4

---

## 8. Document Metadata

| Attribute | Value |
|-----------|-------|
| Author | Claude (AI-assisted analysis) |
| Prompt | RE-006 |
| Source Files | 33 JCL, 10 COBOL, 2 scheduler configs, 3 shell scripts |
| Output Location | `docs/reverse-engineering/05-specialized/BATCH-WORKFLOWS.md` |
| Work Files | `.work/reverse-engineering/specialized/batch-workflows/` |

---

*This document was generated as part of the CardDemo reverse engineering prompt suite. For the master index, see RE-000.*
