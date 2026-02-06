# C4 Level 2: Container View - CardDemo Application

## Executive Summary

This document describes the Container (L2) view of the CardDemo mainframe application. Containers represent separately runnable/deployable units that execute code or store data. In the mainframe context, these map to runtime environments like CICS regions, batch subsystems, and data stores.

---

## Container Inventory

### Core Containers

| Container | Technology | Program Count | Purpose |
|-----------|------------|---------------|---------|
| **CICS Online Region** | CICS TS / COBOL | 19 core programs | Interactive transaction processing |
| **Batch Subsystem** | JES2 / JCL / COBOL | 12 programs | Scheduled batch processing |
| **BMS Presentation** | BMS Mapsets | 17 mapsets | 3270 terminal screen definitions |
| **VSAM Data Store** | VSAM KSDS | 6+ files | Primary data persistence |

### Optional Extension Containers

| Container | Technology | Program Count | Directory | Purpose |
|-----------|------------|---------------|-----------|---------|
| **DB2 Database** | DB2 + COBOL | 3 programs | `app-transaction-type-db2/` | Transaction type management |
| **IMS Database** | IMS hierarchical + COBOL | 4 programs | `app-authorization-ims-db2-mq/` | Authorization data |
| **MQ Messaging** | IBM MQ | 2 programs | `app-vsam-mq/` | Async account extraction |

---

## Container Details

### 1. CICS Online Region

**Technology**: IBM CICS Transaction Server, COBOL
**Transaction IDs**: CC00, CA00, CM00, CB00, plus feature-specific

**Programs (19 core + 9 extension)**:

| Program | Transaction | Function | BMS Mapset |
|---------|-------------|----------|------------|
| COSGN00C | CC00 | User signon/authentication | COSGN00 |
| COADM01C | CA00 | Admin menu navigation | COADM01 |
| COMEN01C | CM00 | User menu navigation | COMEN01 |
| COACTVWC | CM00 | Account view | COACTVW |
| COACTUPC | CM00 | Account update | COACTUP |
| COCRDLIC | CM00 | Card list | COCRDLI |
| COCRDSLC | CM00 | Card view/select | COCRDSL |
| COCRDUPC | CM00 | Card update | COCRDUP |
| COTRN00C | CM00 | Transaction list | COTRN00 |
| COTRN01C | CM00 | Transaction view | COTRN01 |
| COTRN02C | CM00 | Transaction add | COTRN02 |
| COBIL00C | CB00 | Bill payment | COBIL00 |
| CORPT00C | CM00 | Transaction reports | CORPT00 |
| COUSR00C | CA00 | User list | COUSR00 |
| COUSR01C | CA00 | User add | COUSR01 |
| COUSR02C | CA00 | User update | COUSR02 |
| COUSR03C | CA00 | User delete | COUSR03 |
| COBSWAIT | -- | Wait utility | -- |
| CSUTLDTC | -- | Date validation utility (CALL) | -- |

**Extension Programs** (in `app-authorization-ims-db2-mq/`):

| Program | Function | Technology |
|---------|----------|------------|
| COPAUA0C | Authorization processing | IMS + DB2 + MQ |
| COPAUS0C | Pending authorization view | IMS + DB2 |
| COPAUS1C | Authorization details | IMS + DB2 |
| COPAUS2C | Authorization update | IMS + DB2 |

**Extension Programs** (in `app-transaction-type-db2/`):

| Program | Function | Technology |
|---------|----------|------------|
| COTRTLIC | Transaction type list | DB2 |
| COTRTUPC | Transaction type update | DB2 |
| COBTUPDT | Batch type update | DB2 |

**Extension Programs** (in `app-vsam-mq/`):

| Program | Function | Technology |
|---------|----------|------------|
| COACCT01 | Account extraction | VSAM + MQ |
| CODATE01 | Date utility | COBOL |

*Source: `app/cbl/CO*.cbl`, extension directories*

---

### 2. Batch Subsystem

**Technology**: JES2, JCL, COBOL batch programs
**Execution**: Scheduled via JES2 or on-demand

**Programs (12)**:

| Program | Function | Input Files | Output Files |
|---------|----------|-------------|--------------|
| CBACT01C | Account file read | ACCTDAT | Report |
| CBACT02C | Account file update | ACCTDAT, Input | ACCTDAT |
| CBACT03C | Account add | Input | ACCTDAT |
| CBACT04C | Account delete | Input | ACCTDAT |
| CBCUS01C | Customer maintenance | CUSTDAT, Input | CUSTDAT |
| CBTRN01C | Transaction read | TRANSACT | Report |
| CBTRN02C | Daily transaction posting | DALYTRAN, XREF, ACCT | TRANSACT, TCATBAL, REJECTS |
| CBTRN03C | Interest calculation | ACCTDAT, TRANSACT | ACCTDAT |
| CBEXPORT | Data export (to ASCII) | VSAM files | ASCII files |
| CBIMPORT | Data import (from ASCII) | ASCII files | VSAM files |
| CBSTM03A | Statement generation (main) | TRANSACT, CUSTDAT, ACCTDAT, CCXREF | Statement files (text + HTML) |
| CBSTM03B | Statement generation (file I/O subroutine) | VSAM files | Called by CBSTM03A |

**Batch Job Workflows**:

| Workflow | Programs | Schedule |
|----------|----------|----------|
| Full Batch Cycle | CBACT02C → CBTRN02C → CBTRN03C | Nightly |
| Transaction Posting | CBTRN02C | Nightly |
| Interest Calculation | CBTRN01C, CBTRN03C | Nightly |
| Data Migration | CBEXPORT, CBIMPORT | On-demand |

*Source: `app/cbl/CB*.cbl`, `app/jcl/*.jcl`*

---

### 3. BMS Presentation Layer

**Technology**: Basic Mapping Support (BMS), 3270 datastream
**Screen Size**: 24x80 terminal standard

**Mapsets (17)**:

| Mapset | Map(s) | Function | Used By |
|--------|--------|----------|---------|
| COSGN00 | COSGN0A | Signon screen | COSGN00C |
| COADM01 | COADM1A | Admin menu | COADM01C |
| COMEN01 | COMEN1A | User menu | COMEN01C |
| COACTVW | COACTVWA | Account view | COACTVWC |
| COACTUP | COACTUPA | Account update | COACTUPC |
| COCRDLI | COCRDLIA | Card list | COCRDLIC |
| COCRDSL | COCRDSLA | Card select/view | COCRDSLC |
| COCRDUP | COCRDUPA | Card update | COCRDUPC |
| COTRN00 | COTRN00A | Transaction list | COTRN00C |
| COTRN01 | COTRN01A | Transaction view | COTRN01C |
| COTRN02 | COTRN02A | Transaction add | COTRN02C |
| COBIL00 | COBIL0A | Bill payment | COBIL00C |
| CORPT00 | CORPT00A | Reports | CORPT00C |
| COUSR00 | COUSR00A | User list | COUSR00C |
| COUSR01 | COUSR01A | User add | COUSR01C |
| COUSR02 | COUSR02A | User update | COUSR02C |
| COUSR03 | COUSR03A | User delete | COUSR03C |

*Source: `app/bms/*.bms`*

---

### 4. VSAM Data Store

**Technology**: VSAM KSDS (Key-Sequenced Data Set)
**Access**: EXEC CICS FILE (online), COBOL I/O (batch)

**Primary Data Files**:

| File | DD Name | Key | Record Size | Purpose |
|------|---------|-----|-------------|---------|
| ACCTDAT | ACCTFILE | Account ID (11) | 300 bytes | Account master |
| CARDDAT | CARDFILE | Card Number (16) | varies | Card master |
| CUSTDAT | CUSTFILE | Customer ID (9) | varies | Customer master |
| TRANSACT | TRANFILE | Transaction ID (16) | 350 bytes | Transaction log |
| USRSEC | USRSEC | User ID (8) | varies | Security/users |
| CCXREF | XREFFILE | Card Number (16) | 50 bytes | Card-account xref |

**Alternate Indexes**:

| AIX Name | Base File | Alternate Key | Purpose |
|----------|-----------|---------------|---------|
| CXACAIX | CCXREF | Account ID (11) | Lookup cards by account |
| CARDAIX | CARDDAT | Account ID (11) | Lookup cards by account |

**Supporting Files** (Batch):

| File | DD Name | Type | Purpose |
|------|---------|------|---------|
| DALYTRAN | DALYTRAN | Sequential | Daily transaction input |
| DALYREJS | DALYREJS | Sequential | Rejected transactions |
| TCATBAL | TCATBALF | KSDS | Transaction category balances |

*Source: `app/cpy/CVACT*.cpy`, `app/cpy/CVTRA*.cpy`, `app/cpy/CSUSR*.cpy`*

---

### 5. DB2 Database [Optional]

**Technology**: IBM DB2 for z/OS
**Access**: EXEC SQL (embedded SQL in COBOL)

**Tables**:

| Table | Purpose | Programs |
|-------|---------|----------|
| TRAN_TYPE | Transaction type codes | COTRTLIC, COTRTUPC |
| AUTH_PENDING | Pending authorizations | COPAUS0C-2C |

**Integration Pattern**:
- DB2 connection via CICS DB2 attachment
- Static SQL in COBOL programs
- Commit controlled by CICS

*Source: `app-transaction-type-db2/`*

---

### 6. IMS Database [Optional]

**Technology**: IBM IMS DB (hierarchical database)
**Access**: DL/I calls

**Segments**:

| Segment | Purpose | Programs |
|---------|---------|----------|
| AUTH_ROOT | Authorization header | COPAUA0C |
| AUTH_DETAIL | Authorization line items | COPAUS0C-2C |

*Source: `app-authorization-ims-db2-mq/`*

---

### 7. MQ Messaging [Optional]

**Technology**: IBM MQ
**Access**: MQPUT, MQGET, MQOPEN, MQCLOSE

**Queues**:

| Queue | Direction | Purpose | Programs |
|-------|-----------|---------|----------|
| ACCT.EXTRACT.REQ | Input | Account extraction requests | COACCT01 |
| ACCT.EXTRACT.RESP | Output | Extracted account data | COACCT01 |
| AUTH.REQUEST | Output | Authorization requests | COPAUA0C |
| AUTH.RESPONSE | Input | Authorization responses | COPAUA0C |

*Source: `app-vsam-mq/`, `app-authorization-ims-db2-mq/`*

---

## Container Communication

### Communication Protocols

| From | To | Protocol | Mechanism |
|------|-----|----------|-----------|
| CICS Programs | BMS | SEND/RECEIVE MAP | EXEC CICS SEND/RECEIVE |
| CICS Programs | VSAM | EXEC CICS FILE | READ/WRITE/REWRITE/DELETE |
| CICS Programs | CICS Programs | XCTL/LINK | EXEC CICS XCTL/LINK |
| CICS Programs | DB2 | EXEC SQL | Static SQL |
| CICS Programs | IMS | DL/I | EXEC DLI |
| CICS Programs | MQ | MQPUT/MQGET | EXEC MQ |
| Batch Programs | VSAM | COBOL I/O | OPEN/READ/WRITE/CLOSE |
| Batch Programs | Sequential | COBOL I/O | OPEN/READ/WRITE/CLOSE |

### State Management

**COMMAREA (Communication Area)**:
- Passed between CICS programs via XCTL
- Contains session state (user ID, user type, program context)
- Defined in `COCOM01Y.cpy`
- Size: Variable (up to 32KB)

```cobol
* Key COMMAREA fields (COCOM01Y.cpy)
01 CARDDEMO-COMMAREA.
   05 CDEMO-GENERAL-INFO.
      10 CDEMO-FROM-TRANID      PIC X(04).
      10 CDEMO-FROM-PROGRAM     PIC X(08).
      10 CDEMO-TO-TRANID        PIC X(04).
      10 CDEMO-TO-PROGRAM       PIC X(08).
      10 CDEMO-USER-ID          PIC X(08).
      10 CDEMO-USER-TYPE        PIC X(01).
      10 CDEMO-PGM-CONTEXT      PIC 9(01).
```

---

## Container Diagram Summary

### Container Relationships

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                            CardDemo System                                   │
│                                                                             │
│  ┌───────────────┐    ┌───────────────┐    ┌───────────────────────────┐  │
│  │    3270       │    │     BMS       │    │        CICS Region        │  │
│  │  Terminals    │◄──►│  Presentation │◄──►│    (19+ programs)         │  │
│  │               │    │  (17 mapsets) │    │                           │  │
│  └───────────────┘    └───────────────┘    └─────────────┬─────────────┘  │
│                                                          │                  │
│                                            ┌─────────────┼─────────────┐   │
│                                            │             │             │   │
│                                            ▼             ▼             ▼   │
│  ┌───────────────┐                  ┌──────────┐ ┌──────────┐ ┌────────┐  │
│  │    JES2       │                  │   VSAM   │ │   DB2    │ │  IMS   │  │
│  │   Scheduler   │                  │  (6 files)│ │[Optional]│ │[Opt.]  │  │
│  └───────┬───────┘                  └──────────┘ └──────────┘ └────────┘  │
│          │                                 ▲                              │
│          ▼                                 │                              │
│  ┌───────────────┐                         │                              │
│  │    Batch      │─────────────────────────┘           ┌────────────┐    │
│  │  Subsystem    │                                     │    MQ      │    │
│  │ (12 programs) │◄───────────────────────────────────►│ [Optional] │    │
│  └───────────────┘                                     └────────────┘    │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Deployment Considerations

### Resource Requirements

| Container | CICS Region | Memory | CPU |
|-----------|-------------|--------|-----|
| CICS Online | Production | Variable | Shared |
| Batch | N/A | Depends on file size | Dedicated during batch |
| VSAM | N/A | Buffer pools | I/O bound |
| DB2 | N/A | DB2 region | SQL execution |

### Scaling Patterns

**Online Scaling**:
- CICS MRO (Multi-Region Option) for workload distribution
- CICS CICSPlex for multiple regions

**Batch Scaling**:
- Parallel job execution in JES2
- File-based partitioning

---

## Cross-References

- **Level 1 System Context**: [C4-L1-SYSTEM-CONTEXT.md](./C4-L1-SYSTEM-CONTEXT.md)
- **Level 3 Component View**: [C4-L3-COMPONENT.md](./C4-L3-COMPONENT.md)
- **Container Diagram**: [diagrams/container.md](./diagrams/container.md)
- **Data Model**: [../02-data-model/DATA-MODEL.md](../02-data-model/DATA-MODEL.md)
- **Context Map**: [../03-context-model/CONTEXT-MAP.md](../03-context-model/CONTEXT-MAP.md)

---

## Source File References

| Artifact | Source Files |
|----------|--------------|
| CICS Programs | `app/cbl/CO*.cbl` |
| Batch Programs | `app/cbl/CB*.cbl` |
| BMS Mapsets | `app/bms/*.bms` |
| VSAM Copybooks | `app/cpy/CVACT*.cpy`, `app/cpy/CVTRA*.cpy` |
| COMMAREA | `app/cpy/COCOM01Y.cpy` |
| Menu Definitions | `app/cpy/COADM02Y.cpy`, `app/cpy/COMEN02Y.cpy` |
| Extension Programs | `app/app-*/cbl/*.cbl` |
