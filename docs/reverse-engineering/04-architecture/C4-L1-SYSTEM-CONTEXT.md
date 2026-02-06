# C4 Level 1: System Context - CardDemo Application

## Executive Summary

This document describes the System Context (L1) view of the CardDemo mainframe credit card processing application. The System Context diagram shows the CardDemo system as a single box in the center, surrounded by its actors (users) and external systems it interacts with.

---

## System Overview

**CardDemo** is an IBM mainframe application that provides credit card account management capabilities. It processes credit card transactions, manages accounts, handles bill payments, and maintains customer and card information. The application operates in two modes:

1. **Online Processing** - CICS-based interactive transactions via 3270 terminals
2. **Batch Processing** - JES2/JCL scheduled jobs for nightly operations

---

## Actors

The CardDemo application serves four distinct actor types:

### Human Actors

| Actor | User Type | Authentication | Primary Capabilities |
|-------|-----------|----------------|---------------------|
| **Card Holder** | 'U' (User) | USRSEC file | View accounts/cards, make payments, view transactions |
| **Customer Service Representative** | 'U' (User) | USRSEC file | Full account/card management, transaction entry, reports |
| **System Administrator** | 'A' (Admin) | USRSEC file | User management, system configuration, DB2 type maintenance |

### System Actors

| Actor | Interaction Mode | Primary Function |
|-------|-----------------|------------------|
| **Batch Operator** | JCL submission | Schedule and monitor batch job execution |

---

## Actor Capabilities Matrix

### Card Holder Capabilities

| Menu Option | Function | Program | Access Level |
|-------------|----------|---------|--------------|
| 1 | Account View | COACTVWC | View only |
| 3 | Credit Card List | COCRDLIC | View only |
| 4 | Credit Card View | COCRDSLC | View only |
| 6 | Transaction List | COTRN00C | View only |
| 7 | Transaction View | COTRN01C | View only |
| 10 | Bill Payment | COBIL00C | Execute payments |

*Source: `app/cpy/COMEN02Y.cpy:19-98`*

### Customer Service Representative Capabilities

All Card Holder capabilities plus:

| Menu Option | Function | Program | Access Level |
|-------------|----------|---------|--------------|
| 2 | Account Update | COACTUPC | Read/Write |
| 5 | Credit Card Update | COCRDUPC | Read/Write |
| 8 | Transaction Add | COTRN02C | Create |
| 9 | Transaction Reports | CORPT00C | Generate |
| 11 | Pending Authorization View | COPAUS0C | View [Optional] |

*Source: `app/cpy/COMEN02Y.cpy`*

### System Administrator Capabilities

Accessed via Admin Menu (COADM01C):

| Menu Option | Function | Program | Access Level |
|-------------|----------|---------|--------------|
| 1 | User List (Security) | COUSR00C | Browse |
| 2 | User Add (Security) | COUSR01C | Create |
| 3 | User Update (Security) | COUSR02C | Update |
| 4 | User Delete (Security) | COUSR03C | Delete |
| 5 | Transaction Type List/Update (DB2) | COTRTLIC | Read/Write [Optional] |
| 6 | Transaction Type Maintenance (DB2) | COTRTUPC | Read/Write [Optional] |

*Source: `app/cpy/COADM02Y.cpy:19-59`*

### Batch Operator Capabilities

| JCL Job | Function | Programs Invoked |
|---------|----------|------------------|
| Account Refresh | Reload account data | CBACT01C-04C |
| Transaction Posting | Post daily transactions | CBTRN02C |
| Interest Calculation | Calculate daily interest | CBTRN01C, CBTRN03C |
| Customer Maintenance | Update customer records | CBCUS01C |
| Data Export | Export to ASCII format | CBEXPORT |
| Data Import | Import from ASCII format | CBIMPORT |

*Source: `app/jcl/*.jcl`*

---

## External Systems

### Core External Systems

| System | Integration Type | Data Exchange | Status |
|--------|-----------------|---------------|--------|
| **3270 Terminal Network** | BMS/CICS | Screen maps, user input | Required |
| **JES2 Job Scheduler** | JCL submission | Batch job execution | Required |
| **VSAM File System** | COBOL file I/O | Primary data storage | Required |

### Optional Extension Systems

| System | Integration Technology | Function | Directory |
|--------|----------------------|----------|-----------|
| **Card Authorization Network** | IMS DB + IBM MQ | Real-time authorization | `app-authorization-ims-db2-mq/` |
| **DB2 Database** | EXEC SQL | Transaction type management | `app-transaction-type-db2/` |
| **Message Queue** | IBM MQ (MQPUT/MQGET) | Account extraction, async messaging | `app-vsam-mq/` |
| **Core Banking System** | File-based | GL integration (batch) | N/A |
| **Statement Printing Service** | Batch output | Customer statements | N/A |

---

## System Boundaries

### CardDemo System Boundary

The CardDemo system boundary encompasses:

**Inside Boundary:**
- CICS Transaction Processing Region
- Batch Processing Subsystem
- BMS Presentation Layer
- VSAM Data Files
- All COBOL programs (CO*, CB*)
- All copybooks and BMS mapsets

**Outside Boundary:**
- Terminal network infrastructure
- JES2 job scheduling
- External authorization networks
- Core banking interfaces
- Print services

---

## User Types and Session State

User type determination occurs at sign-on and is preserved in the COMMAREA:

```cobol
* From COCOM01Y.cpy:26-28
10 CDEMO-USER-TYPE               PIC X(01).
   88 CDEMO-USRTYP-ADMIN         VALUE 'A'.
   88 CDEMO-USRTYP-USER          VALUE 'U'.
```

### Authentication Flow

```
User → COSGN00C (CC00) → USRSEC File Lookup
                       ↓
            [Password Valid?]
                  │
         ┌───────┴───────┐
         ▼               ▼
   Admin ('A')      User ('U')
         │               │
         ▼               ▼
    COADM01C         COMEN01C
   (Admin Menu)    (User Menu)
```

*Source: `app/cbl/COSGN00C.cbl:230-240`*

---

## Data Flow Overview

### Online Data Flow

```
                    ┌─────────────────┐
                    │  3270 Terminal  │
                    └────────┬────────┘
                             │
                    ┌────────▼────────┐
                    │   BMS Maps      │
                    │  (17 mapsets)   │
                    └────────┬────────┘
                             │
                    ┌────────▼────────┐
                    │  CICS Programs  │
                    │  (18+ online)   │
                    └────────┬────────┘
                             │
              ┌──────────────┼──────────────┐
              │              │              │
     ┌────────▼────┐ ┌───────▼──────┐ ┌────▼───────┐
     │   VSAM      │ │   DB2        │ │   IMS/MQ   │
     │  (6 files)  │ │ [Optional]   │ │ [Optional] │
     └─────────────┘ └──────────────┘ └────────────┘
```

### Batch Data Flow

```
     ┌─────────────────┐
     │   JCL Jobs      │
     │  (JES2)         │
     └────────┬────────┘
              │
     ┌────────▼────────┐
     │  Batch Programs │
     │  (10 programs)  │
     └────────┬────────┘
              │
      ┌───────┴───────┐
      │               │
┌─────▼─────┐  ┌──────▼──────┐
│   VSAM    │  │  Sequential │
│   Files   │  │   Files     │
└───────────┘  └─────────────┘
```

---

## Security Boundaries

### Authentication Boundary

| Checkpoint | Mechanism | File |
|------------|-----------|------|
| User Login | Password validation | USRSEC |
| Menu Access | User type check (A/U) | COMMAREA |
| Function Access | Menu option validation | COMEN02Y/COADM02Y |

### Authorization Model

```
┌──────────────────────────────────────────────────────┐
│                    CardDemo System                    │
├──────────────────────────────────────────────────────┤
│  ┌────────────────────────────────────────────────┐  │
│  │         Admin Zone (User Type 'A')             │  │
│  │  ┌──────────────────────────────────────────┐  │  │
│  │  │  User Management (COUSR00C-03C)          │  │  │
│  │  │  Transaction Type Config (DB2) [Optional]│  │  │
│  │  └──────────────────────────────────────────┘  │  │
│  └────────────────────────────────────────────────┘  │
│                                                      │
│  ┌────────────────────────────────────────────────┐  │
│  │          User Zone (User Type 'U')             │  │
│  │  ┌──────────────────────────────────────────┐  │  │
│  │  │  Account Management                      │  │  │
│  │  │  Card Management                         │  │  │
│  │  │  Transaction Processing                  │  │  │
│  │  │  Bill Payment                            │  │  │
│  │  │  Reports                                 │  │  │
│  │  └──────────────────────────────────────────┘  │  │
│  └────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────┘
```

---

## Technology Stack Summary

| Layer | Technology | Details |
|-------|------------|---------|
| Presentation | BMS | 17 mapsets, 3270 datastream |
| Transaction Processing | CICS TS | Pseudo-conversational |
| Batch Processing | JES2/JCL | Sequential batch jobs |
| Business Logic | COBOL | 28+ programs |
| Data Storage | VSAM KSDS | 6+ data files |
| Extensions | DB2, IMS, MQ | Optional integrations |

---

## Cross-References

- **Level 2 Container View**: [C4-L2-CONTAINER.md](./C4-L2-CONTAINER.md)
- **Level 3 Component View**: [C4-L3-COMPONENT.md](./C4-L3-COMPONENT.md)
- **Level 4 Code Patterns**: [C4-L4-CODE-PATTERNS.md](./C4-L4-CODE-PATTERNS.md)
- **Context Map**: [../03-context-model/CONTEXT-MAP.md](../03-context-model/CONTEXT-MAP.md)
- **Domain Model**: [../01-domain-model/DOMAIN-MODEL.md](../01-domain-model/DOMAIN-MODEL.md)
- **System Context Diagram**: [diagrams/system-context.md](./diagrams/system-context.md)

---

## Source File References

| Artifact | Source File |
|----------|-------------|
| User Types | `app/cpy/COCOM01Y.cpy:26-28` |
| Admin Menu Options | `app/cpy/COADM02Y.cpy` |
| User Menu Options | `app/cpy/COMEN02Y.cpy` |
| Authentication Logic | `app/cbl/COSGN00C.cbl:230-240` |
| Admin Menu Program | `app/cbl/COADM01C.cbl` |
| User Menu Program | `app/cbl/COMEN01C.cbl` |
