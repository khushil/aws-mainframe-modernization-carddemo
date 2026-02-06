# CardDemo COMMAREA Specification

## Overview

The COMMAREA (Communication Area) is the central state contract for the CardDemo CICS application. It provides the mechanism for passing context between programs in a pseudo-conversational environment. This document provides complete field-level documentation of the COMMAREA structure and its role in maintaining application state.

**Source File**: `app/cpy/COCOM01Y.cpy`
**Record Length**: ~155 bytes
**Copybook Version**: CardDemo_v1.0-15-g27d6c6f-68

---

## COMMAREA Structure Diagram

```
┌─────────────────────────────────────────────────────────────────────────┐
│                        CARDDEMO-COMMAREA (01)                           │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                 CDEMO-GENERAL-INFO (05)                         │   │
│  │  ┌───────────────────┬───────────────────────────────────────┐  │   │
│  │  │ CDEMO-FROM-TRANID │ PIC X(04) - Source transaction ID     │  │   │
│  │  │ CDEMO-FROM-PROGRAM│ PIC X(08) - Source program name       │  │   │
│  │  │ CDEMO-TO-TRANID   │ PIC X(04) - Target transaction ID     │  │   │
│  │  │ CDEMO-TO-PROGRAM  │ PIC X(08) - Target program name       │  │   │
│  │  │ CDEMO-USER-ID     │ PIC X(08) - Authenticated user ID     │  │   │
│  │  │ CDEMO-USER-TYPE   │ PIC X(01) - User role (A=Admin/U=User)│  │   │
│  │  │ CDEMO-PGM-CONTEXT │ PIC 9(01) - Program state (0/1)       │  │   │
│  │  └───────────────────┴───────────────────────────────────────┘  │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                         │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                CDEMO-CUSTOMER-INFO (05)                         │   │
│  │  ┌───────────────────┬───────────────────────────────────────┐  │   │
│  │  │ CDEMO-CUST-ID     │ PIC 9(09) - Selected customer ID      │  │   │
│  │  │ CDEMO-CUST-FNAME  │ PIC X(25) - Customer first name       │  │   │
│  │  │ CDEMO-CUST-MNAME  │ PIC X(25) - Customer middle name      │  │   │
│  │  │ CDEMO-CUST-LNAME  │ PIC X(25) - Customer last name        │  │   │
│  │  └───────────────────┴───────────────────────────────────────┘  │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                         │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                 CDEMO-ACCOUNT-INFO (05)                         │   │
│  │  ┌───────────────────┬───────────────────────────────────────┐  │   │
│  │  │ CDEMO-ACCT-ID     │ PIC 9(11) - Selected account ID       │  │   │
│  │  │ CDEMO-ACCT-STATUS │ PIC X(01) - Account status            │  │   │
│  │  └───────────────────┴───────────────────────────────────────┘  │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                         │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                   CDEMO-CARD-INFO (05)                          │   │
│  │  ┌───────────────────┬───────────────────────────────────────┐  │   │
│  │  │ CDEMO-CARD-NUM    │ PIC 9(16) - Selected card number      │  │   │
│  │  └───────────────────┴───────────────────────────────────────┘  │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                         │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                   CDEMO-MORE-INFO (05)                          │   │
│  │  ┌───────────────────┬───────────────────────────────────────┐  │   │
│  │  │ CDEMO-LAST-MAP    │ PIC X(07) - Last displayed map name   │  │   │
│  │  │ CDEMO-LAST-MAPSET │ PIC X(07) - Last displayed mapset     │  │   │
│  │  └───────────────────┴───────────────────────────────────────┘  │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Field-by-Field Specification

### CDEMO-GENERAL-INFO Section

This section contains navigation and session state information.

#### CDEMO-FROM-TRANID
| Attribute | Value |
|-----------|-------|
| **Level** | 10 |
| **PIC Clause** | PIC X(04) |
| **Bytes** | 4 |
| **Purpose** | Source transaction ID that invoked current program |
| **Valid Values** | CC00, CA00, CM00, or blank |
| **Set By** | Calling program before XCTL |
| **Used By** | Current program for breadcrumb/return logic |

#### CDEMO-FROM-PROGRAM
| Attribute | Value |
|-----------|-------|
| **Level** | 10 |
| **PIC Clause** | PIC X(08) |
| **Bytes** | 8 |
| **Purpose** | Source program name that invoked current program |
| **Valid Values** | Any CO*.cbl program name (e.g., COSGN00C, COMEN01C) |
| **Set By** | Calling program before XCTL |
| **Used By** | Current program for return navigation |

#### CDEMO-TO-TRANID
| Attribute | Value |
|-----------|-------|
| **Level** | 10 |
| **PIC Clause** | PIC X(04) |
| **Bytes** | 4 |
| **Purpose** | Target transaction ID for RETURN TRANSID |
| **Valid Values** | CC00, CA00, CM00, or blank |
| **Set By** | Current program when preparing navigation |
| **Used By** | CICS RETURN TRANSID command |

#### CDEMO-TO-PROGRAM
| Attribute | Value |
|-----------|-------|
| **Level** | 10 |
| **PIC Clause** | PIC X(08) |
| **Bytes** | 8 |
| **Purpose** | Target program name for XCTL |
| **Valid Values** | Any CO*.cbl program name |
| **Set By** | Menu programs, PF3 handlers |
| **Used By** | XCTL PROGRAM command |

#### CDEMO-USER-ID
| Attribute | Value |
|-----------|-------|
| **Level** | 10 |
| **PIC Clause** | PIC X(08) |
| **Bytes** | 8 |
| **Purpose** | Authenticated user identifier |
| **Valid Values** | Uppercase alphanumeric user ID from USRSEC |
| **Set By** | COSGN00C after successful authentication |
| **Used By** | All programs for audit/display purposes |
| **Security** | Must match SEC-USR-ID in USRSEC file |

#### CDEMO-USER-TYPE
| Attribute | Value |
|-----------|-------|
| **Level** | 10 |
| **PIC Clause** | PIC X(01) |
| **Bytes** | 1 |
| **Purpose** | User role for access control |
| **Valid Values** | 'A' (Admin) or 'U' (User) |
| **Set By** | COSGN00C from SEC-USR-TYPE |
| **Used By** | Menu routing, access control checks |
| **88 Levels** | CDEMO-USRTYP-ADMIN ('A'), CDEMO-USRTYP-USER ('U') |

**Code Reference** (`COCOM01Y.cpy:26-28`):
```cobol
10 CDEMO-USER-TYPE               PIC X(01).
   88 CDEMO-USRTYP-ADMIN         VALUE 'A'.
   88 CDEMO-USRTYP-USER          VALUE 'U'.
```

#### CDEMO-PGM-CONTEXT
| Attribute | Value |
|-----------|-------|
| **Level** | 10 |
| **PIC Clause** | PIC 9(01) |
| **Bytes** | 1 |
| **Purpose** | Pseudo-conversational state indicator |
| **Valid Values** | 0 (initial entry) or 1 (re-entry) |
| **Set By** | Current program on state transitions |
| **Used By** | All programs for first-time vs. re-entry logic |
| **88 Levels** | CDEMO-PGM-ENTER (0), CDEMO-PGM-REENTER (1) |

**Code Reference** (`COCOM01Y.cpy:29-31`):
```cobol
10 CDEMO-PGM-CONTEXT             PIC 9(01).
   88 CDEMO-PGM-ENTER            VALUE 0.
   88 CDEMO-PGM-REENTER          VALUE 1.
```

---

### CDEMO-CUSTOMER-INFO Section

This section contains the currently selected customer context.

#### CDEMO-CUST-ID
| Attribute | Value |
|-----------|-------|
| **Level** | 10 |
| **PIC Clause** | PIC 9(09) |
| **Bytes** | 9 |
| **Purpose** | Selected customer identifier |
| **Valid Values** | 9-digit numeric customer ID |
| **Set By** | Account/Card programs after selection |
| **Used By** | Programs requiring customer context |

#### CDEMO-CUST-FNAME
| Attribute | Value |
|-----------|-------|
| **Level** | 10 |
| **PIC Clause** | PIC X(25) |
| **Bytes** | 25 |
| **Purpose** | Customer first name for display |
| **Valid Values** | Text or spaces |
| **Set By** | Programs that read CUSTDAT |
| **Used By** | Screen displays |

#### CDEMO-CUST-MNAME
| Attribute | Value |
|-----------|-------|
| **Level** | 10 |
| **PIC Clause** | PIC X(25) |
| **Bytes** | 25 |
| **Purpose** | Customer middle name for display |
| **Valid Values** | Text or spaces |
| **Set By** | Programs that read CUSTDAT |
| **Used By** | Screen displays |

#### CDEMO-CUST-LNAME
| Attribute | Value |
|-----------|-------|
| **Level** | 10 |
| **PIC Clause** | PIC X(25) |
| **Bytes** | 25 |
| **Purpose** | Customer last name for display |
| **Valid Values** | Text or spaces |
| **Set By** | Programs that read CUSTDAT |
| **Used By** | Screen displays |

---

### CDEMO-ACCOUNT-INFO Section

This section contains the currently selected account context.

#### CDEMO-ACCT-ID
| Attribute | Value |
|-----------|-------|
| **Level** | 10 |
| **PIC Clause** | PIC 9(11) |
| **Bytes** | 11 |
| **Purpose** | Selected account identifier |
| **Valid Values** | 11-digit numeric account ID |
| **Set By** | Account view/update programs |
| **Used By** | Card, Transaction, Bill Payment programs |
| **Key Field** | Maps to ACCT-ID in ACCTDAT |

#### CDEMO-ACCT-STATUS
| Attribute | Value |
|-----------|-------|
| **Level** | 10 |
| **PIC Clause** | PIC X(01) |
| **Bytes** | 1 |
| **Purpose** | Selected account status |
| **Valid Values** | 'Y' (Active), 'N' (Inactive) |
| **Set By** | Account programs after read |
| **Used By** | Validation logic |

---

### CDEMO-CARD-INFO Section

This section contains the currently selected card context.

#### CDEMO-CARD-NUM
| Attribute | Value |
|-----------|-------|
| **Level** | 10 |
| **PIC Clause** | PIC 9(16) |
| **Bytes** | 16 |
| **Purpose** | Selected card number |
| **Valid Values** | 16-digit numeric card number |
| **Set By** | Card selection programs |
| **Used By** | Card management, Transaction programs |
| **Key Field** | Maps to CARD-NUM in CARDDAT |
| **Security** | Full card number - handle with care |

---

### CDEMO-MORE-INFO Section

This section contains screen/map tracking information.

#### CDEMO-LAST-MAP
| Attribute | Value |
|-----------|-------|
| **Level** | 10 |
| **PIC Clause** | PIC X(07) |
| **Bytes** | 7 |
| **Purpose** | Last displayed BMS map name |
| **Valid Values** | BMS map name (e.g., COSGN0A, COMEN1A) |
| **Set By** | After SEND MAP |
| **Used By** | Screen refresh logic |

#### CDEMO-LAST-MAPSET
| Attribute | Value |
|-----------|-------|
| **Level** | 10 |
| **PIC Clause** | PIC X(07) |
| **Bytes** | 7 |
| **Purpose** | Last displayed BMS mapset name |
| **Valid Values** | BMS mapset name (e.g., COSGN00, COMEN01) |
| **Set By** | After SEND MAP |
| **Used By** | Screen refresh logic |

---

## State Transition Rules

### Session Initialization (COSGN00C)

When a user successfully authenticates:

```cobol
MOVE WS-TRANID    TO CDEMO-FROM-TRANID    *> 'CC00'
MOVE WS-PGMNAME   TO CDEMO-FROM-PROGRAM   *> 'COSGN00C'
MOVE WS-USER-ID   TO CDEMO-USER-ID        *> User's login ID
MOVE SEC-USR-TYPE TO CDEMO-USER-TYPE      *> 'A' or 'U'
MOVE ZEROS        TO CDEMO-PGM-CONTEXT    *> First entry
```

### Program Entry (All Programs)

On initial entry to any program:

```cobol
IF NOT CDEMO-PGM-REENTER
    SET CDEMO-PGM-REENTER TO TRUE
    MOVE LOW-VALUES TO <screen-output-area>
    PERFORM SEND-SCREEN
ELSE
    PERFORM RECEIVE-SCREEN
    PERFORM PROCESS-INPUT
END-IF
```

### Navigation Between Programs

When transferring to another program via XCTL:

```cobol
MOVE WS-TRANID  TO CDEMO-FROM-TRANID
MOVE WS-PGMNAME TO CDEMO-FROM-PROGRAM
MOVE ZEROS      TO CDEMO-PGM-CONTEXT
EXEC CICS XCTL
    PROGRAM(target-program)
    COMMAREA(CARDDEMO-COMMAREA)
END-EXEC
```

### Return to Menu (PF3)

When returning to previous menu:

```cobol
MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM    *> or menu program
EXEC CICS XCTL
    PROGRAM(CDEMO-TO-PROGRAM)
END-EXEC
```

### Pseudo-Conversational Return

At end of each CICS task:

```cobol
EXEC CICS RETURN
    TRANSID(WS-TRANID)
    COMMAREA(CARDDEMO-COMMAREA)
    LENGTH(LENGTH OF CARDDEMO-COMMAREA)
END-EXEC
```

---

## State Diagram

```mermaid
stateDiagram-v2
    [*] --> Unauthenticated: CICS START

    Unauthenticated --> Authenticated: LOGIN SUCCESS
    note right of Authenticated
        CDEMO-USER-ID = user
        CDEMO-USER-TYPE = A/U
        CDEMO-PGM-CONTEXT = 0
    end note

    Authenticated --> AdminMenu: USER-TYPE = 'A'
    Authenticated --> UserMenu: USER-TYPE = 'U'

    AdminMenu --> UserOperation: SELECT OPTION
    UserMenu --> UserOperation: SELECT OPTION

    UserOperation --> UserOperation: RE-ENTER (PGM-CONTEXT=1)
    UserOperation --> AdminMenu: PF3 (Admin)
    UserOperation --> UserMenu: PF3 (User)

    AdminMenu --> Unauthenticated: PF3 (LOGOUT)
    UserMenu --> Unauthenticated: PF3 (LOGOUT)
```

---

## Extended COMMAREA Sections

Some programs define extended COMMAREA sections for program-specific data. These are appended to the standard CARDDEMO-COMMAREA.

### Batch Job Information (Not in standard COMMAREA)

Batch programs do not use COMMAREA as they run outside CICS. However, similar context is maintained in working storage.

### Transaction Add Context (COTRN02C)

The transaction add program uses CDEMO-CARD-NUM and CDEMO-ACCT-ID from the COMMAREA to pre-populate transaction entry screens.

### Report Context (CORPT00C)

Report programs may use CDEMO-ACCT-ID and CDEMO-CUST-ID to filter report output.

---

## Modernization Implications

### COMMAREA to JWT Claims Mapping

| COMMAREA Field | JWT Claim | Notes |
|----------------|-----------|-------|
| CDEMO-USER-ID | `sub` (subject) | Primary user identifier |
| CDEMO-USER-TYPE | `role` or custom claim | Authorization scope |
| CDEMO-FROM-PROGRAM | Not needed | Stateless - no breadcrumb |
| CDEMO-TO-PROGRAM | Not needed | Stateless routing |
| CDEMO-PGM-CONTEXT | Not needed | No pseudo-conversational |

### COMMAREA to API Schema Mapping

| COMMAREA Section | Modern Pattern |
|------------------|----------------|
| Customer Info | Path parameter `/customers/{custId}` |
| Account Info | Path parameter `/accounts/{acctId}` |
| Card Info | Path parameter `/cards/{cardNum}` |
| Navigation | Not needed - REST is stateless |
| Screen State | Not needed - client maintains state |

### Session State Strategy

For modernization, replace COMMAREA with:

1. **JWT Tokens**: User identity and roles
2. **Path Parameters**: Resource identifiers (acctId, cardNum)
3. **Query Parameters**: Filtering and pagination
4. **Request Body**: Mutation data
5. **Client State**: React/Angular state management

### API Request Example

**Legacy COMMAREA-based flow**:
```
COMMAREA: {USER-ID: 'ADMIN001', USER-TYPE: 'A', ACCT-ID: 12345678901}
XCTL PROGRAM('COACTUPC') COMMAREA(...)
```

**Modern REST equivalent**:
```http
GET /api/v1/accounts/12345678901
Authorization: Bearer eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9...
```

JWT payload contains:
```json
{
  "sub": "ADMIN001",
  "role": "admin",
  "iat": 1709683200,
  "exp": 1709686800
}
```

---

## Validation Rules

### Field Validation

| Field | Validation Rule |
|-------|-----------------|
| CDEMO-USER-ID | Must exist in USRSEC file |
| CDEMO-USER-TYPE | Must be 'A' or 'U' |
| CDEMO-PGM-CONTEXT | Must be 0 or 1 |
| CDEMO-ACCT-ID | Must be 11 numeric digits |
| CDEMO-CARD-NUM | Must be 16 numeric digits |
| CDEMO-CUST-ID | Must be 9 numeric digits |

### Cross-Field Validation

| Rule | Description |
|------|-------------|
| Admin Access | If CDEMO-USER-TYPE = 'U', cannot access admin programs |
| Account Ownership | CDEMO-ACCT-ID must be linked to CDEMO-CARD-NUM via CCXREF |
| Program Flow | CDEMO-TO-PROGRAM must be valid program name if set |

---

## Source References

| Item | Source |
|------|--------|
| COMMAREA Definition | `app/cpy/COCOM01Y.cpy:19-45` |
| Session Initialization | `app/cbl/COSGN00C.cbl:224-228` |
| Admin Menu Options | `app/cpy/COADM02Y.cpy:19-60` |
| User Menu Options | `app/cpy/COMEN02Y.cpy:19-98` |
| User Type Conditions | `app/cpy/COCOM01Y.cpy:27-28` |
| Program Context Conditions | `app/cpy/COCOM01Y.cpy:30-31` |

---

## Cross-References

- **Context Map**: See [CONTEXT-MAP.md](./CONTEXT-MAP.md) for bounded context relationships
- **Navigation Flows**: See [NAVIGATION-FLOWS.md](./NAVIGATION-FLOWS.md) for program flow diagrams
- **Domain Model**: See [DOMAIN-MODEL.md](../01-domain-model/DOMAIN-MODEL.md) for entity definitions
