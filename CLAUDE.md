# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

CardDemo is an AWS mainframe credit card processing demonstration application. It showcases IBM mainframe patterns including CICS transaction processing, VSAM file management, and optional DB2/IMS/MQ integration. The application simulates credit card account management with features like account inquiry, card management, bill payment, and transaction tracking.

## Build Commands

### Local Syntax Check (GnuCOBOL)
```bash
# Check COBOL syntax locally
scripts/local_compile.sh
# Or directly:
cobc -I app/cpy/ -T test.txt -tsymbols --std=ibm-strict <file.cbl>
```

### Remote Mainframe Compilation
Requires FTP tunnel on port 2121:
```bash
scripts/remote_compile.sh <filename> <extension> <basename>
# Example: scripts/remote_compile.sh COSGN00C.cbl .cbl COSGN00C
```

### Batch Workflows
```bash
scripts/run_full_batch.sh      # Complete batch cycle (refresh data, post transactions, calculate interest)
scripts/run_interest_calc.sh   # Daily interest calculations only
scripts/run_posting.sh         # Transaction posting only
```

## Directory Structure

```
app/
├── cbl/          # COBOL programs (39 files)
├── cpy/          # Copybooks - shared data structures (41 files)
├── bms/          # BMS mapsets - CICS screen definitions (21 files)
├── jcl/          # JCL batch job control (38 files)
├── asm/          # Assembler utilities
├── data/
│   ├── ASCII/    # Reference data in text format
│   └── EBCDIC/   # Mainframe-format data files
├── app-authorization-ims-db2-mq/   # Extension: Auth processing with IMS+DB2+MQ
├── app-transaction-type-db2/       # Extension: Transaction type management with DB2
└── app-vsam-mq/                    # Extension: Account extraction with MQ
scripts/          # Build and deployment shell scripts
```

## Code Architecture

### Program Naming Conventions
- `CO*` prefix: Online CICS programs (e.g., `COSGN00C` = signon screen)
- `CB*` prefix: Batch programs (e.g., `CBTRN01C` = transaction batch)
- `*C` suffix: COBOL program files
- `*Y` suffix: Copybooks (e.g., `COCOM01Y` = common communication area)

### Core Online Programs (CICS Transactions)
| Prefix | Function |
|--------|----------|
| COSGN* | Signon/authentication |
| COADM* | Admin menu |
| COBIL* | Bill payment |
| COCRD* | Card management |
| COACT* | Account inquiry |
| COTRN* | Transaction inquiry |
| COUSR* | User management |

### Core Batch Programs
| Program | Purpose |
|---------|---------|
| CBACT01C-04C | Account file maintenance |
| CBCUS01C | Customer maintenance |
| CBTRN01C-03C | Transaction processing (interest, posting) |
| CBEXPORT/CBIMPORT | Data export/import with EBCDIC conversion |

### Data Files (VSAM KSDS)
- `ACCTDAT` - Account master (key: Account ID)
- `CARDDAT` - Card master (key: Card number)
- `CUSTDAT` - Customer master (key: Customer ID)
- `TRANSACT` - Daily transaction log
- `CCXREF` - Card-to-account cross-reference

### CICS Program Pattern
Each online function follows this pattern:
1. BMS mapset defines the screen layout (`app/bms/`)
2. COBOL program handles logic (`app/cbl/CO*.cbl`)
3. Copybooks provide shared data structures (`app/cpy/`)
4. VSAM files store persistent data

## Technology Stack

- **Languages**: COBOL, JCL, BMS, Assembler
- **Transaction Processing**: CICS
- **Data Storage**: VSAM (KSDS with alternate indexes)
- **Optional Extensions**: DB2, IMS DB, IBM MQ
