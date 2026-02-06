# CardDemo Codebase Manifest

**Document Version:** 1.0
**Generated:** 2026-02-06
**Purpose:** Canonical source-of-truth for all asset counts. All other documents should reference this manifest for consistent metrics.

---

## Asset Summary

| Asset Type | Core Count | Extension Count | Total | Location |
|------------|-----------|-----------------|-------|----------|
| **COBOL programs** | 31 | 13 | 44 | `app/cbl/` + `app/app-*/cbl/` |
| **Copybooks** | 30 | 11 | 41 | `app/cpy/` + `app/app-*/cpy/` |
| **BMS copybooks** | 17 | 4 | 21 | `app/cpy-bms/` + `app/app-*/cpy-bms/` |
| **BMS mapsets** | 17 | 4 | 21 | `app/bms/` + `app/app-*/bms/` |
| **JCL files** | 38 | 8 | 46 | `app/jcl/` + `app/app-*/jcl/` |
| **Assembler** | 2 | 0 | 2 | `app/asm/` |
| **IMS DBD/PSB** | 0 | 8 | 8 | `app/app-authorization-ims-db2-mq/ims/` |
| **DB2 DDL** | 0 | 6 | 6 | `app/app-*/ddl/` |
| **DB2 DCL** | 0 | 3 | 3 | `app/app-*/dcl/` |
| **DB2 CTL** | 0 | 7 | 7 | `app/app-transaction-type-db2/ctl/` |
| **CSD definitions** | 0 | 3 | 3 | `app/app-*/csd/` |
| **Data files (ASCII)** | 9 | 0 | 9 | `app/data/ASCII/` |
| **Data files (EBCDIC)** | 13 | 1 | 14 | `app/data/EBCDIC/` + `app/app-*/data/EBCDIC/` |

---

## Core COBOL Programs (31 files in `app/cbl/`)

### File Extension Breakdown

| Extension | Count | Files |
|-----------|-------|-------|
| .cbl (lowercase) | 29 | All except below |
| .CBL (uppercase) | 2 | CBSTM03A.CBL, CBSTM03B.CBL |

### Category Breakdown

| Category | Prefix | Count | Programs |
|----------|--------|-------|----------|
| Online CICS | CO* | 17 | COSGN00C, COADM01C, COMEN01C, COACTVWC, COACTUPC, COCRDLIC, COCRDSLC, COCRDUPC, COTRN00C, COTRN01C, COTRN02C, COBIL00C, CORPT00C, COUSR00C, COUSR01C, COUSR02C, COUSR03C |
| Batch | CB* | 12 | CBACT01C, CBACT02C, CBACT03C, CBACT04C, CBCUS01C, CBTRN01C, CBTRN02C, CBTRN03C, CBEXPORT, CBIMPORT, CBSTM03A, CBSTM03B |
| Utility | CS* | 1 | CSUTLDTC |
| Utility | COB* | 1 | COBSWAIT |

---

## Core Copybooks (30 files in `app/cpy/`)

### File Extension Breakdown

| Extension | Count | Files |
|-----------|-------|-------|
| .cpy (lowercase) | 29 | All except below |
| .CPY (uppercase) | 1 | COSTM01.CPY |

### Complete List

COADM02Y, COCOM01Y, CODATECN, COMEN02Y, COSTM01, COTTL01Y, CSDAT01Y, CSLKPCDY, CSMSG01Y, CSMSG02Y, CSSETATY, CSSTRPFY, CSUSR01Y, CSUTLDPY, CSUTLDWY, CUSTREC, CVACT01Y, CVACT02Y, CVACT03Y, CVCRD01Y, CVCUS01Y, CVEXPORT, CVTRA01Y, CVTRA02Y, CVTRA03Y, CVTRA04Y, CVTRA05Y, CVTRA06Y, CVTRA07Y, UNUSED1Y

---

## Core BMS Mapsets (17 files in `app/bms/`)

COACTUP, COACTVW, COADM01, COBIL00, COCRDLI, COCRDSL, COCRDUP, COMEN01, CORPT00, COSGN00, COTRN00, COTRN01, COTRN02, COUSR00, COUSR01, COUSR02, COUSR03

---

## Core BMS Copybooks (17 files in `app/cpy-bms/`)

All .CPY extension. One-to-one match with BMS mapsets above.

---

## Core JCL Files (38 files in `app/jcl/`)

### File Extension Breakdown

| Extension | Count | Files |
|-----------|-------|-------|
| .jcl (lowercase) | 33 | All except below |
| .JCL (uppercase) | 5 | CREASTMT.JCL, FTPJCL.JCL, INTRDRJ1.JCL, INTRDRJ2.JCL, TXT2PDF1.JCL |

### Complete List

ACCTFILE, CARDFILE, CBADMCDJ, CBEXPORT, CBIMPORT, CLOSEFIL, COMBTRAN, CREASTMT, CUSTFILE, DALYREJS, DEFCUST, DEFGDGB, DEFGDGD, DISCGRP, DUSRSECJ, ESDSRRDS, FTPJCL, INTCALC, INTRDRJ1, INTRDRJ2, OPENFIL, POSTTRAN, PRTCATBL, READACCT, READCARD, READCUST, READXREF, REPTFILE, TCATBALF, TRANBKP, TRANCATG, TRANFILE, TRANIDX, TRANREPT, TRANTYPE, TXT2PDF1, WAITSTEP, XREFFILE

---

## Extension Programs (13 files across 3 directories)

| Extension Directory | Programs | Count |
|-------------------|----------|-------|
| app-authorization-ims-db2-mq | COPAUA0C, COPAUS0C, COPAUS1C, COPAUS2C, CBPAUP0C (.cbl); DBUNLDGS, PAUDBLOD, PAUDBUNL (.CBL) | 8 |
| app-transaction-type-db2 | COTRTLIC, COTRTUPC, COBTUPDT (.cbl) | 3 |
| app-vsam-mq | COACCT01, CODATE01 (.cbl) | 2 |

---

## Extension Copybooks (11 files across 2 directories)

| Extension Directory | Copybooks | Count |
|-------------------|-----------|-------|
| app-authorization-ims-db2-mq | CCPAUERY, CCPAURLY, CCPAURQY, CIPAUDTY, CIPAUSMY, IMSFUNCS (.cpy); PADFLPCB, PASFLPCB, PAUTBPCB (.CPY) | 9 |
| app-transaction-type-db2 | CSDB2RPY, CSDB2RWY (.cpy) | 2 |

---

## Extension JCL (8 files across 2 directories)

| Extension Directory | JCL Files | Count |
|-------------------|-----------|-------|
| app-authorization-ims-db2-mq | CBPAUP0J (.jcl); DBPAUTP0 (.jcl); LOADPADB, UNLDGSAM, UNLDPADB (.JCL) | 5 |
| app-transaction-type-db2 | CREADB21, MNTTRDB2, TRANEXTR (.jcl) | 3 |

---

## Data Files

### ASCII Reference Data (9 files in `app/data/ASCII/`)

acctdata.txt, carddata.txt, cardxref.txt, custdata.txt, dailytran.txt, discgrp.txt, tcatbal.txt, trancatg.txt, trantype.txt

### EBCDIC Data Files (13 files in `app/data/EBCDIC/`)

AWS.M2.CARDDEMO.ACCDATA.PS, AWS.M2.CARDDEMO.ACCTDATA.PS, AWS.M2.CARDDEMO.CARDDATA.PS, AWS.M2.CARDDEMO.CARDXREF.PS, AWS.M2.CARDDEMO.CUSTDATA.PS, AWS.M2.CARDDEMO.DALYTRAN.PS, AWS.M2.CARDDEMO.DALYTRAN.PS.INIT, AWS.M2.CARDDEMO.DISCGRP.PS, AWS.M2.CARDDEMO.EXPORT.DATA.PS, AWS.M2.CARDDEMO.TCATBALF.PS, AWS.M2.CARDDEMO.TRANCATG.PS, AWS.M2.CARDDEMO.TRANTYPE.PS, AWS.M2.CARDDEMO.USRSEC.PS

### Extension EBCDIC Data (1 file)

AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat (in `app/app-authorization-ims-db2-mq/data/EBCDIC/`)

---

## CICS Transaction IDs (Core)

| Trans ID | Initial Program | Scope | Description |
|----------|----------------|-------|-------------|
| CC00 | COSGN00C | All users | Sign-on/authentication |
| CA00 | COADM01C | Admin only | Admin menu |
| CM00 | COMEN01C | All authenticated | Main menu + business functions |

> **Note:** All 17 online programs share only 3 TransIDs. Programs are invoked via XCTL within the same transaction context. Extension programs (IMS/DB2/MQ) define additional TransIDs in their respective CSD files.

---

## Assembler Utilities (2 files in `app/asm/`)

COBDATFT.asm, MVSWAIT.asm

---

## Uppercase Extension Note

Several files use uppercase extensions (.CBL, .CPY, .JCL), which is significant on case-sensitive file systems:

| Type | Uppercase Files |
|------|----------------|
| COBOL (.CBL) | CBSTM03A.CBL, CBSTM03B.CBL (core); DBUNLDGS.CBL, PAUDBLOD.CBL, PAUDBUNL.CBL (ext) |
| Copybook (.CPY) | COSTM01.CPY (core); PADFLPCB.CPY, PASFLPCB.CPY, PAUTBPCB.CPY (ext) |
| JCL (.JCL) | CREASTMT.JCL, FTPJCL.JCL, INTRDRJ1.JCL, INTRDRJ2.JCL, TXT2PDF1.JCL (core); LOADPADB.JCL, UNLDGSAM.JCL, UNLDPADB.JCL (ext) |
| BMS copybook (.CPY) | All 17 core + 4 ext (standard convention for BMS-generated copybooks) |

---

## Cross-References

| Document | Relevance |
|----------|-----------|
| [PROGRAM-INVENTORY.md](./PROGRAM-INVENTORY.md) | Full program details with LOC, complexity, access matrix |
| [COPYBOOK-INVENTORY.md](./COPYBOOK-INVENTORY.md) | Copybook field details and dependency map |
| [FILE-INVENTORY.md](./FILE-INVENTORY.md) | VSAM file specifications and GDG bases |
| [TRANSACTION-INVENTORY.md](./TRANSACTION-INVENTORY.md) | TransID mappings and navigation flows |

---

## Document Control

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-02-06 | Remediation RM-AUX | Initial canonical manifest from codebase ground truth |

---

*This document is the single source of truth for codebase asset counts. All other reverse engineering documents should reference this manifest rather than independently counting assets.*
