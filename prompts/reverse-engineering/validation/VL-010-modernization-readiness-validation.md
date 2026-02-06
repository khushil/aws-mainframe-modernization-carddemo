# VL-010: Modernization Readiness Validation

## Prompt

```xml
<context>
  <project>CardDemo - AWS mainframe credit card processing demonstration application</project>

  <role>
    <persona>Validation Analyst specializing in modernization readiness assessment verification</persona>

    <validation_expertise>
      <skill>Cross-referencing complexity scores against actual COBOL source metrics</skill>
      <skill>Verifying LOC counts against real file line counts with tolerance analysis</skill>
      <skill>Validating GOTO locations via grep against actual source files</skill>
      <skill>Confirming complexity scoring factors are justified with verifiable evidence</skill>
      <skill>Assessing migration roadmap feasibility against real program groupings</skill>
      <skill>Detecting inflated or fabricated modernization metrics</skill>
      <skill>Verifying technical debt claims against actual code patterns</skill>
      <skill>Confirming risk assessments are grounded in actual code characteristics</skill>
    </validation_expertise>

    <mainframe_expertise>
      <skill>COBOL complexity measurement: LOC counting, GOTO density, nesting depth, cyclomatic analysis</skill>
      <skill>CICS coupling indicators: EXEC CICS command frequency, BMS map references, COMMAREA usage</skill>
      <skill>Data access patterns: READ/WRITE/REWRITE/DELETE counts, file handle analysis</skill>
      <skill>Business logic density: COMPUTE, EVALUATE, IF/ELSE branching ratios</skill>
      <skill>Technical debt identification: GOTO classification (structured vs unstructured), dead code detection</skill>
      <skill>Batch vs online complexity differences and modernization implications</skill>
    </mainframe_expertise>

    <carddemo_context>
      Ground truth inventory (verified from source tree):
      - 31 COBOL programs in app/cbl/ (29 .cbl + 2 .CBL: CBSTM03A.CBL, CBSTM03B.CBL)
      - 30 copybooks in app/cpy/ (29 .cpy + 1 .CPY: COSTM01.CPY)
      - 17 BMS mapsets in app/bms/
      - 38 JCL files in app/jcl/
      - Total LOC across all 31 programs: 20,650
      - Largest program: COACTUPC.cbl at 4,236 lines
      - Smallest program: COBSWAIT.cbl at 41 lines
      - Programs with GOTO usage: 8 (CBSTM03A, CBSTM03B, COACTUPC, COACTVWC, COCRDLIC, COCRDSLC, COCRDUPC, CORPT00C)
      - Card copybook is CVCRD01Y.cpy (NOT CVCAR00Y.cpy — that file does not exist)
      - User security copybook is CSUSR01Y.cpy (NOT COUSR00Y.cpy — that file does not exist)
    </carddemo_context>

    <mindset>Trust nothing. Verify every LOC count, every complexity score, every GOTO location, every migration recommendation against actual source code. Modernization assessments are particularly prone to hallucination because they synthesize many metrics — each metric must be independently verifiable.</mindset>
  </role>

  <objective>
    <primary_goal>
      Validate the RE-010 modernization readiness documentation for correctness, accuracy, completeness, and absence of hallucinations by cross-referencing every metric, score, and recommendation against the actual CardDemo source code.
    </primary_goal>

    <validation_targets>
      <target>docs/reverse-engineering/07-modernization/MODERNIZATION-READINESS.md</target>
      <target>docs/reverse-engineering/07-modernization/MIGRATION-ROADMAP.md</target>
    </validation_targets>

    <success_criteria>
      <criterion>Program count is exactly 31 (NOT 39 — the RE-010 prompt itself contains this error)</criterion>
      <criterion>LOC counts within 5% tolerance of actual file line counts (wc -l)</criterion>
      <criterion>Total LOC approximately 20,650 (within 5% tolerance)</criterion>
      <criterion>All GOTO locations verifiable via grep in actual source files</criterion>
      <criterion>Complexity scores justified with verifiable evidence from source</criterion>
      <criterion>All 31 programs scored individually on the 6-factor framework</criterion>
      <criterion>COACTUPC.cbl scored as highest complexity (4,236 lines, largest program)</criterion>
      <criterion>Migration phases reference real program groupings based on actual dependencies</criterion>
      <criterion>Risk assessments grounded in actual code characteristics, not generic statements</criterion>
      <criterion>Technical debt items traceable to specific file:line locations</criterion>
      <criterion>CVCAR00Y.cpy and COUSR00Y.cpy hallucinations flagged if present</criterion>
      <criterion>Phased roadmap assigns all 31 programs to specific migration phases</criterion>
    </success_criteria>
  </objective>

  <codebase_location>/home/ubuntu/src/aws-mainframe-modernization-carddemo</codebase_location>
</context>

<ground_truth>
  <cobol_programs count="31" total_loc="20650">
    <online_programs count="19">
      <program name="COACTUPC.cbl" loc="4236" goto="yes" notes="Largest program, highest complexity expected"/>
      <program name="COACTVWC.cbl" loc="941" goto="yes"/>
      <program name="COADM01C.cbl" loc="288" goto="no"/>
      <program name="COBIL00C.cbl" loc="572" goto="no"/>
      <program name="COBSWAIT.cbl" loc="41" goto="no" notes="Smallest program, trivial complexity"/>
      <program name="COCRDLIC.cbl" loc="1459" goto="yes"/>
      <program name="COCRDSLC.cbl" loc="887" goto="yes"/>
      <program name="COCRDUPC.cbl" loc="1560" goto="yes"/>
      <program name="COMEN01C.cbl" loc="308" goto="no"/>
      <program name="CORPT00C.cbl" loc="649" goto="yes"/>
      <program name="COSGN00C.cbl" loc="260" goto="no"/>
      <program name="COTRN00C.cbl" loc="699" goto="no"/>
      <program name="COTRN01C.cbl" loc="330" goto="no"/>
      <program name="COTRN02C.cbl" loc="783" goto="no"/>
      <program name="COUSR00C.cbl" loc="695" goto="no"/>
      <program name="COUSR01C.cbl" loc="299" goto="no"/>
      <program name="COUSR02C.cbl" loc="414" goto="no"/>
      <program name="COUSR03C.cbl" loc="359" goto="no"/>
      <program name="CSUTLDTC.cbl" loc="157" goto="no"/>
    </online_programs>
    <batch_programs count="12">
      <program name="CBACT01C.cbl" loc="430" goto="no"/>
      <program name="CBACT02C.cbl" loc="178" goto="no"/>
      <program name="CBACT03C.cbl" loc="178" goto="no"/>
      <program name="CBACT04C.cbl" loc="652" goto="no"/>
      <program name="CBCUS01C.cbl" loc="178" goto="no"/>
      <program name="CBEXPORT.cbl" loc="582" goto="no"/>
      <program name="CBIMPORT.cbl" loc="487" goto="no"/>
      <program name="CBSTM03A.CBL" loc="924" goto="yes"/>
      <program name="CBSTM03B.CBL" loc="230" goto="yes"/>
      <program name="CBTRN01C.cbl" loc="494" goto="no"/>
      <program name="CBTRN02C.cbl" loc="731" goto="no"/>
      <program name="CBTRN03C.cbl" loc="649" goto="no"/>
    </batch_programs>
  </cobol_programs>

  <copybooks count="30">
    COADM02Y.cpy, COCOM01Y.cpy, CODATECN.cpy, COMEN02Y.cpy, COSTM01.CPY, COTTL01Y.cpy,
    CSDAT01Y.cpy, CSLKPCDY.cpy, CSMSG01Y.cpy, CSMSG02Y.cpy, CSSETATY.cpy, CSSTRPFY.cpy,
    CSUSR01Y.cpy, CSUTLDPY.cpy, CSUTLDWY.cpy, CUSTREC.cpy,
    CVACT01Y.cpy, CVACT02Y.cpy, CVACT03Y.cpy, CVCRD01Y.cpy, CVCUS01Y.cpy, CVEXPORT.cpy,
    CVTRA01Y.cpy, CVTRA02Y.cpy, CVTRA03Y.cpy, CVTRA04Y.cpy, CVTRA05Y.cpy,
    CVTRA06Y.cpy, CVTRA07Y.cpy, UNUSED1Y.cpy
  </copybooks>

  <bms_mapsets count="17"/>
  <jcl_files count="38"/>

  <loc_distribution>
    <band name="Very Large (>2000 LOC)" count="1">COACTUPC.cbl (4236)</band>
    <band name="Large (1000-2000 LOC)" count="2">COCRDUPC.cbl (1560), COCRDLIC.cbl (1459)</band>
    <band name="Medium (500-999 LOC)" count="10">COACTVWC (941), CBSTM03A (924), COCRDSLC (887), COTRN02C (783), CBTRN02C (731), COTRN00C (699), COUSR00C (695), CBACT04C (652), CORPT00C (649), CBTRN03C (649)</band>
    <band name="Small (200-499 LOC)" count="13">CBEXPORT (582), COBIL00C (572), CBTRN01C (494), CBIMPORT (487), CBACT01C (430), COUSR02C (414), COUSR03C (359), COTRN01C (330), COMEN01C (308), COUSR01C (299), COADM01C (288), COSGN00C (260), CBSTM03B (230)</band>
    <band name="Tiny (<200 LOC)" count="5">CBACT02C (178), CBACT03C (178), CBCUS01C (178), CSUTLDTC (157), COBSWAIT (41)</band>
  </loc_distribution>

  <goto_programs count="8">
    <description>Only 8 of 31 programs contain GO TO statements. These are verifiable via grep.</description>
    <program>CBSTM03A.CBL</program>
    <program>CBSTM03B.CBL</program>
    <program>COACTUPC.cbl</program>
    <program>COACTVWC.cbl</program>
    <program>COCRDLIC.cbl</program>
    <program>COCRDSLC.cbl</program>
    <program>COCRDUPC.cbl</program>
    <program>CORPT00C.cbl</program>
  </goto_programs>

  <complexity_expectations>
    <note>COACTUPC.cbl should be the highest-scored program: 4,236 LOC, GOTOs present, multi-file data access, complex BMS handling, extensive validation logic</note>
    <note>COCRDUPC.cbl and COCRDLIC.cbl should score high: 1,500+ LOC, GOTOs present, multi-file access</note>
    <note>COBSWAIT.cbl should be lowest complexity: 41 LOC, minimal logic</note>
    <note>Menu programs (COADM01C, COMEN01C) should score low: under 310 LOC, simple navigation</note>
    <note>Batch programs have CICS coupling score of 1 (no CICS) but may have data complexity</note>
  </complexity_expectations>

  <known_hallucinations>
    <hallucination id="H1">CVCAR00Y.cpy does NOT exist — actual card copybook is CVCRD01Y.cpy</hallucination>
    <hallucination id="H2">COUSR00Y.cpy does NOT exist — actual user security copybook is CSUSR01Y.cpy</hallucination>
    <hallucination id="H3">39 programs is WRONG — actual count is 31 programs (the RE-010 prompt itself says 39 in its success criteria, which is incorrect)</hallucination>
    <hallucination id="H4">Any LOC count for COACTUPC.cbl significantly different from 4,236 is fabricated</hallucination>
    <hallucination id="H5">Any claim of more than 8 programs containing GOTO statements is fabricated</hallucination>
  </known_hallucinations>
</ground_truth>

<validation_methodology>
  <phase name="1_load_documentation">
    <description>Read all modernization readiness documentation and catalog every verifiable claim</description>
    <steps>
      <step>Read docs/reverse-engineering/07-modernization/MODERNIZATION-READINESS.md</step>
      <step>Read docs/reverse-engineering/07-modernization/MIGRATION-ROADMAP.md</step>
      <step>Extract all claimed: program counts, LOC counts, complexity scores, GOTO locations, technical debt items, migration phase assignments</step>
      <step>Create claims inventory organized by claim type (count, LOC, score, location, recommendation)</step>
      <step>Save claims inventory to .work/reverse-engineering/validation/vl-010/claims-inventory.yaml</step>
    </steps>
  </phase>

  <phase name="2_program_count_and_loc_verification">
    <description>Verify program count is 31 and all LOC counts are within 5% tolerance</description>
    <steps>
      <step>Confirm exactly 31 programs are listed (NOT 39 — flag immediately if 39 is claimed)</step>
      <step>Run wc -l on every program in app/cbl/ to get actual line counts</step>
      <step>For each program, compare documented LOC against actual: abs(documented - actual) / actual must be less than or equal to 0.05</step>
      <step>Flag any LOC count outside 5% tolerance as a factual error</step>
      <step>Verify total LOC is approximately 20,650 (within 5% tolerance)</step>
      <step>Confirm COACTUPC.cbl is identified as the largest program at approximately 4,236 lines</step>
      <step>Confirm COBSWAIT.cbl is identified as the smallest program at approximately 41 lines</step>
      <step>Record findings in .work/reverse-engineering/validation/vl-010/loc-verification.yaml</step>
    </steps>
  </phase>

  <phase name="3_goto_and_technical_debt_verification">
    <description>Verify all GOTO locations and technical debt claims against actual source</description>
    <steps>
      <step>Run grep for "GO TO" across all programs in app/cbl/ to establish ground truth</step>
      <step>Confirm only 8 programs contain GOTO: CBSTM03A, CBSTM03B, COACTUPC, COACTVWC, COCRDLIC, COCRDSLC, COCRDUPC, CORPT00C</step>
      <step>For each documented GOTO location (file:line), verify the GO TO statement exists at that line</step>
      <step>Verify GOTO classification (structured error exit vs unstructured flow control) is correct</step>
      <step>For each documented hardcoded value, verify it exists at the cited location</step>
      <step>For each documented missing error handling claim, verify by checking file I/O status checks</step>
      <step>For each monolithic paragraph claim, verify paragraph length at cited location</step>
      <step>Record findings in .work/reverse-engineering/validation/vl-010/debt-verification.yaml</step>
    </steps>
  </phase>

  <phase name="4_complexity_score_verification">
    <description>Verify that complexity scores are justified with verifiable evidence</description>
    <steps>
      <step>Confirm all 31 programs have individual scores on the 6-factor framework (Size, CICS, Data, Logic, Flow, Deps)</step>
      <step>Verify Size scores against actual LOC (score 1: less than 500, 2: 500-1000, 3: 1000-2000, 4: 2000-4000, 5: more than 4000)</step>
      <step>Verify CICS coupling scores: batch programs (CB*) should score 1, online programs (CO*) should score 2+ based on EXEC CICS command analysis</step>
      <step>Verify Data Access scores by counting file access statements (READ, WRITE, REWRITE, DELETE) in each program</step>
      <step>Verify Business Logic scores by checking COMPUTE, EVALUATE, and IF statement density</step>
      <step>Verify Control Flow scores against GOTO presence and nesting depth</step>
      <step>Verify External Dependencies scores against LINK/XCTL and copybook usage</step>
      <step>Confirm weighted score calculation is arithmetically correct (weights: Size 15%, CICS 20%, Data 20%, Logic 25%, Flow 10%, Deps 10%)</step>
      <step>Confirm COACTUPC.cbl has the highest complexity score reflecting its 4,236 LOC, GOTO usage, multi-file access</step>
      <step>Record findings in .work/reverse-engineering/validation/vl-010/score-verification.yaml</step>
    </steps>
  </phase>

  <phase name="5_migration_roadmap_verification">
    <description>Verify migration roadmap assigns all 31 programs to phases with valid rationale</description>
    <steps>
      <step>Confirm all 31 programs appear in the phased roadmap (no programs missing, no phantom programs)</step>
      <step>Verify program groupings in each phase make logical sense (e.g., authentication before core services)</step>
      <step>Confirm dependency sequencing is correct — programs that depend on shared data are migrated together or in correct order</step>
      <step>Verify migration approach recommendations (Rehost/Refactor/Rearchitect/Rewrite/Replace) are consistent with complexity scores</step>
      <step>Check that high-complexity programs are not naively assigned to "Refactor" without justification</step>
      <step>Verify risk assessments reference actual code characteristics (specific LOC counts, GOTO presence, coupling metrics)</step>
      <step>Confirm effort estimates (relative) are proportional to documented complexity scores</step>
      <step>Verify Mermaid diagrams (dependency graphs, pie charts) are syntactically valid and data-consistent</step>
      <step>Record findings in .work/reverse-engineering/validation/vl-010/roadmap-verification.yaml</step>
    </steps>
  </phase>

  <phase name="6_report_generation">
    <description>Produce the structured validation report</description>
    <output>docs/reverse-engineering/validation/VL-010-modernization-readiness-report.md</output>
    <format>
      - Verdict: PASS (100) / FAIL (less than 100)
      - Score breakdown by category (5 categories, weighted)
      - Critical findings (wrong program count, fabricated LOC, phantom GOTO locations, unverifiable scores)
      - Major findings (missing programs in scoring, incomplete roadmap coverage, unjustified complexity ratings)
      - Minor findings (formatting issues, diagram problems, rounding discrepancies)
      - Hallucination inventory table
      - LOC accuracy table (documented vs actual with tolerance check)
      - Completeness gaps table (programs missing from scoring or roadmap)
      - Specific remediation recommendations
    </format>
  </phase>
</validation_methodology>

<scoring_rubric>
  <category name="Source Reference Accuracy" weight="35" severity="Critical">
    <description>LOC counts verified within 5% tolerance, GOTO locations confirmed, technical debt locations verifiable</description>
    <scoring>
      100: All LOC counts within 5% tolerance, all GOTO locations verified, all debt items traceable
      80: 90%+ LOC counts within tolerance, no fabricated GOTO locations
      60: 80%+ LOC counts within tolerance, minor location discrepancies
      40: Multiple LOC counts outside tolerance or fabricated locations found
      20: Systematic LOC fabrication or phantom technical debt items
      0: LOC counts are unreliable, GOTO locations fabricated
    </scoring>
  </category>
  <category name="Factual Accuracy" weight="25" severity="Critical">
    <description>Program count correct (31 not 39), complexity scores justified, migration approaches appropriate</description>
    <scoring>
      100: Program count = 31, all scores justified with evidence, approaches well-reasoned
      80: Program count correct, 90%+ scores justified, approaches mostly appropriate
      60: Program count correct, some scores lack evidence, approaches generally reasonable
      40: Program count wrong (39 instead of 31) — automatic cap at 40 for this category
      20: Multiple fundamental errors in counts, scores, or approaches
      0: Pervasive factual inaccuracies throughout
    </scoring>
  </category>
  <category name="Completeness" weight="20" severity="Major">
    <description>All 31 programs scored individually, all programs assigned to migration phases, complete debt inventory</description>
    <scoring>
      100: All 31 programs scored, all assigned to phases, 50+ debt items cataloged
      80: 28+ programs scored, 90%+ assigned to phases, 40+ debt items
      60: 25+ programs scored, 75%+ assigned to phases, 30+ debt items
      40: Fewer than 25 programs scored or major phase assignment gaps
      20: Fewer than 20 programs scored
      0: Severely incomplete assessment
    </scoring>
  </category>
  <category name="Quantitative Accuracy" weight="10" severity="Major">
    <description>Weighted score calculations correct, distribution counts consistent, effort estimates proportional</description>
    <scoring>
      100: All weighted calculations verified correct, distributions match, estimates proportional
      80: Minor arithmetic discrepancies, distributions mostly correct
      60: Some calculation errors but overall trends correct
      40: Systematic calculation errors or inconsistent distributions
      20: Weighted scores do not match documented factor scores
      0: Calculations are unreliable
    </scoring>
  </category>
  <category name="Documentation Quality" weight="10" severity="Minor">
    <description>Mermaid diagrams valid, markdown well-formed, tables consistent, cross-references resolve</description>
    <scoring>
      100: Professional quality, all diagrams render, clear writing, consistent tables
      80: Minor formatting issues, diagrams mostly correct
      60: Some broken diagrams, inconsistent table formatting
      40: Multiple rendering issues, unclear sections
      20: Poor formatting throughout
      0: Unusable documentation
    </scoring>
  </category>
</scoring_rubric>

<output_specification>
  <report_file>docs/reverse-engineering/validation/VL-010-modernization-readiness-report.md</report_file>
  <work_directory>.work/reverse-engineering/validation/vl-010/</work_directory>

  <report_structure>
    # Validation Report: Modernization Readiness Assessment (RE-010)

    ## Verdict: [PASS|FAIL] — Score: [NN]/100

    ## Score Breakdown
    [Weighted scoring table with 5 categories]

    ## Critical Findings
    [Wrong program count (39 vs 31), fabricated LOC counts, phantom GOTO locations, unverifiable complexity scores]

    ## Major Findings
    [Missing programs in scoring table, incomplete roadmap phase assignments, unjustified migration approaches]

    ## Minor Findings
    [Formatting issues, diagram problems, arithmetic rounding]

    ## Hallucination Inventory
    [Table of every hallucinated claim — phantom programs, fabricated LOC, wrong GOTO locations]

    ## LOC Accuracy Table
    [Program | Documented LOC | Actual LOC | Delta | Within 5%? for all 31 programs]

    ## Complexity Score Verification
    [Table verifying each factor score against evidence]

    ## Migration Roadmap Coverage
    [Table: Phase | Programs Assigned | Programs Missing]

    ## Completeness Gaps
    [Table of expected vs actual coverage]

    ## Recommendations
    [Specific remediation actions with priority]

    ## Remediation Manifest
    | ID | Finding | Target File | Location | Current (Wrong) | Required (Correct) | Source Evidence | Remediation Action | RE Prompt |
    |----|---------|-------------|----------|-----------------|--------------------|-----------------|--------------------|-----------|
    | R-001 | [finding] | [doc path] | [section/line] | [wrong] | [correct] | [source:line] | [fix] | [RE-NNN] |
    ### Remediation Instructions
    For each row: (1) Read target at location, (2) Verify wrong value, (3) Replace with correct, (4) Re-validate
    ### Affected RE Prompts
    [List RE prompts needing re-execution]
  </report_structure>
</output_specification>

<foundational_principles>
  <principle id="1">Program count must be exactly 31 — any claim of 39 programs is a known hallucination from the RE-010 prompt's own success criteria</principle>
  <principle id="2">LOC counts are verifiable via wc -l and must be within 5% tolerance — no exceptions for "estimated" or "approximate" counts</principle>
  <principle id="3">GOTO locations must be verifiable via grep — every claimed file:line must contain an actual GO TO statement</principle>
  <principle id="4">Complexity scores must be justified by verifiable source characteristics — not generic descriptions</principle>
  <principle id="5">COACTUPC.cbl at 4,236 lines must be scored as the highest complexity program — any assessment that does not reflect this is suspect</principle>
  <principle id="6">Batch programs (CB*) must have CICS coupling score of 1 since they contain no CICS commands</principle>
  <principle id="7">Only 8 of 31 programs contain GOTO — any claim of more programs with GOTO is fabricated</principle>
  <principle id="8">Migration roadmap must account for all 31 programs — no programs may be omitted from phase assignments</principle>
</foundational_principles>

<critical_reminders>
  <reminder id="1">The RE-010 prompt itself contains the error "All 39 programs scored" — the actual count is 31. The generated documentation may inherit this error. Flag it as a CRITICAL finding.</reminder>
  <reminder id="2">LOC tolerance is 5%, not 10%. For COACTUPC.cbl (4,236 lines), acceptable range is 4,024-4,448. Anything outside this range is a fabrication.</reminder>
  <reminder id="3">Only 8 programs contain GO TO statements. If the documentation claims GOTO in COBIL00C, COSGN00C, COADM01C, or any other non-GOTO program, that is fabricated.</reminder>
  <reminder id="4">CVCAR00Y.cpy does NOT exist. If referenced in modernization readiness docs, flag as hallucination.</reminder>
  <reminder id="5">COUSR00Y.cpy does NOT exist. If referenced in modernization readiness docs, flag as hallucination.</reminder>
  <reminder id="6">Verify weighted score arithmetic: Score = (Size*0.15) + (CICS*0.20) + (Data*0.20) + (Logic*0.25) + (Flow*0.10) + (Deps*0.10). Recalculate and compare.</reminder>
  <reminder id="7">COBSWAIT.cbl is only 41 lines — it should have the lowest complexity score. If scored higher than 1.5, demand justification.</reminder>
  <reminder id="8">Batch programs have NO CICS coupling. If any CB* program is scored above 1 for CICS coupling, that is an error.</reminder>
  <reminder id="9">Complexity distributions (pie charts) must sum to 31 programs. If they sum to 39 or any other number, the visualization is wrong.</reminder>
  <reminder id="10">Risk assessments must cite specific code characteristics (e.g., "COACTUPC has 4,236 LOC and GOTO usage at lines X, Y, Z") — not generic risk language.</reminder>
</critical_reminders>

<context_compaction_survival>
  <work_tracking_directory>
    <path>.work/reverse-engineering/validation/vl-010/</path>
    <purpose>Persist validation progress to survive context window compaction</purpose>
  </work_tracking_directory>

  <progress_tracking_schema>
    <file>progress.yaml</file>
    <structure>
      validation_phase: "load_docs|loc_verification|goto_debt_verification|score_verification|roadmap_verification|reporting"
      current_document: "filename being validated"
      documents_completed: ["list of validated documents"]
      documents_remaining: ["list of remaining documents"]
      loc_checks:
        total_programs: 31
        programs_verified: N
        within_tolerance: N
        outside_tolerance: N
      goto_checks:
        total_goto_programs: 8
        verified: N
        fabricated: N
      score_checks:
        programs_scored: N
        scores_verified: N
        arithmetic_errors: N
      findings:
        critical: N
        major: N
        minor: N
      next_action: "Detailed description of next step"
      last_updated: "ISO timestamp"
    </structure>
  </progress_tracking_schema>

  <resumption_protocol>
    <step>1. Read .work/reverse-engineering/validation/vl-010/progress.yaml if it exists</step>
    <step>2. Load any existing findings from the work directory (loc-verification.yaml, debt-verification.yaml, score-verification.yaml, roadmap-verification.yaml)</step>
    <step>3. Continue from next_action without re-validating completed items</step>
    <step>4. Update progress.yaml after each phase completion</step>
  </resumption_protocol>
</context_compaction_survival>

<begin>
  <instruction>
    Check for existing progress:
    1. Read .work/reverse-engineering/validation/vl-010/progress.yaml if it exists
    2. If progress exists and validation_phase != "complete":
       - Load existing findings
       - Resume from next_action
    3. If no progress or starting fresh:
       - Begin with Phase 1: Load Documentation
       - Create initial progress.yaml
    4. After completing each phase:
       - Update progress.yaml
       - Write intermediate findings immediately
    5. Key verification commands to use:
       - wc -l app/cbl/*.cbl app/cbl/*.CBL — verify LOC counts
       - grep -n "GO TO" app/cbl/PROGRAM.cbl — verify GOTO locations
       - grep -c "EXEC CICS" app/cbl/PROGRAM.cbl — verify CICS coupling
       - grep -c "READ\|WRITE\|REWRITE\|DELETE" app/cbl/PROGRAM.cbl — verify data access
    6. Continue through all phases until validation report is complete
    7. Write final report to docs/reverse-engineering/validation/VL-010-modernization-readiness-report.md
  </instruction>
</begin>
```

## Usage

Execute this prompt with Claude Code to validate the RE-010 modernization readiness documentation. The prompt will:

1. **Load** all modernization readiness and migration roadmap documentation
2. **Verify** program count is exactly 31 (not 39) and LOC counts are within 5% tolerance
3. **Confirm** GOTO locations and technical debt claims against actual source via grep
4. **Validate** complexity scores are justified with verifiable evidence from source code
5. **Check** migration roadmap covers all 31 programs with logically sequenced phases
6. **Report** findings with severity ratings, LOC accuracy tables, and remediation guidance

## Expected Output

| File | Description |
|------|-------------|
| `VL-010-modernization-readiness-report.md` | Structured validation report with verdict, LOC accuracy table, score verification, and findings |

## Prerequisites

- RE-010 must have been executed and output exists in `docs/reverse-engineering/07-modernization/`
- Access to source code in `app/cbl/`, `app/cpy/`, `app/bms/`
- VL-001 through VL-004 validation reports should be available for cross-reference

## Depends On

- VL-001 (Domain Model Validation)
- VL-002 (Data Model Validation)
- VL-003 (Context Model Validation)
- VL-004 (Architecture Validation)

This is a Phase 6 validation prompt, executed in parallel with VL-011.

## Blocks

- VL-000 (Cross-Document Consistency)
