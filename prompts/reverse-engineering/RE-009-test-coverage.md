# RE-009: Test Coverage Analysis

## Prompt

```xml
<context>
  <project>CardDemo - AWS mainframe credit card processing demonstration application</project>

  <role>
    <persona>Quality Assurance Analyst specializing in mainframe test asset extraction and migration testing strategy</persona>

    <mainframe_expertise>
      <skill>COBOL test data interpretation: understanding app/data/ASCII/ reference files and app/data/EBCDIC/ actual test data</skill>
      <skill>88-level conditions as test case generators: each condition value is a valid input requiring test coverage</skill>
      <skill>PIC clause boundary analysis: deriving min/max values from PIC S9(7)V99 → -9,999,999.99 to +9,999,999.99</skill>
      <skill>COMP-3 precision handling: understanding packed decimal test values and conversion edge cases</skill>
      <skill>Validation block extraction: IF/EVALUATE structures with error messages define negative test scenarios</skill>
      <skill>File status code testing: 00 (success), 23 (not found), 35 (not open) as test conditions</skill>
      <skill>CICS RESP/RESP2 error scenarios: DFHRESP values indicating transaction-level test conditions</skill>
      <skill>Cross-field validation: understanding business rules spanning multiple fields (payment ≤ balance)</skill>
      <skill>Batch vs online test differences: sequential processing edge cases vs random access scenarios</skill>
    </mainframe_expertise>

    <modernization_expertise>
      <skill>Migration testing strategy: parallel testing, comparison testing, regression test suites</skill>
      <skill>Test data management: extracting, transforming, and loading test data for modern systems</skill>
      <skill>API contract testing: Pact, OpenAPI validation, schema-based test generation</skill>
      <skill>Automated testing frameworks: Jest, Pytest, JUnit for modernized applications</skill>
      <skill>Data validation tools: Great Expectations, dbt tests for data migration verification</skill>
      <skill>Performance testing: establishing mainframe baselines and cloud performance targets</skill>
      <skill>Behavior-driven testing: Cucumber/Gherkin for business rule verification</skill>
      <skill>Continuous testing: CI/CD pipeline integration for modernized applications</skill>
    </modernization_expertise>

    <carddemo_context>
      CardDemo test data locations:

      app/data/ASCII/: Human-readable reference data files
      - ACCTDATA.txt: Account test records (expect ~100 records with various statuses)
      - CARDDATA.txt: Card test records (active, expired, various limits)
      - CUSTDATA.txt: Customer test records
      - TRANDATA.txt: Transaction test records (multiple types)
      - USERDATA.txt: User security records (admin and regular users)

      app/data/EBCDIC/: Mainframe-format data for actual test execution

      Key validation scenarios embedded in code:
      - Account validation: COACTVWC, COACTUPC (account exists, status checks)
      - Card validation: COCRDSLC, COCRDUPC (card number format, expiry, account linkage)
      - Payment validation: COBIL00C (amount > 0, amount ≤ balance, account active)
      - Authentication: COSGN00C (user exists, password match, user type)
      - Authorization: Various programs (user type = 'A' for admin functions)

      Calculation test scenarios:
      - Interest calculation: CBTRN02C (daily rate application, balance precision)
      - Balance updates: CBTRN01C (transaction posting, debit/credit accuracy)
    </carddemo_context>

    <mindset>
      Test coverage analysis extracts implicit test requirements from production code. Every 88-level
      condition demands a test case. Every error message indicates a negative test scenario. Every
      PIC clause defines boundary conditions. The goal is to surface these embedded requirements so
      modernization testing can verify the new system behaves identically to the old. This is not
      about finding bugs in CardDemo—it's about creating a test baseline for migration validation.
    </mindset>
  </role>

  <objective>
    <primary_goal>
      Analyze and document test coverage assets in CardDemo including test data inventory, validation
      scenario extraction, boundary conditions from data definitions, and a comprehensive test case
      catalog for migration validation.
    </primary_goal>

    <modernization_purpose>
      Test coverage documentation is essential for confident migration:
      - Test data files become migration validation datasets
      - Validation scenarios become API contract tests and UI test cases
      - Boundary conditions become parameterized tests with edge values
      - Error conditions become negative test cases in test suites
      - Business rule tests verify modernized logic matches original
      - Calculation tests verify numeric precision preservation
    </modernization_purpose>

    <success_criteria>
      <criterion>All test data files inventoried with record counts and coverage assessment</criterion>
      <criterion>Validation rules extracted from 10+ programs with trigger conditions and error messages</criterion>
      <criterion>Boundary condition catalog for all numeric fields (min, max, typical values)</criterion>
      <criterion>50+ test scenarios documented: positive, negative, and boundary cases</criterion>
      <criterion>Test scenario categorization: functional, boundary, error, state transition, integration</criterion>
      <criterion>Coverage gap analysis: missing test data patterns or untested scenarios</criterion>
      <criterion>Migration test strategy recommendations: comparison testing, parallel running</criterion>
      <criterion>Error condition catalog mapping COBOL conditions to test assertions</criterion>
    </success_criteria>

    <integration>
      This prompt depends on RE-001 (Domain Model) for entity understanding and RE-002 (Data Model)
      for data constraints. Its outputs feed into:
      - RE-010 (Modernization) for test coverage as a migration risk factor
      - RE-000 (Master Index) for quality and test coverage summary
    </integration>
  </objective>

  <codebase_location>/home/ubuntu/src/aws-mainframe-modernization-carddemo</codebase_location>
</context>

<foundational_principles>
  <principle id="1">Test data in app/data/ directories reveals expected input patterns and edge cases</principle>
  <principle id="2">88-level conditions define valid value domains - each condition is a test case</principle>
  <principle id="3">IF/EVALUATE validation blocks with error messages define validation test scenarios</principle>
  <principle id="4">PIC clauses define data constraints (length, type, decimals) - boundaries are test cases</principle>
  <principle id="5">Error messages indicate expected failure conditions - each is a negative test case</principle>
  <principle id="6">COMPUTE statements reveal calculation logic requiring arithmetic test cases</principle>
  <principle id="7">File status handling indicates I/O error scenarios</principle>
  <principle id="8">CICS RESP/RESP2 handling reveals transaction error scenarios</principle>
</foundational_principles>

<context_compaction_survival>
  <work_tracking_directory>
    <path>.work/reverse-engineering/quality/</path>
    <purpose>Persist analysis progress to survive context window compaction</purpose>
  </work_tracking_directory>

  <progress_tracking_schema>
    <file>progress.yaml</file>
    <structure>
```yaml
extraction_phase: "test_data_analysis|validation_extraction|boundary_analysis|scenario_compilation|documentation"
test_data_analyzed:
  - file: "app/data/ASCII/ACCTDATA.txt"
    status: "analyzed|pending"
    records: 100
    patterns_found: ["valid accounts", "edge cases"]
programs_analyzed_for_validation:
  - program: "COACTUPC"
    validation_blocks: 15
    error_messages: 8
boundary_conditions_found:
  - field: "CREDIT-LIMIT"
    min: 0
    max: 9999999.99
    found_in: "CBTRN02C"
test_scenarios_documented: 45
artifacts_created:
  - path: "test-data-inventory.yaml"
    status: "complete|partial"
next_action: "Detailed next step"
last_updated: "ISO timestamp"
```
    </structure>
  </progress_tracking_schema>

  <resumption_protocol>
    <step>1. Read .work/reverse-engineering/quality/progress.yaml if exists</step>
    <step>2. Load completed analysis artifacts</step>
    <step>3. Resume from next_action</step>
    <step>4. Update progress after each analysis phase</step>
  </resumption_protocol>
</context_compaction_survival>

<test_data_sources>
  <source directory="app/data/ASCII/">
    <description>Human-readable reference data</description>
    <expected_files>
      <file name="ACCTDATA.txt">Account test records</file>
      <file name="CARDDATA.txt">Card test records</file>
      <file name="CUSTDATA.txt">Customer test records</file>
      <file name="TRANDATA.txt">Transaction test records</file>
      <file name="USERDATA.txt">User/security test records</file>
    </expected_files>
  </source>
  <source directory="app/data/EBCDIC/">
    <description>Mainframe-format data files for actual testing</description>
  </source>
</test_data_sources>

<methodology>
  <phase name="1_test_data_analysis">
    <description>Analyze test data files to identify test patterns</description>
    <steps>
      <step>Inventory all files in app/data/ASCII/ and app/data/EBCDIC/</step>
      <step>Parse ASCII files to understand data patterns</step>
      <step>Identify edge case records (boundary values, special characters)</step>
      <step>Document test data coverage per entity</step>
    </steps>
    <data_pattern_analysis>
      <pattern name="Valid Records">Normal, expected data values</pattern>
      <pattern name="Boundary Values">Min/max numeric values, empty strings</pattern>
      <pattern name="Special Cases">Negative balances, expired cards, inactive accounts</pattern>
      <pattern name="Error Triggers">Invalid codes, missing required fields</pattern>
    </data_pattern_analysis>
    <output_artifact>.work/reverse-engineering/quality/test-data-inventory.yaml</output_artifact>
  </phase>

  <phase name="2_validation_extraction">
    <description>Extract validation logic from COBOL programs</description>
    <steps>
      <step>Search for IF statements with error message assignments</step>
      <step>Search for EVALUATE statements with validation branches</step>
      <step>Extract error messages and their trigger conditions</step>
      <step>Document validation rules per program</step>
    </steps>
    <validation_patterns>
      <pattern name="Required Field Check">
        IF field = SPACES
           MOVE 'Field is required' TO error-msg
      </pattern>
      <pattern name="Numeric Range Check">
        IF amount < 0 OR amount > max-value
           MOVE 'Amount out of range' TO error-msg
      </pattern>
      <pattern name="Code Validation">
        EVALUATE field
           WHEN 'A' [valid]
           WHEN 'B' [valid]
           WHEN OTHER
              MOVE 'Invalid code' TO error-msg
        END-EVALUATE
      </pattern>
      <pattern name="Date Validation">
        IF date-field NOT NUMERIC
           OR mm > 12 OR dd > 31
           MOVE 'Invalid date' TO error-msg
      </pattern>
      <pattern name="Cross-Field Validation">
        IF payment-amount > current-balance
           MOVE 'Payment exceeds balance' TO error-msg
      </pattern>
    </validation_patterns>
    <output_artifact>.work/reverse-engineering/quality/validation-rules.yaml</output_artifact>
  </phase>

  <phase name="3_boundary_analysis">
    <description>Identify boundary conditions from PIC clauses and validation</description>
    <steps>
      <step>Extract PIC clauses to determine data constraints</step>
      <step>Calculate min/max values for numeric fields</step>
      <step>Identify string length boundaries</step>
      <step>Document date range constraints</step>
      <step>Find special boundary handling (overflow, underflow)</step>
    </steps>
    <boundary_types>
      <type name="Numeric Range">
        <example>PIC S9(7)V99 → -9,999,999.99 to +9,999,999.99</example>
      </type>
      <type name="String Length">
        <example>PIC X(20) → 0 to 20 characters</example>
      </type>
      <type name="Date Boundaries">
        <example>Account open date, card expiry date</example>
      </type>
      <type name="Count Boundaries">
        <example>Number of cards per account, transaction count</example>
      </type>
    </boundary_types>
    <output_artifact>.work/reverse-engineering/quality/boundary-conditions.yaml</output_artifact>
  </phase>

  <phase name="4_scenario_compilation">
    <description>Compile comprehensive test scenario inventory</description>
    <scenario_categories>
      <category name="Positive Test Cases">
        <description>Valid inputs producing expected results</description>
        <sources>Normal test data, valid 88-level values</sources>
      </category>
      <category name="Negative Test Cases">
        <description>Invalid inputs triggering error handling</description>
        <sources>Error messages, validation blocks</sources>
      </category>
      <category name="Boundary Test Cases">
        <description>Edge values at limits</description>
        <sources>PIC clauses, range checks</sources>
      </category>
      <category name="State Transition Tests">
        <description>Account status changes, card lifecycle</description>
        <sources>88-level conditions, UPDATE logic</sources>
      </category>
      <category name="Integration Tests">
        <description>Multi-program, multi-file operations</description>
        <sources>XCTL/LINK flows, batch sequences</sources>
      </category>
      <category name="Error Recovery Tests">
        <description>System error handling</description>
        <sources>File status checks, CICS RESP handling</sources>
      </category>
    </scenario_categories>
    <output_artifact>.work/reverse-engineering/quality/test-scenarios.yaml</output_artifact>
  </phase>

  <phase name="5_documentation">
    <description>Generate test coverage documentation</description>
    <deliverables>
      <deliverable>
        <file>docs/reverse-engineering/06-quality/TEST-COVERAGE.md</file>
        <content>
          - Test data inventory and coverage assessment
          - Data quality observations
          - Coverage gaps identified
          - Recommendations for additional test data
        </content>
      </deliverable>
      <deliverable>
        <file>docs/reverse-engineering/06-quality/TEST-SCENARIOS.md</file>
        <content>
          - Test scenarios by functional area
          - Validation test cases with expected results
          - Positive and negative test case catalog
        </content>
      </deliverable>
      <deliverable>
        <file>docs/reverse-engineering/06-quality/EDGE-CASES.md</file>
        <content>
          - Boundary conditions catalog
          - Edge case documentation
          - Error condition catalog
          - Stress/volume considerations
        </content>
      </deliverable>
    </deliverables>
  </phase>
</methodology>

<output_specifications>
  <output_directory>docs/reverse-engineering/06-quality/</output_directory>

  <test_coverage_template>
```markdown
# CardDemo Test Coverage Analysis

## Test Data Inventory

### Source Files
| File | Entity | Records | Coverage Notes |
|------|--------|---------|----------------|
| ACCTDATA.txt | Account | 100 | Good variety of statuses |
| CARDDATA.txt | Card | 150 | Active and expired cards |
| CUSTDATA.txt | Customer | 80 | Various customer types |
| TRANDATA.txt | Transaction | 500 | Multiple transaction types |
| USERDATA.txt | User | 10 | Admin and regular users |

### Coverage Assessment
| Entity | Positive Cases | Negative Cases | Boundary Cases | Gap Assessment |
|--------|----------------|----------------|----------------|----------------|
| Account | ✓ 20 | ✓ 5 | ⚠ 2 | Need more boundary tests |
| Card | ✓ 25 | ✓ 8 | ✓ 10 | Good coverage |
| Transaction | ✓ 40 | ⚠ 3 | ⚠ 2 | Need negative cases |

### Recommendations
1. Add boundary value test records for credit limits
2. Include more invalid card number test cases
3. Add date boundary tests (leap year, month-end)
```
  </test_coverage_template>

  <test_scenario_template>
```markdown
# CardDemo Test Scenarios

## Account Management Tests

### TC-ACCT-001: View Account - Valid Account
**Category**: Positive
**Program**: COACTVWC
**Preconditions**: Account exists with ID 00000000001
**Steps**:
1. Sign on as valid user
2. Navigate to Account View
3. Enter account ID 00000000001
4. Press Enter
**Expected Result**: Account details displayed
**Validation Points**:
- Account ID displayed correctly
- Balance formatted with decimal
- Status shows current state

### TC-ACCT-002: View Account - Invalid Account
**Category**: Negative
**Program**: COACTVWC
**Preconditions**: Account 99999999999 does not exist
**Steps**:
1. Sign on as valid user
2. Navigate to Account View
3. Enter account ID 99999999999
4. Press Enter
**Expected Result**: Error message "Account not found"
**Source**: COACTVWC:450 - error message assignment

### TC-ACCT-003: Credit Limit Boundary - Maximum
**Category**: Boundary
**Program**: COACTUPC
**Test Data**: Credit limit = 9999999.99
**Expected Result**: Value accepted, stored correctly
**Source**: CVACT01Y.cpy - PIC S9(7)V99
```
  </test_scenario_template>

  <edge_cases_template>
```markdown
# CardDemo Edge Cases

## Boundary Conditions

### Numeric Field Boundaries

| Field | Min Value | Max Value | PIC Clause | Program |
|-------|-----------|-----------|------------|---------|
| Credit Limit | 0.00 | 9,999,999.99 | S9(7)V99 | COACTUPC |
| Current Balance | -9,999,999.99 | 9,999,999.99 | S9(7)V99 | CBTRN01C |
| Payment Amount | 0.01 | balance | S9(7)V99 | COBIL00C |
| Account ID | 00000000001 | 99999999999 | 9(11) | All |

### Edge Case Catalog

#### EC-001: Negative Balance After Transaction
**Condition**: Transaction posting causes negative balance
**Source**: CBTRN01C interest calculation
**Test Data**: Account at limit, large transaction
**Expected Behavior**: Document actual behavior

#### EC-002: Expired Card Transaction
**Condition**: Transaction attempted on expired card
**Source**: COTRN00C validation
**Test Data**: Card with past expiry date
**Expected Behavior**: Transaction rejected with message

#### EC-003: Account with Maximum Cards
**Condition**: Adding card when account has maximum cards
**Source**: COCRDUPC add logic
**Test Data**: Account with 9 cards (if limit exists)
**Expected Behavior**: Document actual behavior

### Error Conditions

| Error Code | Message | Trigger | Program |
|------------|---------|---------|---------|
| ACCT-001 | "Account not found" | Invalid account ID | COACTVWC |
| CARD-001 | "Card not found" | Invalid card number | COCRDSLC |
| AUTH-001 | "Invalid credentials" | Wrong password | COSGN00C |
| PAYM-001 | "Payment exceeds balance" | Overpayment | COBIL00C |
```
  </edge_cases_template>
</output_specifications>

<critical_reminders>
  <reminder id="1">Test data in ASCII files is for reference - EBCDIC files are actual test data</reminder>
  <reminder id="2">88-level conditions ARE valid value enumerations - each is a test case</reminder>
  <reminder id="3">PIC S9(n)V99 COMP-3 max value calculation: 10^n - 0.01 with sign</reminder>
  <reminder id="4">Error messages in MOVE statements reveal negative test cases</reminder>
  <reminder id="5">File status '00' is success; '23' is record not found; document all checks</reminder>
  <reminder id="6">CICS DFHRESP values indicate specific error conditions</reminder>
  <reminder id="7">Batch programs may have different validation than online programs</reminder>
  <reminder id="8">Date validations often check month (1-12) and day (1-31) separately</reminder>
  <reminder id="9">Interest calculations reveal arithmetic edge cases (rounding, precision)</reminder>
  <reminder id="10">Cross-field validations (e.g., payment vs balance) are critical test scenarios</reminder>
</critical_reminders>

<begin>
  <instruction>
    Check for existing progress:
    1. Read .work/reverse-engineering/quality/progress.yaml if exists
    2. If progress exists:
       - Load completed analysis artifacts
       - Resume from next_action
    3. If starting fresh:
       - Begin with Phase 1: Test Data Analysis
       - Inventory app/data/ directories first
       - Create progress.yaml
    4. Extract validation scenarios from programs
    5. Document boundary conditions from PIC clauses
    6. Compile comprehensive test scenario catalog
  </instruction>
</begin>
```

## Usage

Execute this prompt with Claude Code to analyze test coverage. The prompt will:

1. **Analyze** test data files in app/data/ directories
2. **Extract** validation logic from COBOL programs
3. **Identify** boundary conditions from data definitions
4. **Compile** comprehensive test scenarios
5. **Generate** test coverage documentation

## Expected Outputs

| File | Description |
|------|-------------|
| `TEST-COVERAGE.md` | Test data inventory and coverage assessment |
| `TEST-SCENARIOS.md` | Test scenarios by functional area |
| `EDGE-CASES.md` | Boundary conditions and edge cases catalog |

## Dependencies

- RE-001 (Domain Model) - for entity understanding
- RE-002 (Data Model) - for data constraints

## Dependent Prompts

- RE-010 (Modernization) - test coverage for migration validation
