       IDENTIFICATION DIVISION.
       PROGRAM-ID. CS370PROGRAM1.
       AUTHOR. P W ASKEW.
      ******************************************************************
      * This program serves to build on the principles presented in the
      * first COBOL project adding EVALUATE and IF statements to the mix
      * The problem given is a CEO needs us to write a report on the 
      * costs for increasing the salaries and health insurance of two 
      * stores in AL
      * ******
      * INPUT:
      *    The PR2F21-AL.txt file contains the following
      *        1.  Store ID
      *        2.  Employee ID
      *        3.  Employee Position
      *        4.  Employee Last Name
      *        5.  Employee First Name
      *        6.  Employee Middle Initial
      *        7.  Hire Date
      *        8.  Employee Status
      *        9.  Separation Date
      *        10. Starting Yearly Salary
      *        11. Date of Last Pay Increase
      *        12. Current Yearly Salary
      *        13. Number of Dependents
      *        14. Health Plan
      *        15. Health Insurance Cost
      *        16. Dental Plan Cost
      *        17. Dental Insurance Cost
      * *******
      * OUTPUT:
      *    The AL-Employee-Report file contains the following
      *    *************
      *    DETAIL LINE:
      *        1.  Employee ID
      *        2.  Employee Position
      *        3.  Employee Last Name
      *        4.  Employee's Increased Salary
      *        5.  Employee's Increased Health Insurance
      *        6.  Employee's Increased Dental Insurance
      *    **************
      *    STORE TOTALS
      *        1.  Salary Total
      *        2.  Health Insurance Total
      *        3.  Dental Insurance Total
      *    **************
      *    FINAL TOTALS
      *        1.  Salary Total
      *        2.  Health Insurance Total
      *        3.  Dental Insurance Total
      *    *************
      * CALCULATIONS
      *    INCREASE THE SALARY BY 5% 
      *    INCREASE THE HEALTH INSURANCE COST 2.5%
      *    INCREASE THE DENTAL INSURANCE COST 1.5%
      *
      *    ADD EACH EMPLOYEE'S CURRENT SALARY TO A RUNNING TOTAL SALAY
      *    FOR EACH STORE AND AN OVERALL TOTAL
      *
      *    DO THE SAME FOR DENTAL AND HEALTH.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE
               ASSIGN TO 'PR2FA21-AL.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT EMP-REPORT-FILE
               ASSIGN TO PRINTER 'AL-Employee-Report.txt'.

       DATA DIVISION.
       FILE SECTION.

       FD EMPLOYEE-FILE
           RECORD CONTAINS 85 CHARACTERS.

       01  EMPLOYEE-RECORD.
           05  EMP-STORE-ID            PIC A(4).
           05  EMP-ID                  PIC X(5).
           05  EMP-POSITION            PIC A(2).
           05  EMP-LAST-NAME           PIC X(10).
           05  EMP-FIRST-NAME          PIC X(10).
           05  FILLER                  PIC X(11).
           05  FILLER                  PIC X(1).
           05  FILLER                  PIC 9(8).
           05  FILLER                  PIC 9(8).
           05  FILLER                  PIC 9(8).
           05  EMP-CURRENT-SALARY      PIC 999999V99.
           05  EMP-NUM-DEPENDENTS      PIC 99.
           05  EMP-HEALTH-PLAN         PIC A.
           05  EMP-HEAlTH-COST          PIC 999.
           05  EMP-DENTAL-PLAN         PIC A.
           05  EMP-DENTAL-COST         PIC 999.

       FD EMP-REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.

       01  REPORT-RECORD               PIC X(80).

       WORKING-STORAGE SECTION.
       
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                PIC X           VALUE ' '.
               88 NO-MORE-DATA                         VALUE 'N'.
           05  FIRST-RECORD                            VALUE 'Y'.

       01  STORE-TOTAL-FIELDS.
           05  SL-SALARY-TOTAL         PIC S9(7)V99    VALUE +0.
           05  SL-HEALTH-TOTAL         PIC S9(5)V99    VALUE +0.
           05  SL-DENTAL-TOTAL         PIC S9(5)V99    VALUE +0.

       01  OVERALL-TOTAL-FIELDS.
           05  OT-SALARY-TOTAL         PIC S9(8)V99    VALUE +0.
           05  OT-HEALTH-TOTAL         PIC S9(6)V99    VALUE +0.
           05  OT-DENTAL-TOTAL         PIC S9(6)V99    VALUE +0.

       01  TEMP-FIELDS.
           05  NUM-OF-EMPLOYEES        PIC 9999        VALUE 0.
           05  LAST-STORE-ID           PIC XXXX        VALUE SPACES.
           05  TEMP-EMP-SALARY         PIC 999999V99   VALUE 0.
           05  TEMP-EMP-HEALTH         PIC 999V99      VALUE 0.
           05  TEMP-EMP-DENTAL         PIC 999V99      VALUE 0.

       01  CURRENT-DATE.
           05  CD-YEAR                 PIC XXXX.
           05  CD-MONTH                PIC XX.
           05  CD-DAY                  PIC XX.

       01  REPORT-FIELDS.
           05  PROPER-SPACING          PIC S9          VALUE +2.

       01  CONSTANTS.
           05  SALARY-INCREASE         PIC S9V99       VALUE +1.05.
           05  HEALTH-INCREASE         PIC S9V999      VALUE +1.025.
           05  DENTAL-INCREASE         PIC S9V999      VALUE +1.015.

      *********************    OUTPUT AREA     *************************

       01  HEADING-ONE.
           05  H1-DATE.
               10  H1-MONTH        PIC XX.
               10  FILLER          PIC X         VALUE '/'.
               10  H1-DAY          PIC XX.
               10  FILLER          PIC X         VALUE '/'.
               10  H1-YEAR         PIC XXXX.
           05                          PIC X(25)       VALUE SPACES.
           05                          PIC A(13)       VALUE 
                                                       'BENNETT SHOES'.
           05                          PIC A(20)       VALUE SPACES.
           05                          PIC XXX         VALUE 'PWA'.
           
       01  HEADING-TWO.
           05                          PIC X(29)       VALUE SPACES.
           05                          PIC X(23)       VALUE 
                                             'ALABAMA EMPLOYEE REPORT'.
       01  STORE-LABEL-HEADING.
           05                          PIC X(9)       VALUE 
                                                     '  STORE: '.
           05  SLH-STORE-LOCATION       PIC A(10).

       01  HEADING-FOUR.
           05                          PIC X(3)        VALUE SPACES.
           05                          PIC X(3)        VALUE 'EMP'.
           05                          PIC X(7)        VALUE SPACES.
           05                          PIC X(3)        VALUE 'EMP'.
           05                          PIC X(9)        VALUE SPACES.
           05                          PIC X(3)        VALUE 'EMP'.
           05                          PIC X(8)        VALUE SPACES.
           05                          PIC X(9)        VALUE 
                                                           'INCREASED'.
           05                          PIC X(4)        VALUE SPACES.
           05                          PIC X(9)        VALUE 
                                                           'INCREASED'.
           05                          PIC X(4)        VALUE SPACES.
           05                          PIC X(9)        VALUE 
                                                           'INCREASED'.

       01  HEADING-FIVE.
           05                          PIC X(3)        VALUE SPACES.
           05                          PIC X(2)        VALUE 'ID'.
           05                          PIC X(8)        VALUE SPACES.
           05                          PIC X(3)        VALUE 'POS'.
           05                          PIC X(6)        VALUE SPACES.
           05                          PIC X(9)        VALUE 
                                                       'LAST NAME'.
           05                          PIC X(6)        VALUE SPACES.
           05                          PIC X(6)        VALUE 'SALARY'.
           05                          PIC X(7)        VALUE SPACES.
           05                          PIC X(6)        VALUE 'HEALTH'.
           05                          PIC X(7)        VALUE SPACES.
           05                          PIC X(6)        VALUE 'DENTAL'.


       01  DETAIL-LINE.
           05                          PIC X(2)        VALUE SPACES.
           05  DL-EMP-ID               PIC X(5).
           05                          PIC X(2)        VALUE SPACES.
           05  DL-EMP-POS              PIC A(10).
           05                          PIC X(3)        VALUE SPACES.
           05  DL-EMP-LAST-NAME        PIC X(10).
           05                          PIC X(3)        VALUE SPACES.
           05  DL-EMP-INC-SALARY       PIC $ZZZ,ZZ9.99.
           05                          PIC X(4)        VALUE SPACES.
           05  DL-EMP-INC-HEALTH       PIC $Z,ZZ9.99.
           05                          PIC X(4)        VALUE SPACES.
           05  DL-EMP-INC-DENTAL       PIC $Z,ZZ9.99.

       01  STORE-TOTAL-LINE.
           05                          PIC X(19)       VALUE SPACES.
           05                          PIC X(14)       VALUE
                                                   'STORE TOTALS: '.
           05  STORE-SALARY-TOTAL      PIC $Z,ZZZ,ZZ9.99.
           05                          PIC X(3)        VALUE SPACES.
           05  STORE-HEALTH-TOTAL      PIC $ZZ,ZZ9.99.
           05                          PIC X(3)        VALUE SPACES.
           05  STORE-DENTAL-TOTAL      PIC $ZZ,ZZ9.99.


       01  OVERALL-TOTAL-LINE.
           05                          PIC X(18)       VALUE SPACES.
           05                          PIC X(14)       VALUE
                                                   'GRAND TOTALS: '.
           05  OVERALL-SALARY-TOTAL      PIC $ZZ,ZZZ,ZZ9.99.
           05                          PIC X(2)        VALUE SPACES.
           05  OVERALL-HEALTH-TOTAL      PIC $ZZZ,ZZ9.99.
           05                          PIC X(2)        VALUE SPACES.
           05  OVERALL-DENTAL-TOTAL      PIC $ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       
       10-CONTROL-MODULE.
           
           PERFORM 15-HSKPING-ROUTINE
           PERFORM 25-PROCESS-INPUT-FILE
           PERFORM 40-EOF-ROUTINE
           .
       15-HSKPING-ROUTINE.

           OPEN INPUT EMPLOYEE-FILE
               OUTPUT EMP-REPORT-FILE

           ACCEPT CURRENT-DATE FROM DATE YYYYMMDD
           MOVE CD-MONTH TO H1-MONTH
           MOVE CD-DAY TO H1-DAY
           MOVE CD-YEAR TO H1-YEAR

           
           PERFORM 20-MAIN-HEADER-ROUTINE
           .

       20-MAIN-HEADER-ROUTINE.

           WRITE REPORT-RECORD FROM HEADING-ONE
               AFTER ADVANCING PROPER-SPACING

           MOVE 2 TO PROPER-SPACING

           WRITE REPORT-RECORD FROM HEADING-TWO
               AFTER ADVANCING PROPER-SPACING
               
           .
       
       25-PROCESS-INPUT-FILE.
           PERFORM UNTIL NO-MORE-DATA
               READ EMPLOYEE-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END 
                       PERFORM 30-PROCESS-EMPLOYEE-DATA
               END-READ
           END-PERFORM

           
           .
       
       30-PROCESS-EMPLOYEE-DATA.
           

           EVALUATE TRUE
               WHEN FIRST-RECORD IS EQUAL TO 'Y'
                  MOVE 'N' TO FIRST-RECORD
                  MOVE EMP-STORE-ID TO LAST-STORE-ID
                  PERFORM 31-STORE-HEADER-ROUTINE
                  

               WHEN EMP-STORE-ID IS NOT EQUAL TO LAST-STORE-ID
                   PERFORM 32-STORE-CHANGE-ROUTINE
                   MOVE EMP-STORE-ID TO LAST-STORE-ID
                   MOVE 3 TO PROPER-SPACING
                   PERFORM 31-STORE-HEADER-ROUTINE


           END-EVALUATE
           


           MOVE EMP-ID TO DL-EMP-ID

           EVALUATE TRUE
               WHEN EMP-POSITION IS EQUAL TO'SM'
                   MOVE 'MANAGER' TO DL-EMP-POS
               WHEN EMP-POSITION IS EQUAL TO 'SS'
                   MOVE 'SUPERVISOR' TO DL-EMP-POS
               WHEN EMP-POSITION IS EQUAL TO 'OW'
                   MOVE 'OFFICE' TO DL-EMP-POS
               WHEN EMP-POSITION IS EQUAL TO 'SA'
                   MOVE 'SALES' TO DL-EMP-POS
               WHEN EMP-POSITION IS EQUAL TO 'SE'
                   MOVE 'SECURITY' TO DL-EMP-POS
           END-EVALUATE

           MOVE EMP-LAST-NAME TO DL-EMP-LAST-NAME

           
           MULTIPLY EMP-CURRENT-SALARY BY SALARY-INCREASE GIVING
                                           TEMP-EMP-SALARY
           MULTIPLY EMP-HEALTH-COST BY HEALTH-INCREASE GIVING
                                           TEMP-EMP-HEALTH
           MULTIPLY EMP-DENTAL-COST BY DENTAL-INCREASE GIVING
                                           TEMP-EMP-DENTAL
           
           MOVE TEMP-EMP-SALARY TO DL-EMP-INC-SALARY
           ADD TEMP-EMP-SALARY TO SL-SALARY-TOTAL GIVING
                                       SL-SALARY-TOTAL

           MOVE TEMP-EMP-HEALTH TO DL-EMP-INC-HEALTH
           ADD TEMP-EMP-HEALTH TO SL-HEALTH-TOTAL GIVING
                                       SL-HEALTH-TOTAL

           MOVE TEMP-EMP-DENTAL TO DL-EMP-INC-DENTAL
           ADD TEMP-EMP-DENTAL TO SL-DENTAL-TOTAL GIVING
                                       SL-DENTAL-TOTAL





           MOVE DETAIL-LINE TO REPORT-RECORD
           PERFORM 35-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING


           .


       31-STORE-HEADER-ROUTINE.
           IF LAST-STORE-ID IS EQUAL TO 'BHAM'
              MOVE 'BIRMINGHAM' TO SLH-STORE-LOCATION
           END-IF

           IF LAST-STORE-ID IS EQUAL TO 'HUNT'
              MOVE 'HUNTSVILLE' TO SLH-STORE-LOCATION
           END-IF


           WRITE REPORT-RECORD FROM STORE-LABEL-HEADING
               AFTER ADVANCING PROPER-SPACING

           MOVE 2 TO PROPER-SPACING
           WRITE REPORT-RECORD FROM HEADING-FOUR
               AFTER ADVANCING PROPER-SPACING

           MOVE 1 TO PROPER-SPACING
           WRITE REPORT-RECORD FROM HEADING-FIVE
               AFTER ADVANCING PROPER-SPACING

           MOVE 2 TO PROPER-SPACING
       .

       32-STORE-CHANGE-ROUTINE.
           
           MOVE SL-SALARY-TOTAL TO STORE-SALARY-TOTAL
           MOVE SL-DENTAL-TOTAL TO STORE-DENTAL-TOTAL
           MOVE SL-HEALTH-TOTAL TO STORE-HEALTH-TOTAL

           ADD SL-SALARY-TOTAL TO OT-SALARY-TOTAL GIVING OT-SALARY-TOTAL
           ADD SL-DENTAL-TOTAL TO OT-DENTAL-TOTAL GIVING OT-DENTAL-TOTAL
           ADD SL-HEALTH-TOTAL TO OT-HEALTH-TOTAL GIVING OT-HEALTH-TOTAL

           MOVE ZEROS TO SL-SALARY-TOTAL
           MOVE ZEROS TO SL-DENTAL-TOTAL
           MOVE ZEROS TO SL-HEALTH-TOTAL
           
           MOVE 2 TO PROPER-SPACING

           WRITE REPORT-RECORD FROM STORE-TOTAL-LINE
               AFTER ADVANCING PROPER-SPACING
       .

       35-WRITE-A-LINE.
           WRITE REPORT-RECORD
               AFTER ADVANCING PROPER-SPACING
           .

       40-EOF-ROUTINE.
           PERFORM 32-STORE-CHANGE-ROUTINE
           PERFORM 45-TOTAL-SALARY-ROUTINE
           CLOSE EMPLOYEE-FILE
               EMP-REPORT-FILE
           STOP RUN
           .

       45-TOTAL-SALARY-ROUTINE.
           MOVE OT-SALARY-TOTAL TO OVERALL-SALARY-TOTAL
           MOVE OT-DENTAL-TOTAL TO OVERALL-DENTAL-TOTAL
           MOVE OT-HEALTH-TOTAL TO OVERALL-HEALTH-TOTAL

           MOVE 3 TO PROPER-SPACING

           WRITE REPORT-RECORD FROM OVERALL-TOTAL-LINE
               AFTER ADVANCING PROPER-SPACING

           .


