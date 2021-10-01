       IDENTIFICATION DIVISION.
       PROGRAM-ID. CS370PROGRAM1.
       AUTHOR. P W ASKEW.
      ******************************************************************
      * This program serves to give practive with the basics of COBOL
      * The problem given is a CEO needs us to write a report on the 
      * employees that have not recieved a raise in the last year
      * ******
      * INPUT:
      *    The PR1F21-Knox.txt file contains the following
      *        1.  Store ID
      *        2.  Employee ID
      *        3.  Employee Position
      *        4.  Employee Last Name
      *        5.  Employee First Name
      *        6.  Employee Middle Initial
      *        7.  Hire Date
      *        8.  Employee Status
      *        9.  Separation Date
      *        10.  Starting Yearly Salary
      *        11.  Date of Last Pay Increase
      *        12.  Current Yearly Salary
      * *******
      * OUTPUT:
      *    The SALARY REPORT file contains the following
      *    *************
      *    DETAIL LINE:
      *        1.  Employee ID
      *        2.  Employee Position
      *        3.  Employee First Name
      *        4.  Employee Last Name
      *        5.  Employee Status
      *        6.  Date of Last Pay Increase
      *        7.  Current Salary
      *    **************
      *    FINAL TOTALS
      *        1.  Salary Total
      *    *************
      * CALCULATIONS
      *    ADD EACH EMPLOYEE'S CURRENT SALARY TO A RUNNING TOTAL SALAY
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE
               ASSIGN TO 'PR1FA21-Knox.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT EMP-REPORT-FILE
               ASSIGN TO PRINTER 'Knox-Salary-Report'.

       DATA DIVISION.
       FILE SECTION.

       FD EMPLOYEE-FILE
           RECORD CONTAINS 75 CHARACTERS.

       01  EMPLOYEE-RECORD.
           05  EMP-STORE-ID            PIC A(4).
           05  EMP-ID                  PIC X(5).
           05  EMP-POSITION            PIC A(2).
           05  EMP-LAST-NAME           PIC X(10).
           05  EMP-FIRST-NAME          PIC X(10).
           05  FILLER                  PIC X(11).
           05  EMP-STATUS              PIC X(1).
           05  FILLER                  PIC 9(8).
           05  FILLER                  PIC 9(8).
           05  EMP-LAST-RAISE-DATE     PIC 9(8).
           05  EMP-CURRENT-SALARY      PIC 999999V99.

       FD EMP-REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.

       01  REPORT-RECORD               PIC X(80).

       WORKING-STORAGE SECTION.
       
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                PIC X           VALUE ' '.
               88 NO-MORE-DATA                         VALUE 'N'.

       01  TOTAL-FIELDS.
           05  TF-SALARY-TOTAL         PIC S9(7)V99    VALUE +0.

       01  REPORT-FIELDS.
           05  PROPER-SPACING          PIC S9          VALUE +2.


      *********************    OUTPUT AREA     *************************

       01  HEADING-ONE.
           05  H1-DATE                 PIC 9999/99/99. 
           05                          PIC X(25)       VALUE SPACES.
           05                          PIC A(13)       VALUE 
                                                       'BENNETT SHOES'.
           05                          PIC A(20)       VALUE SPACES.
           05                          PIC XXX         VALUE 'PWA'.
           
       01  HEADING-TWO.
           05                          PIC X(34)       VALUE SPACES.
           05                          PIC X(15)       VALUE 
                                                   'EMPLOYEE REPORT'.
       01  HEADING-THREE.
           05                          PIC X(35)       VALUE SPACES.
           05                          PIC X(13)       VALUE 
                                                       'KNOXVILLE, TN'.
       01  HEADING-FOUR.
           05                          PIC X(3)        VALUE SPACES.
           05                          PIC X(3)        VALUE 'EMP'.
           05                          PIC X(4)        VALUE SPACES.
           05                          PIC X(3)        VALUE 'EMP'.
           05                          PIC X(6)        VALUE SPACES.
           05                          PIC X(3)        VALUE 'EMP'.
           05                          PIC X(9)        VALUE SPACES.
           05                          PIC X(3)        VALUE 'EMP'.
           05                          PIC X(8)        VALUE SPACES.
           05                          PIC X(3)        VALUE 'EMP'.
           05                          PIC X(6)        VALUE SPACES.
           05                          PIC X(4)        VALUE 'LAST'.
           05                          PIC X(7)        VALUE SPACES.
           05                          PIC X(8)        VALUE 'CURRENT'.

       01  HEADING-FIVE.
           05                          PIC X(3)        VALUE SPACES.
           05                          PIC X(2)        VALUE 'ID'.
           05                          PIC X(5)        VALUE SPACES.
           05                          PIC X(3)        VALUE 'POS'.
           05                          PIC X(2)        VALUE SPACES.
           05                          PIC X(10)       VALUE 
                                                       'FIRST NAME'.
           05                          PIC X(3)        VALUE SPACES.
           05                          PIC X(9)        VALUE 
                                                           'LAST NAME'.
           05                          PIC X(3)        VALUE SPACES.
           05                          PIC X(6)        VALUE 'STATUS'.
           05                          PIC X(3)        VALUE SPACES.
           05                          PIC X(8)        VALUE 'INCREASE'.
           05                          PIC X(6)        VALUE SPACES.
           05                          PIC X(6)        VALUE 'SALARY'.


       01  DETAIL-LINE.
           05                          PIC X(2)        VALUE SPACES.
           05  DL-EMP-ID               PIC X(5).
           05                          PIC X(3)        VALUE SPACES.
           05  DL-EMP-POS              PIC A(2).
           05                          PIC X(3)        VALUE SPACES.
           05  DL-EMP-FIRST-NAME       PIC X(10).
           05                          PIC X(3)        VALUE SPACES.
           05  DL-EMP-LAST-NAME        PIC X(10).
           05                          PIC X(4)        VALUE SPACES.
           05  DL-EMP-STATUS           PIC A(1).
           05                          PIC X(5)        VALUE SPACES.
           05  DL-EMP-LAST-RAISE-DATE  PIC 99/99/9999.
           05                          PIC X(3)        VALUE SPACES.
           05  DL-EMP-CURRENT-SALARY   PIC $999,999.99.


       01  TOTAL-LINE.
           05  FILLER                  PIC X(45)         VALUE SPACES.
           05                          PIC X(13)       VALUE 
                                                       'SALARY TOTAL:'.
           05                          PIC X(1)        VALUE SPACES.
           05  TL-SALARY-TOTAL         PIC $9,999,999.99.

       PROCEDURE DIVISION.
       
       10-CONTROL-MODULE.
           
           PERFORM 15-HSKPING-ROUTINE
           PERFORM 25-PROCESS-INPUT-FILE
           PERFORM 40-EOF-ROUTINE
           .
       15-HSKPING-ROUTINE.

           OPEN INPUT EMPLOYEE-FILE
               OUTPUT EMP-REPORT-FILE
           ACCEPT H1-DATE FROM DATE YYYYMMDD
           PERFORM 20-HEADER-ROUTINE
           .

       20-HEADER-ROUTINE.
           WRITE REPORT-RECORD FROM HEADING-ONE
               AFTER ADVANCING PROPER-SPACING

           MOVE 2 TO PROPER-SPACING

           WRITE REPORT-RECORD FROM HEADING-TWO
               AFTER ADVANCING PROPER-SPACING

           WRITE REPORT-RECORD FROM HEADING-THREE
               AFTER ADVANCING PROPER-SPACING

           WRITE REPORT-RECORD FROM HEADING-FOUR
               AFTER ADVANCING PROPER-SPACING

           MOVE 1 TO PROPER-SPACING
           WRITE REPORT-RECORD FROM HEADING-FIVE
               AFTER ADVANCING PROPER-SPACING
               
           MOVE 2 TO PROPER-SPACING
           .
       
       25-PROCESS-INPUT-FILE.
           PERFORM UNTIL NO-MORE-DATA
               READ EMPLOYEE-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END 
                       PERFORM 30-PASS-EMPLOYEE-DATA
               END-READ
           END-PERFORM
           .
       
       30-PASS-EMPLOYEE-DATA.
           
           MOVE EMP-ID TO DL-EMP-ID
           MOVE EMP-POSITION TO DL-EMP-POS
           MOVE EMP-FIRST-NAME TO DL-EMP-FIRST-NAME
           MOVE EMP-LAST-NAME TO DL-EMP-LAST-NAME
           MOVE EMP-STATUS TO DL-EMP-STATUS
           MOVE EMP-LAST-RAISE-DATE TO DL-EMP-LAST-RAISE-DATE
           MOVE EMP-CURRENT-SALARY TO DL-EMP-CURRENT-SALARY

           MOVE DETAIL-LINE TO REPORT-RECORD
           PERFORM 35-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING

           ADD EMP-CURRENT-SALARY TO TF-SALARY-TOTAL

           .
       35-WRITE-A-LINE.
           WRITE REPORT-RECORD
               AFTER ADVANCING PROPER-SPACING
           .

       40-EOF-ROUTINE.
           PERFORM 45-TOTAL-SALARY-ROUTINE
           CLOSE EMPLOYEE-FILE
               EMP-REPORT-FILE
           STOP RUN
           .

       45-TOTAL-SALARY-ROUTINE.
           MOVE TF-SALARY-TOTAL TO TL-SALARY-TOTAL
           MOVE 2 TO PROPER-SPACING

           WRITE REPORT-RECORD FROM TOTAL-LINE
               AFTER ADVANCING PROPER-SPACING

           .


