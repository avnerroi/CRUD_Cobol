       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRACTICE.
      * Programmed by: Roi

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT
               ASSIGN TO "gradecrud.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  STUDENT.
       01  STUDENT-REC.
      * ONE SPACE BETWEEN FIELDS SO GRADES.TXT IS EASY TO READ
           05  SID      PIC 9(4).
           05  FILLER   PIC X.
           05  NAME     PIC X(30).
           05  FILLER   PIC X.
           05  PRELIM   PIC 9(3).
           05  FILLER   PIC X.
           05  MIDTERM  PIC 9(3).
           05  FILLER   PIC X.
           05  FINALS   PIC 9(3).

       01  STUDENT-LINE PIC X(100).

       WORKING-STORAGE SECTION.

       01  WS-FS            PIC XX.

       01  WS-CHOICE        PIC X(10) VALUE SPACES.
       01  WS-EOF           PIC X VALUE "N".
       01  WS-VALID         PIC X VALUE "N".
       01  WS-OVERFLOW      PIC X VALUE "N".
       01  WS-CONTINUE      PIC X VALUE "Y".
       01  WS-CONFIRM       PIC X VALUE "N".
       01  WS-FOUND         PIC X VALUE "N".
       01  WS-DUP           PIC X VALUE "N".
      * HOW MANY RECORDS ARE LOADED, AND LOOP COUNTER
       01  WS-COUNT         PIC 9(4) VALUE 0.
       01  WS-I             PIC 9(4) VALUE 0.
       01  WS-J             PIC 9(4) VALUE 0.
      * PAGE CONTROL FOR THE VIEW (3 STUDENTS PER PAGE)
       01  WS-PAGE          PIC 9(4) VALUE 0.
       01  WS-PAGES         PIC 9(4) VALUE 0.
       01  WS-ON-PAGE       PIC 9 VALUE 0.
       01  WS-PAGE-OUT      PIC ZZZ9.
       01  WS-PAGES-OUT     PIC ZZZ9.
       01  WS-PAUSE         PIC X.
       01  WS-PAD           PIC X(35) VALUE SPACES.
      * RECORD NUMBER THE USER PICKS (EDIT / DELETE)
       01  WS-NUM-IN        PIC X(10).
       01  WS-NUM           PIC 9(4) VALUE 0.
       01  WS-NUM-OUT       PIC ZZZ9.
      * TEMPORARY INPUT FIELDS
       01  WS-SID           PIC 9(4).
       01  WS-SID-IN        PIC X(10).
       01  WS-SID-TXT       PIC X(4).
       01  WS-NAME          PIC X(30).
       01  WS-PRELIM        PIC 9(3).
       01  WS-MIDTERM       PIC 9(3).
       01  WS-FINALS        PIC 9(3).
       01  WS-GRADE         PIC 9(3).
       01  WS-LABEL         PIC X(7).
       01  WS-STEP          PIC 9 VALUE 1.
       01  WS-GRADE-IN      PIC X(10).
       01  WS-LEN           PIC 9(2).
       01  WS-BAD           PIC 9(4) VALUE 0.
      * AVERAGE AND RESULT (COMPUTED, NOT STORED IN THE FILE)
       01  WS-AVG           PIC 9(3)V99.
       01  WS-AVG-OUT       PIC ZZ9.99.
       01  WS-RESULT        PIC X(6).
      * EDITED FIELDS FOR DISPLAY (NO LEADING ZEROS)
       01  WS-P-OUT         PIC ZZ9.
       01  WS-M-OUT         PIC ZZ9.
       01  WS-F-OUT         PIC ZZ9.
      * SEARCH FIELDS
       01  WS-SEARCH        PIC X(30).
       01  WS-SEARCH-LEN    PIC 9(2).
       01  WS-UPPER-NAME    PIC X(30).
       01  WS-NAME-UP       PIC X(30).
       01  WS-TALLY         PIC 9(4).
       01  WS-TALLY2        PIC 9(4).
      * TABLE HOLDING ALL RECORDS IN MEMORY
       01  WS-TABLE.
           05  WS-ENTRY OCCURS 1000 TIMES.
               10  T-SID     PIC 9(4).
               10  T-NAME    PIC X(30).
               10  T-PRELIM  PIC 9(3).
               10  T-MIDTERM PIC 9(3).
               10  T-FINALS  PIC 9(3).

       PROCEDURE DIVISION.

      * MAIN MENU - REPEATS UNTIL THE USER CHOOSES 6
       MAIN-PARA.
           PERFORM UNTIL WS-CHOICE = "6"
               DISPLAY " "
               DISPLAY "===== STUDENT RECORDS ====="
               DISPLAY "1. ADD RECORD"
               DISPLAY "2. VIEW RECORD"
               DISPLAY "3. EDIT RECORD"
               DISPLAY "4. DELETE RECORD"
               DISPLAY "5. SEARCH RECORD"
               DISPLAY "6. EXIT"
               DISPLAY "ENTER YOUR CHOICE: "
               ACCEPT WS-CHOICE
               INSPECT WS-CHOICE REPLACING ALL X"0D" BY SPACE
               EVALUATE WS-CHOICE
                   WHEN "1"
                       PERFORM ADD-RECORD
                   WHEN "2"
                       PERFORM VIEW-RECORD
                   WHEN "3"
                       PERFORM EDIT-RECORD
                   WHEN "4"
                       PERFORM DELETE-RECORD
                   WHEN "5"
                       PERFORM SEARCH-RECORD
                   WHEN "6"
                       DISPLAY "GOODBYE!"
                   WHEN OTHER
                       DISPLAY "INVALID CHOICE. TRY AGAIN."
               END-EVALUATE
           END-PERFORM
           STOP RUN.

      * ---------------- ADD ----------------
      * LOAD THE EXISTING RECORDS FIRST (TO CHECK FOR A DUPLICATE ID
      * AND A DUPLICATE NAME), THEN OPEN EXTEND = ADD NEW RECORDS AT
      * THE END OF THE FILE
       ADD-RECORD.
           PERFORM LOAD-FILE
           OPEN EXTEND STUDENT
           IF WS-FS(1:1) NOT = "0"
               DISPLAY "ERROR: CANNOT OPEN FILE. STATUS " WS-FS
           ELSE
               MOVE "Y" TO WS-CONTINUE
               PERFORM UNTIL WS-CONTINUE NOT = "Y"
                             AND WS-CONTINUE NOT = "y"
                   PERFORM GET-SID
                   PERFORM GET-NAME
                   PERFORM GET-ALL-GRADES
                   PERFORM CALC-RESULT
                   MOVE SPACES TO STUDENT-REC
                   MOVE WS-SID TO SID
                   MOVE WS-NAME TO NAME
                   MOVE WS-PRELIM TO PRELIM
                   MOVE WS-MIDTERM TO MIDTERM
                   MOVE WS-FINALS TO FINALS
                   WRITE STUDENT-REC
      *            KEEP THE TABLE UP TO DATE FOR THE DUPLICATE CHECKS
                   IF WS-COUNT < 1000
                       ADD 1 TO WS-COUNT
                       MOVE WS-SID TO T-SID(WS-COUNT)
                       MOVE WS-NAME TO T-NAME(WS-COUNT)
                       MOVE WS-PRELIM TO T-PRELIM(WS-COUNT)
                       MOVE WS-MIDTERM TO T-MIDTERM(WS-COUNT)
                       MOVE WS-FINALS TO T-FINALS(WS-COUNT)
                   END-IF
                   MOVE WS-PRELIM TO WS-P-OUT
                   MOVE WS-MIDTERM TO WS-M-OUT
                   MOVE WS-FINALS TO WS-F-OUT
                   MOVE WS-AVG TO WS-AVG-OUT
                   DISPLAY " "
                   DISPLAY "STUDENT ID:   " WS-SID
                   DISPLAY "STUDENT NAME: " WS-NAME
                   DISPLAY "PRELIM:       " WS-P-OUT
                   DISPLAY "MIDTERM:      " WS-M-OUT
                   DISPLAY "FINALS:       " WS-F-OUT
                   DISPLAY "AVERAGE:      " WS-AVG-OUT
                   DISPLAY "RESULT:       " WS-RESULT
                   DISPLAY "RECORD ADDED."
                   DISPLAY "ADD ANOTHER STUDENT? (Y/N): "
                   ACCEPT WS-CONTINUE
               END-PERFORM
               CLOSE STUDENT
           END-IF.

      * ---------------- VIEW ----------------
       VIEW-RECORD.
           PERFORM LOAD-FILE
           IF WS-COUNT = 0
               DISPLAY "NO RECORDS FOUND."
           ELSE
               PERFORM VIEW-PAGES
           END-IF.


       VIEW-PAGES.
           COMPUTE WS-PAGES = (WS-COUNT + 2) / 3
           MOVE 1 TO WS-PAGE
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-PAGE > WS-PAGES
               PERFORM SHOW-REPORT-HEADER
               PERFORM SHOW-HEADER
               MOVE 0 TO WS-ON-PAGE
               PERFORM UNTIL WS-ON-PAGE = 3 OR WS-I > WS-COUNT
                   PERFORM SHOW-ONE
                   ADD 1 TO WS-I
                   ADD 1 TO WS-ON-PAGE
               END-PERFORM
               MOVE WS-PAGE TO WS-PAGE-OUT
               MOVE WS-PAGES TO WS-PAGES-OUT
               DISPLAY " "
               DISPLAY "PAGE " FUNCTION TRIM(WS-PAGE-OUT)
                   " OF " FUNCTION TRIM(WS-PAGES-OUT)
               IF WS-PAGE < WS-PAGES
                   DISPLAY "PRESS ENTER FOR THE NEXT PAGE..."
                   ACCEPT WS-PAUSE
      *            CLEAR THE SCREEN BEFORE SHOWING THE NEXT PAGE
                   DISPLAY SPACE WITH BLANK SCREEN
               END-IF
               ADD 1 TO WS-PAGE
           END-PERFORM
           IF WS-OVERFLOW = "Y"
               DISPLAY "WARNING: ONLY THE FIRST 1000 RECORDS SHOWN."
           END-IF.


       SHOW-REPORT-HEADER.
           DISPLAY " "
           DISPLAY WS-PAD(1:21)
               "POLYTECHNIC UNIVERSITY OF THE PHILIPPINES"
           DISPLAY WS-PAD(1:35) "TAGUIG CAMPUS"
           DISPLAY " "
           DISPLAY " "
           DISPLAY WS-PAD(1:30) "LIST OF STUDENT GRADES"
           DISPLAY " "
           DISPLAY " ".

      * ---------------- EDIT ----------------

       EDIT-RECORD.
           PERFORM LOAD-FILE
           IF WS-COUNT = 0
               DISPLAY "NO RECORDS TO EDIT."
           ELSE
               IF WS-OVERFLOW = "Y"
                   DISPLAY "TOO MANY RECORDS TO EDIT SAFELY."
               ELSE
                   PERFORM SHOW-ALL
                   PERFORM ASK-RECORD-NUMBER
                   IF WS-VALID = "Y"
                       PERFORM EDIT-SELECTED
                   END-IF
               END-IF
           END-IF.

       EDIT-SELECTED.
           DISPLAY "EDITING: " T-SID(WS-NUM) " " T-NAME(WS-NUM)
           DISPLAY "ENTER NEW NAME (LEAVE BLANK TO KEEP): "
           MOVE SPACES TO WS-NAME
           ACCEPT WS-NAME
           INSPECT WS-NAME REPLACING ALL X"0D" BY SPACE
           IF WS-NAME NOT = SPACES
               MOVE WS-NAME TO T-NAME(WS-NUM)
           END-IF
           PERFORM GET-ALL-GRADES
           MOVE WS-PRELIM TO T-PRELIM(WS-NUM)
           MOVE WS-MIDTERM TO T-MIDTERM(WS-NUM)
           MOVE WS-FINALS TO T-FINALS(WS-NUM)
           PERFORM SAVE-FILE
           DISPLAY "RECORD UPDATED.".

      * ---------------- DELETE ----------------
      * LOAD ALL RECORDS, REMOVE ONE FROM THE TABLE, REWRITE THE FILE
       DELETE-RECORD.
           PERFORM LOAD-FILE
           IF WS-COUNT = 0
               DISPLAY "NO RECORDS TO DELETE."
           ELSE
               IF WS-OVERFLOW = "Y"
                   DISPLAY "TOO MANY RECORDS TO DELETE SAFELY."
               ELSE
                   PERFORM SHOW-ALL
                   PERFORM ASK-RECORD-NUMBER
                   IF WS-VALID = "Y"
                       PERFORM DELETE-SELECTED
                   END-IF
               END-IF
           END-IF.

       DELETE-SELECTED.
           DISPLAY "DELETE: " T-SID(WS-NUM) " " T-NAME(WS-NUM)
           DISPLAY "ARE YOU SURE? (Y/N): "
           ACCEPT WS-CONFIRM
           IF WS-CONFIRM = "Y" OR WS-CONFIRM = "y"
      *        SHIFT EVERY RECORD AFTER THE DELETED ONE UP BY ONE
               PERFORM VARYING WS-I FROM WS-NUM BY 1
                       UNTIL WS-I >= WS-COUNT
                   MOVE WS-ENTRY(WS-I + 1) TO WS-ENTRY(WS-I)
               END-PERFORM
               SUBTRACT 1 FROM WS-COUNT
               PERFORM SAVE-FILE
               DISPLAY "RECORD DELETED."
           ELSE
               DISPLAY "DELETE CANCELLED."
           END-IF.

      * ---------------- SEARCH ----------------

       SEARCH-RECORD.
           PERFORM LOAD-FILE
           IF WS-COUNT = 0
               DISPLAY "NO RECORDS FOUND."
           ELSE
               DISPLAY "ENTER NAME OR ID TO SEARCH: "
               MOVE SPACES TO WS-SEARCH
               ACCEPT WS-SEARCH
               INSPECT WS-SEARCH REPLACING ALL X"0D" BY SPACE
               IF WS-SEARCH = SPACES
                   DISPLAY "SEARCH CANNOT BE EMPTY."
               ELSE
                   PERFORM SEARCH-TABLE
               END-IF
           END-IF.

       SEARCH-TABLE.
      *    CONVERT THE SEARCH TEXT TO UPPERCASE
           INSPECT WS-SEARCH CONVERTING
               "abcdefghijklmnopqrstuvwxyz"
               TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      *    FIND THE LENGTH OF THE TEXT (IGNORING TRAILING SPACES)
           MOVE 30 TO WS-SEARCH-LEN
           PERFORM UNTIL WS-SEARCH-LEN = 1
                   OR WS-SEARCH(WS-SEARCH-LEN:1) NOT = SPACE
               SUBTRACT 1 FROM WS-SEARCH-LEN
           END-PERFORM
           MOVE "N" TO WS-FOUND
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-COUNT
               MOVE T-NAME(WS-I) TO WS-UPPER-NAME
               INSPECT WS-UPPER-NAME CONVERTING
                   "abcdefghijklmnopqrstuvwxyz"
                   TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
               MOVE 0 TO WS-TALLY
               INSPECT WS-UPPER-NAME TALLYING WS-TALLY
                   FOR ALL WS-SEARCH(1:WS-SEARCH-LEN)
               MOVE T-SID(WS-I) TO WS-SID-TXT
               MOVE 0 TO WS-TALLY2
               INSPECT WS-SID-TXT TALLYING WS-TALLY2
                   FOR ALL WS-SEARCH(1:WS-SEARCH-LEN)
               IF WS-TALLY > 0 OR WS-TALLY2 > 0
                   IF WS-FOUND = "N"
                       PERFORM SHOW-HEADER
                       MOVE "Y" TO WS-FOUND
                   END-IF
                   PERFORM SHOW-ONE
               END-IF
           END-PERFORM
           IF WS-FOUND = "N"
               DISPLAY "NO MATCHING RECORD FOUND."
           END-IF.

      * ---------------- HELPER PARAGRAPHS ----------------

       LOAD-FILE.
           MOVE 0 TO WS-COUNT
           MOVE "N" TO WS-EOF
           MOVE "N" TO WS-OVERFLOW
           MOVE 0 TO WS-BAD
           OPEN INPUT STUDENT
           IF WS-FS(1:1) = "0"
               PERFORM UNTIL WS-EOF = "Y"
                   READ STUDENT
                       AT END
                           MOVE "Y" TO WS-EOF
                       NOT AT END
                           PERFORM STORE-RECORD
                   END-READ
               END-PERFORM
               CLOSE STUDENT
               IF WS-BAD > 0
                   DISPLAY "WARNING: " WS-BAD
                       " UNREADABLE LINE(S) IGNORED."
               END-IF
           END-IF.

       STORE-RECORD.
           INSPECT STUDENT-LINE REPLACING ALL X"0D" BY SPACE
      *    SKIP EMPTY LINES
           IF STUDENT-LINE NOT = SPACES
               PERFORM CHECK-LINE
           END-IF.

      * A LINE IS GOOD ONLY IF THE ID IS 4 DIGITS, THE NAME IS NOT
      * BLANK AND ALL THREE GRADES ARE NUMBERS FROM 50 TO 100
       CHECK-LINE.
           MOVE "N" TO WS-VALID
           IF SID IS NUMERIC AND NAME NOT = SPACES
               IF PRELIM IS NUMERIC AND MIDTERM IS NUMERIC
                       AND FINALS IS NUMERIC
                   IF PRELIM >= 50 AND PRELIM <= 100
                      AND MIDTERM >= 50 AND MIDTERM <= 100
                      AND FINALS >= 50 AND FINALS <= 100
                       MOVE "Y" TO WS-VALID
                   END-IF
               END-IF
           END-IF
           IF WS-VALID = "N"
               ADD 1 TO WS-BAD
           ELSE
               IF WS-COUNT < 1000
                   ADD 1 TO WS-COUNT
                   MOVE SID TO T-SID(WS-COUNT)
                   MOVE NAME TO T-NAME(WS-COUNT)
                   MOVE PRELIM TO T-PRELIM(WS-COUNT)
                   MOVE MIDTERM TO T-MIDTERM(WS-COUNT)
                   MOVE FINALS TO T-FINALS(WS-COUNT)
               ELSE
                   MOVE "Y" TO WS-OVERFLOW
               END-IF
           END-IF.

      * WRITE THE WHOLE TABLE BACK TO GRADES.TXT (OVERWRITES THE FILE)
       SAVE-FILE.
           OPEN OUTPUT STUDENT
           IF WS-FS(1:1) NOT = "0"
               DISPLAY "ERROR: CANNOT SAVE FILE. STATUS " WS-FS
           ELSE
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-COUNT
                   MOVE SPACES TO STUDENT-REC
                   MOVE T-SID(WS-I) TO SID
                   MOVE T-NAME(WS-I) TO NAME
                   MOVE T-PRELIM(WS-I) TO PRELIM
                   MOVE T-MIDTERM(WS-I) TO MIDTERM
                   MOVE T-FINALS(WS-I) TO FINALS
                   WRITE STUDENT-REC
               END-PERFORM
               CLOSE STUDENT
           END-IF.

      * AVERAGE = (PRELIM + MIDTERM + FINALS) / 3
      * PASSED IF THE AVERAGE IS 75 OR HIGHER, OTHERWISE FAILED
       CALC-RESULT.
           COMPUTE WS-AVG ROUNDED =
               (WS-PRELIM + WS-MIDTERM + WS-FINALS) / 3
           IF WS-AVG >= 75
               MOVE "PASSED" TO WS-RESULT
           ELSE
               MOVE "FAILED" TO WS-RESULT
           END-IF.

       SHOW-HEADER.
           DISPLAY "  NO  ID    NAME                           "
               "PRELIM  MIDTERM  FINALS  AVERAGE  RESULT".

       SHOW-ONE.
           MOVE WS-I TO WS-NUM-OUT
           MOVE T-PRELIM(WS-I) TO WS-PRELIM
           MOVE T-MIDTERM(WS-I) TO WS-MIDTERM
           MOVE T-FINALS(WS-I) TO WS-FINALS
           PERFORM CALC-RESULT
           MOVE WS-PRELIM TO WS-P-OUT
           MOVE WS-MIDTERM TO WS-M-OUT
           MOVE WS-FINALS TO WS-F-OUT
           MOVE WS-AVG TO WS-AVG-OUT
           DISPLAY WS-NUM-OUT "  " T-SID(WS-I) "  " T-NAME(WS-I)
               "    " WS-P-OUT
               "      " WS-M-OUT
               "     " WS-F-OUT
               "   " WS-AVG-OUT
               "  " WS-RESULT.

       SHOW-ALL.
           PERFORM SHOW-HEADER
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-COUNT
               PERFORM SHOW-ONE
           END-PERFORM
           IF WS-OVERFLOW = "Y"
               DISPLAY "WARNING: ONLY THE FIRST 1000 RECORDS SHOWN."
           END-IF.

      * ASK FOR A RECORD NUMBER AND CHECK IT IS BETWEEN 1 AND WS-COUNT
       ASK-RECORD-NUMBER.
           MOVE "N" TO WS-VALID
           DISPLAY "ENTER RECORD NUMBER: "
           MOVE SPACES TO WS-NUM-IN
           ACCEPT WS-NUM-IN
           INSPECT WS-NUM-IN REPLACING ALL X"0D" BY SPACE
      *    COUNT THE CHARACTERS TYPED (UP TO THE FIRST SPACE)
           MOVE 0 TO WS-LEN
           INSPECT WS-NUM-IN TALLYING WS-LEN
               FOR CHARACTERS BEFORE INITIAL SPACE
           IF WS-LEN >= 1 AND WS-LEN <= 4
               IF WS-NUM-IN(1:WS-LEN) IS NUMERIC
                   MOVE WS-NUM-IN(1:WS-LEN) TO WS-NUM
                   IF WS-NUM >= 1 AND WS-NUM <= WS-COUNT
                       MOVE "Y" TO WS-VALID
                   END-IF
               END-IF
           END-IF
           IF WS-VALID = "N"
               DISPLAY "INVALID RECORD NUMBER."
           END-IF.

      * ASK FOR THE STUDENT ID - MUST BE EXACTLY 4 DIGITS AND NOT
      * ALREADY USED BY ANOTHER STUDENT. REPEATS UNTIL VALID.
       GET-SID.
           MOVE "N" TO WS-VALID
           PERFORM UNTIL WS-VALID = "Y"
               DISPLAY "ENTER STUDENT ID: "
               MOVE SPACES TO WS-SID-IN
               ACCEPT WS-SID-IN
               INSPECT WS-SID-IN REPLACING ALL X"0D" BY SPACE
      *        COUNT THE CHARACTERS TYPED (UP TO THE FIRST SPACE)
               MOVE 0 TO WS-LEN
               INSPECT WS-SID-IN TALLYING WS-LEN
                   FOR CHARACTERS BEFORE INITIAL SPACE
               IF WS-LEN = 4 AND WS-SID-IN(1:4) IS NUMERIC
                   MOVE WS-SID-IN(1:4) TO WS-SID
                   PERFORM CHECK-DUPLICATE
                   IF WS-DUP = "Y"
                       DISPLAY "THAT ID IS ALREADY USED."
                   ELSE
                       MOVE "Y" TO WS-VALID
                   END-IF
               ELSE
                   DISPLAY "INVALID ID. ENTER EXACTLY 4 DIGITS."
               END-IF
           END-PERFORM.

      * LOOK FOR WS-SID IN THE TABLE
       CHECK-DUPLICATE.
           MOVE "N" TO WS-DUP
           PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > WS-COUNT OR WS-DUP = "Y"
               IF T-SID(WS-J) = WS-SID
                   MOVE "Y" TO WS-DUP
               END-IF
           END-PERFORM.

      * ASK FOR A NAME - REPEATS UNTIL IT IS NOT EMPTY AND NOT
      * ALREADY USED BY ANOTHER STUDENT
       GET-NAME.
           MOVE "N" TO WS-VALID
           PERFORM UNTIL WS-VALID = "Y"
               MOVE SPACES TO WS-NAME
               DISPLAY "ENTER YOUR NAME: "
               ACCEPT WS-NAME
               INSPECT WS-NAME REPLACING ALL X"0D" BY SPACE
               IF WS-NAME = SPACES
                   DISPLAY "NAME CANNOT BE EMPTY."
               ELSE
                   PERFORM CHECK-DUP-NAME
                   IF WS-DUP = "Y"
                       DISPLAY "THAT NAME IS ALREADY USED."
                   ELSE
                       MOVE "Y" TO WS-VALID
                   END-IF
               END-IF
           END-PERFORM.

      * LOOK FOR WS-NAME IN THE TABLE (IGNORING UPPER/LOWER CASE)
       CHECK-DUP-NAME.
           MOVE "N" TO WS-DUP
           MOVE WS-NAME TO WS-NAME-UP
           INSPECT WS-NAME-UP CONVERTING
               "abcdefghijklmnopqrstuvwxyz"
               TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > WS-COUNT OR WS-DUP = "Y"
               MOVE T-NAME(WS-J) TO WS-UPPER-NAME
               INSPECT WS-UPPER-NAME CONVERTING
                   "abcdefghijklmnopqrstuvwxyz"
                   TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
               IF WS-UPPER-NAME = WS-NAME-UP
                   MOVE "Y" TO WS-DUP
               END-IF
           END-PERFORM.

      * ASK FOR PRELIM, MIDTERM AND FINALS ONE AFTER THE OTHER.
      * IF A GRADE IS INVALID (BELOW 50 OR ABOVE 100), ONLY THAT
      * GRADE IS ASKED AGAIN. THE ONES ALREADY ACCEPTED ARE KEPT.
       GET-ALL-GRADES.
           MOVE 1 TO WS-STEP
           PERFORM UNTIL WS-STEP > 3
               EVALUATE WS-STEP
                   WHEN 1
                       MOVE "PRELIM" TO WS-LABEL
                   WHEN 2
                       MOVE "MIDTERM" TO WS-LABEL
                   WHEN 3
                       MOVE "FINALS" TO WS-LABEL
               END-EVALUATE
               PERFORM GET-GRADE
               IF WS-VALID = "Y"
                   EVALUATE WS-STEP
                       WHEN 1
                           MOVE WS-GRADE TO WS-PRELIM
                       WHEN 2
                           MOVE WS-GRADE TO WS-MIDTERM
                       WHEN 3
                           MOVE WS-GRADE TO WS-FINALS
                   END-EVALUATE
                   ADD 1 TO WS-STEP
               END-IF
           END-PERFORM.

      * ASK FOR ONE GRADE (ONE TRY ONLY)
      * WS-VALID = "Y" IF IT IS A WHOLE NUMBER FROM 50 TO 100
       GET-GRADE.
           DISPLAY "ENTER " FUNCTION TRIM(WS-LABEL) " GRADE: "
           MOVE "N" TO WS-VALID
           MOVE SPACES TO WS-GRADE-IN
           ACCEPT WS-GRADE-IN
           INSPECT WS-GRADE-IN REPLACING ALL X"0D" BY SPACE
           PERFORM CHECK-GRADE
           IF WS-VALID = "N"
               DISPLAY "INVALID GRADE. MUST BE 50 TO 100."
           END-IF.

       CHECK-GRADE.
      *    COUNT THE CHARACTERS TYPED (UP TO THE FIRST SPACE)
           MOVE 0 TO WS-LEN
           INSPECT WS-GRADE-IN TALLYING WS-LEN
               FOR CHARACTERS BEFORE INITIAL SPACE
      *    MUST BE 1 TO 3 DIGITS, THEN BETWEEN 50 AND 100
           IF WS-LEN >= 1 AND WS-LEN <= 3
               IF WS-GRADE-IN(1:WS-LEN) IS NUMERIC
                   MOVE WS-GRADE-IN(1:WS-LEN) TO WS-GRADE
                   IF WS-GRADE >= 50 AND WS-GRADE <= 100
                       MOVE "Y" TO WS-VALID
                   END-IF
               END-IF
           END-IF.
