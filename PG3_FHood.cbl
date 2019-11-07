       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PG3.
       AUTHOR.         Forrest Hood.
      *
      *  This program gives a detailed report on candy inventory in 5
      *  different warehouses and from 4 different vendors.
      *
       ENVIRONMENT DIVISION.
      
       INPUT-OUTPUT SECTION.
      
       FILE-CONTROL.
           SELECT Candy-Inventory
               ASSIGN TO "PR3FA19.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
      
           SELECT Candy-Report-File
               ASSIGN TO PRINTER "Candy Report".

       DATA DIVISION.
       FILE SECTION.

       FD  Candy-Inventory
           RECORD CONTAINS 143 CHARACTERS.

       01  Candy-Record.
           05 CR-Warehouse                 PIC X(4).
           05 CR-Vendor                    PIC X.
           05 CR-CandyID                   PIC X(3).
           05 CR-Data  OCCURS 5 TIMES.
               10 CRD-Name                 PIC X(15).
               10 CRD-Size                 PIC A.
               10 CRD-Type                 PIC AA.
               10 CRD-Stock                PIC S9(4).
               10 CRD-Price                PIC S999v99.

       FD  Candy-Report-File
           RECORD CONTAINS 80 CHARACTERS.

       01  Report-Line                     PIC X(80).

       WORKING-STORAGE SECTION.
    
       01  Flags-N-Switches.
           05 EOF-Flag                     PIC X  VALUE 'N'.
               88 No-More-Data                    VALUE 'Y'.
           05 First-Run                    PIC X  VALUE 'Y'.
           05 Proper-Spacing               PIC 9 .
           05 Invalid-String               PIC X(7) VALUE 'INVALID'.
           05 Bad-String                   PIC X(3) VALUE 'BAD'.
           05 Sub                          PIC 9 .
       
       01  Total-Fields.
           05 TF-Candy-Cost                PIC S9(6)v99  VALUE ZERO.
           05 TF-Candy-Total               PIC S9(7)v99  VALUE ZERO.
           05 TF-Vendor-Total              PIC S9(7)v99  VALUE ZERO.
           05 TF-Warehouse                 PIC S9(8)v99  VALUE ZERO.
           05 TF-Grand-Total               PIC S9(9)v99  VALUE ZERO.

       01  Holds.
           05 Warehouse-Hold               PIC X(4).
           05 Vendor-Hold                  PIC X.
           05 Candy-Hold                   PIC X(3).
           05 C-H                       PIC S9(6)v99  VALUE ZERO.

       01  WS-Current-Date.
           05  WS-Year                     PIC 99.
           05  WS-Month                    PIC 99.
           05  WS-Day                      PIC 99.

      **************        OUTPUT AREA        ********************

       01  Heading-One.
           05                    PIC X(31)  VALUE SPACES.
           05 H1-Title           PIC X(18)  VALUE 'GLENCOE CANDY, LTD'.
           05                    PIC X(16)  VALUE SPACES.

       01  Heading-Two.
           05                    PIC X(7)  VALUE SPACES.
            05  H2-Date.
               10  H2-Month                PIC Z9.
               10                          PIC X    VALUE '/'.
               10  H2-Day                  PIC 99.
               10                          PIC X    VALUE '/'.
               10                          PIC XX   VALUE'20'.
               10  H2-Year                 PIC 99.
           05                    PIC X(15) VALUE SPACES.
           05 H2-Title           PIC X(16) VALUE 'INVENTORY REPORT'.
           05                    PIC X(9)  VALUE SPACES.
           05 H2-Page            PIC X(6)  VALUE 'PAGE: '.
           05 H2-Page-Num        PIC 99    VALUE 00 .

       01  Heading-Three.
           05 H3-WH              PIC X(13) VALUE '  WAREHOUSE: '.
           05 H3-WarehouseID     PIC X(4).

       01  Heading-Four.
           05 H4-V               PIC X(13) VALUE '     VENDOR: '.
           05 H4-Vendor          PIC X(18).

       01  Heading-Five.
           05 H5-C               PIC X(13) VALUE '      CANDY: '.
           05 H5-CandyID         PIC X(3).

       01  Heading-Six.
           05                    PIC X(10) VALUE SPACES.
           05 H6-Name            PIC X(4)  VALUE 'NAME'.
           05                    PIC X(10) VALUE SPACES.
           05 H6-Size            PIC X(4)  VALUE 'SIZE'.
           05                    PIC X(8)  VALUE SPACES.
           05 H6-Type            PIC X(4)  VALUE 'TYPE'.
           05                    PIC X(3)  VALUE SPACES.
           05 H6-Stock           PIC X(8)  VALUE 'IN STOCK'.
           05                    PIC X(4)  VALUE SPACES.
           05 H6-Cost            PIC X(10) VALUE 'TOTAL COST'.

       01  Detail-Line.
           05                    PIC X(5)  VALUE SPACES.
           05 DL-Name            PIC X(13).
           05                    PIC X(4)  VALUE SPACES.
           05 DL-Size            PIC X(10).
           05                    PIC X(5)  VALUE SPACES.
           05 DL-Type            PIC XX.
           05                    PIC X(6)  VALUE SPACES.
           05 DL-Stock           PIC Z999.
           05                    PIC X(6)  VALUE SPACES.
           05 DL-Cost            PIC $$$,$$$.99 .

       01  Total-Candy-Line.
           05                    PIC X(19) VALUE SPACES.
           05 CL-Title           PIC X(13) VALUE 'TOTAL CANDY: '.
           05 CL-Name            PIC X(13).
           05                    PIC X(8)  VALUE SPACES.
           05 CL-Total           PIC $,$$$,$$$.99 .

       01  Total-Vendor-Line.
           05                    PIC X(14) VALUE SPACES.
           05 VL-Title           PIC X(18) VALUE 'TOTAL FOR VENDOR: '.
           05 VL-Name            PIC X(18).
           05                    PIC X(3)  VALUE SPACES.
           05 VL-Total           PIC $,$$$,$$$.99 .

       01  Total-Warehouse-Line.
           05                    PIC X(11) VALUE SPACES.
           05 WL-Title          PIC X(21) VALUE 'TOTAL FOR WAREHOUSE: '.
           05 WL-Name            PIC X(4).
           05                    PIC X(16) VALUE SPACES.
           05 WL-Total           PIC $$,$$$,$$$.99 .

       01  Grand-Total-Line.
           05                    PIC X(19) VALUE SPACES.
           05 GT-Title           PIC X(12) VALUE 'GRAND TOTAL:'.
           05                    PIC X(20) VALUE SPACES.
           05 GT-Total           PIC $$$,$$$,$$$.99 .

       PROCEDURE DIVISION.

       100-Main-Function.
           PERFORM 150-Housekeeping
           PERFORM 200-Page-Headings
           PERFORM 300-Read-File
           PERFORM 500-Grand-Total
           PERFORM 1000-End-Function
           .
      * Opens input and output and gets the current date
       150-Housekeeping.
           OPEN INPUT Candy-Inventory
           OPEN OUTPUT Candy-Report-File
           ACCEPT WS-Current-Date FROM DATE
           MOVE WS-Month TO H2-Month
           MOVE WS-Day TO H2-Day
           MOVE WS-Year TO H2-Year
           .
      * Prints the headings at the top of every page
       200-Page-Headings.
           ADD 1 TO H2-Page-Num
           MOVE 1 TO Proper-Spacing
           WRITE Report-Line FROM Heading-One
               AFTER ADVANCING PAGE
           WRITE Report-Line FROM Heading-Two
               AFTER ADVANCING Proper-Spacing
           MOVE 2 TO Proper-Spacing
           .
      * Prints the warehouse heading
       225-Print-Warehouse.
            MOVE Warehouse-Hold TO H3-WarehouseID
            MOVE 2 TO Proper-Spacing
            WRITE Report-Line FROM Heading-Three
                AFTER ADVANCING Proper-Spacing
            .
      * Validates and expands the vendor and prints the vendor heading
       250-Print-Vendor.
            EVALUATE TRUE
                WHEN Vendor-Hold = 'A'
                   MOVE 'Atomic Sweets' TO H4-Vendor
                WHEN Vendor-Hold = 'B'
                    MOVE 'Boozie Sweets' TO H4-Vendor
                WHEN Vendor-Hold = 'N'
                    MOVE 'Nellies Sweet Shop' TO H4-Vendor
                WHEN Vendor-Hold = 'T'
                    MOVE 'TigerTreats' TO H4-Vendor
                WHEN OTHER
                    STRING Invalid-String DELIMITED BY ' '
                               ' ' DELIMITED BY SIZE
                           Vendor-Hold DELIMITED BY SIZE
                           INTO H4-Vendor
                    END-STRING
            END-EVALUATE
            MOVE 2 TO Proper-Spacing
            WRITE Report-Line FROM Heading-Four
                AFTER ADVANCING Proper-Spacing
            .
      * Prints the candy heading
       275-Print-Candy.
            MOVE Candy-Hold TO H5-CandyID
            MOVE 2 TO Proper-Spacing
            WRITE Report-Line FROM Heading-Five
               AFTER ADVANCING Proper-Spacing
            WRITE Report-Line FROM Heading-Six
                AFTER ADVANCING Proper-Spacing

            .
      * Opens the file and reads it line by line
       300-Read-File.
            PERFORM UNTIL No-More-Data
                READ Candy-Inventory
                    AT END
                       MOVE 'Y' TO EOF-Flag
                    NOT AT END
                       PERFORM 400-Process-File
                END-READ
            END-PERFORM
            .
      * Processes and validates the input and prints the detail line, 
      * determines when the candy, vendor, or warehouse has changed
      * and calls the paragraphs that handle printing the total lines, 
      * and does the math for the total costs.
       400-Process-File.
            EVALUATE TRUE
                WHEN First-Run = 'Y'
                   MOVE 'N' TO First-Run
                   MOVE CR-Warehouse TO Warehouse-Hold
                   MOVE CR-Vendor TO Vendor-Hold
                   MOVE CR-CandyID TO Candy-Hold
                   PERFORM 225-Print-Warehouse
                   PERFORM 250-Print-Vendor
                   PERFORM 275-Print-Candy
                WHEN Warehouse-Hold NOT = CR-Warehouse
                   PERFORM 425-Warehouse-Break
                   PERFORM 200-Page-Headings
                   MOVE CR-Warehouse TO Warehouse-Hold
                   MOVE CR-Vendor TO Vendor-Hold
                   MOVE CR-CandyID TO Candy-Hold
                   PERFORM 225-Print-Warehouse
                   PERFORM 250-Print-Vendor
                   PERFORM 275-Print-Candy
                WHEN Vendor-Hold NOT = CR-Vendor
                    PERFORM 450-Vendor-Break
                    MOVE CR-Vendor TO Vendor-Hold
                    MOVE CR-CandyID TO Candy-Hold
                    PERFORM 250-Print-Vendor
                    PERFORM 275-Print-Candy
                WHEN Candy-Hold NOT = CR-CandyID
                    PERFORM 475-Candy-Break
                    MOVE CR-CandyID TO Candy-Hold
                    PERFORM 275-Print-Candy
            END-EVALUATE

            MOVE 1 TO Sub
            PERFORM UNTIL Sub > 5
                IF CRD-Size(Sub) NOT = SPACES THEN
      *  Only prints the candy name once per candy
                    IF Sub = 1 THEN
                        MOVE CRD-Name(Sub) TO DL-Name
                    ELSE 
                        MOVE SPACES TO DL-Name
                    END-IF
      * Validates that the input isn't blank
                    EVALUATE TRUE
                        WHEN CRD-Size(Sub) = 'L'
                            MOVE 'Large' TO DL-Size
                        WHEN CRD-Size(Sub) = 'M'
                            MOVE 'Medium' TO DL-Size
                        WHEN CRD-Size(Sub) = 'S'
                            MOVE 'Small' TO DL-Size
                        WHEN CRD-Size(Sub) = 'G'
                            MOVE 'Gift' TO DL-Size
                        WHEN CRD-Size(Sub) = 'X'
                            MOVE 'Sample' TO DL-Size
                        WHEN OTHER
                            STRING Bad-String DELIMITED BY ' '
                                       ' ' DELIMITED BY SIZE
                                   CRD-Size(Sub) DELIMITED BY SIZE
                                   INTO DL-Size
                            END-STRING
                    END-EVALUATE
                    MOVE CRD-Type(Sub) TO DL-Type
      * Validates the price and stock and adds the total price of the
      * candy to the totals.
                    IF CRD-Price(Sub) IS NUMERIC THEN 
                        IF CRD-Stock(Sub) IS NUMERIC THEN
                           MOVE CRD-Stock(Sub) TO DL-Stock
                           COMPUTE C-H = CRD-Price(Sub) * CRD-Stock(Sub)
                           ADD C-H TO TF-Candy-Total
                           ADD C-H TO TF-Vendor-Total
                           ADD C-H TO TF-Warehouse
                           ADD C-H TO TF-Grand-Total
                           MOVE C-H TO DL-Cost
                           MOVE ZEROS TO C-H
                        ELSE
                            MOVE ZEROS TO DL-Cost
                            MOVE ZEROS TO DL-Stock
                        END-IF
                    ELSE
                        MOVE ZEROS TO DL-Cost
                        MOVE ZEROS TO DL-Stock
                    END-IF

                    WRITE Report-Line FROM Detail-Line
                        AFTER ADVANCING Proper-Spacing
                    MOVE 1 TO Proper-Spacing
                END-IF
                ADD 1 TO Sub
            END-PERFORM
            .

      * Calls the vendor break, prints the warehouse total line, and 
      * resets the warehouse total
       425-Warehouse-Break.
           PERFORM 450-Vendor-Break
           MOVE Warehouse-Hold TO WL-Name
           MOVE TF-Warehouse TO WL-Total
           MOVE 2 TO Proper-Spacing
           WRITE Report-Line FROM Total-Warehouse-Line
               AFTER ADVANCING Proper-Spacing
           MOVE ZEROS TO TF-Warehouse

           .
      * Calls the candy break, validates and expands the vendor, prints
      * the vendor total line, nd resets the vendor total.
       450-Vendor-Break.
            PERFORM 475-Candy-Break
            EVALUATE TRUE
                WHEN Vendor-Hold = 'A'
                   MOVE 'Atomic Sweets' TO VL-Name
                WHEN Vendor-Hold = 'B'
                    MOVE 'Boozie Sweets' TO VL-Name
                WHEN Vendor-Hold = 'N'
                    MOVE 'Nellies Sweet Shop' TO VL-Name
                WHEN Vendor-Hold = 'T'
                    MOVE 'TigerTreats' TO VL-Name
                WHEN OTHER
                    STRING Invalid-String DELIMITED BY ' '
                               ' ' DELIMITED BY SIZE
                           Vendor-Hold DELIMITED BY SIZE
                           INTO VL-Name
                    END-STRING
            END-EVALUATE
            MOVE TF-Vendor-Total TO VL-Total
            MOVE 2 TO Proper-Spacing
            WRITE Report-Line FROM Total-Vendor-Line
                AFTER ADVANCING Proper-Spacing
            MOVE ZEROS TO TF-Vendor-Total
           .
      * Prints the candy total line and resets the candy total.
       475-Candy-Break.
           MOVE Candy-Hold TO CL-Name
           MOVE TF-Candy-Total TO CL-Total
           MOVE 2 TO Proper-Spacing
           WRITE Report-Line FROM Total-Candy-Line
               AFTER ADVANCING Proper-Spacing
           MOVE ZEROS TO TF-Candy-Total
           .
      * Prints the total for all warehouses.
       500-Grand-Total.
           PERFORM 425-Warehouse-Break
           MOVE TF-Grand-Total TO GT-Total
           MOVE 3 TO Proper-Spacing
           WRITE Report-Line FROM Grand-Total-Line
               AFTER ADVANCING Proper-Spacing
            .

       1000-End-Function.
           CLOSE Candy-Inventory
           CLOSE Candy-Report-File
           STOP RUN
           .

           