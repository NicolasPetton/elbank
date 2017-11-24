Feature: Display reports

  Scenario: Reporting all transactions
    When I make a full report
    Then I should see:
      """
       Date          Label               Category            Amount 
      ══════════════════════════════════════════════════════════════
       2017-11-01    Rent                Expenses:Rent     -450.00  
       2017-11-18    CB Restaurant       Expenses:Food      -31.00  
       2017-11-20    Paycheck            Income:Salary     2300.00  
       2017-11-20    Bakery xxx          Expenses:Food       -4.25  
       2017-11-24    CB Supermarket 1    Expenses:Food     -124.00  
      """

  Scenario: Reporting all transactions within a month
    When I make a monthly report
    Then I should see:
      """
      Period: November 2017
      
       Date          Label               Category            Amount 
      ══════════════════════════════════════════════════════════════
       2017-11-01    Rent                Expenses:Rent     -450.00  
       2017-11-18    CB Restaurant       Expenses:Food      -31.00  
       2017-11-20    Paycheck            Income:Salary     2300.00  
       2017-11-20    Bakery xxx          Expenses:Food       -4.25  
       2017-11-24    CB Supermarket 1    Expenses:Food     -124.00  
      ══════════════════════════════════════════════════════════════
                                                           1690.75  
      """

  Scenario: Sort column
    When I make a monthly report with ":sort-by amount"
    Then I should see:
      """
      Period: November 2017
      
       Date          Label               Category            Amount 
      ══════════════════════════════════════════════════════════════
       2017-11-01    Rent                Expenses:Rent     -450.00  
       2017-11-24    CB Supermarket 1    Expenses:Food     -124.00  
       2017-11-18    CB Restaurant       Expenses:Food      -31.00  
       2017-11-20    Bakery xxx          Expenses:Food       -4.25  
       2017-11-20    Paycheck            Income:Salary     2300.00  
      ══════════════════════════════════════════════════════════════
                                                           1690.75  
      """

  Scenario: Reverting the sort order
    When I make a monthly report with ":sort-by amount"
    And I press "s"
    Then I should see:
      """
      Period: November 2017
      
       Date          Label               Category            Amount 
      ══════════════════════════════════════════════════════════════
       2017-11-20    Paycheck            Income:Salary     2300.00  
       2017-11-20    Bakery xxx          Expenses:Food       -4.25  
       2017-11-18    CB Restaurant       Expenses:Food      -31.00  
       2017-11-24    CB Supermarket 1    Expenses:Food     -124.00  
       2017-11-01    Rent                Expenses:Rent     -450.00  
      ══════════════════════════════════════════════════════════════
                                                           1690.75  
      """

  Scenario: Grouping
    When I make a monthly report with ":group-by category"
    Then I should see:
      """
      Period: November 2017
      
       Date          Label               Category            Amount 
      ══════════════════════════════════════════════════════════════
                                                                    
       Expenses:Food                                                 
      ──────────────────────────────────────────────────────────────
       2017-11-18    CB Restaurant       Expenses:Food      -31.00  
       2017-11-20    Bakery xxx          Expenses:Food       -4.25  
       2017-11-24    CB Supermarket 1    Expenses:Food     -124.00  
      ──────────────────────────────────────────────────────────────
                                                           -159.25  
                                                                    
       Expenses:Rent                                                 
      ──────────────────────────────────────────────────────────────
       2017-11-01    Rent                Expenses:Rent     -450.00  
      ──────────────────────────────────────────────────────────────
                                                           -450.00  
                                                                    
       Income:Salary                                                 
      ──────────────────────────────────────────────────────────────
       2017-11-20    Paycheck            Income:Salary     2300.00  
      ──────────────────────────────────────────────────────────────
                                                           2300.00  
      ══════════════════════════════════════════════════════════════
                                                           1690.75  
      """

  Scenario: Grouping and sorting reports
    When I make a monthly report with ":group-by category :sort-by amount"
    Then I should see:
      """
      Period: November 2017
      
       Date          Label               Category            Amount 
      ══════════════════════════════════════════════════════════════
                                                                    
       Expenses:Rent                                                 
      ──────────────────────────────────────────────────────────────
       2017-11-01    Rent                Expenses:Rent     -450.00  
      ──────────────────────────────────────────────────────────────
                                                           -450.00  
                                                                    
       Expenses:Food                                                 
      ──────────────────────────────────────────────────────────────
       2017-11-24    CB Supermarket 1    Expenses:Food     -124.00  
       2017-11-18    CB Restaurant       Expenses:Food      -31.00  
       2017-11-20    Bakery xxx          Expenses:Food       -4.25  
      ──────────────────────────────────────────────────────────────
                                                           -159.25  
                                                                    
       Income:Salary                                                 
      ──────────────────────────────────────────────────────────────
       2017-11-20    Paycheck            Income:Salary     2300.00  
      ──────────────────────────────────────────────────────────────
                                                           2300.00  
      ══════════════════════════════════════════════════════════════
                                                           1690.75  
      """

  Scenario: Selecting columns
    When I make a monthly report with ":columns (date label amount)"
    Then I should see:
      """
      Period: November 2017
      
       Date          Label                  Amount 
      ═════════════════════════════════════════════
       2017-11-01    Rent                 -450.00  
       2017-11-18    CB Restaurant         -31.00  
       2017-11-20    Paycheck             2300.00  
       2017-11-20    Bakery xxx             -4.25  
       2017-11-24    CB Supermarket 1     -124.00  
      ═════════════════════════════════════════════
                                          1690.75  
      """

  Scenario: Filtering by category
    When I make a monthly report
    And I press "fcExpenses:Food"
    Then I should see:
      """
      Period: November 2017
      Category: Expenses:Food
      
       Date          Label               Category            Amount 
      ══════════════════════════════════════════════════════════════
       2017-11-18    CB Restaurant       Expenses:Food      -31.00  
       2017-11-20    Bakery xxx          Expenses:Food       -4.25  
       2017-11-24    CB Supermarket 1    Expenses:Food     -124.00  
      ══════════════════════════════════════════════════════════════
                                                           -159.25  
      """

  Scenario: Filtering by category
    When I make a monthly report
    And I press "fcExpenses"
    Then I should see:
      """
      Period: November 2017
      Category: Expenses
      
       Date          Label               Category            Amount 
      ══════════════════════════════════════════════════════════════
       2017-11-01    Rent                Expenses:Rent     -450.00  
       2017-11-18    CB Restaurant       Expenses:Food      -31.00  
       2017-11-20    Bakery xxx          Expenses:Food       -4.25  
       2017-11-24    CB Supermarket 1    Expenses:Food     -124.00  
      ══════════════════════════════════════════════════════════════
                                                           -609.25  
      """
