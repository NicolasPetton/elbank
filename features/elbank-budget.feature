Feature: Display reports

  Scenario: Budget reporting
    When I make a budget report
    Then I should see:
      """
      Budget report for November 2017

      [Customize budgets]

       Expenses:Food           53%                            159.25 of 300.00 budgeted

       Expenses:Rent                   100%                   450.00 of 450.00 budgeted


       Total: 609.25 of 750.00 budgeted.
      """
