:- dynamic expense/5.

% Initialize with sample data
initialize_expenses :-
    retractall(expense(_, _, _, _, _)),
    assert(expense(1, date(2024, 1, 15), 'Food', 25.50, 'Lunch at restaurant')),
    assert(expense(2, date(2024, 1, 15), 'Transport', 12.00, 'Bus fare')),
    assert(expense(3, date(2024, 1, 16), 'Entertainment', 45.00, 'Movie tickets')),
    assert(expense(4, date(2024, 1, 16), 'Food', 15.75, 'Groceries')),
    assert(expense(5, date(2024, 1, 17), 'Utilities', 120.00, 'Electricity bill')),
    write('Expense database initialized.'), nl.

% Add new expense
add_expense(Date, Category, Amount, Description) :-
    get_next_id(NextID),
    assert(expense(NextID, Date, Category, Amount, Description)),
    format('Expense added! ID: ~w', [NextID]), nl.

get_next_id(NextID) :-
    findall(ID, expense(ID, _, _, _, _), IDs),
    (   IDs = [] -> NextID = 1
    ;   max_list(IDs, MaxID), NextID is MaxID + 1
    ).

% List all expenses
list_expenses :-
    findall(expense(ID, Date, Category, Amount, Description),
            expense(ID, Date, Category, Amount, Description),
            Expenses),
    (   Expenses = [] -> write('No expenses found.'), nl
    ;   display_expenses(Expenses)
    ).

display_expenses([]).
display_expenses([expense(ID, Date, Category, Amount, Description)|Rest]) :-
    format('ID: ~w | Date: ~w | ~w | $~2f | ~w',
           [ID, Date, Category, Amount, Description]), nl,
    display_expenses(Rest).

% Find expense by ID
find_expense(ID) :-
    expense(ID, Date, Category, Amount, Description),
    format('ID: ~w~nDate: ~w~nCategory: ~w~nAmount: $~2f~nDescription: ~w~n',
           [ID, Date, Category, Amount, Description]).

% Update expense
update_expense(ID, NewDate, NewCategory, NewAmount, NewDescription) :-
    retract(expense(ID, _, _, _, _)),
    assert(expense(ID, NewDate, NewCategory, NewAmount, NewDescription)),
    format('Expense ~w updated!', [ID]), nl.

% Delete expense
delete_expense(ID) :-
    retract(expense(ID, _, _, _, _)),
    format('Expense ~w deleted!', [ID]), nl.

% Statistics
total_expenses(Total) :-
    findall(Amount, expense(_, _, _, Amount, _), Amounts),
    sum_list(Amounts, Total).

% Main menu
main :-
    initialize_expenses,
    repeat,
    nl,
    write('=== Expense Manager ==='), nl,
    write('1. Add Expense'), nl,
    write('2. List Expenses'), nl,
    write('3. Find Expense'), nl,
    write('4. Update Expense'), nl,
    write('5. Delete Expense'), nl,
    write('6. Total Expenses'), nl,
    write('7. Exit'), nl,
    write('Choice: '),
    read(Choice),
    handle_choice(Choice),
    Choice =:= 7.

handle_choice(1) :-
    write('Date (date(Y,M,D)): '), read(Date),
    write('Category: '), read(Category),
    write('Amount: '), read(Amount),
    write('Description: '), read(Description),
    add_expense(Date, Category, Amount, Description).

handle_choice(2) :-
    list_expenses.

handle_choice(3) :-
    write('ID: '), read(ID),
    (   find_expense(ID) -> true
    ;   format('Expense ~w not found!', [ID]), nl
    ).

handle_choice(4) :-
    write('ID: '), read(ID),
    (   expense(ID, _, _, _, _) ->
        write('New Date: '), read(NewDate),
        write('New Category: '), read(NewCategory),
        write('New Amount: '), read(NewAmount),
        write('New Description: '), read(NewDescription),
        update_expense(ID, NewDate, NewCategory, NewAmount, NewDescription)
    ;   format('Expense ~w not found!', [ID]), nl
    ).

handle_choice(5) :-
    write('ID: '), read(ID),
    (   expense(ID, _, _, _, _) ->
        delete_expense(ID)
    ;   format('Expense ~w not found!', [ID]), nl
    ).

handle_choice(6) :-
    total_expenses(Total),
    format('Total Expenses: $~2f~n', [Total]).

handle_choice(7) :-
    write('Goodbye!'), nl.

handle_choice(_) :-
    write('Invalid choice!'), nl.

% Start the application
:- main.
