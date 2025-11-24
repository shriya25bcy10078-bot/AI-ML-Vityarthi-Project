:- dynamic expense/5.

% Initialize with sample data
initialize_expenses :-
    retractall(expense(_, _, _, _, _)),
    assert(expense(1, date(2024, 1, 15), 'Food', 25.50, 'Lunch at restaurant')),
    assert(expense(2, date(2024, 1, 15), 'Transport', 12.00, 'Bus fare')),
    assert(expense(3, date(2024, 1, 16), 'Fun', 45.00, 'Movie tickets')),
    assert(expense(4, date(2024, 1, 16), 'Food', 15.75, 'Groceries')),
    assert(expense(5, date(2024, 1, 17), 'Utilities', 120.00, 'Electricity bill')),
    write('Expense stuff loaded.'), nl.

% Add new expense
add_expense(Date, Cat, Amt, Desc) :-
    get_next_id(NextID),
    assert(expense(NextID, Date, Cat, Amt, Desc)),
    format('Expense added! ID: ~w', [NextID]), nl.

get_next_id(NextID) :-
    findall(ID, expense(ID, _, _, _, _), All),
    (   All == [] -> NextID = 1
    ;   max_list(All, Max), NextID is Max + 1
    ).

% List all expenses
list_expenses :-
    findall(expense(ID, Date, Cat, Amt, Desc),
            expense(ID, Date, Cat, Amt, Desc),
            Expenses),
    (   Expenses == [] -> write('No expenses found.'), nl
    ;   show_expense(Expenses)
    ).

show_expense([]).
show_expense([expense(ID, Date, Cat, Amt, Desc)|Rest]) :-
    format('ID: ~w | Date: ~w | ~w | $~2f | ~w',
           [ID, Date, Cat, Amt, Desc]), nl,
    show_expense(Rest).

% Find expense by ID
find_expense(ID) :-
    expense(ID, Date, Cat, Amt, Desc),
    format('ID: ~w~nDate: ~w~nCategory: ~w~nAmount: $~2f~nDescription: ~w~n',
           [ID, Date, Cat, Amt, Desc]).

% Update expense
update_expense(ID, NewDate, NewCat, NewAmt, NewDesc) :-
    retract(expense(ID, _, _, _, _)),
    assert(expense(ID, NewDate, NewCat, NewAmt, NewDesc)),
    format('Expense ~w update succesfully!', [ID]), nl.

% Delete expense
delete_expense(ID) :-
    retract(expense(ID, _, _, _, _)),
    format('Deleted Expense!', [ID]), nl.

% Statistics
total_expenses(Total) :-
    findall(Amt, expense(_, _, _, Amt, _), AmountList),
    sum_list(AmountList, Total).

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
    write('Category: '), read(Cat),
    write('Amount: '), read(Amt),
    write('Description: '), read(Desc),
    add_expense(Date, Cat, Amt, Desc).

handle_choice(2) :-
    list_expenses.

handle_choice(3) :-
    write('ID: '), read(ID),
    (   find_expense(ID) -> true
    ;   format('Expense ~w not found!', [ID]), nl
    ).

handle_choice(4) :-
    write('ID to update : '), read(ID),
    (   expense(ID, _, _, _, _) ->
        write('New Date: '), read(NewDate),
        write('New Category: '), read(NewCat),
        write('New Amount: '), read(NewAmt),
        write('New Description: '), read(NewDesc),
        update_expense(ID, NewDate, NewCat, NewAmt, NewDesc)
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
