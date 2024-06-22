% daghan erdonmez
% 2021400093
% compiling: yes
% complete: yes


:- ['cmpefarm.pro'].
:- init_from_map.


% 1- agents_distance(+Agent1, +Agent2, -Distance)
agents_distance(Agent1, Agent2, Distance) :-
    get_dict(x, Agent1, X1),
    get_dict(x, Agent2, X2),
    get_dict(y, Agent1, Y1),
    get_dict(y, Agent2, Y2),

    DX is abs(X1-X2),
    DY is abs(Y1-Y2),

    Distance is DX + DY.

% 2- number_of_agents(+State, -NumberOfAgents)
number_of_agents([Agents|_], NumberOfAgents) :-
    dict_size(Agents, NumberOfAgents).
    

% 3- value_of_farm(+State, -Value)
value_of_farm([Agents, Objects, _, _], TotalValue) :-
    calculate_agents_value(Agents, AgentsValue),
    calculate_objects_value(Objects, ObjectsValue),
    TotalValue is AgentsValue + ObjectsValue.

% Helper predicate to calculate the total value of all agents
calculate_agents_value(Agents, TotalValue) :-
    dict_pairs(Agents, _, Pairs),
    findall(Value, (
       member(_-Agent, Pairs),
        get_dict(subtype, Agent, Subtype),
        value(Subtype, Value)
        ), Values),
    my_sum_list(Values, TotalValue). % Sum all the values

% Helper predicate to calculate the total value of all consumable objects
calculate_objects_value(Objects, TotalValue) :-
    dict_pairs(Objects, _, Pairs),
    findall(Value, (
        member(_-Object, Pairs), % For each object in Objects
        get_dict(subtype, Object, Subtype),
        value(Subtype, Value)
    ), Values),
    my_sum_list(Values, TotalValue). % Sum all the values

% sum_list(List, Sum) - Sum is the sum of all elements in List.
% Wrapper predicate to initiate the accumulator.
my_sum_list(List, Sum) :-
    my_sum_list(List, 0, Sum).

% Base case: If the list is empty, the sum is the value of the accumulator.
my_sum_list([], Acc, Acc).

% Recursive case: Add the head of the list to the accumulator, and proceed with the tail.
my_sum_list([H|T], Acc, Sum) :-
    NewAcc is Acc + H,
    my_sum_list(T, NewAcc, Sum).


% 4- find_food_coordinates(+State, +AgentId, -Coordinates)
% find_food_coordinates/3 - Finds coordinates of food sources for wolves
find_food_coordinates([Agents, _, _, _], AgentId, Coordinates) :-
    get_dict(AgentId, Agents, Agent),
    get_dict(subtype, Agent, wolf),  % Match 'wolf' subtype
    dict_pairs(Agents, _, Pairs),
    findall([X, Y], (
        my_member(_-OtherAgent, Pairs),
        get_dict(subtype, OtherAgent, OtherSubtype),
        can_eat(wolf, OtherSubtype), 
        get_dict(x, OtherAgent, X),
        get_dict(y, OtherAgent, Y)
    ), Coordinates),
    Coordinates \= [].  % Ensuring coordinates list is not empty

% find_food_coordinates/3 - Finds coordinates of food sources for herbivores
find_food_coordinates([Agents, Objects, _, _], AgentId, Coordinates) :-
    get_dict(AgentId, Agents, Agent),
    get_dict(subtype, Agent, Subtype),
    \+ Subtype = wolf,  % Ensuring the subtype is not 'wolf'
    dict_pairs(Objects, _, Pairs),
    findall([X, Y], (
        my_member(_-Object, Pairs),
        get_dict(subtype, Object, ObjectSubtype),
        can_eat(Subtype, ObjectSubtype),  % Check if the agent subtype can eat the object subtype
        get_dict(x, Object, X),
        get_dict(y, Object, Y)
    ), Coordinates),
    Coordinates \= [].  % Ensure the coordinates list is not empty

% Checks if an element is a member of a list
my_member(Element, [Element|_]).
my_member(Element, [_|Tail]) :- my_member(Element, Tail).

% 5- find_nearest_agent(+State, +AgentId, -Coordinates, -NearestAgent)
find_nearest_agent([Agents, _, _, _], AgentId, Coordinates, NearestAgent) :-
    get_dict(AgentId, Agents, Agent), % Get the dictionary of the current agent
    dict_pairs(Agents, _, Pairs), % Get all agents in dictionary form
    my_exclude(=(AgentId-_), Pairs, OtherAgents), % Exclude the current agent from the search
    findall(Dist-OtherAgent, ( % Find all other agents and their distances to the current agent
        member(_-OtherAgent, OtherAgents),
        agents_distance(Agent, OtherAgent, Dist)
    ), Distances),
    my_sort(Distances, [_-NearestAgent|_]), % Sort agents by distance and select the nearest one
    get_dict(x, NearestAgent, X), % Get the x coordinate of the nearest agent
    get_dict(y, NearestAgent, Y), % Get the y coordinate of the nearest agent
    Coordinates = [X, Y]. % Return the coordinates

% Insert element into a sorted list of pairs (distance-agent)
my_insert(Dist-Agent, [], [Dist-Agent]).
my_insert(Dist1-Agent1, [Dist2-Agent2|Rest], [Dist1-Agent1,Dist2-Agent2|Rest]) :- Dist1 =< Dist2.
my_insert(Dist1-Agent1, [Dist2-Agent2|Rest], [Dist2-Agent2|SortedRest]) :-
    Dist1 > Dist2,
    my_insert(Dist1-Agent1, Rest, SortedRest).

% Sort the list using insertion sort
my_sort([], []).
my_sort([Dist-Agent|Rest], Sorted) :-
    my_sort(Rest, SortedRest),
    my_insert(Dist-Agent, SortedRest, Sorted).

    my_exclude(_, [], []).
my_exclude(P, [X|Xs], Result) :-
    call(P, X),                 % Check if P(X) is true
    !,                          % Cut to prevent backtracking if P(X) is true
    my_exclude(P, Xs, Result).  % Continue with the rest of the list
my_exclude(P, [X|Xs], [X|Result]) :-
    my_exclude(P, Xs, Result).  % Include X in the result if P(X) is false

% 6- find_nearest_food(+State, +AgentId, -Coordinates, -FoodType, -Distance)
find_nearest_food([Agents, _, _, _], AgentId, Coordinates, FoodType, Distance) :-
    get_dict(AgentId, Agents, Agent),
    get_dict(subtype, Agent, wolf),  % Get the subtype of the agent
    dict_pairs(Agents, _, Pairs),  % Get all objects in dictionary form
    findall(Dist-[X, Y]-FoodSubtype, (
        member(_-AgentToEat, Pairs),  % For each object
        get_dict(x, AgentToEat, X),
        get_dict(y, AgentToEat, Y),
        get_dict(subtype, AgentToEat, FoodSubtype),  % Get the subtype of the object
        can_eat(wolf, FoodSubtype),  % Check if the agent subtype can eat the object subtype
        agents_distance(Agent, AgentToEat, Dist)  % Calculate the Manhattan distance
    ), Distances),
    Distances \= [],  % Ensure the distances list is not empty, fail if it is
    format('Distances before sorting: ~w~n', [Distances]),
    my_sort2(Distances, [MinDist-[X, Y]-FoodSubtype|_]),  % Sort by distance and select the nearest one
    Coordinates = [X, Y],  % Set the coordinates output
    FoodType = FoodSubtype,  % Set the food type output
    Distance = MinDist.  % Set the distance output

find_nearest_food([Agents, Objects, _, _], AgentId, Coordinates, FoodType, Distance) :-
    get_dict(AgentId, Agents, Agent),
    get_dict(subtype, Agent, Subtype),  % Get the subtype of the agent
    \+ Subtype = wolf,
    dict_pairs(Objects, _, Pairs),  % Get all objects in dictionary form
    findall(Dist-[X, Y]-FoodSubtype, (
        member(_-Object, Pairs),  % For each object
        get_dict(x, Object, X),
        get_dict(y, Object, Y),
        get_dict(subtype, Object, FoodSubtype),  % Get the subtype of the object
        can_eat(Subtype, FoodSubtype),  % Check if the agent subtype can eat the object subtype
        agent_object_distance(Agent, Object, Dist)  % Calculate the Manhattan distance
    ), Distances),
    Distances \= [],  % Ensure the distances list is not empty, fail if it is
    format('Distances before sorting: ~w~n', [Distances]),
    my_sort2(Distances, [MinDist-[X, Y]-FoodSubtype|_]),  % Sort by distance and select the nearest one
    Coordinates = [X, Y],  % Set the coordinates output
    FoodType = FoodSubtype,  % Set the food type output
    Distance = MinDist.  % Set the distance output

% Helper predicate to calculate Manhattan distance between an agent and an object
agent_object_distance(Agent, Object, Distance) :-
    get_dict(x, Agent, X1),
    get_dict(x, Object, X2),
    get_dict(y, Agent, Y1),
    get_dict(y, Object, Y2),
    DX is abs(X1-X2),
    DY is abs(Y1-Y2),
    Distance is DX + DY.
    %('Calculated Distance: ~w between ~w and ~w~n', [Distance, [X1, Y1], [X2, Y2]]).


% Base case: Insert into an empty list
my_insert2(X, [], [X]).

% Inserting into a non-empty list: insert in sorted order by distance
my_insert2(Dist-[X, Y]-Type, [Dist1-[X1, Y1]-Type1|T], [Dist-[X, Y]-Type, Dist1-[X1, Y1]-Type1|T]) :-
    Dist =< Dist1, !.

my_insert2(Dist-[X, Y]-Type, [Dist1-[X1, Y1]-Type1|T], [Dist1-[X1, Y1]-Type1|SortedRest]) :-
    Dist > Dist1,
    my_insert2(Dist-[X, Y]-Type, T, SortedRest).

% Sorting function using the insertion method
my_sort2([], []).
my_sort2([H|T], Sorted) :-
    my_sort2(T, SortedT),
    my_insert2(H, SortedT, Sorted).

% 7- move_to_coordinate(+State, +AgentId, +X, +Y, -ActionList, +DepthLimit)
move_to_coordinate(State, AgentId, X, Y, [], _) :-
    State = [Agents, _, _, _],
    get_dict(AgentId, Agents, Agent),
    get_dict(x, Agent, CurrentX),
    get_dict(y, Agent, CurrentY),
    CurrentX == X,
    CurrentY == Y.

% Recursive case for move_to_coordinate/6
move_to_coordinate(State, AgentId, X, Y, ActionList, DepthLimit) :-
    DepthLimit > 0,  % Ensure there is remaining depth to continue recursion
    NewDepthLimit is DepthLimit - 1,
    State = [Agents, _, _, _],
    get_dict(AgentId, Agents, Agent),
    get_dict(subtype, Agent, Subtype),
    
    % Fetch all possible actions this agent can make
    findall(Action, can_move(Subtype, Action), PossibleActions),
    
    % Try each action recursively
    member(Action, PossibleActions),
    move(State, AgentId, Action, NewState),
    move_to_coordinate(NewState, AgentId, X, Y, RemainingActionList, NewDepthLimit),
    
    % Append current action to the list of actions that resulted in a successful path
    ActionList = [Action|RemainingActionList].

% 8- move_to_nearest_food(+State, +AgentId, -ActionList, +DepthLimit)
move_to_nearest_food(State, AgentId, ActionList, DepthLimit) :-
    find_nearest_food(State, AgentId, Coordinates, _, _),  % Find the nearest food
    Coordinates = [X, Y],
    move_to_coordinate(State, AgentId, X, Y, ActionList, DepthLimit).  % Move to the coordinates of the nearest food

% 9- consume_all(+State, +AgentId, -NumberOfMoves, -Value, NumberOfChildren +DepthLimit)
consume_all(State, AgentId, NumberOfMoves, Value, NumberOfChildren, DepthLimit):-
    consume_all_helper(State, AgentId, NumberOfMoves, Value, NumberOfChildren, DepthLimit, _).

consume_all_helper(State, AgentId, NumberOfMoves, Value, NumberOfChildren, DepthLimit, NewStateOut) :-
    print_state(State),
    State = [Agents, _, _, _],
    get_dict(AgentId, Agents, _),
    %format('x'),
    find_nearest_foods(State, AgentId, SortedDistances), % Finds and sorts nearest food sources by distance.
    try_reach_and_consume(State, AgentId, NumMoves, _, _, DepthLimit, SortedDistances, NewState),
    NewStateOut = NewState, % Updates the state output parameter.
    NumberOfMoves is NumMoves, % Accumulates the number of moves taken.
    value_of_farm(NewState, Value), % Calculates the total value of the farm in the new state.
    total_children(NewState, NumberOfChildren). % Counts the total number of children in the new state.

try_reach_and_consume(State, _, NumMoves, _, _, _, [], NewStateOut):-
    NewStateOut = State,  % Updates the output state if no more food is available.
    NumMoves is 0.  % Sets moves to 0 when no food is available.

try_reach_and_consume(State, AgentId, NumMoves, Value, NumberOfChildren, DepthLimit, SortedDistances, NewStateOut) :-
    SortedDistances = [_-[X, Y]-_|_],  % Extracts coordinates of nearest food.
    find_shortest_path_to_coordinate(State, AgentId, X, Y, ActionList, DepthLimit),  % Finds shortest path to the food.
    make_series_of_actions(ActionList, State, AgentId, NewState),  % Applies actions to move towards the food.
    size_of_list(ActionList, Size),  % Calculates the number of actions (moves).
    eat(NewState, AgentId, NewNewState),  % Consumes the food and updates the state.
    consume_all_helper(NewNewState, AgentId, NumberOfMoves, Value, NumberOfChildren, DepthLimit, NewNewNewState),  % Recurses to consume next nearest food.
    NewStateOut = NewNewNewState,  % Updates the output state after consuming.
    NumMoves is NumberOfMoves + Size.  % Updates the total number of moves.

try_reach_and_consume(State, AgentId, NumberOfMoves, Value, NumberOfChildren, DepthLimit, SortedDistances, NewStateOut) :-
    SortedDistances = [_MinDist-[X, Y]-_|Rest],  % Extracts food location and other available foods.
    \+ find_shortest_path_to_coordinate(State, AgentId, X, Y, _, DepthLimit),  % Checks if no path to food exists within the depth limit.
    try_reach_and_consume(State, AgentId, NumberOfMoves, Value, NumberOfChildren, DepthLimit, Rest, NewStateOut).  % Attempts to consume next nearest food if current is unreachable.

find_nearest_foods([Agents, _, _, _], AgentId, SortedDistances) :-
    get_dict(AgentId, Agents, Agent),
    get_dict(subtype, Agent, wolf),  % Get the subtype of the agent
    dict_pairs(Agents, _, Pairs),  % Get all objects in dictionary form
    findall(Dist-[X, Y]-FoodSubtype, (
        member(_-AgentToEat, Pairs),  % For each object
        get_dict(x, AgentToEat, X),
        get_dict(y, AgentToEat, Y),
        get_dict(subtype, AgentToEat, FoodSubtype),  % Get the subtype of the object
        can_eat(wolf, FoodSubtype),  % Check if the agent subtype can eat the object subtype
        agents_distance(Agent, AgentToEat, Dist)  % Calculate the Manhattan distance
    ), Distances),
    %format('Distances before sorting: ~w~n', [Distances]),
    my_sort2(Distances, SortedDistances). %format: [MinDist-[X, Y]-FoodSubtype|_]


find_nearest_foods([Agents, Objects, _, _], AgentId, SortedDistances) :-
    get_dict(AgentId, Agents, Agent),
    get_dict(subtype, Agent, Subtype),  % Get the subtype of the agent
    \+ Subtype = wolf,
    dict_pairs(Objects, _, Pairs),  % Get all objects in dictionary form
    findall(Dist-[X, Y]-FoodSubtype, (
        member(_-Object, Pairs),  % For each object
        get_dict(x, Object, X),
        get_dict(y, Object, Y),
        get_dict(subtype, Object, FoodSubtype),  % Get the subtype of the object
        can_eat(Subtype, FoodSubtype),  % Check if the agent subtype can eat the object subtype
        agent_object_distance(Agent, Object, Dist)  % Calculate the Manhattan distance
    ), Distances),
    %format('Distances before sorting: ~w~n', [Distances]),
    my_sort2(Distances, SortedDistances).

find_shortest_path_to_coordinate(State, AgentId, X, Y, ActionList, MaxDepthLimit) :-
    my_between(1, MaxDepthLimit, CurrentDepthLimit),  % Increment depth limit from 1 to MaxDepthLimit
    move_to_coordinate(State, AgentId, X, Y, ActionList, CurrentDepthLimit),  % Attempt to find a path at current depth limit
    !.  % Cut to prevent further backtracking once a solution is found

my_between(Low, High, Low) :- Low =< High.  % Base case: Low is within the range
my_between(Low, High, Value) :-
    Low < High,  % Ensure that there is room to increment
    NextLow is Low + 1,  % Increment the lower bound
    my_between(NextLow, High, Value).  % Recursive call with the new lower bound

% Base case: The size of an empty list is 0.
size_of_list([], 0).

% Recursive case: Split the list into head (First element) and tail (rest of the list),
% then recursively calculate the size of the tail.
size_of_list([_|Tail], Size) :-
    size_of_list(Tail, SubSize),
    Size is SubSize + 1.

% total_children/2 - Calculates the total number of children of all agents in a given state
total_children(State, TotalChildren) :-
    State = [Agents, _, _, _],  % Extract the Agents dictionary from the state
    dict_pairs(Agents, _, Pairs),  % Get all key-value pairs from the dictionary
    sum_children_pairs(Pairs, TotalChildren).  % Sum the 'children' count from each agent pair

% sum_children_pairs/2 - Sums children counts from a list of key-agent pairs
sum_children_pairs([], 0).  % Base case: empty list, sum is 0
sum_children_pairs([_-Agent|Tail], TotalChildren) :-
    get_dict(children, Agent, Children),  % Get the 'children' field from the agent dictionary
    sum_children_pairs(Tail, TailChildren),  % Recursively sum the rest
    TotalChildren is Children + TailChildren.  % Add current children to total

