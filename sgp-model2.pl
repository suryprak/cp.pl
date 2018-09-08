:- use_module(library(lists)).
:- use_module(library(clpfd)).

social_golfer(G, S, W, Order, Consistency) :-
	Con = [consistency(Consistency)],
	golf(G, S, W, Schedule, Byrow, Bycol, Con),
	golfer_ordering(Order, Byrow, Bycol, All),
	print_schedule(S,Schedule,Set,All),
        statistics.

golf(G, S, W, Schedule, GolfersByRow, GolfersByCol, Con) :-
	schedule(0, G, S, W, Schedule, GolfersByRow, GolfersByCol, Con),
	Schedule = [First|Next],
	append(First, Golfers),
	(   foreach(Week,Next),
	    param(S)
	do  (   foreach([P|Ps],Week),
		param(S)
	    do  P/S #= Q0,
		(   foreach(P1,Ps),
		    fromto(Q0,Q1,Q2,_),
		    param(S)
		do  P1/S #= Q2,
		    Q1 #< Q2
		)
	    ),
	    schedule_week(0, S, Week)
	),
	golfers_socialize(Schedule, G, S,Con),
	schedule_alldifferent(0, S, Next, Con),
	labeling([min,bisect], Golfers), !.

schedule(W, _, _, W, [], [], [], _).
schedule(I, G, S, W, [Week|Schedule], [ByRow|ByRows], [ByCol|ByCols], Con) :-
	(   for(_,1,G),
	    foreach(Group,Week),
	    param([G,S])
	do  length(Group, S),
	    GS is G*S-1,
	    domain(Group, 0, GS)
	),
	append(Week, ByRow),
	all_different(ByRow, Con),
	transpose(Week, WeekT),
	append(WeekT, ByCol),
	J is I+1,
	schedule(J, G, S, W, Schedule, ByRows, ByCols, Con).

	/* constraint model based on scalar_product */

	golfers_socialize(Schedule, G, S, Con) :-
		append(Schedule, Groups),
		groups_meets(Groups, Tuples, [], MeetVars, []),
		GS is G*S,
		(   foreach([A,B,C],Tuples),
		    param([GS,Con])
		do  scalar_product([GS,1], [A,B], #=, C, Con)
		),
		all_distinct(MeetVars, Con).

	groups_meets([], Tuples, Tuples) --> [].
	groups_meets([Group|Groups], Tuples1, Tuples3) -->
		group_meets(Group, Tuples1, Tuples2),
		groups_meets(Groups, Tuples2, Tuples3).

	group_meets([], Tuples, Tuples) --> [].
	group_meets([P|Ps], Tuples1, Tuples3) -->
		group_meets(Ps, P, Tuples1, Tuples2),
		group_meets(Ps, Tuples2, Tuples3).

	group_meets([], _, Tuples, Tuples) --> [].
	group_meets([Q|Qs], P, [[P,Q,PQ]|Tuples1], Tuples2) --> [PQ],
		group_meets(Qs, P, Tuples1, Tuples2).

	schedule_week(S, S, Week) :-
		S1 is S-1,
		schedule_week(Week, S1).
	schedule_week(I, S, [[I|_]|Week]) :-
		J is I+1,
		schedule_week(J, S, Week).

	schedule_week([], _).
	schedule_week([[J|_]|Week], I) :-
		I #< J,
		schedule_week(Week, J).

	schedule_alldifferent(S, S, _Schedule, _).
	schedule_alldifferent(I, S, Schedule, Con) :-
		(   foreach(Week,Schedule),
		    foreach(Ith,Part),
		    param(I)
		do  nth0(I, Week, [_|Ith])
		),
		append(Part, Conc),
		all_different(Conc, Con),
		J is I+1,
		schedule_alldifferent(J, S, Schedule, Con).

golfer_ordering(bycol, _, All, All).
golfer_ordering(byrow, All, _, All).
golfer_ordering(bycolall, _, Cols, [All]) :-
	append(Cols, All).
golfer_ordering(byrowall, Rows, _, [All]) :-
	append(Rows, All).

print_schedule(S,Schedule,Set,All):-
	( ( foreach(Set,All)
	   do labeling(min,Set)
	  )
	),
	G_s is S,
	(   foreach(Round,Schedule),
	    count(Wk,1,_)
	do  format('Week ~d:\n', [Wk]),
	    ( foreach(G_s,Round)
	   do  print(G_s),print('\n')
            )
         ),
	print('\n').

