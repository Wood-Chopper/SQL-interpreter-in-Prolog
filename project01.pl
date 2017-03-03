%SQL parser
parse(String, T, V) :- string_codes(String, Codes), phrase(expression(T, V), Codes).
parse(String, T) :- string_codes(String, Codes), phrase(expression(T), Codes).

%DONE
expression(select(var(A), table(T)), V) --> select, white,
											attributserie(A), white,
											from, white,
											name(T), soft_white,
											semicolon,
											{selectE(A, T, V)}.

%DONE
expression(create_table(table(T), var(A)), _) --> create, white,
													table, white,
													name(T), white, leftb, soft_white,
													attributserie(A), soft_white, rightb, soft_white,
													semicolon,
													{createE(T, A)}.

%DONE
expression(insert(table(T), val(A)), _) --> insert, white,
											into, white,
											name(T), white,
											values, white, leftb, soft_white,
											valueserie(A), soft_white, rightb, soft_white,
											semicolon,
											{insertE(T, A)}.

%DONE
expression(drop(table(T)), _) --> drop, white,
									table, white,
									name(T), soft_white,
									semicolon,
									{dropE(T)}.

%DONE
expression(select_where(var(A), table(T), condition(C)), V) --> select, white,
																attributserie(A), white,
																from, white,
																name(T), white,
																where, white,
																conditionalserie(C), soft_white,
																semicolon,
																{select_whereE(A, T, C, V)}.

%DONE
expression(delete(table(T), condition(C)), _) --> delete, white,
												from, white,
												name(T), white,
												where, white,
												conditionalserie(C), soft_white,
												semicolon,
												{deleteE(T, C)}.

%...
expression(update(table(T), set(S), condition(C))) --> update, white,
														name(T), white,
														set, white,
														setserie(S), white,
														where, white,
														conditionalserie(C), soft_white,
														semicolon,
														{updateE(T, S, C)}.

digit(C) --> [H], {char_code(C,H), char_type(C,digit)}.

alpha(C) --> [H], {char_code(C,H), char_type(C,alpha)}.

select --> [H0], {char_code("S", H0)},
			[H1], {char_code("E", H1)},
			[H2], {char_code("L", H2)},
			[H3], {char_code("E", H3)},
			[H4], {char_code("C", H4)},
			[H5], {char_code("T", H5)}.

create --> [H0], {char_code("C", H0)},
			[H1], {char_code("R", H1)},
			[H2], {char_code("E", H2)},
			[H3], {char_code("A", H3)},
			[H4], {char_code("T", H4)},
			[H5], {char_code("E", H5)}.

table --> [H0], {char_code("T", H0)},
			[H1], {char_code("A", H1)},
			[H2], {char_code("B", H2)},
			[H3], {char_code("L", H3)},
			[H4], {char_code("E", H4)}.

insert --> [H0], {char_code("I", H0)},
			[H1], {char_code("N", H1)},
			[H2], {char_code("S", H2)},
			[H3], {char_code("E", H3)},
			[H4], {char_code("R", H4)},
			[H5], {char_code("T", H5)}.

into --> [H0], {char_code("I", H0)},
			[H1], {char_code("N", H1)},
			[H2], {char_code("T", H2)},
			[H3], {char_code("O", H3)}.

values --> [H0], {char_code("V", H0)},
			[H1], {char_code("A", H1)},
			[H2], {char_code("L", H2)},
			[H3], {char_code("U", H3)},
			[H4], {char_code("E", H4)},
			[H5], {char_code("S", H5)}.

delete --> [H0], {char_code("D", H0)},
			[H1], {char_code("E", H1)},
			[H2], {char_code("L", H2)},
			[H3], {char_code("E", H3)},
			[H4], {char_code("T", H4)},
			[H5], {char_code("E", H5)}.

drop --> [H0], {char_code("D", H0)},
			[H1], {char_code("R", H1)},
			[H2], {char_code("O", H2)},
			[H3], {char_code("P", H3)}.

update --> [H0], {char_code("U", H0)},
			[H1], {char_code("P", H1)},
			[H2], {char_code("D", H2)},
			[H3], {char_code("A", H3)},
			[H4], {char_code("T", H4)},
			[H5], {char_code("E", H5)}.

set --> [H0], {char_code("S", H0)},
		[H1], {char_code("E", H1)},
		[H2], {char_code("T", H2)}.

from --> [H0], {char_code("F", H0)},
			[H1], {char_code("R", H1)},
			[H2], {char_code("O", H2)},
			[H3], {char_code("M", H3)}.

and --> [H0], {char_code("A", H0)},
		[H1], {char_code("N", H1)},
		[H2], {char_code("D", H2)}.

or --> [H0], {char_code("O", H0)},
		[H1], {char_code("R", H1)}.

where --> [H0], {char_code("W", H0)},
			[H1], {char_code("H", H1)},
			[H2], {char_code("E", H2)},
			[H3], {char_code("R", H3)},
			[H4], {char_code("E", H4)}.

only_white --> [H], {char_code(" ", H)}.

white --> only_white, soft_white.

soft_white --> [].
soft_white --> white, soft_white.

star --> [H], {char_code("*", H)}.

comma --> [H], {char_code(",", H)}.

semicolon --> [H], {char_code(";", H)}.

quotation --> [H], {char_code("\"", H)}.

leftb --> [H], {char_code("(", H)}.

rightb --> [H], {char_code(")", H)}.

dash --> [H], {char_code("-", H)}.

name(N) --> alpha(H), morealphas(T), {string_chars(N1, [H|T]), atom_string(N, N1)}.

morealphas([]) --> [].

morealphas([H|T]) --> alpha(H), morealphas(T).

number(N) --> digit(H), moredigits(T), {number_chars(N, [H|T])}.

moredigits([]) --> [].

moredigits([H|T]) --> digit(H), moredigits(T).

attributserie([]) --> [].

attributserie(all) --> star.

attributserie([H]) --> name(H).

attributserie([H|T]) --> name(H), soft_white, comma, soft_white, attributserie(T).

conditionalserie(A) --> condition(A).

conditionalserie(cond(or(A,B))) --> condition(A), white, or, white, leftb, soft_white, conditionalserie(B), soft_white, rightb.

conditionalserie(cond(or(A,B))) --> leftb, soft_white, conditionalserie(A), soft_white, rightb, white, or, white, condition(B).

conditionalserie(cond(or(A,B))) --> condition(A), white, or, white, condition(B).

conditionalserie(cond(or(A,B))) --> leftb, soft_white, conditionalserie(A), soft_white, rightb, white, or, white, leftb, soft_white, conditionalserie(B), soft_white, rightb.

conditionalserie(cond(and(A,B))) --> condition(A), white, and, white, leftb, soft_white, conditionalserie(B), soft_white, rightb.

conditionalserie(cond(and(A,B))) --> leftb, soft_white, conditionalserie(A), soft_white, rightb, white, and, white, condition(B).

conditionalserie(cond(and(A,B))) --> condition(A), white, and, white, condition(B).

conditionalserie(cond(and(A,B))) --> leftb, soft_white, conditionalserie(A), soft_white, rightb, white, and, white, leftb, soft_white, conditionalserie(B), soft_white, rightb.

condition(cond(A, B, C)) --> name(A), soft_white, operator(B), soft_white, value(C).

operator(op(=)) --> [H], {char_code("=", H)}.

operator(op(<)) --> [H], {char_code("<", H)}.

operator(op(>)) --> [H], {char_code(">", H)}.

operator(op(<>)) --> [H0], {char_code("<", H0)},
						[H1], {char_code(">", H1)}.

value(num(A)) --> number(A).

value(num(-A)) --> dash, number(A).

value(string(A)) --> quotation, name(A), quotation.

valueserie([]) --> [].

valueserie([H]) --> value(H).

valueserie([H|T]) --> value(H), soft_white, comma, soft_white, valueserie(T).

setserie([]) --> [].

setserie([H]) --> condition(H).

setserie([H|T]) --> condition(H), soft_white, comma, soft_white, setserie(T).

%Loading the database.
load :- load_file("Client.csv"),
		load_file("Commande.csv"),
		load_file("Detail.csv"),
		load_file("Produit.csv").

load_file(Name) :- open(Name, read, File),
					read_line_to_codes(File, C),
					string_codes(St, C),
					split_string(St, ",", "", LT),
					length(LT, Arity),
					split_string(Name, ".", "", [NH|_]),
					string_lower(NH, LowerName),
					atom_string(AtomName, LowerName),
					assert(arity(AtomName, Arity)),
					add_var_knowledge(LT, AtomName, 0),
					csv_read_file(Name, [_|Rows], [functor(AtomName), artity(Arity)]),
					maplist(assert, Rows).

add_var_knowledge([], _, _).
add_var_knowledge([H|T], Table, K) :- atom_string(HAtom, H),
										assert(var(Table, K, HAtom)),
										K1 is K+1,
										add_var_knowledge(T, Table, K1).

%Processing select all
selectE(all, X, V) :- arity(X, Ar), gen_empty_list(L, Ar), findall(L, apply(X, L), V).

%Processing select with only some columns
selectE(L, X, V) :- var_index(L, I, X),
					sort(0, @<, I, IS),
					selectE(all, X, VA),
					keep_var_all(IS, VA, V).

%Processing select where
select_whereE(all, T, C, V) :- selectE(all, T, VTot),
								filter(T, VTot, C, V).

select_whereE(A, T, C, V) :- select_whereE(all, T, C, VF),
								var_index(A, I, T),
								sort(0, @<, I, IS),
								keep_var_all(IS, VF, V).

%cond(truc,op("="),num(3))
%cond(and(cond(truc,op("="),num(3)),cond(bide,op("<"),num(2))))
filter(_, [], _, []).
filter(T, [HTot|VTot], cond(A,op(Op),Val), [HTot|Res]) :- var(T, Ind, A),
																nth0(Ind, HTot, Elem),
																satisfy(Elem, Op, Val),
																filter(T, VTot, cond(A,op(Op),Val), Res).

filter(T, [HTot|VTot], cond(A,op(Op),Val), Res) :- var(T, Ind, A),
														nth0(Ind, HTot, Elem),
														neg(satisfy(Elem, Op, Val)),
														filter(T, VTot, cond(A,op(Op),Val), Res).

filter(T, VTot, cond(or(C1,C2)), Res) :- filter(T, VTot, C1, Res1),
											filter(T, VTot, C2, Res2),
											union(Res1, Res2, Res).

filter(T, VTot, cond(and(C1,C2)), Res) :- filter(T, VTot, C1, Res1),
											filter(T, VTot, C2, Res2),
											intersection(Res1, Res2, Res).

satisfy(Elem, <, num(Val)) :- Elem < Val.
satisfy(Elem, >, num(Val)) :- Elem > Val.
satisfy(Elem, =, num(Val)) :- Elem = Val.
satisfy(Elem, <>, num(Val)) :- Elem \= Val.

satisfy(Elem, =, string(Val)) :- Elem = Val.

%Function that match the name of a columns with the index of the columns regarding the table
var_index([], [], _).
var_index([VH|VL], [IH|IL], T) :- var(T, IH, VH), var_index(VL, IL, T).

%Function that remove the columns that we don't want in the result
keep_var_all(_, [], []).
keep_var_all(I, [VH|VL], [Rs|R]) :- keep_var(I, VH, 0, Rs), keep_var_all(I, VL, R).

keep_var([], _, _, []).
keep_var([IH|IL], [VH|VL], K, [VH|R]) :- IH =:= K, K1 is K+1, keep_var(IL, VL, K1, R).
keep_var(I, [_|VL], K, R) :- K1 is K+1, keep_var(I, VL, K1, R).

%Processing create table
createE(T, A) :- neg(arity(T, _)),
					length(A ,S),
					assert(arity(T, S)),
					add_columns(T, A, 0).

add_columns(_, [], _).
add_columns(T,[AH|AL], K) :- assert(var(T, K, AH)), K1 is K+1, add_columns(T, AL, K1).

neg(X) :- X, !, fail.
neg(_).

%Processing insert into table
insertE(T, A) :- arity(T, X),
					length(A, L),
					X=:=L,
					reformat(A, R),
					In =.. [T|R],
					assert(In). 

%Reformate the table A of the form [num(2), string(azerty),...] into [2, 'azerty',...]
reformat([],[]).
reformat([A|AL], [R|RL]) :- A = num(R), reformat(AL, RL).
reformat([A|AL], [R|RL]) :- A = string(T), atom_string(T, R), reformat(AL, RL).

%Processing drop table
dropE(T) :- arity(T, A),
			retractall(var(T, _, _)),
			retract(arity(T, A)),
			gen_empty_list(L, A),
			Cl =.. [T|L],
			retractall(Cl).

%Generate an uninitialized list of C elements
gen_empty_list([], 0).
gen_empty_list([_|L], C) :- C2 is C-1, gen_empty_list(L, C2).

%Processing delete
deleteE(T, C) :- select_whereE(all, T, C, Res), remove(T, Res).

remove(_, []).
remove(T, [R|Res]) :- Cl =.. [T|R], retract(Cl), remove(T, Res).

%Processing update
%updateE(T, S, C) :- select_whereE(all, T, C, Res), ... 



