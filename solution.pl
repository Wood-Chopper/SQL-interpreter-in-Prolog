%Data representation
load :- DB = [
arity(client, 6),
arity(commande, 3),
arity(detail, 3),
arity(produit, 4),

var(client, 0, ncli),
var(client, 1, nom),
var(client, 2, adresse),
var(client, 3, localite),
var(client, 4, cat),
var(client, 5, compte),
var(commande, 0, ncom_co),
var(commande, 1, ncli_co),
var(commande, 2, date),
var(detail, 0, ncom_de),
var(detail, 1, npro_de),
var(detail, 2, qcom),
var(produit, 0, npro),
var(produit, 1, libelle),
var(produit, 2, prix),
var(produit, 3, qstock),

client('B062', 'Goffin', '72, rue de la Gare', 'Namur', 'B2', -3200),
client('B112', 'Hansenne', '23, rue Dumont', 'Poitiers', 'C1', 1250),
client('B332', 'Monti', '112, rue Neuve', 'Geneve', 'B2', 0),
client('B512', 'Gillet', '14, rue de l Etat', 'Toulouse', 'B1', -8700),
client('C003', 'Avron', '8, rue de la Cure', 'Toulouse', 'B1', -1700),
client('C123', 'MERCIER', '25, rue Lemaitre', 'Namur', 'C1', -2300),
client('C400', 'Ferard', '65, rue du Tertre', 'Poitiers', 'B2', 350),
client('D063', 'Mercier', '201, boulevard du Nord', 'Toulouse', 'B2', -2250),
client('F010', 'Toussaint', '5, rue Godefroid', 'Poitiers', 'C1', 0),
client('F400', 'Jacob', '78, chemin du Moulin', 'Bruxelles', 'C2', 0),
client('K111', 'Vanbist', '180, rue Florimont', 'Lille', 'B1', 720),
client('L422', 'Franck', '60, rue de Wepion', 'Namur', 'C1', 0),
client('S127', 'Vanderka', '3, avenue des Roses', 'Namur', 'C1', -4580),
client('S712', 'Guillaume', '14a, chemin des Roses', 'Paris', 'B1', 0),
client('F011', 'PONCELET', '17, Clos des Erables', 'Toulouse', 'B2', 0),
client('K729', 'NEUMAN', '40, rue Bransart', 'Toulouse', 'B2', 0),

commande(30178, 'K111', '2008-12-22'),
commande(30179, 'C400', '2008-12-22'),
commande(30182, 'S127', '2008-12-23'),
commande(30184, 'C400', '2008-12-23'),
commande(30185, 'F011', '2009-01-02'),
commande(30186, 'C400', '2009-01-02'),
commande(30188, 'B512', '2009-01-02'),

detail(30178, 'CS464', 25),
detail(30179, 'CS262', 60),
detail(30179, 'PA60', 20),
detail(30182, 'PA60', 30),
detail(30184, 'CS464', 120),
detail(30185, 'CS464', 260),
detail(30185, 'PA60', 15),
detail(30185, 'PS222', 600),
detail(30186, 'PA45', 3),
detail(30188, 'CS464', 180),
detail(30188, 'PA60', 70),
detail(30188, 'PH222', 92),
detail(30184, 'PA45', 20),
detail(30188, 'PA45', 22),

produit('CS262', 'Chev. Sapin 200*6*2', 75, 45),
produit('CS264', 'Chev. Sapin 200*6*4', 120, 2690),
produit('CS464', 'Chev. Sapin 400*6*4', 220, 450),
produit('PA60', 'Pointe Acier 60 (10K)', 95, 134),
produit('PS222', 'PL. Sapin 200*20*2', 185, 1220),
produit('PA45', 'POINTE ACIER 45 (20K)', 105, 580),
produit('PH222', 'PL. HETRE 200x20x2', 185, 1220),

table_db(client),
table_db(commande),
table_db(detail),
table_db(produit)
],
set_prolog_flag(answer_write_options,[max_depth(0)]),
serial_assert(DB), !.

serial_assert([]).
serial_assert([Cl|L]) :- assert(Cl), serial_assert(L).

%Reset all the data from scratch
reset :- findall(X, table_db(X), T),
			serial_drop(T),
			load, !.

serial_drop([]).
serial_drop([T|L]) :- dropE(T), serial_drop(L).

%Adding a csv to the existing database
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
					maplist(assert, Rows),
					assert(table_db(AtomName)), !.

add_var_knowledge([], _, _).
add_var_knowledge([H|T], Table, K) :- atom_string(HAtom, H),
										assert(var(Table, K, HAtom)),
										K1 is K+1,
										add_var_knowledge(T, Table, K1).

%SQL parser
parse(String, T, V) :- string_codes(String, Codes), phrase(expression(T, V), Codes).

%Expression SELECT . FROM .
expression(select(var(A), table(T)), V) --> select, white,
											attributserie(A), white,
											from, white,
											name(T), soft_white,
											semicolon,
											{selectE(A, T, V)}, !.

%Expression CREATE TABLE . (.) ;
expression(create_table(table(T), var(A)), _) --> create, white,
													table, white,
													name(T), white, leftb, soft_white,
													attributserie(A), soft_white, rightb, soft_white,
													semicolon,
													{createE(T, A)}, !.

%Expression INSERT INTO . VALUES (.) ;
expression(insert(table(T), val(A)), _) --> insert, white,
											into, white,
											name(T), white,
											values, white, leftb, soft_white,
											valueserie(A), soft_white, rightb, soft_white,
											semicolon,
											{insertE(T, A)}, !.

%Expression DROP TABLE . ;
expression(drop(table(T)), _) --> drop, white,
									table, white,
									name(T), soft_white,
									semicolon,
									{dropE(T)}, !.

%Expression SELECT . FROM . WHERE . ;
expression(select_where(var(A), table(T), conditions(C)), V) --> select, white,
																attributserie(A), white,
																from, white,
																name(T), white,
																where, white,
																conditionalserie(C), soft_white,
																semicolon,
																{select_whereE(A, T, C, V)}, !.

%Expression DELETE FROM . WHERE . ;
expression(delete(table(T), conditions(C)), _) --> delete, white,
													from, white,
													name(T), white,
													where, white,
													conditionalserie(C), soft_white,
													semicolon,
													{deleteE(T, C)}, !.

%Expression UPDATE . SET . WHERE . ;
expression(update(table(T), modifications(S), conditions(C)), _) --> update, white,
																		name(T), white,
																		set, white,
																		setserie(S), white,
																		where, white,
																		conditionalserie(C), soft_white,
																		semicolon,
																		{updateE(T, S, C)}, !.

%Expression SELECT . FROM . CROSS JOIN . ;
expression(cross_join(table_var(TA), join(table1(T1), table2(T2))), V) --> select, white,
																		tableattributserie(TA), white,
																		from, white,
																		name(T1), white,
																		cross, white,
																		join, white,
																		name(T2), soft_white,
																		semicolon,
																		{cross_joinE(TA, T1, T2, V)}, !.

%Expression SELECT . FROM . INNER JOIN . ON . ;
expression(inner_join(table_var(TA), join(table1(T1), table2(T2)), on(attr1(A1), attr2(A2))), V) --> select, white,
																										tableattributserie(TA), white,
																										from, white,
																										name(T1), white,
																										inner, white,
																										join, white,
																										name(T2), white,
																										on, white,
																										tableattribut(A1), soft_white,
																										operator(op(=)), soft_white,
																										tableattribut(A2), soft_white,
																										semicolon,
																										{inner_joinE(TA, T1, T2, A1, A2, V)}, !.

%Grammar: words and characters
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

join --> [H0], {char_code("J", H0)},
			[H1], {char_code("O", H1)},
			[H2], {char_code("I", H2)},
			[H3], {char_code("N", H3)}.

cross --> [H0], {char_code("C", H0)},
			[H1], {char_code("R", H1)},
			[H2], {char_code("O", H2)},
			[H3], {char_code("S", H3)},
			[H4], {char_code("S", H4)}.

inner --> [H0], {char_code("I", H0)},
			[H1], {char_code("N", H1)},
			[H2], {char_code("N", H2)},
			[H3], {char_code("E", H3)},
			[H4], {char_code("R", H4)}.

on --> [H0], {char_code("O", H0)},
		[H1], {char_code("N", H1)}.

only_white --> [H], {char_code(" ", H)}.

star --> [H], {char_code("*", H)}.

comma --> [H], {char_code(",", H)}.

semicolon --> [H], {char_code(";", H)}.

quotation --> [H], {char_code("\"", H)}.

leftb --> [H], {char_code("(", H)}.

rightb --> [H], {char_code(")", H)}.

dash --> [H], {char_code("-", H)}.

dot --> [H], {char_code(".", H)}.

underscore --> [H], {char_code("_", H)}.

%Grammar
white --> only_white, soft_white.

soft_white --> [].

soft_white --> white, soft_white.

name(N) --> alpha(H), morealphas(T), {string_chars(N1, [H|T]), atom_string(N, N1)}.

name(N) --> digit(H), morealphas(T), {string_chars(N1, [H|T]), atom_string(N, N1)}.

alpha(C) --> [H], {char_code(C,H), char_type(C,alpha)}.

morealphas([]) --> [].

morealphas([H|T]) --> alpha(H), morealphas(T).

morealphas([H|T]) --> digit(H), morealphas(T).

morealphas(['_'|T]) --> underscore, morealphas(T).

morealphas(['-'|T]) --> dash, morealphas(T).

number(N) --> digit(H), moredigits(T), {number_chars(N, [H|T])}.

digit(C) --> [H], {char_code(C,H), char_type(C,digit)}.

moredigits([]) --> [].

moredigits([H|T]) --> digit(H), moredigits(T).

attributserie([]) --> [].

attributserie(all) --> star.

attributserie([H]) --> name(H).

attributserie([H|T]) --> name(H), soft_white, comma, soft_white, attributserie(T).

tableattribut(table_attr(T, A)) --> name(T), dot, name(A).

tableattribut(table_attr(T, A)) --> name(A), {var(T, _, A)}.

tableattributserie([]) --> [].

tableattributserie(all) --> star.

tableattributserie([H]) --> tableattribut(H).

tableattributserie([H|T]) --> tableattribut(H), soft_white, comma, soft_white, tableattributserie(T).

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

operator(op(<=)) --> [H0], {char_code("<", H0)},
						[H1], {char_code("=", H1)}.

operator(op(>=)) --> [H0], {char_code(">", H0)},
						[H1], {char_code("=", H1)}.
setserie([]) --> [].

setserie([H]) --> set(H).

setserie([H|T]) --> set(H), soft_white, comma, soft_white, setserie(T).

set(set(N, V)) --> name(N), soft_white, operator(op(=)), soft_white, value(V).

value(num(A)) --> number(A).

value(num(-A)) --> dash, number(A).

value(string(A)) --> quotation, name(A), quotation.

valueserie([]) --> [].

valueserie([H]) --> value(H).

valueserie([H|T]) --> value(H), soft_white, comma, soft_white, valueserie(T).


%Calculation of the queries%

%Calculating select all
selectE(all, X, V) :- arity(X, Ar), gen_empty_list(L, Ar), findall(L, apply(X, L), V).

%Processing select with only some columns
selectE(L, X, V) :- var_index(L, I, X),
					sort(0, @<, I, IS),
					selectE(all, X, VA),
					keep_var_all(IS, VA, V).

%Calculating select where
select_whereE(all, T, C, V) :- selectE(all, T, VTot),
								filter(T, VTot, C, V).

select_whereE(A, T, C, V) :- select_whereE(all, T, C, VF),
								var_index(A, I, T),
								sort(0, @<, I, IS),
								keep_var_all(IS, VF, V).

%Example of condition tree
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
satisfy(Elem, <=, num(Val)) :- Elem =< Val.
satisfy(Elem, >=, num(Val)) :- Elem >= Val.
satisfy(Elem, =, num(Val)) :- Elem = Val.
satisfy(Elem, <>, num(Val)) :- Elem \= Val.

satisfy(Elem, =, string(Val)) :- Elem = Val.
satisfy(Elem, <>, string(Val)) :- Elem \= Val.

%Calculating create table
createE(T, A) :- neg(table_db(T)),
					length(A ,S),
					assert(arity(T, S)),
					add_columns(T, A, 0),
					assert(table_db(T)).

add_columns(_, [], _).
add_columns(T,[AH|AL], K) :- assert(var(T, K, AH)), K1 is K+1, add_columns(T, AL, K1).

neg(X) :- X, !, fail.
neg(_).

%Calculating insert into table
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

%Calculating drop table
dropE(T) :- arity(T, A),
			retractall(var(T, _, _)),
			retract(arity(T, A)),
			gen_empty_list(L, A),
			Cl =.. [T|L],
			retractall(Cl),
			Cl2 =..[table_db,T],
			retract(Cl2).

%Calculating delete
deleteE(T, C) :- select_whereE(all, T, C, Res), remove(T, Res).

remove(_, []).
remove(T, [R|Res]) :- Cl =.. [T|R], retract(Cl), remove(T, Res).

%Calculating update
updateE(Table, Sets, Conditions) :- select_whereE(all, Table, Conditions, ResultTot),
									apply_per_row(ResultTot, Sets, Table).

apply_per_row([], _, _).
apply_per_row([Old|L], S, T) :- arity(T, Ar),
								gen_empty_list(New, Ar),
								fill_with_sets(New, S, T),
								complete_unchanged(New, Old),
								assert_retract(New, Old, T),
								apply_per_row(L, S, T).

%Fill 'New' with the data into '[set(Var,Val)|S]'
fill_with_sets(_, [], _).
fill_with_sets(New, [set(Var, Val)|S], Table) :- var_index([Var], [Ind], Table),
													fill(Ind, New, Val),
													fill_with_sets(New, S, Table).

fill(Ind, New, num(Val)) :- nth0(Ind, New, Val).
fill(Ind, New, string(Val)) :- nth0(Ind, New, Val).

%Fill the empty elements of first arg with the elements of the second arg
complete_unchanged([], []).
complete_unchanged([Old|N], [Old|O]) :- complete_unchanged(N, O).
complete_unchanged([_|N], [_|O]) :- complete_unchanged(N, O).

%Assert New and retract Old
assert_retract(New, Old, Table) :- NewCl =.. [Table|New],
									OldCl =.. [Table|Old],
									assert(NewCl),
									retract(OldCl).

%Calculating cross join
cross_joinE(all, Table1, Table2, Result) :- selectE(all, Table1, Res1),
											selectE(all, Table2, Res2),
											cross(Res1, Res2, Result).

cross_joinE(A, Table1, Table2, Result) :- cross_joinE(all, Table1, Table2, ResTot),
											extract_attr(Table1, A, I1),
											sort(0, @<, I1, Ind1),
											extract_attr(Table2, A, I2),
											sort(0, @<, I2, Ind2),
											arity(Table1, Pad),
											add_to_list(Ind2, Pad, Ind2Pad),
											append(Ind1, Ind2Pad, Indices),
											keep_var_all(Indices, ResTot, Result).

%Cross two table
cross([], _, []).
cross([H|L1], L2, R) :- sub_cross(H, L2, RH),
								cross(L1, L2, RP),
								append(RH, RP, R).

sub_cross(_, [], []).
sub_cross(E, [H|L], [RH|R]) :- append(E, H, RH),
								sub_cross(E, L, R).

%Return a table with the indexes of the attributes inside the second argument related to 'Table'
extract_attr(_, [], []).
extract_attr(Table, [table_attr(Table, A)|Attrs], [R|Res]) :- var(Table, R, A),
																extract_attr(Table, Attrs, Res).
extract_attr(Table, [table_attr(_, _)|Attrs], R) :- extract_attr(Table, Attrs, R).

%Apply the addition of a number on all the elements of a list.
add_to_list([], _, []).
add_to_list([H|L], N, [HR|R]) :- HR is H + N, add_to_list(L, N, R).

%Calculating inner join
inner_joinE(A, Table1, Table2, table_attr(Table2, Attr2), table_attr(Table1, Attr1), Result) :- inner_joinE(A, Table1, Table2, table_attr(Table1, Attr1), table_attr(Table2, Attr2), Result).

inner_joinE(all, Table1, Table2, table_attr(Table1, Attr1), table_attr(Table2, Attr2), Result) :- cross_joinE(all, Table1, Table2, ResTot),
																									arity(Table1, Pad),
																									var(Table1, Ind1, Attr1),
																									var(Table2, IndPre2, Attr2),
																									Ind2 is IndPre2 + Pad,
																									filter_inner(ResTot, Ind1, Ind2, Result).

inner_joinE(A, Table1, Table2, table_attr(Table1, Attr1), table_attr(Table2, Attr2), Result) :- inner_joinE(all, Table1, Table2, table_attr(Table1, Attr1), table_attr(Table2, Attr2), ResTot),
																								extract_attr(Table1, A, I1),
																								sort(0, @<, I1, Ind1),
																								extract_attr(Table2, A, I2),
																								sort(0, @<, I2, Ind2),
																								arity(Table1, Pad),
																								add_to_list(Ind2, Pad, Ind2Pad),
																								append(Ind1, Ind2Pad, Indices),
																								keep_var_all(Indices, ResTot, Result).

filter_inner([], _, _, []).
filter_inner([H|T], Ind1, Ind2, [H|R]) :- nth0(Ind1, H, E),
											nth0(Ind2, H, E),
											filter_inner(T, Ind1, Ind2, R).
filter_inner([_|T], Ind1, Ind2, R) :- filter_inner(T, Ind1, Ind2, R).


%Helpful functions%

%Function that remove the columns that we don't want in the result
keep_var_all(_, [], []).
keep_var_all(I, [VH|VL], [Rs|R]) :- keep_var(I, VH, 0, Rs), keep_var_all(I, VL, R).

keep_var([], _, _, []).
keep_var([IH|IL], [VH|VL], K, [VH|R]) :- IH =:= K, K1 is K+1, keep_var(IL, VL, K1, R).
keep_var(I, [_|VL], K, R) :- K1 is K+1, keep_var(I, VL, K1, R).

%Function that match the name of a columns with the index of the columns regarding the table
var_index([], [], _).
var_index([VH|VL], [IH|IL], T) :- var(T, IH, VH), var_index(VL, IL, T).


%Generate an uninitialized list of C elements
gen_empty_list([], 0).
gen_empty_list([_|L], C) :- C2 is C-1, gen_empty_list(L, C2).







