% Author:
% Date: 3/22/2015

?- set_prolog_stack(global, limit(3*10**9)).


branch_name(0,h).
branch_name(S,N):- HS is S*2+1,
atomic_list_concat([c,S,h,HS],N).

straight_chain_alkene(N, A):-  sca(N, 1, A).


check_validity(Li,I,L1):-
                nth0(I,Li,In),
                max_branch([In],Num),
                 Num=<I,
                 R is L1-I,
                Num=<R,
                I1 is I+1,
                (check_validity(Li,I1,L1);
                (L1=I)).



sca(1, 1, [carb(h,h,h,h)]).
sca(N, C, A):-
                N>1,
                (
                (C = 1,append([carb(h,h,h,c)],R,A), C1 is C+1, sca(N,C1,R));
                (C>1,N>C,append([carb(c,h,h,c)],R,A), C1 is C+1, sca(N,C1,R));
                (C=N, A =[carb(c,h,h,h)])
                ).
 seqA(X,Y,X) :- X=<Y.
 seqA(X,Y,Z) :- X<Y,
                   X1 is X + 1,
                   seqA(X1,Y,Z).
branched_alkane(N,BA):-
                straight_chain_alkene(N,BC),
                branchedhelper(BC,BA,N).
branchedhelper(BC,BA,SN):-
                length(BC,N),
                N>3,
                N2 is N - 2,
                seqA(1,N2,Z),
                Z1 is Z + 1,
                  MaxCL is (N - 1 ) / 3,
                seqA(1,MaxCL,CL),
                Z2 is Z1+CL,
                Z >= CL,
                S2 is N - Z2,
                S2 >=CL,
                nth0(Z, BC, Elem1) ,
                nth0(Z1, BC, Elem2) ,

                split2(BC,0,Z,0,LP1),
                split2(BC,Z2,N,Z2,LP2),
                merge2(Elem1,Elem2,CB,CL,T),
		Z >= T, S2>=T,
                append(LP1,CB,LC1),
                append(LC1,LP2,BT),
                RR1 is (N - Z2) + Z ,
                total_branch_number2(BT,F),J is (N - Z2) + Z + F, J is SN-1,check_validity(BT,1,RR1),
                (
                BA=BT;

                branchedhelper(BT,BA,SN)).


split2(_,_,I2,N,L2) :- N = I2,L2 =[].
split2(L,I,I2,N,L2) :-
                        N < I2,N>=I,
                        nth0(N, L, Elem1),
                        N1 is N + 1,
                        split2(L,I,I2,N1,L3),
                        append([Elem1], L3, L2).
merge2(carb(A,B,C,D),carb(E,F,G,H),R,CL,T) :-
                                         branch_name(CL,N),
                                         (
                                        (H=h,append(carb(A,B,C,D),carb(E,F,G,H),R),T is CL);
                                        (H\=h,B=C,C=F,F=G,G=h,R=[carb(c,N,h,c)],T is CL);
                                        (H\=h,B\=h, F=h,C=h,N=B, G=h, R=[carb(c,B,N,c)],T is CL);
                                         ( H\=h,B\=h, F=h,C=h,N\=B,branch_number(N,X1),branch_number(B,X2),X2>X1, G=h, R=[carb(c,N,B,c)] ,T is X2)).
total_branch_number2([],0)   .
total_branch_number2([H|T],Z) :- total_branch_number(H,Z1),    total_branch_number2(T,Y),Z is Y + Z1.
total_branch_number(carb(_,B,C,_),T) :-  branch_number(B,Z2),branch_number(C,Z3),T is Z2+Z3.
max_branch([carb(_,B,C,_)],T) :-  branch_number(B,Z2),branch_number(C,Z3),T is max(Z2,Z3).
branch_number(h,0).
branch_number(c,1).
branch_number(X,Z) :- sub_atom(X,1,1,_,Y),atom_number(Y,Z) .

mirror(L1, L2):-
                length(L1,L),
                Ll1 is L-1,
                split2(L1,1,Ll1,1,LT),
                reverse(LT,LTR),
                append([carb(h,h,h,c)],LTR, LT2),
                append(LT2,[carb(c,h,h,h)],L2).



isomers(N,X):-
                straight_chain_alkene(N,T),

                 setof(BA, branched_alkane(N,BA), Set),
                   isomers_helper(N,X1,1,Set)  ,
                   X =[T|X1].
isomers_helper(_,X,S,Set) :-length(Set,N), S >= N, X = Set.

isomers_helper(N,X,S,Set) :-
                           length(Set,N2),
                            S<N2,
                nth0(S,Set,Ind),
                mirror(Ind, L2),

                (( (\+member(L2,Set);(L2 == Ind)),  I1 is S + 1, isomers_helper(N2,X,I1,Set));
                ( \+(L2 == Ind),member(L2,Set), nth0(_,Set,L2,B),  I1 is S + 1, isomers_helper(N2,X,I1,B))).







