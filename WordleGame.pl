main:-
   write('Welcome to Pro-Wordle!'),nl,
   write('----------------------'),nl,nl,
   build_kb,nl,
   write('Done building the words database...'),
   play.
 
build_kb:-
   write('Please enter a word and its category on separate lines:'),nl,
   read(X),
   (X\=done,
   read(Y),nl,
   assert(word(X,Y)),
   build_kb);
   ( X=done ,!).

is_category(C):-
    word(_,C).
	
categories(L):-
   setof(C,is_category(C),L1),
   remove_dup(L1,L).
remove_dup([],[]).
remove_dup([H|T],L):-
    member(H,T),!,
	remove_dup(T,L).
remove_dup([H|T],[H|T1]):-
	remove_dup(T,T1).
   
pick_word(W,L,C):-
  available_length(L),!,
  word(W,C).
  
available_length(L):-
    word(W,_),
	atom_chars(W,List),
	length(List,L).
correct_letters(L1,[],[]).
correct_letters(L1,[H2|T2],[H2|T]):-
    member(H2,L1),
	correct_letters(T2,L1,T).
correct_letters(L1,[H2|T2],CL):-
    \+member(H2,L1),
	correct_letters(T2,L1,CL). 
correct_positions(_,[],[]):-!.
correct_positions([],_,[]).
correct_positions([H|T1],[H|T2],[H|T]):-
    correct_positions(T1,T2,T).
correct_positions([H1|T1],[H2|T2],P):-
    H1\=H2,
    correct_positions(T1,T2,P).
    
    
play:-
   write('The available categories are:'),
   categories(L),
   write(L),nl,
   
   help1(Y),
   help2(Z),
   Z1 is Z+1,
   write('Game started. You have'),write(Z1),write(' guesses.'),nl,nl,
   help3(Z,Z1,Y).
   
   
   
   
help1(Y):-
   write('Choose a category:'),
   read(Y),
   (\+is_category(Y),
   write('This category does not exist.'),nl,
   help1(Y));
   is_category(Y).
   
help2(Z):-
   write('Choose a length:'),
   setof(Lengths,available_length(Lengths),L),
   read(Z), 
   (\+member(Z,Lengths),
   write('There are no words of this length.'),nl,
   help2(Z));
   member(Z,Lengths).
   
   
help3(Z,0,C):- write(' You lost !'),!.

help3(Z,Z1,C):-
   write('Enter a word composed of '),write(Z),write(' letters:'),nl,
   read(G),
   atom_chars(G,G1),
   help4(G1,Z1,Z,C).
   
help4(G1,Z1,Z,C):-
   \+length(G1,Z),
   write('Word is not composed of '),
   write(Z),
   write(' letters. Try again.'),nl,
   write('Remaining Guesses are '),
   write(Z1),nl,
   help3(Z,Z1,C).

help4(G1,Z1,Z,C):-   
   length(G1,Z),  
   pick_word(W,Z,C),
   atom_chars(W,W1),
   
   (W1=G1,
   write('You Won!'));
   
  ( W1\=G1, correct_letters(G1,W1,R) ,
   write('Correct letters are:  '),write(R),nl,
   correct_positions(G1,W1,R1), write('Correct letters in correct positions are:  '), write(R1),nl,
   Z2 is Z1-1 ,
   write('Remaining Guesses are '),write(Z2), help3(Z,Z2,C)).
   
   
  
 
       

















