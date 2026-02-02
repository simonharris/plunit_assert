% Playground: A minimal text adventure game for Prolog
% There are two rooms: a and b. Only b contains the treasure.
% The user starts in a random room and can move or pick up the treasure.

:- module(playground, [play/0]).

:- dynamic current_room/1.

room(a).
room(b).
treasure_room(b).

description(a, 'You are in room A. There is nothing here.').
description(b, 'You are in room B. There is a treasure here!').

default_start_room :-
    random_between(0, 1, N),
    (N =:= 0 -> Room = a ; Room = b),
    retractall(current_room(_)),
    asserta(current_room(Room)).

play :-
    default_start_room,
    write('Welcome to Playground!'), nl,
    game_loop.

game_loop :-
    current_room(Room),
    description(Room, Desc),
    write(Desc), nl,
    write('What do you want to do? (move a | move b | pick treasure | look | where | quit)'), nl,
    read_line_to_string(user_input, Input),
    handle_input(Input).

handle_input("move a") :-
    retractall(current_room(_)),
    asserta(current_room(a)),
    write('You move to room A.'), nl,
    game_loop.
handle_input("move b") :-
    retractall(current_room(_)),
    asserta(current_room(b)),
    write('You move to room B.'), nl,
    game_loop.
handle_input("pick treasure") :-
    current_room(Room),
    (treasure_room(Room) ->
        write('Congratulations! You found the treasure and win!'), nl
    ;
        write('No treasure here. Try another room.'), nl,
        game_loop
    ).
handle_input("look") :-
    current_room(Room),
    (treasure_room(Room) ->
        write('You look around and see a treasure here!'), nl
    ;
        write('You look around. There is nothing of interest.'), nl
    ),
    game_loop.
handle_input("where") :-
    current_room(Room),
    format('You are currently in room ~w.~n', [Room]),
    game_loop.
handle_input("quit") :-
    write('Thanks for playing! Goodbye.'), nl.
handle_input(_) :-
    write('Unknown command. Try again.'), nl,
    game_loop.
