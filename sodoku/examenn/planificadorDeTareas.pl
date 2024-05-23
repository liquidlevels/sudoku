:- dynamic tarea/4.

% tarea(Id, Nombre, Duracion, Deadline)
tarea(1, 'Comprar alimentos', 2, 10).
tarea(2, 'Hacer ejercicio', 1, 8).
tarea(3, 'Leer un libro', 1, 14).
tarea(4, 'Trabajar en el proyecto', 3, 18).

% horario(Hora, Ocupado)
:- dynamic horario/2.

% Inicializar horarios vacíos de 6 AM a 6 PM (6 - 18)
inicializar_horarios :-
    between(6, 18, Hora),
    assertz(horario(Hora, libre)),
    fail.
inicializar_horarios.

% Asignar tareas a horarios disponibles
asignar_tareas :-
    findall(Id, tarea(Id, _, _, _), Tareas),
    asignar_tareas(Tareas).

asignar_tareas([]).
asignar_tareas([Id | Rest]) :-
    tarea(Id, Nombre, Duracion, Deadline),
    asignar_tarea(Nombre, Duracion, Deadline),
    asignar_tareas(Rest).

asignar_tarea(_, 0, _) :- !.
asignar_tarea(Nombre, Duracion, Deadline) :-
    horario(Hora, libre),
    Hora =< Deadline,
    retract(horario(Hora, libre)),
    assertz(horario(Hora, ocupado(Nombre))),
    DuracionRestante is Duracion - 1,
    asignar_tarea(Nombre, DuracionRestante, Deadline).

% Mostrar el horario de manera amigable
mostrar_horario :-
    write('Horario de tareas:'), nl,
    between(6, 18, Hora),
    mostrar_hora(Hora),
    fail.
mostrar_horario.

mostrar_hora(Hora) :-
    horario(Hora, libre),
    format('~w:00 - Libre~n', [Hora]).
mostrar_hora(Hora) :-
    horario(Hora, ocupado(Tarea)),
    format('~w:00 - ~w~n', [Hora, Tarea]).

% Limpiar horarios y tareas
limpiar :-
    retractall(horario(_, _)),
    retractall(tarea(_, _, _, _)).

% Ejecución del planificador
planificar :-
    limpiar,
    inicializar_horarios,
    asignar_tareas,
    mostrar_horario.

