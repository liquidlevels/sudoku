:- dynamic tareas/1.
:- dynamic horario/2.
:- use_module(library(readutil)).
:- use_module(library(http/json)).

% Inicializar horarios vacíos de 6 AM a 6 PM (6 - 18)
inicializar_horarios :-
    forall(between(6, 22, Hora), assertz(horario(Hora, libre))).

% Asignar tareas a horarios disponibles
asignar_tareas :-
    tareas(Tareas),
    maplist(asignar_tarea_por_id, Tareas).

asignar_tarea_por_id(tarea(Id, Nombre, Duracion, Deadline)) :-
    asignar_tarea(Nombre, Duracion, Deadline).

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
    forall(between(6, 22, Hora), mostrar_hora(Hora)).

mostrar_hora(Hora) :-
    ( horario(Hora, libre) ->
        format('~w:00 - Libre~n', [Hora])
    ; horario(Hora, ocupado(Tarea)),
        format('~w:00 - ~w~n', [Hora, Tarea])
    ).

% Limpiar horarios y tareas
limpiar :-
    retractall(horario(_, _)),
    retractall(tareas(_)).

% Leer una línea de entrada del usuario y convertirla a un número
leer_numero(Prompt, Numero) :-
    write(Prompt),
    read_line_to_string(user_input, Input),
    (   number_string(Numero, Input) ->
        true
    ;   write('Entrada no válida. Por favor, ingrese un número.'), nl,
        leer_numero(Prompt, Numero)
    ).

% Leer una línea de entrada del usuario
leer_string(Prompt, String) :-
    write(Prompt),
    read_line_to_string(user_input, String).

% Agregar tarea con input del usuario
agregar_tarea :-
    leer_numero('Ingrese el ID de la tarea: ', Id),
    leer_string('Ingrese el nombre de la tarea: ', Nombre),
    leer_numero('Ingrese la duración de la tarea en horas: ', Duracion),
    leer_numero('Ingrese la deadline de la tarea (hora límite): ', Deadline),
    (   retract(tareas(Ts)) -> true ; Ts = [] ),
    assertz(tareas([tarea(Id, Nombre, Duracion, Deadline)|Ts])),
    write('Tarea agregada exitosamente.'), nl,
    guardar_tareas.

% Convertir una tarea a una estructura adecuada para JSON
tarea_json(tarea(Id, Nombre, Duracion, Deadline), json([id=Id, nombre=Nombre, duracion=Duracion, deadline=Deadline])).

% Guardar tareas en un archivo
guardar_tareas :-
    tareas(Tareas),
    maplist(tarea_json, Tareas, TareasJSON),
    open('tareas.json', write, Stream),
    json_write(Stream, TareasJSON, [width(128)]),
    close(Stream).

% Cargar tareas desde un archivo
cargar_tareas :-
    (   exists_file('tareas.json')
    ->  open('tareas.json', read, Stream),
        json_read(Stream, TareasJSON),
        close(Stream),
        maplist(json_to_tarea, TareasJSON, Tareas),
        assertz(tareas(Tareas)),
        write('Tareas cargadas exitosamente.'), nl
    ;   write('No se encontró el archivo tareas.json.'), nl,
        assertz(tareas([]))
    ).

json_to_tarea(json([id=Id, nombre=Nombre, duracion=Duracion, deadline=Deadline]), tarea(Id, Nombre, Duracion, Deadline)).

% Mostrar tareas agregadas
mostrar_tareas :-
    tareas(Tareas),
    (   Tareas \= []
    ->  write('Tareas agregadas:'), nl,
        forall(member(tarea(Id, Nombre, Duracion, Deadline), Tareas),
               format('ID: ~w, Nombre: ~w, Duración: ~w horas, Deadline: ~w:00~n', [Id, Nombre, Duracion, Deadline]))
    ;   write('No hay tareas agregadas.'), nl
    ).

% Ejecución del planificador
planificar :-
    limpiar,
    inicializar_horarios,
    cargar_tareas,
    asignar_tareas,
    mostrar_horario.

% Inicializar el sistema al inicio
inicializar :-
    cargar_tareas.

