:- dynamic tareas/1.
:- dynamic horario/2.
:- use_module(library(readutil)).
:- use_module(library(http/json)).

% Inicializar horarios vacíos de 6 AM a 10 PM (6 - 22)
inicializar_horarios :-
    forall(between(6, 21, Hora), assertz(horario(Hora, libre))).

% Asignar tareas a horarios disponibles
asignar_tareas :-
    tareas(Tareas),
    maplist(asignar_tarea_por_id, Tareas),
    mostrar_horario.

asignar_tarea_por_id(tarea(Id, Nombre, Duracion, HoraLimite, HoraInicio)) :-
    ( HoraInicio = -1 ->
        asignar_tarea(Nombre, Duracion, HoraLimite)
    ;   asignar_tarea_desde_hora(Nombre, Duracion, HoraLimite, HoraInicio)
    ).

asignar_tarea(_, 0, _) :- !.
asignar_tarea(Nombre, Duracion, HoraLimite) :-
    horario(Hora, libre),
    Hora < HoraLimite,
    retract(horario(Hora, libre)),
    assertz(horario(Hora, ocupado(Nombre))),
    DuracionRestante is Duracion - 1,
    HoraSiguiente is Hora + 1,
    asignar_tarea(Nombre, DuracionRestante, HoraLimite).
asignar_tarea(Nombre, _, HoraLimite) :-
    format('Error: no se pudo asignar la tarea "~w" antes de la hora límite ~w:00 debido a un conflicto de horario.~n', [Nombre, HoraLimite]), !.

asignar_tarea_desde_hora(_, 0, _, _) :- !.
asignar_tarea_desde_hora(Nombre, Duracion, HoraLimite, HoraInicio) :-
    horario(HoraInicio, libre),
    HoraInicio < HoraLimite,
    retract(horario(HoraInicio, libre)),
    assertz(horario(HoraInicio, ocupado(Nombre))),
    DuracionRestante is Duracion - 1,
    NuevaHoraInicio is HoraInicio + 1,
    asignar_tarea_desde_hora(Nombre, DuracionRestante, HoraLimite, NuevaHoraInicio).
asignar_tarea_desde_hora(Nombre, _, HoraLimite, _) :-
    format('Error: no se pudo asignar la tarea "~w" antes de la hora límite ~w:00 debido a un conflicto de horario.~n', [Nombre, HoraLimite]), !.

% Mostrar el horario de manera amigable
mostrar_horario :-
    write('Horario de tareas:'), nl,
    forall(between(6, 21, Hora), mostrar_hora(Hora)).

mostrar_hora(Hora) :-
    HoraFin is Hora + 1,
    ( horario(Hora, libre) ->
        format('~|~`0t~d~2+:00 - ~|~`0t~d~2+:00: Libre~n', [Hora, HoraFin])
    ; horario(Hora, ocupado(Tarea)),
        format('~|~`0t~d~2+:00 - ~|~`0t~d~2+:00: ~w~n', [Hora, HoraFin, Tarea])
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

% Preguntar si se desea iniciar la tarea a partir de una hora específica
preguntar_inicio_especifico(HoraInicio) :-
    leer_string('¿Desea iniciar la tarea a partir de una hora específica? (si/no): ', Respuesta),
    (   Respuesta = "si" ->
        leer_numero('Ingrese la hora de inicio (en formato 24 horas): ', HoraInicio)
    ;   HoraInicio = -1 % Indica que no hay hora de inicio específica
    ).

% Asignar tarea con input del usuario
agregar_tarea :-
    leer_numero('Ingrese el ID de la tarea: ', Id),
    leer_string('Ingrese el nombre de la tarea: ', Nombre),
    leer_numero('Ingrese la duración de la tarea en horas: ', Duracion),
    leer_numero('Ingrese la hora límite de la tarea (en formato 24 horas): ', HoraLimite),
    preguntar_inicio_especifico(HoraInicio),
    (   retract(tareas(Ts)) -> true ; Ts = [] ),
    assertz(tareas([tarea(Id, Nombre, Duracion, HoraLimite, HoraInicio)|Ts])),
    write('Tarea agregada exitosamente.'), nl,
    guardar_tareas,
    mostrar_horario.

% Convertir una tarea a una estructura adecuada para JSON
tarea_json(tarea(Id, Nombre, Duracion, HoraLimite, HoraInicio), json([id=Id, nombre=Nombre, duracion=Duracion, hora_limite=HoraLimite, hora_inicio=HoraInicio])).

% Mostrar tareas agregadas
mostrar_tareas :-
    tareas(Tareas),
    (   Tareas \= []
    ->  write('Tareas agregadas:'), nl,
        forall(member(tarea(Id, Nombre, Duracion, HoraLimite, HoraInicio), Tareas),
               ( HoraInicio = -1 ->
                 format('ID: ~w, Nombre: ~w, Duración: ~w horas, Hora límite: ~|~`0t~d~2+:00, Hora de inicio: No especificada~n', [Id, Nombre, Duracion, HoraLimite])
               ; format('ID: ~w, Nombre: ~w, Duración: ~w horas, Hora límite: ~|~`0t~d~2+:00, Hora de inicio: ~|~`0t~d~2+:00~n', [Id, Nombre, Duracion, HoraLimite, HoraInicio])
               ))
    ;   write('No hay tareas agregadas.'), nl
    ).

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

json_to_tarea(json([id=Id, nombre=Nombre, duracion=Duracion, hora_limite=HoraLimite, hora_inicio=HoraInicio]), tarea(Id, Nombre, Duracion, HoraLimite, HoraInicio)).

% Eliminar todo el contenido del archivo tareas.json
eliminar_tareas :-
    open('tareas.json', write, Stream),
    write(Stream, '[]'),
    close(Stream),
    retractall(tareas(_)),
    assertz(tareas([])),
    write('Todas las tareas han sido eliminadas.'), nl.

% Eliminar una tarea específica por su ID
eliminar_tarea :-
    leer_numero('Ingrese el id de la tarea a eliminar: ', Id),
    tareas(Tareas),
    select(tarea(Id, Nombre, Duracion, HoraLimite, HoraInicio), Tareas, NuevasTareas),
    retract(tareas(Tareas)),
    assertz(tareas(NuevasTareas)),
    guardar_tareas,
    format('Tarea con id ~w ha sido eliminada.~n', [Id]).

% Ejecución del planificador
planificar :-
    limpiar,
    inicializar_horarios,
    cargar_tareas,
    asignar_tareas.

% Inicializar el sistema al inicio
inicializar :-
    cargar_tareas.

