%PERSONAJES
% personaje(Nombre, Velocidad, Alcance, Fuerza, Dificultad, Tipo, Peso, Recuperacion, Proyectiles, Aereo)
%Velocidad: muy_alta, alta, media, baja
%Alcance: muy_alta, alta, media, baja
%Fuerza: muy_alta, alta, media, baja
%Dificultad: facil, media, avanzada 
%Tipo: zoner, rushdown, brawler, swordie, grappler
%Peso: ligero, medio, pesado
%Recuperacion: buena, media, mala
%Proyectiles: si, no
%Aereo: si, no

:- set_prolog_flag(encoding, utf8).

personaje(mario, media, media, media, facil, brawler, medio, buena, si, si).
personaje(donkey_kong, baja, media, muy_alta, media, grappler, pesado, mala, no, no).
personaje(link, media, alta, alta, media, zoner, medio, media, si, si).
personaje(samus, baja, alta, alta, media, zoner, pesado, buena, si, no).
personaje(dark_samus, baja, alta, alta, media, zoner, pesado, buena, si, no).
personaje(yoshi, alta, media, media, media, brawler, medio, buena, si, si).
personaje(kirby, baja, baja, media, facil, brawler, ligero, buena, no, si).
personaje(fox, muy_alta, baja, media, avanzada, rushdown, ligero, mala, si, si).
personaje(pikachu, muy_alta, baja, media, avanzada, rushdown, ligero, buena, si, si).
personaje(luigi, media, baja, alta, avanzada, grappler, medio, mala, si, no).
personaje(ness, baja, media, alta, media, zoner, medio, media, si, si).
personaje(captain_falcon, muy_alta, media, alta, media, rushdown, medio, media, no, no).
personaje(jigglypuff, baja, baja, baja, avanzada, aerial, ligero, buena, no, si).
personaje(peach, media, media, media, avanzada, brawler, medio, buena, si, si).
personaje(daisy, media, media, media, avanzada, brawler, medio, buena, si, si).
personaje(bowser, baja, media, muy_alta, media, brawler, pesado, media, no, no).
personaje(ice_climbers, media, baja, alta, avanzada, brawler, medio, mala, no, no).
personaje(sheik, muy_alta, baja, baja, avanzada, rushdown, ligero, buena, no, si).
personaje(zelda, baja, alta, alta, media, zoner, medio, buena, si, no).
personaje(doctor_mario, media, baja, alta, media, brawler, medio, mala, si, no).
personaje(pichu, muy_alta, baja, media, avanzada, rushdown, ligero, buena, si, si).
personaje(falco, alta, media, media, media, rushdown, medio, buena, si, si).
personaje(marth, alta, alta, media, media, swordie, medio, buena, no, si).
personaje(lucina, alta, alta, media, media, swordie, medio, buena, no, si).
personaje(young_link, alta, media, media, media, zoner, ligero, buena, si, si).
personaje(ganondorf, baja, media, muy_alta, facil, brawler, pesado, mala, no, no).
personaje(mewtwo, alta, alta, alta, avanzada, zoner, ligero, buena, si, si).
personaje(roy, alta, media, alta, media, swordie, medio, media, no, si).
personaje(chrom, alta, media, alta, media, swordie, medio, media, no, si).
personaje(mr_game_and_watch, media, baja, media, avanzada, brawler, ligero, buena, si, si).
personaje(meta_knight, alta, baja, media, avanzada, rushdown, ligero, buena, no, si).
personaje(pit, media, media, media, facil, brawler, medio, buena, si, si).
personaje(dark_pit, media, media, media, facil, brawler, medio, buena, si, si).
personaje(zero_suit_samus, muy_alta, media, media, avanzada, rushdown, ligero, buena, si, si).
personaje(wario, media, baja, alta, media, brawler, medio, buena, no, si).
personaje(snake, baja, alta, alta, avanzada, zoner, pesado, buena, si, no).
personaje(ike, media, alta, muy_alta, media, swordie, pesado, media, no, si).
personaje(entrenador_pokemon, media, media, media, media, brawler, medio, media, si, si).
personaje(diddy_kong, alta, media, media, media, rushdown, ligero, buena, si, si).
personaje(lucas, baja, media, alta, media, zoner, medio, buena, si, si).
personaje(sonic, muy_alta, baja, baja, media, rushdown, ligero, buena, no, si).
personaje(king_dedede, baja, media, muy_alta, media, grappler, pesado, mala, si, no).
personaje(olimar, baja, alta, media, avanzada, zoner, ligero, buena, si, no).
personaje(lucario, media, media, alta, avanzada, brawler, medio, buena, no, si).
personaje(rob, baja, alta, alta, media, zoner, pesado, buena, si, no).
personaje(toon_link, alta, media, media, media, zoner, ligero, buena, si, si).
personaje(wolf, alta, media, alta, media, rushdown, medio, media, si, si).
personaje(villager, baja, media, media, media, zoner, ligero, buena, si, no).
personaje(mega_man, media, alta, media, avanzada, zoner, medio, buena, si, no).
personaje(wii_fit_trainer, media, media, media, avanzada, brawler, medio, buena, si, si).
personaje(rosalina_and_luma, baja, alta, alta, avanzada, zoner, medio, buena, si, no).
personaje(greninja, muy_alta, media, media, avanzada, rushdown, ligero, buena, si, si).
personaje(palutena, media, alta, media, media, zoner, medio, buena, si, si).
personaje(pac_man, media, media, media, avanzada, zoner, medio, buena, si, no).
personaje(robin, baja, alta, alta, avanzada, zoner, medio, mala, si, no).
personaje(shulk, media, muy_alta, alta, avanzada, swordie, medio, media, no, si).
personaje(bowser_jr, media, media, alta, media, brawler, pesado, media, si, no).
personaje(duck_hunt, baja, alta, media, avanzada, zoner, medio, buena, si, no).
personaje(ryu, media, media, alta, avanzada, brawler, medio, media, si, no).
personaje(ken, alta, media, alta, avanzada, brawler, medio, media, si, no).
personaje(cloud, alta, alta, alta, media, swordie, medio, media, no, si).
personaje(corrin, media, alta, media, media, swordie, medio, buena, si, si).
personaje(bayonetta, alta, media, media, avanzada, rushdown, medio, buena, si, si).
personaje(inkling, alta, media, media, media, rushdown, ligero, buena, si, si).
personaje(ridley, media, alta, alta, media, brawler, pesado, media, si, si).
personaje(simon, baja, muy_alta, media, media, zoner, medio, mala, si, no).
personaje(richter, baja, muy_alta, media, media, zoner, medio, mala, si, no).
personaje(king_k_rool, baja, alta, alta, facil, zoner, pesado, buena, si, no).
personaje(isabelle, baja, media, media, media, zoner, ligero, buena, si, no).
personaje(incineroar, baja, baja, muy_alta, media, grappler, pesado, mala, no, no).

:- dynamic preguntar_peso/1, preguntar_recuperacion/1, preguntar_proyectiles/1, preguntar_aereo/1, enemigo/1, preferencia_velocidad/1, preferencia_alcance/1, preferencia_fuerza/1, preferencia_dificultad/1, preferencia_tipo/1, preferencia_peso/1, preferencia_recuperacion/1, preferencia_proyectiles/1, preferencia_aereo/1.

iniciar :-
    retractall(enemigo(_)),
    retractall(preferencia_velocidad(_)),
    retractall(preferencia_alcance(_)),
    retractall(preferencia_fuerza(_)),
    retractall(preferencia_dificultad(_)),
    retractall(preferencia_tipo(_)),
    retractall(preferencia_peso(_)),
    retractall(preferencia_recuperacion(_)),
    retractall(preferencia_proyectiles(_)),
    retractall(preferencia_aereo(_)),
    preguntar_enemigo(Enemigo),
    preguntar_velocidad(Vel),
    preguntar_alcance(Alc),
    preguntar_fuerza(Fue),
    preguntar_dificultad(Dif),
    preguntar_tipo(Tip),
    preguntar_peso(Pes),
    preguntar_recuperacion(Rec),
    preguntar_proyectiles(Pro),
    preguntar_aereo(Aer),
    recomendar_personaje(Enemigo, Vel, Alc, Fue, Dif, Tip, Pes, Rec, Pro, Aer),
    preguntar_repetir.
%1
preguntar_enemigo(Enemigo) :-
    nl, write('1 ¿Contra que personaje juegas? (escribe el nombre en minusculas, espacios escritos con "_")'), nl, nl,
    read_line_to_string(user_input, Respuesta),
    string_lower(Respuesta, RespuestaLower),
    atom_string(Atom, RespuestaLower),
    ( personaje(Atom, _, _, _, _, _, _, _, _, _) ->
        asserta(enemigo(Atom)),
        Enemigo = Atom
    ;
        nl, write('El personaje no existe en la base de conocimiento.'), nl,
        nl, write('Intentalo de nuevo'), nl,
        preguntar_enemigo(Enemigo)
    ).
%2
preguntar_velocidad(Velocidad) :-
    nl, write('2 ¿Que velocidad prefieres? (muy_alta, alta, media, baja)'), nl, nl,
    write('Recuerda: a mayor velocidad, mas facil esquivar y acercarte al rival,'), nl,
    write('pero los personajes rapidos suelen tener menos fuerza o ser mas ligeros.'), nl, nl,
    read_line_to_string(user_input, Respuesta),
    string_lower(Respuesta, RespuestaLower),
    atom_string(Atom, RespuestaLower),
    ( member(Atom, [muy_alta, alta, media, baja]) ->
        asserta(preferencia_velocidad(Atom)),
        Velocidad = Atom
    ;
        nl, write('Respuesta no valida. Intenta de nuevo.'), nl,
        preguntar_velocidad(Velocidad)
    ).
%3
preguntar_alcance(Alcance) :-
    nl, write('3 ¿Que alcance te gustaria que tuviera tu personaje? (muy_alta, alta, media, baja)'), nl, nl,
    write('Recuerda: mayor alcance te permite atacar desde mas lejos, pero'), nl,
    write('los ataques suelen ser mas lentos o menos efectivos en combate cercano.'), nl, nl,
    read_line_to_string(user_input, Respuesta),
    string_lower(Respuesta, RespuestaLower),
    atom_string(Atom, RespuestaLower),
    ( member(Atom, [muy_alta, alta, media, baja]) ->
        asserta(preferencia_alcance(Atom)),
        Alcance = Atom
    ;
        nl, write('Respuesta no valida. Intenta de nuevo.'), nl,
        preguntar_alcance(Alcance)
    ).
%4
preguntar_fuerza(Fuerza) :-
    nl, write('4 ¿Que fuerza te gustaria que tuviera tu personaje? (muy_alta, alta, media, baja)'), nl, nl,
    write('Recuerda: entre mas fuerza tenga, mas lento sera el personaje.'), nl, nl,
    read_line_to_string(user_input, Respuesta),
    string_lower(Respuesta, RespuestaLower),
    atom_string(Atom, RespuestaLower),
    ( member(Atom, [muy_alta, alta, media, baja]) ->
        asserta(preferencia_fuerza(Atom)),
        Fuerza = Atom
    ;
        nl, write('Respuesta no valida. Intenta de nuevo.'), nl,
        preguntar_fuerza(Fuerza)
    ).
%5
preguntar_dificultad(Dificultad) :-
    nl, write('5 ¿Que dificultad te gustaria que tuviera tu personaje? (facil, media, avanzada)'), nl, nl,
    write('Recuerda: facil = intuitivo; media = equilibrado; avanzada = poderoso pero dificil de dominar.'), nl, nl,
    read_line_to_string(user_input, Respuesta),
    string_lower(Respuesta, RespuestaLower),
    atom_string(Atom, RespuestaLower),
    ( member(Atom, [facil, media, avanzada]) ->
        asserta(preferencia_dificultad(Atom)),
        Dificultad = Atom
    ;
        nl, write('Respuesta no valida. Intenta de nuevo.'), nl,
        preguntar_dificultad(Dificultad)
    ).
%6
preguntar_tipo(Tipo) :-
    nl, write('6 ¿Que tipo te gustaria que fuera tu personaje? (zoner, rushdown, brawler, swordie, grappler)'), nl, nl,
    write('Zoner: controla la distancia | Rushdown: agresivo y rapido | Brawler: cuerpo a cuerpo equilibrado'), nl,
    write('Swordie: usa espada | Grappler: ataques potentes cuerpo a cuerpo'), nl, nl,
    read_line_to_string(user_input, Respuesta),
    string_lower(Respuesta, RespuestaLower),
    atom_string(Atom, RespuestaLower),
    %write('Debug: Atom = '), write(Atom), nl,
    ( member(Atom, [zoner, rushdown, brawler, swordie, grappler]) ->
        asserta(preferencia_tipo(Atom)),
        Tipo = Atom
    ;
        nl, write('Respuesta no valida. Intenta de nuevo.'), nl,
        preguntar_tipo(Tipo)
    ).
%7
preguntar_peso(Peso) :-
    nl, write('7 ¿Que peso te gustaria que tuviera tu personaje? (ligero, medio, pesado)'), nl, nl,
    write('Ligero: mas rapido pero sale volando facil | Medio: balanceado | Pesado: resiste mas, pero es lento'), nl, nl,
    read_line_to_string(user_input, Respuesta),
    string_lower(Respuesta, RespuestaLower),
    atom_string(Atom, RespuestaLower),
    ( member(Atom, [ligero, medio, pesado]) ->
        asserta(preferencia_peso(Atom)),
        Peso = Atom
    ;
        nl, write('Respuesta no valida. Intenta de nuevo.'), nl,
        preguntar_peso(Peso)
    ).
%8
preguntar_recuperacion(Recuperacion) :-
    nl, write('8 ¿Que recuperación te gustaria que tuviera tu personaje? (buena, media, mala)'), nl, nl,
    write('Buena: vuelve facilmente al escenario | Media: algo limitada | Mala: vulnerable al caer'), nl, nl,
    read_line_to_string(user_input, Respuesta),
    string_lower(Respuesta, RespuestaLower),
    atom_string(Atom, RespuestaLower),
    ( member(Atom, [buena, media, mala]) ->
        asserta(preferencia_recuperacion(Atom)),
        Recuperacion = Atom
    ;
        nl, write('Respuesta no valida. Intenta de nuevo.'), nl,
        preguntar_recuperacion(Recuperacion)
    ).
%9
preguntar_proyectiles(Proyectiles) :-
    nl, write('9 ¿Te gustaria que tu personaje tenga proyectiles? (si, no)'), nl, nl,
    write('Los proyectiles permiten atacar a distancia, pero muchos personajes sin ellos son mas fuertes cuerpo a cuerpo.'), nl, nl,
    read_line_to_string(user_input, Respuesta),
    string_lower(Respuesta, RespuestaLower),
    atom_string(Atom, RespuestaLower),
    ( member(Atom, [si, no]) ->
        asserta(preferencia_proyectiles(Atom)),
        Proyectiles = Atom
    ;
        nl, write('Respuesta no valida. Intenta de nuevo.'), nl,
        preguntar_proyectiles(Proyectiles)
    ).
%1%0
preguntar_aereo(Aereo) :-
    nl, write('10 ¿Quieres que tu personaje destaque en el juego aereo? (si, no)'), nl, nl,
    write('Personajes con buen juego aereo son fuertes en el aire y pueden presionar desde arriba,'), nl,
    write('pero a veces tienen desventajas en el suelo o menor alcance.'), nl, nl,
    read_line_to_string(user_input, Respuesta),
    string_lower(Respuesta, RespuestaLower),
    atom_string(Atom, RespuestaLower),
    ( member(Atom, [si, no]) ->
        asserta(preferencia_aereo(Atom)),
        Aereo = Atom
    ;
        nl, write('Respuesta no valida. Intenta de nuevo.'), nl,
        preguntar_aereo(Aereo)
    ).

% -----------------------------------------------
% Regla principal: Encuentra el personaje mas similar
% -----------------------------------------------
recomendar_personaje(Enemigo, Vel, Alc, Fue, Dif, Tip, Pes, Rec, Pro, Aer,MejorPersonaje,Detalles,EstadisticasStr) :-
    % Busca coincidencia exacta primero
    ( personaje(Personaje, Vel, Alc, Fue, Dif, Tip, Pes, Rec, Pro, Aer),
      Personaje \= Enemigo ->
        nl, format('¡Perfecto! Te recomiendo a ~w.~n', [Personaje])
    ;
        % Si no hay exacto, busca el mas similar
        findall(
            [Puntaje, Personaje],
            (
                personaje(Personaje, V, A, F, D, T, P, R, Pr, Ae),
                Personaje \= Enemigo,
                calcular_puntaje(Vel, Alc, Fue, Dif, Tip, Pes, Rec, Pro, Aer,
                                V, A, F, D, T, P, R, Pr, Ae, Puntaje)
            ),
            Opciones
        ),
        % Ordena por puntaje y selecciona el mas alto
        (Opciones \= [] ->
            sort(Opciones, OpcionesOrdenadas),
            last(OpcionesOrdenadas, [_, MejorPersonaje]),
            nl, format('El personaje mas similar es ~w.~n', [MejorPersonaje]),
            mostrar_detalles(MejorPersonaje,Detalles),
            mostrar_estadisticas_str(MejorPersonaje, Enemigo, EstadisticasStr)
        ;
            nl, write('No encontre ningun personaje cercano. Prueba cambiando tus respuestas'), nl
        )
    ).

% -----------------------------------------------
% Calculo de similitud (puntaje total)
% -----------------------------------------------
calcular_puntaje(Vel, Alc, Fue, Dif, Tip, Pes, Rec, Pro, Aer,
                 V, A, F, D, T, P, R, Pr, Ae, Puntaje) :-
    % Atributos clave (Tipo y Peso valen doble)
    puntaje_atributo(Tip, T, P1),  % Tipo es prioritario
    puntaje_atributo(Pes, P, P2),  % Peso es prioritario
    puntaje_atributo(Vel, V, P3),
    puntaje_atributo(Alc, A, P4),
    puntaje_atributo(Fue, F, P5),
    puntaje_atributo(Dif, D, P6),
    puntaje_atributo(Rec, R, P7),
    puntaje_atributo(Pro, Pr, P8),
    puntaje_atributo(Aer, Ae, P9),
    Puntaje is (P1 + P2)*2 + P3 + P4 + P5 + P6 + P7 + P8 + P9.

% Puntaje por atributo: 2 (exacto), 1 (relajado), 0 (diferente)
puntaje_atributo(X, X, 2) :- !.
puntaje_atributo(Vel, V, 1) :- cambiar_velocidad(Vel, V).
puntaje_atributo(Alc, A, 1) :- cambiar_alcance(Alc, A).
puntaje_atributo(Fue, F, 1) :- cambiar_fuerza(Fue, F).
puntaje_atributo(Dif, D, 1) :- cambiar_dificultad(Dif, D).
% Tipo (no tiene valores intermedios, solo exacto o 0)
puntaje_atributo(Tipo, Tipo, 2) :- !.
puntaje_atributo(_, _, 0).  % Cualquier otro caso para tipo
puntaje_atributo(Pes, P, 1) :- cambiar_peso(Pes, P).
puntaje_atributo(Rec, R, 1) :- cambiar_recuperacion(Rec, R).
% Proyectiles (booleano, solo exacto o 0)
puntaje_atributo(Pro, Pro, 2) :- !.
puntaje_atributo(_, _, 0).  % Cualquier otro caso para proyectiles

% Aereo (booleano, solo exacto o 0)
puntaje_atributo(Aer, Aer, 2) :- !.
puntaje_atributo(_, _, 0).  % Cualquier otro caso para aereo

% -----------------------------------------------
% Muestra detalles del personaje recomendado
% -----------------------------------------------
mostrar_detalles(Personaje,Detalles) :-
    personaje(Personaje, V, A, F, D, T, P, R, Pr, Ae),
    nl, write('Detalles:'), nl,
    format('  - Dificultad: ~w~n', [D]),
    format('  - Tipo: ~w | Peso: ~w~n', [T, P]),
    format('  - Velocidad: ~w | Alcance: ~w | Fuerza: ~w~n', [V, A, F]),
    format('  - Recuperacion: ~w | Proyectiles: ~w | Aereo: ~w~n', [R, Pr, Ae]),
    with_output_to(atom(Detalles),
        (
            format('Detalles:~n', []),
            format('  - Dificultad: ~w~n', [D]),
            format('  - Tipo: ~w~n', [T]),
            format('  - Peso: ~w~n', [P]),
            format('  - Velocidad: ~w~n', [V]),
            format('  - Alcance: ~w~n', [A]),
            format('  - Fuerza: ~w~n', [F]),
            format('  - Recuperacion: ~w~n', [R]),
            format('  - Proyectiles: ~w~n', [Pr]),
            format('  - Aereo: ~w~n', [Ae])
        )
    ).

mostrar_estadisticas_str(Recomendado, Enemigo, EstadisticasStr) :-
    with_output_to(atom(EstadisticasStr),
        mostrar_estadisticas(Recomendado, Enemigo)
    ).

mostrar_estadisticas(Recomendado, Enemigo) :-
    personaje(Recomendado, RVel, RAlc, RFue, _, RTip, RPes, RRec, RPro, RAer),
    personaje(Enemigo, EVel, EAlc, EFue, _, ETip, EPes, ERec, EPro, EAer),
    nl, format('Estadisticas sobre ~w vs ~w~n', [Recomendado, Enemigo]),
    % Comparacion de atributos
    comparar_velocidad(RVel, EVel),
    comparar_alcance(RAlc, EAlc),
    comparar_fuerza(RFue, EFue),
    comparar_tipo(RTip, ETip),
    comparar_peso(RPes, EPes),
    comparar_recuperacion(RRec, ERec),
    comparar_proyectiles(RPro, EPro),
    comparar_aereo(RAer, EAer).
    

% Reglas de comparacion especificas para cada atributo

comparar_velocidad(RVel, EVel) :-
    valor_num(RVel, RNum), valor_num(EVel, ENum),
    (RNum > ENum ->
    nl, write('  - ¡Eres más veloz! Esto te ayuda a controlar el ritmo del combate y castigar errores rapidamente.')
    ; true ).

comparar_alcance(RAlc, EAlc) :-
    valor_num(RAlc, RNum), valor_num(EAlc, ENum),
    (RNum > ENum ->
    nl, write('  - ¡Mayor alcance! Ideal para mantener al rival a distancia y controlar el espacio.')
    ; true ).

comparar_fuerza(RFue, EFue) :-
    valor_num(RFue, RNum), valor_num(EFue, ENum),
    (RNum > ENum ->
    nl, write('  - ¡Más fuerte! Tus ataques hacen mayor daño y terminan stocks antes.')
    ; true ).

comparar_tipo(RTip, ETip) :-
    ( mejor_counter_tipo(RTip, ETip, Razon) ->
        nl, format('  - ~w  ¡Ventaja estratégica! ~w', [RTip, Razon])
    ; RTip = ETip ->
        nl, format('  - Tipo: Iguales (~w)', [RTip])
    ; true ).

comparar_peso(RPes, EPes) :-
    valor_peso(RPes, RVal),
    valor_peso(EPes, EVal),
    ( RVal > EVal ->
        nl, write('  - Eres más pesado: resistes más daño, pero eres más lento.')
    ; RVal < EVal ->
        nl, write('  - Eres más ligero: cuidado con kills tempranos, pero esquivas mejor.')
    ;
        true  % No hacer nada si son iguales
    ).

comparar_recuperacion(RRec, ERec) :-
    valor_recup(RRec, RVal),
    valor_recup(ERec, EVal),
    ( RVal > EVal ->
        nl, write('  - Tienes mejor recuperación: vuelves al escenario mas facilmente que tu rival.')
    ; RVal < EVal ->
        nl, write('  - El enemigo tiene mejor recuperación: se agresivo cuando este fuera del escenario.')
    ;
        true  % No mostrar nada si son iguales
    ).

comparar_proyectiles(si, no) :-
    nl, write('  - Tienes proyectiles: controla la distancia y presiona desde lejos.').

comparar_proyectiles(no, si) :-
    nl, write('  - El enemigo tiene proyectiles: cierra la distancia con movimientos seguros.').

comparar_proyectiles(_, _) :- true.  % No mostrar nada si ambos tienen o no tienen

comparar_aereo(si, no) :-
    nl, write('  - Dominas el aire: presiona desde arriba y evita el juego en suelo.').

comparar_aereo(no, si) :-
    nl, write('  - El enemigo domina el aire: manten el combate en el suelo.').

comparar_aereo(_, _) :- true.  % No mostrar nada si son iguales

preguntar_repetir :-
    nl, nl,
    write('¿Quieres hacer otra consulta? (si/no)'), nl,
    read_line_to_string(user_input, RespuestaCruda),
    string_lower(RespuestaCruda, Respuesta),  % Por si escriben con mayusculas
    (
        Respuesta = "si" -> nl, iniciar
    ;
        Respuesta = "no" -> nl, write('¡Hasta luego! Gracias por usar el sistema experto.'), nl
    ;
        Respuesta = "" -> repetir  % Si esta vacio, vuelve a preguntar
    ;
        write('No entendí tu respuesta. Por favor escribe "si" o "no".'), nl,
        preguntar_repetir
    ).

%Regla de cambio de atributos. Esto es por si no se encuentra un personaje con las especificaciones dadas
cambiar_velocidad(muy_alta, alta).
cambiar_velocidad(alta, media).
cambiar_velocidad(media, baja).
cambiar_velocidad(baja, muy_alta).

cambiar_alcance(muy_alta, alta).
cambiar_alcance(alta, media).
cambiar_alcance(media, baja).
cambiar_alcance(baja, muy_alta).

cambiar_fuerza(muy_alta, alta).
cambiar_fuerza(alta, media).
cambiar_fuerza(media, baja).
cambiar_fuerza(baja, muy_alta).

cambiar_dificultad(facil, media).
cambiar_dificultad(media, avanzada).
cambiar_dificultad(avanzada, facil).

cambiar_peso(ligero, medio).
cambiar_peso(medio, pesado).
cambiar_peso(pesado, ligero).

cambiar_recuperacion(buena, media).
cambiar_recuperacion(media, mala).
cambiar_recuperacion(mala, buena).

% Valores numericos para comparacion Velocidad, Alcance y Fuerza
valor_num(muy_alta, 4).
valor_num(alta, 3).
valor_num(media, 2).
valor_num(baja, 1).

% Relaciones de counters con explicaciones tacticas
mejor_counter_tipo(rushdown, grappler, 
    "Los rushdown son rapidos y evitan que los grapplers los atrapen con sus movimientos lentos").
mejor_counter_tipo(zoner, brawler, 
    "Los zoners mantienen a raya a los brawlers con proyectiles y ataques a distancia").
mejor_counter_tipo(swordie, zoner, 
    "Los swordies cortan el espacio de los zoners con sus largos ataques cuerpo a cuerpo").
mejor_counter_tipo(brawler, swordie, 
    "Los brawlers rompen las defensas de los swordies con movimientos rapidos y poderosos").
mejor_counter_tipo(grappler, rushdown, 
    "Los grapplers pueden atrapar a los rushdown si esquivan mal sus acercamientos").
mejor_counter_tipo(swordie, brawler, 
    "Los swordies superan en alcance a los brawlers, golpeando primero").
mejor_counter_tipo(zoner, grappler, 
    "Los zoners evitan que los grapplers se acerquen con ataques a distancia").

%Valores numericos para peso
valor_peso(ligero, 1).
valor_peso(medio, 2).
valor_peso(pesado, 3).

%Valores numericos para recuperación
valor_recup(buena, 3).
valor_recup(media, 2).
valor_recup(mala, 1).