% Cargar la biblioteca XPCE para la GUI
:- use_module(library(pce)).

% Cargar el archivo de conocimiento
:- consult('bdplantas.pl').

% Predicado principal para iniciar la GUI
mostrar_gui :-
    new(Dialog, dialog('Información de Plantas')),
    send(Dialog, size, size(700, 650)),

    % Crear el menú desplegable
    new(Menu, menu('Selecciona una Planta', cycle)),
    send(Dialog, append, Menu),



    % Obtener todas las plantas desde nombre_cientifico/2
    findall(Especie, nombre_cientifico(Especie, _), Especies),
    sort(Especies, EspeciesUnicas),
    forall(member(Especie, EspeciesUnicas),
           send(Menu, append, Especie)),


    % Crear área de texto para mostrar la información
    new(Text, editor),
    send(Text, editable, @off),
    send(Text, size, size(60, 60)),

    % Crear área para la imagen
    new(ImageBox, picture),
    send(ImageBox, size, size(195, 195)),
   % send(Horizontally, append, ImageBox),

    new(Boton, button('Origen',
                      message(@prolog, mostrar_origen, Text))),

    new(Boton2, button('Modo de Preparacion',
                      message(@prolog, mostrar_modopreparacion, Text))),

    new(Boton3, button('Botiquin',
                      message(@prolog, mostrar_plantas_botiquin, Text, ImageBox))),

    new(Boton4, button('Nombre Cientifico',
                      message(@prolog, mostrar_nombrecientifico, Text))),

    new(Boton5, button('Enfermedades',
                      message(@prolog, mostrar_enfermedades, Text))),




    % Agregar imagen y texto al layout
    new(Horizontally, dialog_group('')),
    send(Horizontally, append, ImageBox),
    send(Horizontally, append, Text),
    send(Dialog, append, Horizontally),

    send(Horizontally, display, Boton),
    send(Boton,position,point(300,10)),

    send(Horizontally, display, Boton2),
    send(Boton2,position,point(300,40)),

    send(Horizontally, display, Boton4),
    send(Boton4,position,point(300,70)),

    send(Horizontally, display, Boton3),
    send(Boton3,position,point(300,130)),

    send(Horizontally, display, Boton5),
    send(Boton5,position,point(300,100)),


    %send(Horizontally, append, Boton, right),
    % send(Horizontally, append, Botonp, right),
    %send(Horizontally, display, Botonp),
    %send(Botonp,position,point(500,10)),

    % Asociar selección del menú con acción
    send(Menu, message, message(@prolog, mostrar_info_planta, Menu?selection, Text, ImageBox)),

    % Mostrar ventana
    send(Dialog, open).

% Mostrar continentes de origen de todas las plantas
mostrar_enfermedades(Text) :-
    findall(Info,
            (enfermedad(Enfermedad),
             format(atom(Info), '~w ', [Enfermedad])),
            InfoList),
    sort(InfoList, UniqueList),  % Opcional: eliminar duplicados y ordenar
    atomic_list_concat(UniqueList, '\n', InfoText),
    send(Text, contents, InfoText).



% Mostrar continentes de origen de todas las plantas
mostrar_nombrecientifico(Text) :-
    findall(Info,
            (nombre_cientifico(Especie, Continente),
             format(atom(Info), '~w Con nombre ~w', [Especie, Continente])),
            InfoList),
    sort(InfoList, UniqueList),  % Opcional: eliminar duplicados y ordenar
    atomic_list_concat(UniqueList, '\n', InfoText),
    send(Text, contents, InfoText).



% Mostrar continentes de origen de todas las plantas
mostrar_modopreparacion(Text) :-
    findall(Info,
            (modo_preparacion(Especie, Continente),
             format(atom(Info), '~w se prepara con ~w', [Especie, Continente])),
            InfoList),
    sort(InfoList, UniqueList),  % Opcional: eliminar duplicados y ordenar
    atomic_list_concat(UniqueList, '\n', InfoText),
    send(Text, contents, InfoText).


% Mostrar continentes de origen de todas las plantas
mostrar_origen(Text) :-
    findall(Info,
            (continente_origen(Especie, Continente),
             format(atom(Info), '~w proviene de ~w', [Especie, Continente])),
            InfoList),
    sort(InfoList, UniqueList),  % Opcional: eliminar duplicados y ordenar
    atomic_list_concat(UniqueList, '\n', InfoText),
    send(Text, contents, InfoText).


% Recolectar plantas para el botiquín
plantas_botiquin([
    manzanilla,   % Problemas digestivos, insomnio, ansiedad
    llanten,      % Tos, diarrea, heridas
    ajo,          % Reumas, sarna, lombrices, infecciones
    cuachalalate, % Ulceras, infecciones gastrointestinales, problemas bucales
    cola_de_caballo, % Retención de líquidos, infecciones urinarias
    sauco         % Resfriado, fiebre, tos
]).

% Mostrar plantas para el botiquín en el área de texto
mostrar_plantas_botiquin(Text, ImageBox) :-
    plantas_botiquin(Plantas),
    (   Plantas \= []
    ->  findall(Info,
                (member(Especie, Plantas),
                 trata_enfermedad(Especie, Enfermedad),
                 format(atom(Info), '~w trata ~w', [Especie, Enfermedad])),
                InfoList),
        atomic_list_concat(InfoList, '\n', InfoText),
        send(Text, contents, InfoText)
    ;   send(Text, contents, 'No se encontraron plantas para el botiquín.')
    ),

    % Mostrar imagen del botiquín
    atom_concat('imagenes/', 'botiquin', RutaBase),
    (
        atom_concat(RutaBase, '.jpg', RutaJPG),
        exists_file(RutaJPG)
    ->  RutaFinal = RutaJPG
    ;   atom_concat(RutaBase, '.jpeg', RutaJPEG),
        exists_file(RutaJPEG)
    ->  RutaFinal = RutaJPEG
    ;   RutaFinal = none
    ),

    ( RutaFinal \= none ->
        new(Image, image(RutaFinal)),
        new(Bitmap, bitmap(Image)),
        new(Box, box(210,210)),
        send(Box, fill_pattern, Bitmap),
        send(ImageBox, display, Box, point(0,0))
    ;   send(ImageBox, display, text('Imagen no disponible'))
    ).

% Mostrar información y cargar imagen
mostrar_info_planta(Especie, Text, ImageBox) :-
    findall(Info, info_planta(Especie, Info), InfoList),
    sort(InfoList, UniqueInfoList), % Eliminar duplicados
    atomic_list_concat(UniqueInfoList, '\n', InfoText),
    send(Text, contents, InfoText),
    send(ImageBox, clear),

    atom_concat('imagenes/', Especie, RutaBase),
    (
        atom_concat(RutaBase, '.jpg', RutaJPG),
        exists_file(RutaJPG)
    ->
        RutaFinal = RutaJPG
    ;
        atom_concat(RutaBase, '.jpeg', RutaJPEG),
        exists_file(RutaJPEG)
    ->
        RutaFinal = RutaJPEG
    ;
        RutaFinal = none
    ),

    ( RutaFinal \= none ->
        new(Image, image(RutaFinal)),
        new(Bitmap, bitmap(Image)),
        new(Box, box(210,210)),
        send(Box, fill_pattern, Bitmap),
        send(ImageBox, display, Box, point(0,0))
    ;
        send(ImageBox, display, text('Imagen no disponible'))
    ).



% Recolectar información desde hechos
info_planta(Especie, Info) :-
    setof(Info, info_planta_aux(Especie, Info), Infos),
    member(Info, Infos).

% Auxiliar para recolectar información
info_planta_aux(Especie, Info) :-
    nombre_cientifico(Especie, Nombre),
    format(atom(Info), 'Nombre Científico: ~w (~w)', [Especie, Nombre]).

info_planta_aux(Especie, Info) :-
    continente_origen(Especie, Continente),
    format(atom(Info), 'Continente de Origen: ~w', [Continente]).

info_planta_aux(Especie, Info) :-
    pais_origen(Especie, Pais),
    format(atom(Info), 'País de Origen: ~w', [Pais]).

info_planta_aux(Especie, Info) :-
    modo_preparacion(Especie, Modo),
    format(atom(Info), 'Modo de Preparación: ~w', [Modo]).

info_planta_aux(Especie, Info) :-
    trata_enfermedad(Especie, Enfermedad),
    format(atom(Info), 'Trata Enfermedad: ~w', [Enfermedad]).

info_planta_aux(Especie, Info) :-
    accion_efecto_planta(Especie, Efecto),
    format(atom(Info), 'Acción/Efecto: ~w', [Efecto]).

info_planta_aux(Especie, Info) :-
    modo_tratamiento(Especie, Modo),
    format(atom(Info), 'Modo de Tratamiento: ~w', [Modo]).

info_planta_aux(Especie, Info) :-
    precaucion_planta(Especie, Precaucion),
    format(atom(Info), 'Precaución: ~w', [Precaucion]).

info_planta_aux(Especie, Info) :-
    trata_enfermedad(Especie, Enfermedad),
    sintoma_enfermedad(Enfermedad, Sintoma),
    format(atom(Info), 'Síntoma Asociado: ~w (Enfermedad: ~w)', [Sintoma, Enfermedad]).
:- initialization(mostrar_gui).
