% Cargar la biblioteca XPCE para la GUI
:- use_module(library(pce)).

% Cargar el archivo de conocimiento
:- consult('bdplantas.pl').

% Predicado principal para iniciar la GUI
mostrar_gui :-
    new(Dialog, dialog('Información de Plantas')),
    send(Dialog, size, size(900, 700)),

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
    send(Text, size, size(200, 200)),

    % Crear área para la imagen
    new(ImageBox, picture),
    send(ImageBox, size, size(200, 200)),

    % Agregar imagen y texto al layout
    new(Horizontally, dialog_group(horizontal)),
    send(Horizontally, append, ImageBox),
    send(Horizontally, append, Text),
    send(Dialog, append, Horizontally),

    % Asociar selección del menú con acción
    send(Menu, message, message(@prolog, mostrar_info_planta, Menu?selection, Text, ImageBox)),

    % Mostrar ventana
    send(Dialog, open).

% Mostrar información y cargar imagen
mostrar_info_planta(Especie, Text, ImageBox) :-
    % Mostrar texto
    findall(Info, info_planta(Especie, Info), InfoList),
    atomic_list_concat(InfoList, '\n', InfoText),
    send(Text, contents, InfoText),

    % Ruta de la imagen
    atom_concat('imagenes/', Especie, RutaBase),
    atom_concat(RutaBase, '.jpg', RutaImagen),

    send(ImageBox, clear),

    % Cargar imagen si existe
    ( exists_file(RutaImagen) ->
        new(Image, image(RutaImagen)),
        new(Bitmap, bitmap(Image)),
        new(Box, box(210,210)),  % Tamaño fijo de imagen
        send(Box, fill_pattern, Bitmap),
        send(ImageBox, display, Box, point(0,0))
    ;
        send(ImageBox, display, text('Imagen no disponible'))
    ).


% Recolectar información desde hechos
info_planta(Especie, Info) :-
    nombre_cientifico(Especie, Nombre),
    format(atom(Info), 'Nombre Científico: ~w (~w)', [Especie, Nombre]).

info_planta(Especie, Info) :-
    continente_origen(Especie, Continente),
    format(atom(Info), 'Continente de Origen: ~w', [Continente]).

info_planta(Especie, Info) :-
    pais_origen(Especie, Pais),
    format(atom(Info), 'País de Origen: ~w', [Pais]).

info_planta(Especie, Info) :-
    modo_preparacion(Especie, Modo),
    format(atom(Info), 'Modo de Preparación: ~w', [Modo]).

info_planta(Especie, Info) :-
    trata_enfermedad(Especie, Enfermedad),
    format(atom(Info), 'Trata Enfermedad: ~w', [Enfermedad]).

info_planta(Especie, Info) :-
    accion_efecto_planta(Especie, Efecto),
    format(atom(Info), 'Acción/Efecto: ~w', [Efecto]).

info_planta(Especie, Info) :-
    modo_tratamiento(Especie, Modo),
    format(atom(Info), 'Modo de Tratamiento: ~w', [Modo]).

info_planta(Especie, Info) :-
    precaucion_planta(Especie, Precaucion),
    format(atom(Info), 'Precaución: ~w', [Precaucion]).

info_planta(Especie, Info) :-
    sintoma_enfermedad(Enfermedad, Sintoma),
    trata_enfermedad(Especie, Enfermedad),
    format(atom(Info), 'Síntoma Asociado: ~w (Enfermedad: ~w)', [Sintoma, Enfermedad]).

% Iniciar la GUI manualmente con ?- mostrar_gui.

    % Iniciar la GUI automáticamente al cargar el archivo
:- initialization(mostrar_gui).
