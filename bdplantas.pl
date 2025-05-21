%bdplantas.pl - Base de datos de plantas medicinales

% Exportar predicados principales para uso en otros módulos
:- module(bdplantas, [
    nombre_cientifico/2,
    continente_origen/2,
    pais_origen/2,
    modo_preparacion/2,
    enfermedad/1,
    sintoma_enfermedad/2,
    trata_enfermedad/2,
    accion_efecto_planta/2,
    modo_tratamiento/2,
    precaucion_planta/2,
    %cura/2,
    tipo_enfermedad/2,
    %uso_interno/1,
    %uso_externo/1,
    parte_util/2,
    %forma_uso/2,
    compatible/2
    %origen/2,
    %trata/2
]).

% Declarar predicados como discontiguos para evitar advertencias
:- discontiguous planta/1.
:- discontiguous nombre_cientifico/2.
:- discontiguous continente_origen/2.
:- discontiguous pais_origen/2.
:- discontiguous modo_preparacion/2.
:- discontiguous enfermedad/1.
:- discontiguous sintoma_enfermedad/2.
:- discontiguous trata_enfermedad/2.
:- discontiguous accion_efecto_planta/2.
:- discontiguous modo_tratamiento/2.
:- discontiguous precaucion_planta/2.
:- discontiguous cura/2.
:- discontiguous tipo_enfermedad/2.
:- discontiguous parte_util/2.
:- discontiguous compatible/2.

%------------------ ABROJO ------------------
planta(abrojo).
nombre_cientifico(abrojo, tribulus_cistoides).
continente_origen(abrojo, america).
pais_origen(abrojo, mexico).
modo_preparacion(abrojo, cocimiento).
trata_enfermedad(abrojo, infeccion_urinaria).
trata_enfermedad(abrojo, cistitis).
accion_efecto_planta(abrojo, diuretico).
accion_efecto_planta(abrojo, antiinflamatorio).
modo_tratamiento(abrojo, tres_veces_dia).
precaucion_planta(abrojo, embarazo).
enfermedad(infeccion_urinaria).
enfermedad(cistitis).
enfermedad(infeccion_riñon).
sintoma_enfermedad(infeccion_urinaria, ardor_al_orinar).

%------------------ ACONITO ------------------
planta(aconito).
nombre_cientifico(aconito, 'Aconitum napellus').
continente_origen(aconito, europa).
trata_enfermedad(aconito, neuralgia).
trata_enfermedad(aconito, fiebre).
trata_enfermedad(aconito, reumatismo).
accion_efecto_planta(aconito, analgesico_antiinflamatorio).
modo_tratamiento(aconito, uso_externo).
modo_tratamiento(aconito, tintura).
precaucion_planta(aconito, toxicidad_alta).
enfermedad(neuralgia).
enfermedad(fiebre).
enfermedad(reumatismo).
sintoma_enfermedad(neuralgia, dolor_nervioso_intenso).
sintoma_enfermedad(fiebre, temperatura_elevada).
sintoma_enfermedad(reumatismo, dolor_articular).
tipo_enfermedad(neuralgia, nerviosa).
tipo_enfermedad(fiebre, general).
tipo_enfermedad(reumatismo, musculoesqueletica).
parte_util(aconito, raiz).
compatible(aconito, adormidera).

%------------------ ADORMIDERA ------------------
planta(adormidera).
nombre_cientifico(adormidera, 'Papaver somniferum').
continente_origen(adormidera, asia).
trata_enfermedad(adormidera, insomnio).
trata_enfermedad(adormidera, dolor).
trata_enfermedad(adormidera, ansiedad).
accion_efecto_planta(adormidera, no_especificado).
modo_tratamiento(adormidera, uso_interno).
modo_tratamiento(adormidera, infusion).
modo_tratamiento(adormidera, extracto).
precaucion_planta(adormidera, dependencia).
enfermedad(insomnio).
enfermedad(dolor).
enfermedad(ansiedad).
sintoma_enfermedad(insomnio, no_especificado).
sintoma_enfermedad(dolor, no_especificado).
sintoma_enfermedad(ansiedad, no_especificado).
tipo_enfermedad(insomnio, nerviosa).
tipo_enfermedad(dolor, general).
tipo_enfermedad(ansiedad, mental).
parte_util(adormidera, capsula).
compatible(adormidera, aconito).

%------------------ AGUACATE ------------------
planta(aguacate).
nombre_cientifico(aguacate, 'Persea americana').
continente_origen(aguacate, america).
pais_origen(aguacate, mexico).
modo_preparacion(aguacate, infusion).
modo_preparacion(aguacate, cocimiento).
trata_enfermedad(aguacate, estreñimiento).
trata_enfermedad(aguacate, problemas_digestivos).
trata_enfermedad(aguacate, tos).
accion_efecto_planta(aguacate, no_especificado).
modo_tratamiento(aguacate, uso_interno).
precaucion_planta(aguacate, no_especificado).
enfermedad(estreñimiento).
enfermedad(problemas_digestivos).
enfermedad(tos).
sintoma_enfermedad(estreñimiento, no_especificado).
sintoma_enfermedad(problemas_digestivos, no_especificado).
sintoma_enfermedad(tos, no_especificado).
tipo_enfermedad(estreñimiento, digestiva).
tipo_enfermedad(problemas_digestivos, digestiva).
tipo_enfermedad(tos, respiratoria).
parte_util(aguacate, hoja).
compatible(aguacate, ajenjo).

%------------------ AHUEHUETE ------------------
planta(ahuehuete).
nombre_cientifico(ahuehuete, 'Taxodium mucronatum').
continente_origen(ahuehuete, america).
pais_origen(ahuehuete, mexico).
modo_preparacion(ahuehuete, decoccion).
trata_enfermedad(ahuehuete, enfermedades_respiratorias).
trata_enfermedad(ahuehuete, infecciones_piel).
accion_efecto_planta(ahuehuete, no_especificado).
modo_tratamiento(ahuehuete, uso_externo).
precaucion_planta(ahuehuete, no_especificado).
enfermedad(enfermedades_respiratorias).
enfermedad(infecciones_piel).
sintoma_enfermedad(enfermedades_respiratorias, no_especificado).
sintoma_enfermedad(infecciones_piel, no_especificado).
tipo_enfermedad(enfermedades_respiratorias, respiratoria).
tipo_enfermedad(infecciones_piel, cutanea).
parte_util(ahuehuete, corteza).

%------------------ AJENJO ------------------
planta(ajenjo).
nombre_cientifico(ajenjo, 'Artemisia absinthium').
continente_origen(ajenjo, europa).
modo_preparacion(ajenjo, infusion).
modo_preparacion(ajenjo, macerado).
trata_enfermedad(ajenjo, parasitos_intestinales).
trata_enfermedad(ajenjo, problemas_digestivos).
trata_enfermedad(ajenjo, debilidad_general).
accion_efecto_planta(ajenjo, no_especificado).
modo_tratamiento(ajenjo, uso_interno).
precaucion_planta(ajenjo, embarazo).
enfermedad(parasitos_intestinales).
enfermedad(problemas_digestivos).
enfermedad(debilidad_general).
sintoma_enfermedad(parasitos_intestinales, no_especificado).
sintoma_enfermedad(problemas_digestivos, no_especificado).
sintoma_enfermedad(debilidad_general, no_especificado).
tipo_enfermedad(parasitos_intestinales, digestiva).
tipo_enfermedad(problemas_digestivos, digestiva).
tipo_enfermedad(debilidad_general, general).
parte_util(ajenjo, hojas).
compatible(aguacate, ajenjo).

%------------------ AJO ------------------
planta(ajo).
nombre_cientifico(ajo, allium_sativum).
continente_origen(ajo, asia_central).
pais_origen(ajo, siberia).
pais_origen(ajo, norte_de_iran).
modo_preparacion(ajo, con_miel).
modo_preparacion(ajo, machacar).
modo_preparacion(ajo, molido_con_leche).
trata_enfermedad(ajo, reumas).
trata_enfermedad(ajo, sarna).
trata_enfermedad(ajo, tina).
trata_enfermedad(ajo, callos).
trata_enfermedad(ajo, lombrices).
accion_efecto_planta(ajo, antiinflamatorio).
accion_efecto_planta(ajo, vermifugo).
accion_efecto_planta(ajo, febrifugo).
accion_efecto_planta(ajo, diuretico).
accion_efecto_planta(ajo, expectorante).
accion_efecto_planta(ajo, antiparasitario).
modo_tratamiento(ajo, consumo_en_ayunas).
modo_tratamiento(ajo, aplicado_directamente).
precaucion_planta(ajo, problemas_gastrointestinales).
enfermedad(reumas).
enfermedad(sarna).
enfermedad(tina).
enfermedad(callos).
enfermedad(lombrices).
sintoma_enfermedad(reumas, dolor_articular).
sintoma_enfermedad(sarna, erupcion_en_la_piel).
sintoma_enfermedad(tina, manchas_circulares_rojas).
sintoma_enfermedad(callos, piel_engrosada_y_amarillenta).
sintoma_enfermedad(lombrices, picazon_anal).

%------------------ ALBAHACA ------------------
planta(albahaca).
nombre_cientifico(albahaca, ocimum_basilicum).
continente_origen(albahaca, asia_central_y_africa).
pais_origen(albahasa, india).
modo_preparacion(albahaca, jugo).
trata_enfermedad(albahaca, alopecia).
accion_efecto_planta(albahaca, tonico_capilar).
accion_efecto_planta(albahaca, diuretica).
accion_efecto_planta(albahaca, carminativa).
accion_efecto_planta(albahaca, emenagoga).
modo_tratamiento(albahaca, friccionarse_en_pelo).
precaucion_planta(albahaca, reacciones_alergicas).
enfermedad(alopecia).
sintoma_enfermedad(alopecia, caida_de_cabello).

%------------------ ALCACHOFA ------------------
planta(alcachofa).
nombre_cientifico(alcachofa, cynara_scolymus).
continente_origen(alcachofa, africa).
pais_origen(alcachofa, egipto).
modo_preparacion(alcachofa, cocer).
trata_enfermedad(alcachofa, diabetes).
trata_enfermedad(alcachofa, anemia).
accion_efecto_planta(alcachofa, descongestionante).
accion_efecto_planta(alcachofa, antiinflamatorio).
modo_tratamiento(alcachofa, tomarlo_con_gusto).
precaucion_planta(alcachofa, alergias).
enfermedad(diabetes).
enfermedad(anemia).
sintoma_enfermedad(diabetes, sed_excesiva).
sintoma_enfermedad(anemia, cansancio_y_debilidad).

%------------------ ALCANFOR ------------------
planta(alcanfor).
nombre_cientifico(alcanfor, laurus_camphora).
continente_origen(alcanfor, asia).
pais_origen(alcanfor, taiwan).
modo_preparacion(alcanfor, aceite).
trata_enfermedad(alcanfor, gota).
trata_enfermedad(alcanfor, tifoidea).
trata_enfermedad(alcanfor, piquetes_mosco).
trata_enfermedad(alcanfor, artritis).
trata_enfermedad(alcanfor, arteriosclerosis).
accion_efecto_planta(alcanfor, analgesico).
accion_efecto_planta(alcanfor, antiseptico).
modo_tratamiento(alcanfor, untar_aceite_3_veces_al_dia).
precaucion_planta(alcanfor, no_usar_en_exceso).
enfermedad(gota).
enfermedad(tifoidea).
enfermedad(piquetes_mosco).
enfermedad(artritis).
enfermedad(arteriosclerosis).
sintoma_enfermedad(gota, dolor_intenso).
sintoma_enfermedad(gota, hinchazon).
sintoma_enfermedad(gota, enrojecimiento).
sintoma_enfermedad(tifoidea, fiebre_alta).
sintoma_enfermedad(tifoidea, diarrea).
sintoma_enfermedad(tifoidea, manchas_rojas).
sintoma_enfermedad(piquetes_mosco, hinchazon).
sintoma_enfermedad(piquetes_mosco, picazon).
sintoma_enfermedad(artritis, no_especificado).
sintoma_enfermedad(arteriosclerosis, no_especificado).

%------------------ AMAPOLA AMARILLA ------------------
planta(amapola_amarilla).
nombre_cientifico(amapola_amarilla, eschscholzia_californica).
continente_origen(amapola_amarilla, america).
pais_origen(amapola_amarilla, mexico).
modo_preparacion(amapola_amarilla, infusion).
trata_enfermedad(amapola_amarilla, diarrea).
trata_enfermedad(amapola_amarilla, insomnio).
trata_enfermedad(amapola_amarilla, ansiedad_leve).
accion_efecto_planta(amapola_amarilla, sedante_suave).
accion_efecto_planta(amapola_amarilla, antiespasmodico).
modo_tratamiento(amapola_amarilla, tomar_infusion_1_vez_por_la_noche).
precaucion_planta(amapola_amarilla, evitar_en_embarazo).
precaucion_planta(amapola_amarilla, evitar_en_lactancia).
enfermedad(diarrea).
enfermedad(insomnio).
enfermedad(ansiedad_leve).
sintoma_enfermedad(diarrea, heces_blandas).
sintoma_enfermedad(diarrea, dolor_intestinal).
sintoma_enfermedad(insomnio, no_especificado).
sintoma_enfermedad(ansiedad_leve, no_especificado).

%------------------ AMATE ------------------
planta(amate).
nombre_cientifico(amate, ficus_insipida).
continente_origen(amate, america).
pais_origen(amate, mexico).
modo_preparacion(amate, horchata).
trata_enfermedad(amate, reumatismo).
trata_enfermedad(amate, diviesos).
trata_enfermedad(amate, solitaria).
trata_enfermedad(amate, inflamacion).
trata_enfermedad(amate, infecciones_leves).
accion_efecto_planta(amate, antiparasitario).
accion_efecto_planta(amate, antiinflamatorio).
modo_tratamiento(amate, beber_una_taza_cada_8_horas).
precaucion_planta(amate, puede_irritar_la_piel).
enfermedad(reumatismo).
enfermedad(diviesos).
enfermedad(solitaria).
enfermedad(inflamacion).
enfermedad(infecciones_leves).
sintoma_enfermedad(reumatismo, no_especificado).
sintoma_enfermedad(diviesos, dolor).
sintoma_enfermedad(diviesos, inflamacion).
sintoma_enfermedad(solitaria, malestar_estomacal).
sintoma_enfermedad(solitaria, nauseas).
sintoma_enfermedad(solitaria, perdida_apetito).
sintoma_enfermedad(inflamacion, no_especificado).
sintoma_enfermedad(infecciones_leves, no_especificado).

%------------------ ANIS ------------------
planta(anis).
nombre_cientifico(anis, pimpinella_anisum).
continente_origen(anis, asia).
pais_origen(anis, region_del_mediterraneo).
modo_preparacion(anis, infusion).
trata_enfermedad(anis, colitis_leve).
trata_enfermedad(anis, indigestion).
trata_enfermedad(anis, flatulencias).
trata_enfermedad(anis, colicos).
trata_enfermedad(anis, tos).
trata_enfermedad(anis, bronquitis).
accion_efecto_planta(anis, carminativo).
accion_efecto_planta(anis, expectorante).
modo_tratamiento(anis, no_especificado).
precaucion_planta(anis, no_especificado).
enfermedad(colitis).
enfermedad(indigestion).
enfermedad(flatulencias).
enfermedad(colicos).
enfermedad(tos).
enfermedad(bronquitis).
sintoma_enfermedad(colitis, diarrea).
sintoma_enfermedad(colitis, dolor_abdominal).
sintoma_enfermedad(colitis, sangre_en_heces).
sintoma_enfermedad(indigestion, no_especificado).
sintoma_enfermedad(flatulencias, no_especificado).
sintoma_enfermedad(colicos, no_especificado).
sintoma_enfermedad(tos, no_especificado).
sintoma_enfermedad(bronquitis, no_especificado).

%------------------ ANACAHUITE ------------------
planta(anacahuite).
nombre_cientifico(anacahuite, cordia_boissieri).
continente_origen(anacahuite, america).
pais_origen(anacahuite, mexico).
modo_preparacion(anacahuite, cocimiento).
trata_enfermedad(anacahuite, bronquitis).
trata_enfermedad(anacahuite, tos).
trata_enfermedad(anacahuite, pulmones).
trata_enfermedad(anacahuite, resfriado).
accion_efecto_planta(anacahuite, expectorante).
accion_efecto_planta(anacahuite, balsamico).
modo_tratamiento(anacahuite, no_especificado).
precaucion_planta(anacahuite, no_especificado).
enfermedad(bronquitis).
enfermedad(tos).
enfermedad(pulmones).
enfermedad(resfriado).
sintoma_enfermedad(bronquitis, no_especificado).
sintoma_enfermedad(tos, no_especificado).
sintoma_enfermedad(pulmones, no_especificado).
sintoma_enfermedad(resfriado, no_especificado).

%------------------ ARNICA ------------------
planta(arnica).
nombre_cientifico(arnica, arnica_montana).
continente_origen(arnica, europa).
pais_origen(arnica, suiza).
modo_preparacion(arnica, macera).
trata_enfermedad(arnica, golpes).
trata_enfermedad(arnica, torceduras).
trata_enfermedad(arnica, moretones).
accion_efecto_planta(arnica, antiinflamatoria).
accion_efecto_planta(arnica, cicatrizante).
accion_efecto_planta(arnica, analgesica).
modo_tratamiento(arnica, no_especificado).
precaucion_planta(arnica, no_especificado).
enfermedad(golpes).
enfermedad(torceduras).
enfermedad(moretones).
sintoma_enfermedad(golpes, no_especificado).
sintoma_enfermedad(torceduras, no_especificado).
sintoma_enfermedad(moretones, no_especificado).

%------------------ BARBASCO ------------------
planta(barbasco).
nombre_cientifico(barbasco, jacquinia_arborea).
continente_origen(barbasco, america).
pais_origen(barbasco, mexico).
modo_preparacion(barbasco, cocimiento).
trata_enfermedad(barbasco, verrugas).
trata_enfermedad(barbasco, tina).
trata_enfermedad(barbasco, sarna).
trata_enfermedad(barbasco, anticonceptivo).
accion_efecto_planta(barbasco, toxica).
accion_efecto_planta(barbasco, irritante).
accion_efecto_planta(barbasco, pesticida).
modo_tratamiento(barbasco, no_especificado).
precaucion_planta(barbasco, no_especificado).
enfermedad(verrugas).
enfermedad(tina).
enfermedad(sarna).
enfermedad(anticonceptivo).
sintoma_enfermedad(verrugas, no_especificado).
sintoma_enfermedad(tina, no_especificado).
sintoma_enfermedad(sarna, no_especificado).
sintoma_enfermedad(anticonceptivo, no_especificado).

%------------------ BELLADONA ------------------
planta(belladona).
nombre_cientifico(belladona, atropa_belladonna).
continente_origen(belladona, europa).
pais_origen(belladona, europa_central).
modo_preparacion(belladona, macera).
trata_enfermedad(belladona, espasmos).
trata_enfermedad(belladona, colicos).
trata_enfermedad(belladona, dolores_menstruales).
trata_enfermedad(belladona, asma).
trata_enfermedad(belladona, parkinson).
accion_efecto_planta(belladona, antiespasmodica).
accion_efecto_planta(belladona, analgesica).
accion_efecto_planta(belladona, sedante).
accion_efecto_planta(belladona, toxica).
modo_tratamiento(belladona, no_especificado).
precaucion_planta(belladona, no_especificado).
enfermedad(espasmos).
enfermedad(colicos).
enfermedad(dolores_menstruales).
enfermedad(asma).
enfermedad(parkinson).
sintoma_enfermedad(espasmos, no_especificado).
sintoma_enfermedad(colicos, no_especificado).
sintoma_enfermedad(dolores_menstruales, no_especificado).
sintoma_enfermedad(asma, no_especificado).
sintoma_enfermedad(parkinson, no_especificado).

%------------------ BERRO ------------------
planta(berro).
nombre_cientifico(berro, nasturtium_officinale).
continente_origen(berro, europa).
pais_origen(berro, europa_central).
modo_preparacion(berro, infusion).
trata_enfermedad(berro, bronquitis).
accion_efecto_planta(berro, expectorante).
accion_efecto_planta(berro, depurativa).
modo_tratamiento(berro, tomar_te_dos_veces_dia).
precaucion_planta(berro, evitar_dosis_altas_puede_ser_irritante).
enfermedad(bronquitis).
sintoma_enfermedad(bronquitis, tos).

%------------------ BOLDO ------------------
planta(boldo).
nombre_cientifico(boldo, peumus_boldus).
continente_origen(boldo, america).
pais_origen(boldo, chile).
modo_preparacion(boldo, infusion).
trata_enfermedad(boldo, problemas_hepaticos).
accion_efecto_planta(boldo, hepatoprotectora).
accion_efecto_planta(boldo, colagoga).
modo_tratamiento(boldo, tomar_te_despues_comidas).
precaucion_planta(boldo, evitar_uso_prolongado_embarazo).
enfermedad(problemas_hepaticos).
sintoma_enfermedad(problemas_hepaticos, dolor_abdominal).

%------------------ BORRAJA ------------------
planta(borraja).
nombre_cientifico(borraja, borago_officinalis).
continente_origen(borraja, europa).
pais_origen(borraja, mediterraneo).
modo_preparacion(borraja, infusion).
trata_enfermedad(borraja, fiebre).
accion_efecto_planta(borraja, sudorifica).
accion_efecto_planta(borraja, antiinflamatoria).
modo_tratamiento(borraja, tomar_te_dos_tres_veces_dia).
precaucion_planta(borraja, evitar_uso_prolongado_alcaloides_toxicos).
enfermedad(fiebre).
sintoma_enfermedad(fiebre, temperatura_elevada).

%------------------ BUGAMBILIA ------------------
planta(bugambilia).
nombre_cientifico(bugambilia, bougainvillea_glabra).
continente_origen(bugambilia, america).
pais_origen(bugambilia, brasil).
modo_preparacion(bugambilia, cocimiento).
trata_enfermedad(bugambilia, tos).
accion_efecto_planta(bugambilia, expectorante).
accion_efecto_planta(bugambilia, antitusiva).
modo_tratamiento(bugambilia, tomar_te_tres_veces_dia).
precaucion_planta(bugambilia, evitar_en_embarazo_dosis_altas).
enfermedad(tos).
sintoma_enfermedad(tos, irritacion_garganta).

%------------------ BRIONIA ------------------
planta(brionia).
nombre_cientifico(brionia, bryonia_alba).
continente_origen(brionia, europa).
pais_origen(brionia, desconocido).
modo_preparacion(brionia, cocimiento).
trata_enfermedad(brionia, lombrices).
accion_efecto_planta(brionia, purgante).
accion_efecto_planta(brionia, antiparasitaria).
modo_tratamiento(brionia, tomar_te_dosis_bajas).
precaucion_planta(brionia, toxica_dosis_altas).
enfermedad(lombrices).
sintoma_enfermedad(lombrices, molestias_abdominales).

%------------------ CANELA ------------------
planta(canela).
nombre_cientifico(canela, cinnamomum_verum).
continente_origen(canela, asia).
pais_origen(canela, desconocido).
modo_preparacion(canela, infusion).
trata_enfermedad(canela, anemia).
accion_efecto_planta(canela, estimulante).
accion_efecto_planta(canela, antimicrobiana).
modo_tratamiento(canela, tomar_te_o_anadir_alimentos).
precaucion_planta(canela, alergias_coagulacion).
enfermedad(anemia).
sintoma_enfermedad(anemia, fatiga).

%------------------ CARDO SANTO ------------------
planta(cardo_santo).
nombre_cientifico(cardo_santo, cnicus_benedictus).
continente_origen(cardo_santo, america).
pais_origen(cardo_santo, mexico).
modo_preparacion(cardo_santo, cocimiento).
trata_enfermedad(cardo_santo, nubes_ojos).
accion_efecto_planta(cardo_santo, oftalmica).
modo_tratamiento(cardo_santo, aplicar_gotas_ojos).
precaucion_planta(cardo_santo, contacto_prolongado_ojos).
enfermedad(nubes_ojos).
sintoma_enfermedad(nubes_ojos, vision_nublada).

%------------------ CEDRON ------------------
planta(cedron).
nombre_cientifico(cedron, aloysia_citrodora).
continente_origen(cedron, america).
pais_origen(cedron, desconocido).
modo_preparacion(cedron, infusion).
trata_enfermedad(cedron, tos).
accion_efecto_planta(cedron, expectorante).
accion_efecto_planta(cedron, calmante).
modo_tratamiento(cedron, tomar_te_dos_tres_veces_dia).
precaucion_planta(cedron, presion_arterial_baja).
enfermedad(tos).
sintoma_enfermedad(tos, irritacion_garganta).

%------------------ CEMPASUCHIL ------------------
planta(cempasuchil).
nombre_cientifico(cempasuchil, tagetes_erecta).
continente_origen(cempasuchil, america).
pais_origen(cempasuchil, mexico).
modo_preparacion(cempasuchil, te).
trata_enfermedad(cempasuchil, parasitos_intestinales).
accion_efecto_planta(cempasuchil, antiparasitario).
accion_efecto_planta(cempasuchil, tonico).
modo_tratamiento(cempasuchil, tomar_te_en_caso_de_parasitos).
precaucion_planta(cempasuchil, cocimiento_3_veces_al_dia).
enfermedad(parasitos_intestinales).
enfermedad(tumores).
sintoma_enfermedad(parasitos_intestinales, dolor_estomago).
sintoma_enfermedad(tumores, no_especificado).

%------------------ CHAPARRO AMARGOSO ------------------
planta(chaparro_amargoso).
nombre_cientifico(chaparro_amargoso, castela_americana).
continente_origen(chaparro_amargoso, america).
pais_origen(chaparro_amargoso, mexico).
modo_preparacion(chaparro_amargoso, te).
trata_enfermedad(chaparro_amargoso, disenteria_amebiana).
trata_enfermedad(chaparro_amargoso, diarrea).
trata_enfermedad(chaparro_amargoso, flujo).
trata_enfermedad(chaparro_amargoso, hemorragias_internas).
accion_efecto_planta(chaparro_amargoso, antiseptico).
modo_tratamiento(chaparro_amargoso, tomar_te_hojas_y_corteza_2_veces_dia).
precaucion_planta(chaparro_amargoso, en_disenteria_cronica_usar_como_lavativa).
enfermedad(disenteria_amebiana).
enfermedad(diarrea).
enfermedad(flujo).
enfermedad(hemorragias_internas).
sintoma_enfermedad(disenteria_amebiana, diarrea).
sintoma_enfermedad(disenteria_amebiana, dolor_estomago).
sintoma_enfermedad(diarrea, no_especificado,dolor_estomago).
sintoma_enfermedad(flujo, no_especificado).
sintoma_enfermedad(hemorragias_internas, no_especificado).

%------------------ CHICALOTE ------------------
planta(chicalote).
nombre_cientifico(chicalote, argemone_oecholtzia).
continente_origen(chicalote, america).
pais_origen(chicalote, mexico).
modo_preparacion(chicalote, cocimiento).
trata_enfermedad(chicalote, tos).
trata_enfermedad(chicalote, asma).
trata_enfermedad(chicalote, tosferina).
trata_enfermedad(chicalote, epilepsia).
trata_enfermedad(chicalote, artritis).
trata_enfermedad(chicalote, insomnio).
trata_enfermedad(chicalote, ansiedad).
trata_enfermedad(chicalote, colicos_hepaticos).
trata_enfermedad(chicalote, colicos_renales).
trata_enfermedad(chicalote, colicos_intestinales).
trata_enfermedad(chicalote, carnosidad_ojos).
accion_efecto_planta(chicalote, antiespasmodico).
accion_efecto_planta(chicalote, hipnotico).
accion_efecto_planta(chicalote, sedante).
modo_tratamiento(chicalote, cocimiento_2_veces_al_dia).
modo_tratamiento(chicalote, aplicar_leche_directamente_en_ojos_en_casos_de_carnosidad).
precaucion_planta(chicalote, es_un_poco_toxica_usar_con_cuidado).
enfermedad(tos).
enfermedad(asma).
enfermedad(tosferina).
enfermedad(epilepsia).
enfermedad(artritis).
enfermedad(insomnio).
enfermedad(ansiedad).
enfermedad(colicos_hepaticos).
enfermedad(colicos_renales).
enfermedad(colicos_intestinales).
enfermedad(carnosidad_ojos).
sintoma_enfermedad(tos, no_especificado).
sintoma_enfermedad(asma, no_especificado).
sintoma_enfermedad(tosferina, no_especificado).
sintoma_enfermedad(epilepsia, no_especificado).
sintoma_enfermedad(artritis, no_especificado).
sintoma_enfermedad(insomnio, nerviosismo).
sintoma_enfermedad(ansiedad, desesperacion).
sintoma_enfermedad(colicos_hepaticos, no_especificado).
sintoma_enfermedad(colicos_renales, no_especificado).
sintoma_enfermedad(colicos_intestinales, no_especificado).
sintoma_enfermedad(carnosidad_ojos, no_especificado).

%------------------ CHILE ------------------
planta(chile).
nombre_cientifico(chile, capsicum_annuum).
continente_origen(chile, america).
pais_origen(chile, mexico).
modo_preparacion(chile, compresas_calientes).
trata_enfermedad(chile, asma).
trata_enfermedad(chile, reumatismo).
accion_efecto_planta(chile, antiinflamatorio).
accion_efecto_planta(chile, rubefaciente).
modo_tratamiento(chile, aplicar_hojas_en_pecho_para_el_asma).
modo_tratamiento(chile, aplicar_panos_calientes_con_cocimiento_para_reumatismo).
precaucion_planta(chile, no_debe_darse_a_niños_puede_irritar_mucosas_diarrear_inflamar_higado_y_hemorroides).
enfermedad(asma).
enfermedad(reumatismo).
sintoma_enfermedad(asma, dificultad_respiratoria).
sintoma_enfermedad(reumatismo, dolor_articular).

%------------------ CHICHIGUA ------------------
planta(chichigua).
nombre_cientifico(chichigua, solanum_mammosum).
continente_origen(chichigua, america).
pais_origen(chichigua, mexico).
modo_preparacion(chichigua, decoccion).
trata_enfermedad(chichigua, dermatitis).
trata_enfermedad(chichigua, inflamacion).
trata_enfermedad(chichigua, resfriado).
accion_efecto_planta(chichigua, antiinflamatorio).
accion_efecto_planta(chichigua, antiseptico).
modo_tratamiento(chichigua, uso_topico_dos_veces_dia).
precaucion_planta(chichigua, toxica_si_se_ingiere).
enfermedad(dermatitis).
enfermedad(inflamacion).
enfermedad(resfriado).
sintoma_enfermedad(dermatitis, erupcion_cutanea).
sintoma_enfermedad(inflamacion, hinchazon).
sintoma_enfermedad(resfriado, constipado).

%------------------ CILANTRO ------------------
planta(cilantro).
nombre_cientifico(cilantro, coriandrum_sativum).
continente_origen(cilantro, asia).
pais_origen(cilantro, iran).
modo_preparacion(cilantro, infusion).
trata_enfermedad(cilantro, problemas_digestivos).
trata_enfermedad(cilantro, ansiedad).
accion_efecto_planta(cilantro, carminativo).
accion_efecto_planta(cilantro, relajante).
modo_tratamiento(cilantro, dos_veces_dia).
precaucion_planta(cilantro, evitar_en_embarazo_excesivo).
enfermedad(problemas_digestivos).
enfermedad(ansiedad).
sintoma_enfermedad(problemas_digestivos, colicos).
sintoma_enfermedad(ansiedad, nerviosismo).

%------------------ COCOLMECA ------------------
planta(cocolmeca).
nombre_cientifico(cocolmeca, smilax_spinosa).
continente_origen(cocolmeca, america).
pais_origen(cocolmeca, mexico).
modo_preparacion(cocolmeca, decoccion).
trata_enfermedad(cocolmeca, reumatismo).
trata_enfermedad(cocolmeca, acne).
trata_enfermedad(cocolmeca, anemia).
accion_efecto_planta(cocolmeca, depurativo).
accion_efecto_planta(cocolmeca, antiinflamatorio).
modo_tratamiento(cocolmeca, una_vez_dia).
precaucion_planta(cocolmeca, no_usar_por_periodos_prolongados).
enfermedad(reumatismo).
enfermedad(acne).
enfermedad(anemia).
sintoma_enfermedad(reumatismo, dolor_articular).
sintoma_enfermedad(acne, inflamacion_piel).
sintoma_enfermedad(anemia, dolor_pecho).

%------------------ COLA DE CABALLO ------------------
planta(cola_de_caballo).
nombre_cientifico(cola_de_caballo, equisetum_arvense).
continente_origen(cola_de_caballo, europa).
pais_origen(cola_de_caballo, francia).
modo_preparacion(cola_de_caballo, infusion).
trata_enfermedad(cola_de_caballo, retencion_liquidos).
trata_enfermedad(cola_de_caballo, calculos_renales).
accion_efecto_planta(cola_de_caballo, diuretico).
accion_efecto_planta(cola_de_caballo, remineralizante).
modo_tratamiento(cola_de_caballo, una_a_dos_veces_dia).
precaucion_planta(cola_de_caballo, evitar_en_insuficiencia_renal).
enfermedad(retencion_liquidos).
enfermedad(calculos_renales).
sintoma_enfermedad(retencion_liquidos, hinchazon).
sintoma_enfermedad(calculos_renales, dolor_renal).

%------------------ COLCHINO ------------------
planta(colchino).
nombre_cientifico(colchino, argemone_mexicana).
continente_origen(colchino, america).
pais_origen(colchino, mexico).
modo_preparacion(colchino, infusion_de_hojas).
trata_enfermedad(colchino, dolor_estomacal).
trata_enfermedad(colchino, parasitos).
trata_enfermedad(colchino, problemas_hepaticos).
accion_efecto_planta(colchino, analgesico).
accion_efecto_planta(colchino, antiparasitario).
accion_efecto_planta(colchino, hepatoprotector).
modo_tratamiento(colchino, tomar_taza_infusion_dos_veces_al_dia_por_5_dias).
precaucion_planta(colchino, dosis_alta_toxica).
enfermedad(dolor_estomacal).
enfermedad(parasitos).
enfermedad(problemas_hepaticos).
sintoma_enfermedad(dolor_estomacal, dolor_abdominal).
sintoma_enfermedad(parasitos, comezon_anal).
sintoma_enfermedad(parasitos, dolor_abdominal).
sintoma_enfermedad(problemas_hepaticos, color_amarillo_en_la_piel).

%------------------ COMINO ------------------
planta(comino).
nombre_cientifico(comino, cuminum_cyminum).
continente_origen(comino, asia).
modo_preparacion(comino, infusion_de_semillas).
trata_enfermedad(comino, indigestión).
trata_enfermedad(comino, flatulencia).
trata_enfermedad(comino, colico_menstrual).
accion_efecto_planta(comino, carminativo).
accion_efecto_planta(comino, antiespasmodico).
accion_efecto_planta(comino, digestivo).
modo_tratamiento(comino, tomar_taza_infusion_tras_comidas).
precaucion_planta(comino, evitar_en_embarazo).
enfermedad(indigestión).
enfermedad(flatulencia).
enfermedad(colico_menstrual).
sintoma_enfermedad(indigestión, pesadez_estomacal).
sintoma_enfermedad(flatulencia, gases).
sintoma_enfermedad(colico_menstrual, dolor_abdominal).

%------------------ COLPACHI ------------------
planta(colpachi).
nombre_cientifico(colpachi, croton_glabellus).
continente_origen(colpachi, america).
pais_origen(colpachi, mexico).
modo_preparacion(colpachi, decoccion_de_corteza).
trata_enfermedad(colpachi, sarampión).
trata_enfermedad(colpachi, afecciones_de_la_piel).
trata_enfermedad(colpachi, fiebre).
accion_efecto_planta(colpachi, antipiretico).
accion_efecto_planta(colpachi, antiseptico).
accion_efecto_planta(colpachi, dermatologico).
modo_tratamiento(colpachi, aplicar_infusion_tibia_en_piel_2_veces_al_dia).
precaucion_planta(colpachi, evitar_en_lactancia).
enfermedad(sarampión).
enfermedad(afecciones_de_la_piel).
enfermedad(fiebre).
sintoma_enfermedad(sarampión, fiebre).
sintoma_enfermedad(sarampión, manchas_rojas).
sintoma_enfermedad(afecciones_de_la_piel, irritacion).
sintoma_enfermedad(fiebre, temperatura_alta).

%------------------ CUACHALALATE ------------------
planta(cuachalalate).
nombre_cientifico(cuachalalate, amphipterygium_adstringens).
continente_origen(cuachalalate, america).
pais_origen(cuachalalate, mexico).
modo_preparacion(cuachalalate, infusion_o_decoccion_de_corteza).
trata_enfermedad(cuachalalate, ulceras).
trata_enfermedad(cuachalalate, infecciones_gastrointestinales).
trata_enfermedad(cuachalalate, problemas_bucales).
accion_efecto_planta(cuachalalate, cicatrizante).
accion_efecto_planta(cuachalalate, astringente).
accion_efecto_planta(cuachalalate, antiinflamatorio).
modo_tratamiento(cuachalalate, tomar_infusion_3_veces_al_dia_tras_las_comidas).
precaucion_planta(cuachalalate, no_uso_prolongado).
enfermedad(ulceras).
enfermedad(infecciones_gastrointestinales).
enfermedad(problemas_bucales).
sintoma_enfermedad(ulceras, dolor_interno).
sintoma_enfermedad(infecciones_gastrointestinales, diarrea).
sintoma_enfermedad(problemas_bucales, inflamacion_encias).

%------------------ CUAJIOTE ------------------
planta(cuajiote).
nombre_cientifico(cuajiote, bursera_morelensis).
continente_origen(cuajiote, america).
pais_origen(cuajiote, mexico).
modo_preparacion(cuajiote, infusion).
trata_enfermedad(cuajiote, anasarca).
trata_enfermedad(cuajiote, estreñimiento_cronico).
accion_efecto_planta(cuajiote, laxante).
modo_tratamiento(cuajiote, uso_moderado_no_mas_de_3_dias).
precaucion_planta(cuajiote, puede_provocar_gastroenteritis).
precaucion_planta(cuajiote, puede_provocar_rectitis).
precaucion_planta(cuajiote, puede_provocar_enterocolitis).
enfermedad(anasarca).
enfermedad(estreñimiento_cronico).
sintoma_enfermedad(anasarca, hinchazon_generalizada).
sintoma_enfermedad(estreñimiento_cronico, estreñimiento).

%------------------ CUASIA ------------------
planta(cuasia).
nombre_cientifico(cuasia, quassia_amara).
continente_origen(cuasia, america).
pais_origen(cuasia, costa_rica).
pais_origen(cuasia, nicaragua).
pais_origen(cuasia, panama).
pais_origen(cuasia, brasil).
pais_origen(cuasia, peru).
pais_origen(cuasia, venezuela).
pais_origen(cuasia, surinam).
pais_origen(cuasia, colombia).
pais_origen(cuasia, argentina).
pais_origen(cuasia, guyana_francesa).
pais_origen(cuasia, guyana).
pais_origen(cuasia, mexico).
modo_preparacion(cuasia, infusion).
trata_enfermedad(cuasia, diabetes).
trata_enfermedad(cuasia, artritis).
trata_enfermedad(cuasia, reumatismo).
trata_enfermedad(cuasia, dolor_corporal).
trata_enfermedad(cuasia, migraña).
trata_enfermedad(cuasia, dolor_de_estomago).
accion_efecto_planta(cuasia, analgesico).
accion_efecto_planta(cuasia, antiinflamatorio).
accion_efecto_planta(cuasia, hipoglucemiante).
modo_tratamiento(cuasia, una_taza_por_dia).
precaucion_planta(cuasia, puede_provocar_irritacion_gastrica).
precaucion_planta(cuasia, puede_provocar_vomito).
precaucion_planta(cuasia, puede_provocar_estupor).
enfermedad(diabetes).
enfermedad(artritis).
enfermedad(reumatismo).
enfermedad(dolor_corporal).
enfermedad(migraña).
enfermedad(dolor_de_estomago).
sintoma_enfermedad(diabetes, mucha_sed).
sintoma_enfermedad(diabetes, fatiga).
sintoma_enfermedad(diabetes, nivel_alto_de_glucosa).
sintoma_enfermedad(artritis, dolor_articular).
sintoma_enfermedad(artritis, inflamacion).
sintoma_enfermedad(reumatismo, rigidez_articular).
sintoma_enfermedad(reumatismo, hinchazon).
sintoma_enfermedad(dolor_corporal, dolor).
sintoma_enfermedad(migraña, dolor_de_cabeza).
sintoma_enfermedad(dolor_de_estomago, nausea).
sintoma_enfermedad(dolor_de_estomago, dolor_de_estomago).

%------------------ DAMIANA ------------------
planta(damiana).
nombre_cientifico(damiana, turnera_diffusa).
continente_origen(damiana, america).
pais_origen(damiana, mexico).
modo_preparacion(damiana, infusion).
trata_enfermedad(damiana, hipersexualidad).
trata_enfermedad(damiana, alcoholismo).
trata_enfermedad(damiana, diabetes).
trata_enfermedad(damiana, impotencia).
trata_enfermedad(damiana, estrenimiento).
trata_enfermedad(damiana, problemas_renales).
accion_efecto_planta(damiana, afrodisíaco).
accion_efecto_planta(damiana, laxante).
accion_efecto_planta(damiana, diurético).
modo_tratamiento(damiana, infusion_tres_veces_dia).
precaucion_planta(damiana, evitar_en_embarazo).
enfermedad(hipersexualidad).
enfermedad(alcoholismo).
enfermedad(diabetes).
enfermedad(impotencia).
enfermedad(estrenimiento).
enfermedad(problemas_renales).
sintoma_enfermedad(hipersexualidad, no_especificado).
sintoma_enfermedad(alcoholismo, no_especificado).
sintoma_enfermedad(diabetes, sed_excesiva).
sintoma_enfermedad(impotencia, no_especificado).
sintoma_enfermedad(estrenimiento, dificultad_evacuacion).
sintoma_enfermedad(problemas_renales, dolor_renal).

%------------------ DORADILLA ------------------
planta(doradilla).
nombre_cientifico(doradilla, cheilanthes_myriophylla).
continente_origen(doradilla, america).
pais_origen(doradilla, mexico).
modo_preparacion(doradilla, infusion).
trata_enfermedad(doradilla, fiebre).
trata_enfermedad(doradilla, tos).
trata_enfermedad(doradilla, problemas_renales).
accion_efecto_planta(doradilla, diurético).
accion_efecto_planta(doradilla, antipirético).
modo_tratamiento(doradilla, tomar_infusion_dos_veces_dia).
precaucion_planta(doradilla, evitar_en_insuficiencia_renal).
enfermedad(fiebre).
enfermedad(tos).
enfermedad(problemas_renales).
sintoma_enfermedad(fiebre, temperatura_elevada).
sintoma_enfermedad(tos, irritacion_garganta).
sintoma_enfermedad(problemas_renales, dolor_renal).

%------------------ ENCINO ------------------
planta(encino).
nombre_cientifico(encino, quercus_rugosa).
continente_origen(encino, america).
pais_origen(encino, mexico).
modo_preparacion(encino, decoccion).
trata_enfermedad(encino, diarrea).
trata_enfermedad(encino, inflamacion_garganta).
accion_efecto_planta(encino, astringente).
accion_efecto_planta(encino, antiinflamatorio).
modo_tratamiento(encino, tomar_decoccion_dos_veces_dia).
precaucion_planta(encino, evitar_uso_prolongado).
enfermedad(diarrea).
enfermedad(inflamacion_garganta).
sintoma_enfermedad(diarrea, heces_blandas).
sintoma_enfermedad(inflamacion_garganta, dolor_garganta).

%------------------ EPAZOTE ------------------
planta(epazote).
nombre_cientifico(epazote, chenopodium_ambrosioides).
continente_origen(epazote, america).
pais_origen(epazote, mexico).
modo_preparacion(epazote, infusion).
trata_enfermedad(epazote, parasitos_intestinales).
trata_enfermedad(epazote, flatulencias).
trata_enfermedad(epazote, colicos).
accion_efecto_planta(epazote, antiparasitario).
accion_efecto_planta(epazote, carminativo).
modo_tratamiento(epazote, tomar_infusion_una_vez_dia).
precaucion_planta(epazote, evitar_dosis_altas_toxicidad).
enfermedad(parasitos_intestinales).
enfermedad(flatulencias).
enfermedad(colicos).
sintoma_enfermedad(parasitos_intestinales, dolor_abdominal).
sintoma_enfermedad(flatulencias, gases).
sintoma_enfermedad(colicos, dolor_abdominal).

%------------------ ESTAFIATE ------------------
planta(estafiate).
nombre_cientifico(estafiate, artemisia_ludoviciana).
continente_origen(estafiate, america).
pais_origen(estafiate, mexico).
modo_preparacion(estafiate, infusion).
trata_enfermedad(estafiate, parasitos_intestinales).
trata_enfermedad(estafiate, problemas_digestivos).
trata_enfermedad(estafiate, colicos).
accion_efecto_planta(estafiate, antiparasitario).
accion_efecto_planta(estafiate, digestivo).
modo_tratamiento(estafiate, tomar_infusion_una_vez_dia).
precaucion_planta(estafiate, evitar_en_embarazo).
enfermedad(parasitos_intestinales).
enfermedad(problemas_digestivos).
enfermedad(colicos).
sintoma_enfermedad(parasitos_intestinales, dolor_abdominal).
sintoma_enfermedad(problemas_digestivos, malestar_estomacal).
sintoma_enfermedad(colicos, dolor_abdominal).

%------------------ EUCALIPTO ------------------
planta(eucalipto).
nombre_cientifico(eucalipto, eucalyptus_globulus).
continente_origen(eucalipto, oceania).
pais_origen(eucalipto, australia).
modo_preparacion(eucalipto, vaporizacion).
trata_enfermedad(eucalipto, gripe).
trata_enfermedad(eucalipto, resfriado).
accion_efecto_planta(eucalipto, expectorante).
accion_efecto_planta(eucalipto, antiseptico).
modo_tratamiento(eucalipto, inhalar_vapor_2_veces_al_dia).
precaucion_planta(eucalipto, no_ingerir_aceite_esencial_puro).
enfermedad(gripe).
enfermedad(resfriado).
sintoma_enfermedad(gripe, fiebre).
sintoma_enfermedad(resfriado, congestion_nasal).

%------------------ FENOGRECO ------------------
planta(fenogreco).
nombre_cientifico(fenogreco, trigonella_foenum_graecum).
continente_origen(fenogreco, asia).
pais_origen(fenogreco, india).
modo_preparacion(fenogreco, cocimiento).
trata_enfermedad(fenogreco, diabetes).
trata_enfermedad(fenogreco, colesterol).
accion_efecto_planta(fenogreco, hipoglucemiante).
accion_efecto_planta(fenogreco, hipolipemiante).
modo_tratamiento(fenogreco, una_taza_en_ayunas).
precaucion_planta(fenogreco, no_usar_en_embarazo).
enfermedad(diabetes).
enfermedad(colesterol).
sintoma_enfermedad(diabetes, sed_excesiva).
sintoma_enfermedad(colesterol, dolor_en_el_pecho).

%------------------ GENCIANA ------------------
planta(genciana).
nombre_cientifico(genciana, gentiana_lutea).
continente_origen(genciana, europa).
pais_origen(genciana, francia).
modo_preparacion(genciana, maceracion).
trata_enfermedad(genciana, anemia).
trata_enfermedad(genciana, indigestion).
accion_efecto_planta(genciana, estimulante).
accion_efecto_planta(genciana, tonico).
modo_tratamiento(genciana, media_taza_antes_de_las_comidas).
precaucion_planta(genciana, evitar_en_ulceras_gastricas).
enfermedad(anemia).
enfermedad(indigestion).
sintoma_enfermedad(anemia, fatiga).
sintoma_enfermedad(indigestion, hinchazon).

%------------------ GERANIO ------------------
planta(geranio).
nombre_cientifico(geranio, pelargonium_graveolens).
continente_origen(geranio, africa).
pais_origen(geranio, sudafrica).
modo_preparacion(geranio, infusion).
trata_enfermedad(geranio, estrés).
trata_enfermedad(geranio, insomnio).
accion_efecto_planta(geranio, relajante).
accion_efecto_planta(geranio, ansiolítico).
modo_tratamiento(geranio, una_taza_antes_de_dormir).
precaucion_planta(geranio, posible_irritacion_cutanea).
enfermedad(estrés).
enfermedad(insomnio).
sintoma_enfermedad(estrés, tensión_muscular).
sintoma_enfermedad(insomnio, dificultad_para_dormir).

%------------------ GIRASOL ------------------
planta(girasol).
nombre_cientifico(girasol, helianthus_annuus).
continente_origen(girasol, america).
pais_origen(girasol, mexico).
modo_preparacion(girasol, infusion).
trata_enfermedad(girasol, presion_alta).
trata_enfermedad(girasol, fiebre).
accion_efecto_planta(girasol, diurético).
accion_efecto_planta(girasol, antiinflamatorio).
modo_tratamiento(girasol, te_diario_o_compresas).
precaucion_planta(girasol, exceso).
enfermedad(presion_alta).
enfermedad(fiebre).
sintoma_enfermedad(presion_alta, no_especificado).
sintoma_enfermedad(fiebre, no_especificado).

%------------------ GINGSENG ------------------
planta(gingseng).
nombre_cientifico(gingseng, panax_ginseng).
continente_origen(gingseng, asia).
pais_origen(gingseng, china).
modo_preparacion(gingseng, infusion).
trata_enfermedad(gingseng, fatiga).
trata_enfermedad(gingseng, diabetes).
accion_efecto_planta(gingseng, estimulante).
accion_efecto_planta(gingseng, adaptógeno).
modo_tratamiento(gingseng, infusion).
precaucion_planta(gingseng, hipertension).
precaucion_planta(gingseng, embarazo).
enfermedad(fatiga).
enfermedad(diabetes).
sintoma_enfermedad(fatiga, cansancio).
sintoma_enfermedad(fatiga, debilidad).
sintoma_enfermedad(diabetes, no_especificado).

%------------------ GORDOLOBO ------------------
planta(gordolobo).
nombre_cientifico(gordolobo, verbascum_thapsus).
continente_origen(gordolobo, europa).
pais_origen(gordolobo, alemania).
modo_preparacion(gordolobo, infusion).
trata_enfermedad(gordolobo, bronquitis).
trata_enfermedad(gordolobo, tos).
accion_efecto_planta(gordolobo, expectorante).
accion_efecto_planta(gordolobo, antiinflamatorio).
modo_tratamiento(gordolobo, te_2_3_veces_al_dia).
precaucion_planta(gordolobo, infusion).
enfermedad(bronquitis).
enfermedad(tos).
sintoma_enfermedad(bronquitis, flemas).
sintoma_enfermedad(bronquitis, congestion).
sintoma_enfermedad(tos, no_especificado).

%------------------ GRAMA ------------------
planta(grama).
nombre_cientifico(grama, cynodon_dactylon).
continente_origen(grama, africa).
pais_origen(grama, egipto).
modo_preparacion(grama, coccion).
trata_enfermedad(grama, cistitis).
trata_enfermedad(grama, infecciones_urinarias).
accion_efecto_planta(grama, diurético).
accion_efecto_planta(grama, depurativo).
modo_tratamiento(grama, te_o_jarabe).
precaucion_planta(grama, embarazo).
enfermedad(cistitis).
enfermedad(infecciones_urinarias).
sintoma_enfermedad(cistitis, no_especificado).
sintoma_enfermedad(infecciones_urinarias, ardor_al_orinar).

%------------------ GRANADO ------------------
planta(granado).
nombre_cientifico(granado, punica_granatum).
continente_origen(granado, asia).
modo_preparacion(granado, no_especificado).
trata_enfermedad(granado, no_especificado).
accion_efecto_planta(granado, no_especificado).
modo_tratamiento(granado, no_especificado).
precaucion_planta(granado, no_especificado).
enfermedad(no_especificado).
sintoma_enfermedad(no_especificado, no_especificado).

%------------------ GUACAMAYA ------------------
planta(guacamaya).
nombre_cientifico(guacamaya, psittacanthus_calycuatus).
continente_origen(guacamaya, america).
pais_origen(guacamaya, mexico).
modo_preparacion(guacamaya, infusion).
trata_enfermedad(guacamaya, hipertension).
trata_enfermedad(guacamaya, problemas_cardiacos).
accion_efecto_planta(guacamaya, hipotensor).
accion_efecto_planta(guacamaya, cardiotonico).
modo_tratamiento(guacamaya, tomar_infusion_una_vez_dia).
precaucion_planta(guacamaya, evitar_en_hipotension).
enfermedad(hipertension).
enfermedad(problemas_cardiacos).
sintoma_enfermedad(hipertension, presion_alta).
sintoma_enfermedad(problemas_cardiacos, dolor_pecho).

%------------------ GUACO ------------------
planta(guaco).
nombre_cientifico(guaco, mikania_glomerata).
continente_origen(guaco, america).
pais_origen(guaco, brasil).
modo_preparacion(guaco, infusion).
trata_enfermedad(guaco, tos).
trata_enfermedad(guaco, bronquitis).
trata_enfermedad(guaco, alergias_respiratorias).
accion_efecto_planta(guaco, expectorante).
accion_efecto_planta(guaco, antialérgico).
modo_tratamiento(guaco, tomar_infusion_dos_veces_dia).
precaucion_planta(guaco, evitar_en_embarazo).
enfermedad(tos).
enfermedad(bronquitis).
enfermedad(alergias_respiratorias).
sintoma_enfermedad(tos, irritacion_garganta).
sintoma_enfermedad(bronquitis, tos_con_flemas).
sintoma_enfermedad(alergias_respiratorias, estornudos).

%------------------ GUARUMBO ------------------
planta(guarumbo).
nombre_cientifico(guarumbo, cecropia_obtusifolia).
continente_origen(guarumbo, america).
pais_origen(guarumbo, mexico).
modo_preparacion(guarumbo, infusion).
trata_enfermedad(guarumbo, diabetes).
trata_enfermedad(guarumbo, hipertension).
trata_enfermedad(guarumbo, problemas_renales).
accion_efecto_planta(guarumbo, hipoglucemiante).
accion_efecto_planta(guarumbo, diurético).
modo_tratamiento(guarumbo, tomar_infusion_una_vez_dia).
precaucion_planta(guarumbo, evitar_en_hipotension).
enfermedad(diabetes).
enfermedad(hipertension).
enfermedad(problemas_renales).
sintoma_enfermedad(diabetes, sed_excesiva).
sintoma_enfermedad(hipertension, dolor_cabeza).
sintoma_enfermedad(problemas_renales, dolor_renal).

%------------------ HIERBA BUENA ------------------
planta(hierba_buena).
nombre_cientifico(hierba_buena, mentha_spicata).
continente_origen(hierba_buena, europa).
pais_origen(hierba_buena, mediterraneo).
modo_preparacion(hierba_buena, infusion).
trata_enfermedad(hierba_buena, problemas_digestivos).
trata_enfermedad(hierba_buena, colicos).
trata_enfermedad(hierba_buena, flatulencias).
accion_efecto_planta(hierba_buena, carminativo).
accion_efecto_planta(hierba_buena, digestivo).
modo_tratamiento(hierba_buena, tomar_infusion_dos_veces_dia).
precaucion_planta(hierba_buena, evitar_en_reflujo_gastroesofágico).
enfermedad(problemas_digestivos).
enfermedad(colicos).
enfermedad(flatulencias).
sintoma_enfermedad(problemas_digestivos, dolor_abdominal).
sintoma_enfermedad(colicos, dolor_abdominal).
sintoma_enfermedad(flatulencias, gases).

%------------------ HIERBA DE LA GOLONDRINA ------------------
planta(hierba_de_la_golondrina).
nombre_cientifico(hierba_de_la_golondrina, euphorbia_prostrata).
continente_origen(hierba_de_la_golondrina, america).
pais_origen(hierba_de_la_golondrina, mexico).
modo_preparacion(hierba_de_la_golondrina, infusion).
trata_enfermedad(hierba_de_la_golondrina, hemorroides).
trata_enfermedad(hierba_de_la_golondrina, inflamacion).
accion_efecto_planta(hierba_de_la_golondrina, antiinflamatorio).
accion_efecto_planta(hierba_de_la_golondrina, analgésico).
modo_tratamiento(hierba_de_la_golondrina, tomar_infusion_o_aplicar_tópicamente).
precaucion_planta(hierba_de_la_golondrina, evitar_contacto_con_ojos).
enfermedad(hemorroides).
enfermedad(inflamacion).
sintoma_enfermedad(hemorroides, dolor_anal).
sintoma_enfermedad(inflamacion, hinchazon).

%------------------ HIERBA DEL SAPO ------------------
planta(hierba_del_sapo).
nombre_cientifico(hierba_del_sapo, eryngium_carolinae).
continente_origen(hierba_del_sapo, america).
pais_origen(hierba_del_sapo, mexico).
modo_preparacion(hierba_del_sapo, decoccion).
trata_enfermedad(hierba_del_sapo, problemas_renales).
trata_enfermedad(hierba_del_sapo, infecciones_urinarias).
accion_efecto_planta(hierba_del_sapo, diurético).
accion_efecto_planta(hierba_del_sapo, antiséptico).
modo_tratamiento(hierba_del_sapo, tomar_decoccion_dos_veces_dia).
precaucion_planta(hierba_del_sapo, evitar_en_embarazo).
enfermedad(problemas_renales).
enfermedad(infecciones_urinarias).
sintoma_enfermedad(problemas_renales, dolor_renal).
sintoma_enfermedad(infecciones_urinarias, ardor_al_orinar).

%------------------ HIERBA DEL ZORRILLO ------------------
planta(hierba_del_zorrillo).
nombre_cientifico(hierba_del_zorrillo, petiveria_alliacea).
continente_origen(hierba_del_zorrillo, america).
pais_origen(hierba_del_zorrillo, mexico).
modo_preparacion(hierba_del_zorrillo, infusion).
trata_enfermedad(hierba_del_zorrillo, reumatismo).
trata_enfermedad(hierba_del_zorrillo, dolor_muscular).
accion_efecto_planta(hierba_del_zorrillo, antiinflamatorio).
accion_efecto_planta(hierba_del_zorrillo, analgésico).
modo_tratamiento(hierba_del_zorrillo, tomar_infusion_una_vez_dia).
precaucion_planta(hierba_del_zorrillo, evitar_en_embarazo).
enfermedad(reumatismo).
enfermedad(dolor_muscular).
sintoma_enfermedad(reumatismo, dolor_articular).
sintoma_enfermedad(dolor_muscular, dolor_muscular).

%------------------ HINOJO ------------------
planta(hinojo).
nombre_cientifico(hinojo, foeniculum_vulgare).
continente_origen(hinojo, europa).
pais_origen(hinojo, region_mediterranea).
modo_preparacion(hinojo, infusion).
trata_enfermedad(hinojo, gases).
trata_enfermedad(hinojo, flatulencias).
accion_efecto_planta(hinojo, digestivo).
modo_tratamiento(hinojo, tomar_te).
precaucion_planta(hinojo, embarazo).
enfermedad(gases).
enfermedad(flatulencias).
sintoma_enfermedad(gases, distension_abdominal).
sintoma_enfermedad(flatulencias, no_especificado).

%------------------ IPECACUANA ------------------
planta(ipecacuana).
nombre_cientifico(ipecacuana, polygala_hondurana).
continente_origen(ipecacuana, america).
pais_origen(ipecacuana, mexico).
modo_preparacion(ipecacuana, infusion).
trata_enfermedad(ipecacuana, tos).
accion_efecto_planta(ipecacuana, expectorante).
modo_tratamiento(ipecacuana, tomar_infusion).
precaucion_planta(ipecacuana, no_especificado).
enfermedad(tos).
sintoma_enfermedad(tos, tos_seca).

%------------------ JALAPA ------------------
planta(jalapa).
nombre_cientifico(jalapa, ipomea_purga).
continente_origen(jalapa, america).
pais_origen(jalapa, mexico).
modo_preparacion(jalapa, cocimiento).
trata_enfermedad(jalapa, disenteria).
trata_enfermedad(jalapa, estreñimiento).
trata_enfermedad(jalapa, indigestión).
trata_enfermedad(jalapa, apoplejía).
trata_enfermedad(jalapa, congestion_cerebral).
accion_efecto_planta(jalapa, purgante).
modo_tratamiento(jalapa, tomar_en_ayunas).
precaucion_planta(jalapa, no_especificado).
enfermedad(disenteria).
enfermedad(estreñimiento).
enfermedad(indigestión).
enfermedad(apoplejía).
enfermedad(congestion_cerebral).
sintoma_enfermedad(disenteria, diarrea).
sintoma_enfermedad(estreñimiento, dificultad_defecacion).
sintoma_enfermedad(indigestión, malestar_abdominal).
sintoma_enfermedad(apoplejía, no_especificado).
sintoma_enfermedad(congestion_cerebral, no_especificado).

%------------------ JAZMÍN AMARILLO ------------------
planta(jazminamarillo).
nombre_cientifico(jazminamarillo, gelsemium_sempervirens).
continente_origen(jazminamarillo, america).
pais_origen(jazminamarillo, mexico).
modo_preparacion(jazminamarillo, tintura).
trata_enfermedad(jazminamarillo, dolores_de_cabeza).
trata_enfermedad(jazminamarillo, reuma).
trata_enfermedad(jazminamarillo, espasmos).
trata_enfermedad(jazminamarillo, asma_bronquial).
trata_enfermedad(jazminamarillo, menstruacion_dolorosa).
accion_efecto_planta(jazminamarillo, analgésico).
accion_efecto_planta(jazminamarillo, antiespasmódico).
modo_tratamiento(jazminamarillo, tomar_tintura).
precaucion_planta(jazminamarillo, problemas_corazon).
precaucion_planta(jazminamarillo, problemas_riñones).
enfermedad(dolores_de_cabeza).
enfermedad(reuma).
enfermedad(espasmos).
enfermedad(asma_bronquial).
enfermedad(menstruacion_dolorosa).
sintoma_enfermedad(dolores_de_cabeza, dolor_intenso).
sintoma_enfermedad(reuma, dolor_articular).
sintoma_enfermedad(espasmos, contraccion_muscular).
sintoma_enfermedad(asma_bronquial, dificultad_respiratoria).
sintoma_enfermedad(menstruacion_dolorosa, no_especificado).

%------------------ LAVANDA ------------------
planta(lavanda).
nombre_cientifico(lavanda, lavandula_angustifolia).
continente_origen(lavanda, europa).
pais_origen(lavanda, francia).
modo_preparacion(lavanda, infusion).
trata_enfermedad(lavanda, insomnio).
trata_enfermedad(lavanda, ansiedad).
trata_enfermedad(lavanda, dolor_de_cabeza).
accion_efecto_planta(lavanda, relajante).
accion_efecto_planta(lavanda, sedante).
modo_tratamiento(lavanda, tomar_infusion_antes_dormir).
precaucion_planta(lavanda, evitar_en_hipotension).
enfermedad(insomnio).
enfermedad(ansiedad).
enfermedad(dolor_de_cabeza).
sintoma_enfermedad(insomnio, dificultad_para_dormir).
sintoma_enfermedad(ansiedad, nerviosismo).
sintoma_enfermedad(dolor_de_cabeza, dolor_cabeza).

%------------------ LAVANDA (continuación) ------------------
sintoma_enfermedad(insomnio, dificultad_para_dormir).
sintoma_enfermedad(ansiedad, nerviosismo).
sintoma_enfermedad(dolor_de_cabeza, dolor_cabeza).

%------------------ LECHUGA ------------------
planta(lechuga).
nombre_cientifico(lechuga, lactuca_sativa).
continente_origen(lechuga, europa).
pais_origen(lechuga, region_mediterranea).
modo_preparacion(lechuga, infusion).
trata_enfermedad(lechuga, insomnio).
trata_enfermedad(lechuga, ansiedad).
accion_efecto_planta(lechuga, sedante).
accion_efecto_planta(lechuga, relajante).
modo_tratamiento(lechuga, tomar_infusion_antes_dormir).
precaucion_planta(lechuga, evitar_exceso).
enfermedad(insomnio).
enfermedad(ansiedad).
sintoma_enfermedad(insomnio, dificultad_para_dormir).
sintoma_enfermedad(ansiedad, nerviosismo).

%------------------ LENGUA DE VACA ------------------
planta(lengua_de_vaca).
nombre_cientifico(lengua_de_vaca, rumex_crispus).
continente_origen(lengua_de_vaca, europa).
pais_origen(lengua_de_vaca, desconocido).
modo_preparacion(lengua_de_vaca, decoccion).
trata_enfermedad(lengua_de_vaca, problemas_hepaticos).
trata_enfermedad(lengua_de_vaca, estreñimiento).
accion_efecto_planta(lengua_de_vaca, laxante).
accion_efecto_planta(lengua_de_vaca, depurativo).
modo_tratamiento(lengua_de_vaca, tomar_decoccion_una_vez_dia).
precaucion_planta(lengua_de_vaca, evitar_en_embarazo).
enfermedad(problemas_hepaticos).
enfermedad(estreñimiento).
sintoma_enfermedad(problemas_hepaticos, dolor_abdominal).
sintoma_enfermedad(estreñimiento, dificultad_defecacion).

%------------------ LIMA ------------------
planta(lima).
nombre_cientifico(lima, citrus_aurantifolia).
continente_origen(lima, asia).
pais_origen(lima, india).
modo_preparacion(lima, jugo).
trata_enfermedad(lima, resfriado).
trata_enfermedad(lima, infecciones_garganta).
accion_efecto_planta(lima, antiséptico).
accion_efecto_planta(lima, vitamínico).
modo_tratamiento(lima, tomar_jugo_dos_veces_dia).
precaucion_planta(lima, evitar_exposición_solar_tras_uso_tópico).
enfermedad(resfriado).
enfermedad(infecciones_garganta).
sintoma_enfermedad(resfriado, congestion_nasal).
sintoma_enfermedad(infecciones_garganta, dolor_garganta).

%------------------ LLANTÉN ------------------
planta(llanten).
nombre_cientifico(llanten, plantago_major).
continente_origen(llanten, europa).
pais_origen(llanten, desconocido).
modo_preparacion(llanten, infusion).
trata_enfermedad(llanten, tos).
trata_enfermedad(llanten, diarrea).
trata_enfermedad(llanten, heridas).
accion_efecto_planta(llanten, expectorante).
accion_efecto_planta(llanten, astringente).
accion_efecto_planta(llanten, cicatrizante).
modo_tratamiento(llanten, tomar_infusion_o_aplicar_tópicamente).
precaucion_planta(llanten, evitar_en_alergias).
enfermedad(tos).
enfermedad(diarrea).
enfermedad(heridas).
sintoma_enfermedad(tos, irritacion_garganta).
sintoma_enfermedad(diarrea, heces_blandas).
sintoma_enfermedad(heridas, piel_lesionada).

%------------------ LUPULO ------------------
planta(lupulo).
nombre_cientifico(lupulo, humulus_lupulus).
continente_origen(lupulo, europa).
pais_origen(lupulo, alemania).
modo_preparacion(lupulo, infusion).
trata_enfermedad(lupulo, insomnio).
trata_enfermedad(lupulo, ansiedad).
accion_efecto_planta(lupulo, sedante).
accion_efecto_planta(lupulo, relajante).
modo_tratamiento(lupulo, tomar_infusion_antes_dormir).
precaucion_planta(lupulo, evitar_en_depresión).
enfermedad(insomnio).
enfermedad(ansiedad).
sintoma_enfermedad(insomnio, dificultad_para_dormir).
sintoma_enfermedad(ansiedad, nerviosismo).

%------------------ MAGNOLIA ------------------
planta(magnolia).
nombre_cientifico(magnolia, magnolia_grandiflora).
continente_origen(magnolia, america).
pais_origen(magnolia, estados_unidos).
modo_preparacion(magnolia, infusion).
trata_enfermedad(magnolia, ansiedad).
trata_enfermedad(magnolia, hipertension).
accion_efecto_planta(magnolia, relajante).
accion_efecto_planta(magnolia, hipotensor).
modo_tratamiento(magnolia, infusion, diaria).
precaucion_planta(magnolia, evitar_en_hipotension).
enfermedad(ansiedad).
enfermedad(hipertension).
sintoma_enfermedad(ansiedad, nerviosismo).
sintoma_enfermedad(hipertension, dolor_cabeza).

%------------------ MALVA ------------------
planta(malva).
nombre_cientifico(malva, malva_sylvestris).
continente_origen(malva, europa).
pais_origen(malva, region_mediterranea).
modo_preparacion(malva, infusion).
trata_enfermedad(malva, tos).
trata_enfermedad(malva, inflamacion_garganta).
trata_enfermedad(malva, estreñimiento).
accion_efecto_planta(malva, emoliente).
accion_efecto_planta(malva, laxante).
modo_tratamiento(malva, tomar_infusion_dos_veces_dia).
precaucion_planta(malva, evitar_exceso).
enfermedad(tos).
enfermedad(inflamacion_garganta).
enfermedad(estreñimiento).
sintoma_enfermedad(tos, irritacion_garganta).
sintoma_enfermedad(inflamacion_garganta, dolor_garganta).
sintoma_enfermedad(estreñimiento, dificultad_defecacion).

%------------------ MANZANILLA ------------------
planta(manzanilla).
nombre_cientifico(manzanilla, matricaria_chamomilla).
continente_origen(manzanilla, europa).
pais_origen(manzanilla, alemania).
modo_preparacion(manzanilla, infusion).
trata_enfermedad(manzanilla, problemas_digestivos).
trata_enfermedad(manzanilla, insomnio).
trata_enfermedad(manzanilla, ansiedad).
accion_efecto_planta(manzanilla, digestivo).
accion_efecto_planta(manzanilla, sedante).
modo_tratamiento(manzanilla, tomar_infusion_dos_veces_dia).
precaucion_planta(manzanilla, evitar_en_alergias).
enfermedad(problemas_digestivos).
enfermedad(insomnio).
enfermedad(ansiedad).
sintoma_enfermedad(problemas_digestivos, dolor_abdominal).
sintoma_enfermedad(insomnio, dificultad_para_dormir).
sintoma_enfermedad(ansiedad, nerviosismo).

%------------------ MARRUBIO ------------------
planta(marrubio).
nombre_cientifico(marrubio, marrubium_vulgare).
continente_origen(marrubio, europa).
pais_origen(marrubio, region_mediterranea).
modo_preparacion(marrubio, infusion).
trata_enfermedad(marrubio, tos).
trata_enfermedad(marrubio, problemas_digestivos).
accion_efecto_planta(marrubio, expectorante).
accion_efecto_planta(marrubio, digestivo).
modo_tratamiento(marrubio, tomar_infusion_dos_veces_dia).
precaucion_planta(marrubio, evitar_en_embarazo).
enfermedad(tos).
enfermedad(problemas_digestivos).
sintoma_enfermedad(tos, irritacion_garganta).
sintoma_enfermedad(problemas_digestivos, dolor_abdominal).

%------------------ MENTA ------------------
planta(menta).
nombre_cientifico(menta, mentha_piperita).
continente_origen(menta, europa).
pais_origen(menta, inglaterra).
modo_preparacion(menta, infusion).
trata_enfermedad(menta, problemas_digestivos).
trata_enfermedad(menta, dolor_de_cabeza).
trata_enfermedad(menta, resfriado).
accion_efecto_planta(menta, digestivo).
accion_efecto_planta(menta, analgésico).
modo_tratamiento(menta, tomar_infusion_dos_veces_dia).
precaucion_planta(menta, evitar_en_reflujo_gastroesofágico).
enfermedad(problemas_digestivos).
enfermedad(dolor_de_cabeza).
enfermedad(resfriado).
sintoma_enfermedad(problemas_digestivos, dolor_abdominal).
sintoma_enfermedad(dolor_de_cabeza, dolor_cabeza).
sintoma_enfermedad(resfriado, congestion_nasal).

%------------------ MIRTO ------------------
planta(mirto).
nombre_cientifico(mirto, myrtus_communis).
continente_origen(mirto, europa).
pais_origen(mirto, region_mediterranea).
modo_preparacion(mirto, infusion).
trata_enfermedad(mirto, infecciones_urinarias).
trata_enfermedad(mirto, bronquitis).
accion_efecto_planta(mirto, antiséptico).
accion_efecto_planta(mirto, expectorante).
modo_tratamiento(mirto, tomar_infusion_dos_veces_dia).
precaucion_planta(mirto, evitar_exceso).
enfermedad(infecciones_urinarias).
enfermedad(bronquitis).
sintoma_enfermedad(infecciones_urinarias, ardor_al_orinar).
sintoma_enfermedad(bronquitis, tos_con_flemas).

%------------------ NOGAL ------------------
planta(nogal).
nombre_cientifico(nogal, juglans_regia).
continente_origen(nogal, asia).
pais_origen(nogal, persia).
modo_preparacion(nogal, decoccion).
trata_enfermedad(nogal, diabetes).
trata_enfermedad(nogal, problemas_hepaticos).
accion_efecto_planta(nogal, hipoglucemiante).
accion_efecto_planta(nogal, depurativo).
modo_tratamiento(nogal, tomar_decoccion_una_vez_dia).
precaucion_planta(nogal, evitar_en_alergias).
enfermedad(diabetes).
enfermedad(problemas_hepaticos).
sintoma_enfermedad(diabetes, sed_excesiva).
sintoma_enfermedad(problemas_hepaticos, dolor_abdominal).

%------------------ NOPAL ------------------
planta(nopal).
nombre_cientifico(nopal, opuntia_ficus_indica).
continente_origen(nopal, america).
pais_origen(nopal, mexico).
modo_preparacion(nopal, jugo).
trata_enfermedad(nopal, diabetes).
trata_enfermedad(nopal, colesterol).
accion_efecto_planta(nopal, hipoglucemiante).
accion_efecto_planta(nopal, hipolipemiante).
modo_tratamiento(nopal, tomar_jugo_en_ayunas).
precaucion_planta(nopal, evitar_exceso).
enfermedad(diabetes).
enfermedad(colesterol).
sintoma_enfermedad(diabetes, sed_excesiva).
sintoma_enfermedad(colesterol, dolor_pecho).

%------------------ ORÉGANO ------------------
planta(oregano).
nombre_cientifico(oregano, origanum_vulgare).
continente_origen(oregano, europa).
pais_origen(oregano, region_mediterranea).
modo_preparacion(oregano, infusion).
trata_enfermedad(oregano, problemas_digestivos).
trata_enfermedad(oregano, tos).
accion_efecto_planta(oregano, digestivo).
accion_efecto_planta(oregano, expectorante).
modo_tratamiento(oregano, tomar_infusion_dos_veces_dia).
precaucion_planta(oregano, evitar_en_embarazo).
enfermedad(problemas_digestivos).
enfermedad(tos).
sintoma_enfermedad(problemas_digestivos, dolor_abdominal).
sintoma_enfermedad(tos, irritacion_garganta).

%------------------ ORTIGA ------------------
planta(ortiga).
nombre_cientifico(ortiga, urtica_dioica).
continente_origen(ortiga, europa).
pais_origen(ortiga, desconocido).
modo_preparacion(ortiga, infusion).
trata_enfermedad(ortiga, anemia).
trata_enfermedad(ortiga, reumatismo).
trata_enfermedad(ortiga, alergias).
accion_efecto_planta(ortiga, remineralizante).
accion_efecto_planta(ortiga, antiinflamatorio).
modo_tratamiento(ortiga, tomar_infusion_una_vez_dia).
precaucion_planta(ortiga, evitar_contacto_piel_cruda).
enfermedad(anemia).
enfermedad(reumatismo).
enfermedad(alergias).
sintoma_enfermedad(anemia, fatiga).
sintoma_enfermedad(reumatismo, dolor_articular).
sintoma_enfermedad(alergias, estornudos).

%------------------ PASIFLORA ------------------
planta(pasiflora).
nombre_cientifico(pasiflora, passiflora_incarnata).
continente_origen(pasiflora, america).
pais_origen(pasiflora, estados_unidos).
modo_preparacion(pasiflora, infusion).
trata_enfermedad(pasiflora, insomnio).
trata_enfermedad(pasiflora, ansiedad).
trata_enfermedad(pasiflora, hipertension).
accion_efecto_planta(pasiflora, sedante).
accion_efecto_planta(pasiflora, relajante).
modo_tratamiento(pasiflora, tomar_infusion_antes_dormir).
precaucion_planta(pasiflora, evitar_en_hipotension).
enfermedad(insomnio).
enfermedad(ansiedad).
enfermedad(hipertension).
sintoma_enfermedad(insomnio, dificultad_para_dormir).
sintoma_enfermedad(ansiedad, nerviosismo).
sintoma_enfermedad(hipertension, dolor_cabeza).

%------------------ PEREJIL ------------------
planta(perejil).
nombre_cientifico(perejil, petroselinum_crispum).
continente_origen(perejil, europa).
pais_origen(perejil, region_mediterranea).
modo_preparacion(perejil, infusion).
trata_enfermedad(perejil, infecciones_urinarias).
trata_enfermedad(perejil, retencion_liquidos).
accion_efecto_planta(perejil, diurético).
accion_efecto_planta(perejil, antiséptico).
modo_tratamiento(perejil, tomar_infusion_dos_veces_dia).
precaucion_planta(perejil, evitar_en_embarazo).
enfermedad(infecciones_urinarias).
enfermedad(retencion_liquidos).
sintoma_enfermedad(infecciones_urinarias, ardor_al_orinar).
sintoma_enfermedad(retencion_liquidos, hinchazon).

%------------------ PINO ------------------
planta(pino).
nombre_cientifico(pino, pinus_sylvestris).
continente_origen(pino, europa).
pais_origen(pino, escocia).
modo_preparacion(pino, vaporizacion).
trata_enfermedad(pino, resfriado).
trata_enfermedad(pino, bronquitis).
accion_efecto_planta(pino, expectorante).
accion_efecto_planta(pino, antiséptico).
modo_tratamiento(pino, inhalar_vapor_dos_veces_dia).
precaucion_planta(pino, evitar_aceite_esencial_puro).
enfermedad(resfriado).
enfermedad(bronquitis).
sintoma_enfermedad(resfriado, congestion_nasal).
sintoma_enfermedad(bronquitis, tos_con_flemas).

%------------------ QUINA ------------------
planta(quina).
nombre_cientifico(quina, cinchona_officinalis).
continente_origen(quina, america).
pais_origen(quina, peru).
modo_preparacion(quina, decoccion).
trata_enfermedad(quina, fiebre).
trata_enfermedad(quina, malaria).
accion_efecto_planta(quina, antipirético).
accion_efecto_planta(quina, antimalárico).
modo_tratamiento(quina, tomar_decoccion_dos_veces_dia).
precaucion_planta(quina, evitar_en_embarazo).
enfermedad(fiebre).
enfermedad(malaria).
sintoma_enfermedad(fiebre, temperatura_elevada).
sintoma_enfermedad(malaria, fiebre_intermitente).

%------------------ REGALIZ ------------------
planta(regaliz).
nombre_cientifico(regaliz, glycyrrhiza_glabra).
continente_origen(regaliz, europa).
pais_origen(regaliz, region_mediterranea).
modo_preparacion(regaliz, infusion).
trata_enfermedad(regaliz, tos).
trata_enfermedad(regaliz, problemas_digestivos).
accion_efecto_planta(regaliz, expectorante).
accion_efecto_planta(regaliz, digestivo).
modo_tratamiento(regaliz, tomar_infusion_dos_veces_dia).
precaucion_planta(regaliz, evitar_en_hipertension).
enfermedad(tos).
enfermedad(problemas_digestivos).
sintoma_enfermedad(tos, irritacion_garganta).
sintoma_enfermedad(problemas_digestivos, dolor_abdominal).

%------------------ ROMERO ------------------
planta(romero).
nombre_cientifico(romero, rosmarinus_officinalis).
continente_origen(romero, europa).
pais_origen(romero, region_mediterranea).
modo_preparacion(romero, infusion).
trata_enfermedad(romero, problemas_digestivos).
trata_enfermedad(romero, dolor_muscular).
trata_enfermedad(romero, fatiga).
accion_efecto_planta(romero, digestivo).
accion_efecto_planta(romero, estimulante).
modo_tratamiento(romero, tomar_infusion_o_aplicar_tópicamente).
precaucion_planta(romero, evitar_en_hipertension).
enfermedad(problemas_digestivos).
enfermedad(dolor_muscular).
enfermedad(fatiga).
sintoma_enfermedad(problemas_digestivos, dolor_abdominal).
sintoma_enfermedad(dolor_muscular, dolor_muscular).
sintoma_enfermedad(fatiga, cansancio).

%------------------ RUDA ------------------
planta(ruda).
nombre_cientifico(ruda, ruta_graveolens).
continente_origen(ruda, europa).
pais_origen(ruda, region_mediterranea).
modo_preparacion(ruda, infusion).
trata_enfermedad(ruda, colicos).
trata_enfermedad(ruda, problemas_menstruales).
accion_efecto_planta(ruda, antiespasmódico).
accion_efecto_planta(ruda, emenagogo).
modo_tratamiento(ruda, tomar_infusion_una_vez_dia).
precaucion_planta(ruda, evitar_en_embarazo_toxicidad).
enfermedad(colicos).
enfermedad(problemas_menstruales).
sintoma_enfermedad(colicos, dolor_abdominal).
sintoma_enfermedad(problemas_menstruales, dolor_pelvico).

%------------------ SALVIA ------------------
planta(salvia).
nombre_cientifico(salvia, salvia_officinalis).
continente_origen(salvia, europa).
pais_origen(salvia, region_mediterranea).
modo_preparacion(salvia, infusion).
trata_enfermedad(salvia, problemas_digestivos).
trata_enfermedad(salvia, inflamacion_garganta).
trata_enfermedad(salvia, sudoracion_excesiva).
accion_efecto_planta(salvia, digestivo).
accion_efecto_planta(salvia, antiséptico).
modo_tratamiento(salvia, tomar_infusion_dos_veces_dia).
precaucion_planta(salvia, evitar_en_embarazo).
enfermedad(problemas_digestivos).
enfermedad(inflamacion_garganta).
enfermedad(sudoracion_excesiva).
sintoma_enfermedad(problemas_digestivos, dolor_abdominal).
sintoma_enfermedad(inflamacion_garganta, dolor_garganta).
sintoma_enfermedad(sudoracion_excesiva, sudor_excesivo).

%------------------ SAUCO ------------------
planta(sauco).
nombre_cientifico(sauco, sambucus_nigra).
continente_origen(sauco, europa).
pais_origen(sauco, desconocido).
modo_preparacion(sauco, infusion).
trata_enfermedad(sauco, resfriado).
trata_enfermedad(sauco, fiebre).
trata_enfermedad(sauco, tos).
accion_efecto_planta(sauco, sudorífico).
accion_efecto_planta(sauco, expectorante).
modo_tratamiento(sauco, tomar_infusion_dos_veces_dia).
precaucion_planta(sauco, evitar_uso_frutos_crudos_toxicidad).
enfermedad(resfriado).
enfermedad(fiebre).
enfermedad(tos).
sintoma_enfermedad(resfriado, congestion_nasal).
sintoma_enfermedad(fiebre, temperatura_elevada).
sintoma_enfermedad(tos, irritacion_garganta).

%------------------ TABAQUILLO ------------------
planta(tabaquillo).
nombre_cientifico(tabaquillo, nicotiana_glauca).
continente_origen(tabaquillo, america).
pais_origen(tabaquillo, mexico).
modo_preparacion(tabaquillo, cataplasma).
trata_enfermedad(tabaquillo, heridas).
trata_enfermedad(tabaquillo, inflamacion).
accion_efecto_planta(tabaquillo, antiinflamatorio).
accion_efecto_planta(tabaquillo, cicatrizante).
modo_tratamiento(tabaquillo, aplicar_cataplasma_dos_veces_dia).
precaucion_planta(tabaquillo, toxica_si_se_ingiere).
enfermedad(heridas).
enfermedad(inflamacion).
sintoma_enfermedad(heridas, piel_lesionada).
sintoma_enfermedad(inflamacion, hinchazon).

%------------------ TEJOCOTE ------------------
planta(tejocote).
nombre_cientifico(tejocote, crataegus_mexicana).
continente_origen(tejocote, america).
pais_origen(tejocote, mexico).
modo_preparacion(tejocote, infusion).
trata_enfermedad(tejocote, hipertension).
trata_enfermedad(tejocote, problemas_cardiacos).
accion_efecto_planta(tejocote, cardiotónico).
accion_efecto_planta(tejocote, hipotensor).
modo_tratamiento(tejocote, tomar_infusion_una_vez_dia).
precaucion_planta(tejocote, evitar_en_hipotension).
enfermedad(hipertension).
enfermedad(problemas_cardiacos).
sintoma_enfermedad(hipertension, dolor_cabeza).
sintoma_enfermedad(problemas_cardiacos, dolor_pecho).

%------------------ TILA ------------------
planta(tila).
nombre_cientifico(tila, tilia_cordata).
continente_origen(tila, europa).
pais_origen(tila, alemania).
modo_preparacion(tila, infusion).
trata_enfermedad(tila, insomnio).
trata_enfermedad(tila, ansiedad).
trata_enfermedad(tila, hipertension).
accion_efecto_planta(tila, sedante).
accion_efecto_planta(tila, relajante).
modo_tratamiento(tila, tomar_infusion_antes_dormir).
precaucion_planta(tila, evitar_en_hipotension).
enfermedad(insomnio).
enfermedad(ansiedad).
enfermedad(hipertension).
sintoma_enfermedad(insomnio, dificultad_para_dormir).
sintoma_enfermedad(ansiedad, nerviosismo).
sintoma_enfermedad(hipertension, dolor_cabeza).

%------------------ TOMILLO ------------------
planta(tomillo).
nombre_cientifico(tomillo, thymus_vulgaris).
continente_origen(tomillo, europa).
pais_origen(tomillo, region_mediterranea).
modo_preparacion(tomillo, infusion).
trata_enfermedad(tomillo, tos).
trata_enfermedad(tomillo, infecciones_garganta).
trata_enfermedad(tomillo, problemas_digestivos).
accion_efecto_planta(tomillo, expectorante).
accion_efecto_planta(tomillo, antiséptico).
modo_tratamiento(tomillo, tomar_infusion_dos_veces_dia).
precaucion_planta(tomillo, evitar_en_embarazo).
enfermedad(tos).
enfermedad(infecciones_garganta).
enfermedad(problemas_digestivos).
sintoma_enfermedad(tos, irritacion_garganta).
sintoma_enfermedad(infecciones_garganta, dolor_garganta).
sintoma_enfermedad(problemas_digestivos, dolor_abdominal).

%------------------ VALERIANA ------------------
planta(valeriana).
nombre_cientifico(valeriana, valeriana_officinalis).
continente_origen(valeriana, europa).
pais_origen(valeriana, desconocido).
modo_preparacion(valeriana, infusion).
trata_enfermedad(valeriana, insomnio).
trata_enfermedad(valeriana, ansiedad).
trata_enfermedad(valeriana, estrés).
accion_efecto_planta(valeriana, sedante).
accion_efecto_planta(valeriana, relajante).
modo_tratamiento(valeriana, tomar_infusion_antes_dormir).
precaucion_planta(valeriana, evitar_en_depresión).
enfermedad(insomnio).
enfermedad(ansiedad).
enfermedad(estrés).
sintoma_enfermedad(insomnio, dificultad_para_dormir).
sintoma_enfermedad(ansiedad, nerviosismo).
sintoma_enfermedad(estrés, tensión_muscular).

%------------------ VIBORNO ------------------
planta(viborno).
nombre_cientifico(viborno, viburnum_opulus).
continente_origen(viborno, europa).
pais_origen(viborno, desconocido).
modo_preparacion(viborno, infusion).
trata_enfermedad(viborno, colicos_menstruales).
trata_enfermedad(viborno, espasmos).
accion_efecto_planta(viborno, antiespasmódico).
accion_efecto_planta(viborno, relajante).
modo_tratamiento(viborno, tomar_infusion_una_vez_dia).
precaucion_planta(viborno, evitar_en_embarazo).
enfermedad(colicos_menstruales).
enfermedad(espasmos).
sintoma_enfermedad(colicos_menstruales, dolor_pelvico).
sintoma_enfermedad(espasmos, contraccion_muscular).

%------------------ YERBABUENA ------------------
planta(yerbabuena).
nombre_cientifico(yerbabuena, mentha_spicata).
continente_origen(yerbabuena, europa).
pais_origen(yerbabuena, region_mediterranea).
modo_preparacion(yerbabuena, infusion).
trata_enfermedad(yerbabuena, problemas_digestivos).
trata_enfermedad(yerbabuena, flatulencias).
trata_enfermedad(yerbabuena, colicos).
accion_efecto_planta(yerbabuena, digestivo).
accion_efecto_planta(yerbabuena, carminativo).
modo_tratamiento(yerbabuena, tomar_infusion_dos_veces_dia).
precaucion_planta(yerbabuena, evitar_en_reflujo_gastroesofágico).
enfermedad(problemas_digestivos).
enfermedad(flatulencias).
enfermedad(colicos).
sintoma_enfermedad(problemas_digestivos, dolor_abdominal).
sintoma_enfermedad(flatulencias, gases).
sintoma_enfermedad(colicos, dolor_abdominal).

%------------------ ZACATE LIMÓN ------------------
planta(zacate_limon).
nombre_cientifico(zacate_limon, cymbopogon_citratus).
continente_origen(zacate_limon, asia).
pais_origen(zacate_limon, india).
modo_preparacion(zacate_limon, infusion).
trata_enfermedad(zacate_limon, problemas_digestivos).
trata_enfermedad(zacate_limon, ansiedad).
trata_enfermedad(zacate_limon, fiebre).
accion_efecto_planta(zacate_limon, digestivo).
accion_efecto_planta(zacate_limon, relajante).
modo_tratamiento(zacate_limon, tomar_infusion_dos_veces_dia).
precaucion_planta(zacate_limon, evitar_en_hipotension).
enfermedad(problemas_digestivos).
enfermedad(ansiedad).
enfermedad(fiebre).
sintoma_enfermedad(problemas_digestivos, dolor_abdominal).
sintoma_enfermedad(ansiedad, nerviosismo).
sintoma_enfermedad(fiebre, temperatura_elevada).

%------------------ ZAPOTE BLANCO ------------------
planta(zapote_blanco).
nombre_cientifico(zapote_blanco, casimiroa_edulis).
continente_origen(zapote_blanco, america).
pais_origen(zapote_blanco, mexico).
modo_preparacion(zapote_blanco, infusion).
trata_enfermedad(zapote_blanco, hipertension).
trata_enfermedad(zapote_blanco, insomnio).
accion_efecto_planta(zapote_blanco, hipotensor).
accion_efecto_planta(zapote_blanco, sedante).
modo_tratamiento(zapote_blanco, tomar_infusion_una_vez_dia).
precaucion_planta(zapote_blanco, evitar_en_hipotension).
enfermedad(hipertension).
enfermedad(insomnio).
sintoma_enfermedad(hipertension, dolor_cabeza).
sintoma_enfermedad(insomnio, dificultad_para_dormir).

%------------------ UVA ------------------
planta(uva).
nombre_cientifico(uva, vitis_vinifera).
continente_origen(uva, europa).
pais_origen(uva, region_mediterranea).
modo_preparacion(uva, jugo).
trata_enfermedad(uva, colesterol).
trata_enfermedad(uva, problemas_cardiacos).
accion_efecto_planta(uva, antioxidante).
accion_efecto_planta(uva, cardiotónico).
modo_tratamiento(uva, tomar_jugo_diario).
precaucion_planta(uva, evitar_exceso_en_diabetes).
enfermedad(colesterol).
enfermedad(problemas_cardiacos).
sintoma_enfermedad(colesterol, dolor_pecho).
sintoma_enfermedad(problemas_cardiacos, dolor_pecho).


% Nuevas enfermedades y síntomas
enfermedad(quemaduras).
enfermedad(heridas).
enfermedad(problemas_digestivos).
enfermedad(herpes).
sintoma_enfermedad(quemaduras, dolor_piel).
sintoma_enfermedad(quemaduras, enrojecimiento).
sintoma_enfermedad(heridas, piel_lesionada).
sintoma_enfermedad(heridas, sangrado_leve).
sintoma_enfermedad(problemas_digestivos, dolor_abdominal).
sintoma_enfermedad(problemas_digestivos, hinchazon).
sintoma_enfermedad(herpes, ampollas_piel).
sintoma_enfermedad(herpes, picazon).

% Elementos químicos de las plantas
elementos_planta(manzanilla, [flavonoides, camazuleno, bisabolol]).
elementos_planta(sabila, [aloina, antraquinonas, polisacaridos]).
elementos_planta(sauce, [salicina, taninos, flavonoides]).
elementos_planta(quina, [quinina, quinidina, cinconina]).
elementos_planta(adormidera, [morfina, codeina, tebaina]).

% Medicamentos derivados de plantas
produce_medicamento(sauce, aspirina).
produce_medicamento(quina, quinina).
produce_medicamento(adormidera, morfina).
produce_medicamento(adormidera, codeina).

% Efectos de los medicamentos derivados
efectos_medicamento(aspirina, [analgesico, antiinflamatorio, antipiretico]).
efectos_medicamento(quinina, [antimalarico, antipiretico]).
efectos_medicamento(morfina, [analgesico, sedante]).
efectos_medicamento(codeina, [analgesico, antitusivo]).

% Tratamientos con medicamentos no derivados de plantas
tratamiento_medicamento(gripe, 'Paracetamol, descanso, hidratación').
tratamiento_medicamento(herpes, 'Aciclovir, cremas antivirales').
tratamiento_medicamento(diabetes, 'Insulina, metformina, control dietético').
tratamiento_medicamento(tos, 'Dextrometorfano, jarabes expectorantes').
