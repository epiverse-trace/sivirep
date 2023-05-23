default:
  base_path_microdata: "https://portalsivigila.ins.gov.co/_api/"
  file_path_parameters_microdata: "/$value?binaryStringResponseBody=true"
  path_microdata: "web/lists/GetByTitle('Microdatos')/"
  query_path_microdata: "items?$select=*,FileRef&$filter=(A_x00f1_o%20eq%20%27_year_%27)and(NombreEvento%20eq%20%27_disease_%27)"
  query_diseases_by_year_path: "https://portalsivigila.ins.gov.co/_api/web/lists/GetByTitle('Microdatos')/items?$select=Evento,A_x00f1_o,NombreEvento&$orderby=NombreEvento%20asc&$top=1000"
  name_file_split: "/Microdatos/"
  data_delim: ["|", "," , ";", ":", "-"]
  special_populations_cols: ["gp_discapa","gp_desplaz","gp_migrant","gp_carcela","gp_gestan","gp_indigen","gp_pobicfb","gp_mad_com","gp_desmovi","gp_psiquia","gp_vic_vio","gp_otros"]
  special_populations_names: ["Discapacitados","Desplazados","Migrantes","Carcelarios","Gestantes","Indigenas","ICBF","Madres comunitarias","Desmovilizados","Pob. Centros psiquiatricos","Victimas de violencia armada","Otros"]
  sivigila_open_data_path: "https://www.datos.gov.co/api/views/qvnt-2igj/rows.csv?accessType=DOWNLOAD"
  geo_data_path: "https://www.datos.gov.co/api/views/gdxc-w37w/rows.csv?accessType=DOWNLOAD"
  data_folder: "data/"
  age_categorie_conditionals: [ "age_num < 2", "age_num >= 2 && age_num <= 4", "age_num >= 5 && age_num <= 14", "age_num >= 15 && age_num <= 39", "age_num >=40 && age_num <= 59", "age_num >= 60"]
  age_categorie_labels: [ "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más"]
  respiratory_virus_column_names: [ "influenza_a_h1_2009", "influenza_a_h1", "influenza_a_h3", "influenza_b", "metapneumovirus_humano", "rinovirus_enterovirus_humano", "virus_sincitial_respiratorio", "adenovirus"]
  respiratory_virus_names: [ "H1N1 2009", "H1N1", "H3N1", "Influenza B", "Metapneumovirus", "Rinovirus", "VSR", "Adenovirus"]
  dates_column_names: ["fecha_nto", "fec_not", "ini_sin", "fec_hos", "fec_def", "fec_con"]
  depto_column_names: ["cod_dpto_o", "cod_dpto_r"]
  geo_column_names: ["cod_dpto_o", "cod_dpto_r", "cod_mun_o", "cod_mun_r", "cod_mpio_o", "cod_mpio_r"]
  occurrence_geo_diseases:
    cod_dpto_o: [{ 100 : "ACCIDENTE OFIDICO" }, { 205 : "CHAGAS" }, { 100 :  "CHIKUNGUNYA" }, { 210 : "DENGUE" }, { 220 : "DENGUE GRAVE" }, { 310 : "FIEBRE AMARILLA" }, { 420 : "LEISHMANIASIS CUTANEA" }, { 430 :  "LEISHMANIASIS MUCOSA" }, { 410 :  "LEISHMANIASIS VISCERAL" }, { 460 :  "MALARIA ASOCIADA (FORMAS MIXTAS)" }, { 495 :  "MALARIA COMPLICADA" }, { 470 :  "MALARIA FALCIPARUM" }, { 490 :  "MALARIA VIVAX"}]
    cod_dpto_r: [{ 110 : "BAJO PESO AL NACER" }, { 100 :  "CÁNCER DE LA MAMA Y CUELLO UTERINO" }, { 459 :  "CANCER INFANTIL" }, { 100 :  "ENFERMEDADES HUERFANAS - RARAS" }, { 549 :  "MORBILIDAD MATERNA EXTREMA" }, { 550 :  "MORTALIDAD MATERNA" }, { 560 :  "MORTALIDAD PERINATAL Y NEONATAL TARDIA" }, { 112 :  "MORTALIDAD POR DESNUTRICION"}]
  cod_depts_exceptions: [00, 01]
  diseases_exceptions: [{ 300: "tip_cas == 5"}, { 110: "edad = 0, uni_med = 0, tip_cas  ==  5"}, { 110: "tip_cas == 3 || tip_cas ==  4" }, { 459: "tip_cas == 3 || tip_cas ==  4" }, { 210: "con_fin  !=  2, tip_cas  !=  1 && tip_cas  !=  5" }, { 220: "con_fin  !=  2, tip_cas  !=  1 && tip_cas  !=  5" }, { 230: "tip_cas  !=  1" }, { 250: "tip_cas  != 1 && tip_cas  !=  4" }, { 270: "tip_cas  != 1 && tip_cas  !=  4" }, { 290: "tip_cas  != 1 && tip_cas  !=  4" }, { 310: "tip_cas  !=  1 && tip_cas  !=  5" }, { 420: "tip_cas  ==  4" }, { 430: "tip_cas  ==  4" }, { 440: "tip_cas  ==  4" }, { 460: "tip_cas  ==  4" }, { 495: "tip_cas  ==  4" }, { 470: "tip_cas  ==  4" }, { 490: "tip_cas  ==  4" }, { 480: "tip_cas  ==  4" }, { 549: "edad >= 5 && edad <= 59, tip_cas  ==  5, cod_fin == 1" }, { 550: "edad >= 5 && edad <= 59, tip_cas  ==  5, cod_fin == 2" }, { 560: "tip_cas  ==  5, cod_fin == 2" }, { 580: "cod_fin == 2" }, { 112: "cod_fin == 2" }, { 540: "cod_fin == 2" }, { 560: "fecha_nto <= fec_def"}]
  departments: [ "Amazonas", "Antioquia", "Arauca", "Archipiélago de San Andrés, Providencia y Santa Catalina", "Catalina", "Atlántico", "Bogotá D.C.", "Bolívar", "Boyacá", "Caldas", "Caquetá", "Casanare", "Cauca", "Cesar", "Chocó", "Córdoba", "Cundinamarca", "Guainía", "Guaviare", "Huila", "La Guajira", "Magdalena", "Meta", "Nariño", "Norte de Santander", "Putumayo", "Quindío", "Risaralda", "Santander", "Sucre", "Tolima", "Valle del Cauca", "Vaupés", "Vichada" ]
