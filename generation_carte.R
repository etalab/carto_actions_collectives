#===== Document de génération d'une carte pour le sommet PGO ======

#----------------Chargement des librairies

library(maps)
library(leaflet)
library(rgdal)
library(data.table)

#----------------Définition de l'environnement de travail

setwd("~/Documents/ETALAB/carto_amelie/cartographie")

#----------------Chargement des données

pays <- fread("pays_clean.csv")
#associations <- fread("associations.csv")
#associations <- colnames(associations)[-1]

countries <- readOGR("custom_countries_inter.geojson", "OGRGeoJSON")

collective_action <- colnames(pays)[-1]

pays$position <- 1:nrow(pays)

#---------------- Nettoyage des données
# Import de la table préalablement nettoyée avec python.
associations <- fread("associations_clean.csv", sep=";", header = TRUE)
# Transformation des associations en liste d'associations.
associations$Associations <- associations$Associations %>% strsplit("\n")

# -- Methode pour call :
subset(associations, associations$Commitments == '#1. Open Public Procurement')$Associations

#----------------Definition de la palette graphique

pal <- c("#000000",
         "#39b54a",
         "#146c9c",
         "#00aeef",
         "#ed1c24",
         "#fff200",
         "#86328d",
         "#116c9c",
         "#e9194b",
         "#f89824",
         "#056665",
         "#80caa5")

pale <- colorFactor(palette = pal[2:10], domain = 1:9)

#-------------Définition des popups pour les pays concernés

signataires <- apply(pays[, collective_action, with = FALSE], 1, sum)

popup1 <- paste0("<h3 style='color: blue;margin: 0 0 10px 0;text-align: center; font-size: 18px;'>", 
                 countries@data$name,
                 "</h3>",
                 "<h3 style='color: teal;margin: 0 0 10px 0;'><span style='color:black;'>", 
                 apply(pays[, collective_action, with = FALSE], 1, sum),
                 "</span> collective actions taken</h3>",
                 "<h3 style='color: teal;margin: 0 0 10px 0;'>List of actions taken</h3>",
                 "<ul>",
                 apply(pays[, collective_action, with = FALSE], 1, function(x) paste0("<li style='margin: 10px 0;'>", collective_action[x == 1], collapse = "")),
                 "</ul>"
                 )

#-------------Tracé de la carte

map <- leaflet(countries)

map2 <- map %>% addProviderTiles("CartoDB.Positron", options = providerTileOptions(minZoom=2, maxZoom=10)) %>% setMaxBounds(lng1=-180, lng2=300, lat1=-90, lat2=90) %>% 
  setView(lng=30,lat=30, zoom = 2) %>% #fitBounds(-120, -40, 120, 60) %>%
  
  addPolygons(data = countries[pays$position[pays[, collective_action[1], with = FALSE] == 1],],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color = pal[2], popup = popup1[pays$position[pays[, collective_action[1], with = FALSE] == 1]], group = collective_action[1]
  ) %>%
  addPolygons(data = countries[pays$position[pays[, collective_action[2], with = FALSE] == 1],],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color = pal[3], popup = popup1[pays$position[pays[, collective_action[2], with = FALSE] == 1]], group = collective_action[2]
  ) %>%
  addPolygons(data = countries[pays$position[pays[, collective_action[3], with = FALSE] == 1],],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color = pal[4], popup = popup1[pays$position[pays[, collective_action[3], with = FALSE] == 1]], group = collective_action[3]
  ) %>%
  addPolygons(data = countries[pays$position[pays[, collective_action[4], with = FALSE] == 1],],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color = pal[5], popup = popup1[pays$position[pays[, collective_action[4], with = FALSE] == 1]], group = collective_action[4]
  ) %>%
  addPolygons(data = countries[pays$position[pays[, collective_action[5], with = FALSE] == 1],],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color = pal[5+1], popup = popup1[pays$position[pays[, collective_action[5], with = FALSE] == 1]], group = collective_action[5]
  ) %>%
  addPolygons(data = countries[pays$position[pays[, collective_action[6], with = FALSE] == 1],],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color = pal[6+1], popup = popup1[pays$position[pays[, collective_action[6], with = FALSE] == 1]], group = collective_action[6]
  ) %>%
  addPolygons(data = countries[pays$position[pays[, collective_action[7], with = FALSE] == 1],],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color = pal[7+1], popup = popup1[pays$position[pays[, collective_action[7], with = FALSE] == 1]], group = collective_action[7]
  ) %>%
  addPolygons(data = countries[pays$position[pays[, collective_action[8], with = FALSE] == 1],],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color = pal[8+1], popup = popup1[pays$position[pays[, collective_action[8], with = FALSE] == 1]], group = collective_action[8]
  ) %>%
  addPolygons(data = countries[pays$position[pays[, collective_action[9], with = FALSE] == 1],],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color = pal[9+1], popup = popup1[pays$position[pays[, collective_action[9], with = FALSE] == 1]], group = collective_action[9]
  ) %>%
  addPolygons(data = countries[pays$position[pays[, collective_action[10], with = FALSE] == 1],],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color = pal[10+1], popup = popup1[pays$position[pays[, collective_action[10], with = FALSE] == 1]], group = collective_action[10]
  ) %>%
  addPolygons(data = countries[pays$position[pays[, collective_action[11], with = FALSE] == 1],],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color = pal[11+1], popup = popup1[pays$position[pays[, collective_action[11], with = FALSE] == 1]], group = collective_action[11]
  ) %>%
   addPolygons(data = countries[pays$position[pays[, collective_action[12], with = FALSE] == 1],],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
               color = pal[12-10], popup = popup1[pays$position[pays[, collective_action[12], with = FALSE] == 1]], group = collective_action[12]
  ) %>%
  addPolygons(data = countries[pays$position[pays[, collective_action[13], with = FALSE] == 1],],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
               color = pal[13-10], popup = popup1[pays$position[pays[, collective_action[13], with = FALSE] == 1]], group = collective_action[13]
  ) %>%
  addPolygons(data = countries[pays$position[pays[, collective_action[14], with = FALSE] == 1],],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color = pal[14-10], popup = popup1[pays$position[pays[, collective_action[14], with = FALSE] == 1]], group = collective_action[14]
  ) %>%
  addPolygons(data = countries[pays$position[pays[, collective_action[15], with = FALSE] == 1],],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color = pal[15-10], popup = popup1[pays$position[pays[, collective_action[15], with = FALSE] == 1]], group = collective_action[15]
  ) %>%
  addPolygons(data = countries[pays$position[pays[, collective_action[16], with = FALSE] == 1],],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color = pal[16-10], popup = popup1[pays$position[pays[, collective_action[16], with = FALSE] == 1]], group = collective_action[16]
  ) %>%
  addPolygons(data = countries[pays$position[pays[, collective_action[17], with = FALSE] == 1],],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color = pal[17-10], popup = popup1[pays$position[pays[, collective_action[17], with = FALSE] == 1]], group = collective_action[17]
  ) %>%
  addPolygons(data = countries[pays$position[pays[, collective_action[18], with = FALSE] == 1],],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color = pal[18-10], popup = popup1[pays$position[pays[, collective_action[18], with = FALSE] == 1]], group = collective_action[18]
  ) %>%
  addPolygons(data = countries[pays$position[pays[, collective_action[19], with = FALSE] == 1],],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color = pal[19-10], popup = popup1[pays$position[pays[, collective_action[19], with = FALSE] == 1]], group = collective_action[19]
  ) %>%
   addPolygons(data = countries[pays$position[pays[, collective_action[20], with = FALSE] == 1],],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color = pal[20-10], popup = popup1[pays$position[pays[, collective_action[20], with = FALSE] == 1]], group = collective_action[20]
  ) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0,
              popup = popup1
  ) %>%
  addLayersControl(
    baseGroups = as.character(collective_action[c(1:20)]),
    # overlayGroups = as.character(associations[c(1:22)]),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # Pour le dictionnaire "asso_strJSON", il s'aggit d'un copié/collé de asso_dict.json généré avec pandas dans lequel :
  # - On a supprimé les apostrophes simples (') et mis des '\' devant chaque apostrophe double ("). Le tout avec Ctrl + F
  # - On a retiré tous les "1" et recomposé des associations qui étaient tronquées.
  # C'est pas très propre, mais no choice.
  # // var asso_strJSON = '{\" Open Public Procurement\": \"Hivos Open Contracting; Open Contracting Partnership; Institute for Development of Freedom of Information; Georgian Young Lawyers Association:; Development Gateway; Transparency International Ukraine; Transparency Internationals Pharmaceuticals & Helthcare Programme; Open data Institute; One Campaign; Columbia Center on Sustainable Investment; Young Innovations; Public and Private Development Centre (PPDC); Open Knowledge Foundation Germany; CoST\", \" Ending abuse of anonymous companies\": \"Transparency International Ukraine; Publish What you Pay Indonesia; OpenOwnership; Publish What You Pay; One Campaign; Access Info Europe; Open Knowledge Foundation Germany; AfroLeadership\", \" Innovation and data driven approaches to expose and fight corruption\": \"Transparency International; Freedom of Information Center of Armenia; NGO Must Be; Publish What you Pay Indonesia; Open Data for Development (OD4D); HIVOS\", \" Transparency on lobbying\": \"Access Info Europe\", \" Transparency of political party finance\": \"Transparency International Georgia\", \" Access to information\": \"Institute for Development of Freedom of Information (IDFI); Open Society Georgia Foundation; Pan-African Parliament; Georgian Young Lawyers Association; Freedom of Information Center of Armenia; Hivos; Centre for Law and Democracy; The Global Forum for Media Development; Access Info Europe; The Carter Center\", \" Transparency and participation in budgets and fiscal policies\": \"Save the children; Institute for Development of Freedom of Information (IDFI):; Neighbourhood Environment Watch (NEW); Foundation: Action et Humanisme; Dyntra.org; The International Budget Partnership; The ONE Campaign; World Resources Institute (WRI)\", \" Transparency and open contracts in the natural resource sector\": \"Aucune association\", \" Engaging citizens in an open and inclusive law-making process\": \"Pan-African Parliament; The Latin American Network for Legislative Transparency; Georgian Young Lawyers Association (GYLA); Al Hayat Center for Civil Society Development; Transparency International Georgia; Democratise; The Legislative Openness Work\", \"Climate and sustainable development\": \"Aucune association\", \" Inclusive development of national and subnational climate and sustainable development strategies and plans\": \"World Ressources Institute\", \" Tracking climate relevant policy implementation and results\": \"Climate Change Network Nigeria (CCN-Nigeria); World Ressources Institute (WRI)\", \" Greater transparency of information on climate and sustainable development in national budgets\": \"Climate Change Network Nigeria (CCN-Nigeria); The Humanitarian OpenStreetMap Team (HOT); Open Climate Data Repository; GODAN; Development Gateway; Telefonica; World Ressources Institute (WRI)\", \" Disclosure of climate-related investment risks and corresponding mitigation measure\": \"Transparency International Ukraine; Transparency International Georgia; Open Society Georgia Foundation; Freedom of Information Center of Armenia; The International Center for Innovation, Transformation and Excellence; in Governance (INCITEGov); Al Hayat Center for Civil Society Development; Twaweza; Australian Open Government Partnership\", \" Policies and mechanisms to promote and strenghten engagement with civil society\": \"Transparency International Ukraine; Transparency International Georgia; Open Society Georgia Foundation; Freedom of Information Center of Armenia; The International Center for Innovation, Transformation and Excellence; in Governance (INCITEGov); Al Hayat Center for Civil Society Development; Twaweza; Australian Open Government Partnership Network\", \" Increase the responsiveness and accountability of public services to citizens\": \"Save the children; Al Hayat Center for Civil Society Development; Twaweza; CoST; World Vision International\", \" Opening and sharing civic technology tools for opening government\": \"Transparency International Georgia; NGO Must Be; The Open Data Institute; CoST\", \" Collaborative data infrastructures\": \"NGO Must Be; The Open Data Institute\", \"Guidelines principles for open data policies\": \"Open Data for Development (OD4D); The Web Foundation; Bangladesh NGOs Network for Radio and Communication (BNNRC); Pan-African Parliament; NGO Must Be; Australian Open Government Partnership Network; GODAN; Publish What You Pay (PWYP); The GovLab; The Latin America Open Data Initiative; The Open Data Institute; The Open Contracting Partnership; Hivos; Open Knowledge International (OKI); Amis des Etrangers\", \" Creating an Open Source Software policy\": \"LibreItalia; The Nexa Center for Internet & Society (DAUIN, Politecnico di Torino, ITALY); Youth Network for Reform (YONER-LIBERIA); The Open Data Institute; GFOSS - Open Technologies Alliance; CoST; The European Commissions OSOR.eu\", \" Transparency on international trade negotiations\": \"Ghana Integrity Initiative; Columbia Center on Sustainable Investment (CCSI)\"}';

  htmlwidgets::onRender("
    function(el) {

      var asso_strJSON = '{\"#1. Open Public Procurement\": \"Hivos Open Contracting; Open Contracting Partnership; Institute for Development of Freedom of Information; Georgian Young Lawyers Association:; Development Gateway; Transparency International Ukraine; Transparency Internationals Pharmaceuticals & Helthcare Programme; Open data Institute; One Campaign; Columbia Center on Sustainable Investment; Young Innovations; Public and Private Development Centre (PPDC); Open Knowledge Foundation Germany; CoST\", \"#2. Ending abuse of anonymous companies\": \"Transparency International Ukraine; Publish What you Pay Indonesia; OpenOwnership; Publish What You Pay; One Campaign; Access Info Europe; Open Knowledge Foundation Germany; AfroLeadership\", \"#3. Innovation and data driven approaches to expose and fight corruption\": \"Transparency International; Freedom of Information Center of Armenia; NGO Must Be; Publish What you Pay Indonesia; Open Data for Development (OD4D); HIVOS\", \"#4. Transparency on lobbying\": \"Access Info Europe; Directorio Legislativo\", \"#5. Transparency of political party finance\": \"Transparency International Georgia\", \"#6. Access to information\": \"Institute for Development of Freedom of Information (IDFI); Open Society Georgia Foundation; Pan-African Parliament; Georgian Young Lawyers Association; Freedom of Information Center of Armenia; Hivos; Centre for Law and Democracy; The Global Forum for Media Development; Access Info Europe; 1The Carter Center\", \"#7. Transparency and participation in budgets and fiscal policies\": \"Save the children; Institute for Development of Freedom of Information (IDFI):; Neighbourhood Environment Watch (NEW); Foundation: Action et Humanisme; Dyntra.org; The International Budget Partnership; The ONE Campaign; World Resources Institute (WRI)\", \"#8. Transparency and open contracts in the natural resource sector\": \"World Resources Institute; Natural Resource Governance Institute; Carter Center\", \"#9. Engaging citizens in an open and inclusive law-making process\": \"Pan-African Parliament; The Latin American Network for Legislative Transparency; Georgian Young Lawyers Association (GYLA); Al Hayat Center for Civil Society Development; Transparency International Georgia; Democratise; The Legislative Openness Work\", \"#10. Inclusive development of national and subnational climate and sustainable development strategies and plans\": \"World Ressources Institute\", \"#11. Tracking climate relevant policy implementation and results\": \"Climate Change Network Nigeria (CCN-Nigeria); World Ressources Institute (WRI)\", \"#12. Greater transparency of information on climate and sustainable development in national budgets\": \"Climate Change Network Nigeria (CCN-Nigeria); The Humanitarian OpenStreetMap Team (HOT); Open Climate Data Repository; GODAN; Development Gateway; Telefonica; World Ressources Institute (WRI)\", \"#13. Disclosure of climate-related investment risks and corresponding mitigation measure\": \"Transparency International Ukraine; Transparency International Georgia; Open Society Georgia Foundation; Freedom of Information Center of Armenia; The International Center for Innovation, Transformation and Excellence; in Governance (INCITEGov); Al Hayat Center for Civil Society Development; Twaweza; Australian Open Government Partnership\", \"#14. Policies and mechanisms to promote and strenghten engagement with civil society\": \"Transparency International Ukraine; Transparency International Georgia; Open Society Georgia Foundation; Freedom of Information Center of Armenia; The International Center for Innovation, Transformation and Excellence; in Governance (INCITEGov); Al Hayat Center for Civil Society Development; Twaweza; Australian Open Government Partnership Network\", \"#15. Increase the responsiveness and accountability of public services to citizens\": \"Save the children; Al Hayat Center for Civil Society Development; Twaweza; CoST; World Vision International\", \"#16. Opening and sharing civic technology tools for opening government\": \"Transparency International Georgia; NGO Must Be; The Open Data Institute; CoST\", \"#17. Collaborative data infrastructures\": \"NGO Must Be; The Open Data Institute\", \"#18.Guidelines principles for open data policies\": \"Open Data for Development (OD4D); The Web Foundation; Bangladesh NGOs Network for Radio and Communication (BNNRC); Pan-African Parliament; NGO Must Be; Australian Open Government Partnership Network; GODAN; Publish What You Pay (PWYP); The GovLab; The Latin America Open Data Initiative; The Open Data Institute; The Open Contracting Partnership; Hivos; 1Open Knowledge International (OKI); Amis des Etrangers\", \"#19. Creating an Open Source Software policy\": \"LibreItalia; The Nexa Center for Internet & Society (DAUIN, Politecnico di Torino, ITALY); Youth Network for Reform (YONER-LIBERIA); The Open Data Institute; GFOSS - Open Technologies Alliance; CoST; The European Commissions OSOR.eu\", \"#20. Transparency on international trade negotiations\": \"Ghana Integrity Initiative; Columbia Center on Sustainable Investment (CCSI)\"}';
      var asso_obj = JSON.parse(asso_strJSON);
      var group_strJSON = '{\"#1. Open Public Procurement\": \"Anti-Corruption Working Group\", \"#2. Ending abuse of anonymous companies\": \"Anti-Corruption Working Group\", \"#3. Innovation and data driven approaches to expose and fight corruption\": \"Anti-Corruption Working Group\", \"#4. Transparency on lobbying\": \"Anti-Corruption Working Group\", \"#5. Transparency of political party finance\": \" \", \"#6. Access to information\": \"Access to Information Working Group\", \"#7. Transparency and participation in budgets and fiscal policies\": \"Fiscal Openness Working Group\", \"#8. Transparency and open contracts in the natural resource sector\": \"Openness in Natural Resources Working Group\", \"#9. Engaging citizens in an open and inclusive law-making process\": \"Legislative Openness Working Group\", \"#10. Inclusive development of national and subnational climate and sustainable development strategies and plans\": \"Open Climate Working Group\", \"#11. Tracking climate relevant policy implementation and results\": \"Open Climate Working Group\", \"#12. Greater transparency of information on climate and sustainable development in national budgets\": \"Open Climate Working Group\", \"#13. Disclosure of climate-related investment risks and corresponding mitigation measure\": \" \", \"#14. Policies and mechanisms to promote and strenghten engagement with civil society\": \" \", \"#15. Increase the responsiveness and accountability of public services to citizens\": \" \", \"#16. Opening and sharing civic technology tools for opening government\": \" \", \"#17. Collaborative data infrastructures\": \" \", \"#18.Guidelines principles for open data policies\": \"Open Data Working Group\", \"#19. Creating an Open Source Software policy\": \" \", \"#20. Transparency on international trade negotiations\": \" \"}';
      var group_obj = JSON.parse(group_strJSON);
      console.log(asso_obj);
      window.yourGlobalVariable = asso_obj;
      window.yourGlobalVariable = group_obj;
      // Create & style bottombar (containing asso).
      var bottombar = document.createElement(\"div\");
      bottombar.className = 'asso-container';
      bottombar.style.position = 'absolute';
      bottombar.style.width = '60%';
      bottombar.style.height = '25%';
      bottombar.style.bottom = '1%';
      bottombar.style.left = '1%';
      bottombar.style['border-radius'] ='20px';
      bottombar.style['font-size'] = '12px';
      bottombar.style.padding = '20px';
      bottombar.style.border = '1.2px solid #777';
      bottombar.style.background = 'rgba(255, 255, 255, 0.6)';
      bottombar.style['z-index'] = 1000;
      // Add css style
      var style=document.createElement('style');
      style.type='text/css';
      if(style.styleSheet){
        style.styleSheet.cssText='.leaflet-popup-content {overflow-y: auto; height:200px;}';
      }else{
        style.appendChild(document.createTextNode('.leaflet-popup-content {overflow-y: auto; height:200px;}'));
      }
      document.getElementsByTagName('head')[0].appendChild(style);
      // Change leaflet radio style.
      var leaflet_radio = document.getElementsByClassName('leaflet-control-layers');
      leaflet_radio[0].style.background = 'rgba(255, 255, 255, 0.6)';
      leaflet_radio[0].style['font-size'] = '1.12em';
      leaflet_radio[0].style['font-weight'] = 'bold';
      leaflet_radio[0].style.color = '#0022a8';
      leaflet_radio[0].style.width = '50%';
      leaflet_radio[0].style.padding = '20px';
      // Change leaflet radio label style.
      var leaflet_radio_layer = document.getElementsByClassName(\"leaflet-control-layers-base\");
      var leaflet_radio_labels = leaflet_radio_layer[0].getElementsByTagName(\"label\");
      for (var lab = 0; lab < leaflet_radio_labels.length; lab ++) {
        leaflet_radio_labels[lab].style.cssText = 'padding: 5px;'
      }
      var leafletContainer = document.getElementsByClassName(\"leaflet-container\");
      leafletContainer[0].append(bottombar);

      var leaflet_control = document.getElementsByClassName(\"leaflet-control-layers-base\")
      var checkboxes = leaflet_control[0].getElementsByTagName(\"input\");
      var span_checkboxes = leaflet_control[0].getElementsByTagName(\"span\");
      var asso_container = document.getElementsByClassName('asso-container');
      for (var cb = 0; cb < checkboxes.length; cb ++) {
        checkboxes[cb].setAttribute(\"id\", span_checkboxes[cb].innerHTML);
        checkboxes[cb].onclick = function() {
          if (this.checked) {
            var splitListeAsso = asso_obj[this.id.substring(1)].split(';');
            var group = group_obj[this.id.substring(1)];
            console.log('group: ' + group);
            var fullStr = '<h2 style=\"text-align:center; margin-bottom:0; padding-bottom:1px\"> Civil Society Organisations that proposed a contribution </h2><h4 style=\"padding-bottom: 0;margin-bottom: 0 !important; margin-top: 0;padding-top: 0;text-align: center;\">' + group + '</h4><ul style=\"columns:3; padding: 1px; list-style-type: none;\">';
            var strObj = '';
            splitListeAsso.forEach(function(element) {
              if (element !== '' && element != 'Aucune association'){
                strObj = '<li style=\"padding: 4px; font-size: 1.2em;\">' + element + '</li>';
                fullStr += strObj;
              } else {
                strObj = '<p style=\"padding: 4px; font-size: 1.2em; font-style:italic;\">' + element + '</p>';
                fullStr += strObj;
              }
            });
            asso_container[0].innerHTML = fullStr + '</ul>';
           }
        };
        // Move div 8 after div 7 (pas terrible mais pas le choix.)
        var el8 = $('[id^=\" #8\" ]').parent();
        var el7 = $('[id^=\" #7\" ]').parent();
        el8.insertAfter(el7);
        checkboxes[0].click();
      }
  }
")

map2

library(htmlwidgets)
saveWidget(map2, file="index.html")

