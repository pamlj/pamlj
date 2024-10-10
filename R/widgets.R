HTMLWidget <- R6::R6Class(
    "HTMLWidget",
    public = list(
        id = NULL,  # Identificatore unico per istanza
        initialize = function() {
            if (is.null(HTMLWidget$counter)) {
                HTMLWidget$counter <- 0
            }
            HTMLWidget$counter <- HTMLWidget$counter + 1
            self$id <- paste0("accordion-", HTMLWidget$counter)
        },
        generate_accordion = function(title = "Help", content = "", tabs = NULL) {
            unique_id <- self$id  # Usa l'identificatore unico per questa istanza
            html_content <- paste0(
                '<style>',
                '#', unique_id, ' .accordion {',
                '  background-color: #3e6da9;',
                '  color: white;',
                '  cursor: pointer;',
                '  padding: 8px 15px;',
                '  width: 120%;',
                '  border: none;',
                '  text-align: left;',
                '  outline: none;',
                '  font-size: 16px;',
                '  transition: 0.4s;',
                '  display: flex;',
                '  align-items: center;',
                '  position: relative;',
                '  border-top-left-radius: 8px;',
                '  border-top-right-radius: 8px;',
                '}',
                '#', unique_id, ' .accordion svg {',
                '  margin-right: 15px;',
                '  transition: fill 0.4s;',
                '}',
                '#', unique_id, ' .accordion svg .circle {',
                '  fill: white;',
                '}',
                '#', unique_id, ' .accordion svg .horizontal,',
                '#', unique_id, ' .accordion svg .vertical {',
                '  fill: #3e6da9;',
                '  transition: transform 0.8s ease-in-out;',
                '  transform-origin: center;',
                '}',
                '#', unique_id, ' .accordion.active svg .vertical {',
                '  transform: scaleY(0);',
                '}',
                '#', unique_id, ' .panel {',
                '  display: none;',
                '  background-color: white;',
                '  overflow: hidden;',
                '  border-bottom-left-radius: 8px;',
                '  border-bottom-right-radius: 8px;',
                '  border-bottom: 2px solid #3e6da9;',
                '  width: 120%;',
                '  box-sizing: border-box;',
                '  text-align: justify;',
                '}',
                '#', unique_id, ' .panel-content {',
                '  padding: 15px;',
                '  background-color: #e5effa;',  # Colore di sfondo per il contenuto senza tab
                '  border-radius: 8px;',
                '}',
                '#', unique_id, ' .tab-button {',
                '  padding: 8px 15px;',
                '  border: 1px solid #3e6da9;',
                '  border-top-left-radius: 0;',
                '  border-top-right-radius: 0;',
                '  border-bottom-left-radius: 8px;',
                '  border-bottom-right-radius: 8px;',
                '  cursor: pointer;',
                '  background-color: white;',      # Colore di sfondo blu scuro
                '  color: white;',                   # Testo bianco
                '  position: relative;',
                '}',
                '#', unique_id, ' .tab-button.active {',
                '  background-color: #e5effa;',      # Colore di sfondo leggero
                '  color: #3e6da9;',                 # Testo blu scuro
                '}',
                '#', unique_id, ' .tab-button::after {',
                '  content: "";',
                '  position: absolute;',
                '  left: 0;',
                '  bottom: -5px;',
                '  width: 100%;',
                '  height: 5px;',
                '  background: linear-gradient(to bottom, rgba(0, 0, 0, 0.2), transparent);',
                '  border-bottom-left-radius: 8px;',
                '  border-bottom-right-radius: 8px;',
                '}',
                '#', unique_id, ' .tab-container {',
                '  display: flex;',
                '  gap: 2px;',
                '  margin-top: 0;',
                '}',
                '#', unique_id, ' .tab-content {',
                '  padding: 15px;',
                '  border: 1px solid #3e6da9;',
                '  border-radius: 5px;',
                '  background-color: #e5effa;',  # Colore di sfondo uniforme
                '}',
                '</style>',
                '<script>',
                '(function() {',
                '  var acc = document.querySelectorAll("#', unique_id, ' .accordion");',
                '  for (var i = 0; i < acc.length; i++) {',
                '    acc[i].addEventListener("click", function() {',
                '      this.classList.toggle("active");',
                '      var panel = this.nextElementSibling;',
                '      if (panel.style.display === "block") {',
                '        panel.style.display = "none";',
                '      } else {',
                '        panel.style.display = "block";',
                '      }',
                '    });',
                '  }',
                '  var tabButtons = document.querySelectorAll("#', unique_id, ' .tab-button");',
                '  for (var i = 0; i < tabButtons.length; i++) {',
                '    tabButtons[i].addEventListener("click", function(evt) {',
                '      var tabId = this.getAttribute("data-tab");',
                '      var tabContents = document.querySelectorAll("#', unique_id, ' .tab-content");',
                '      for (var j = 0; j < tabContents.length; j++) {',
                '        tabContents[j].style.display = "none";',
                '      }',
                '      var tabButtonsAll = document.querySelectorAll("#', unique_id, ' .tab-button");',
                '      for (var j = 0; j < tabButtonsAll.length; j++) {',
                '        tabButtonsAll[j].classList.remove("active");',
                '      }',
                '      document.getElementById(tabId).style.display = "block";',
                '      this.classList.add("active");',
                '    });',
                '  }',
                '})();',
                '</script>',
                '<div id="', unique_id, '">',
                '<button class="accordion">',
                '  <svg width="20" height="18" viewBox="0 0 24 24">',
                '    <circle class="circle" cx="12" cy="12" r="11" />',
                '    <rect class="horizontal" x="5" y="11" width="15" height="3" />',
                '    <rect class="vertical" x="11" y="5" width="3" height="15" />',
                '  </svg>',
                '  <span style="font-size: 16px;">', title, '</span>',
                '</button>',
                '<div class="panel">',
                if (content != "") {
                    paste0(
                        '<div class="panel-content">', content, '</div>'
                    )
                } else if (!is.null(tabs)) {
                    paste0(
                        '<div class="tab-container">',
                        paste0(lapply(seq_along(tabs), function(i) {
                            tab <- tabs[[i]]
                            mark(tab)
                            active_class <- if (i == 1) ' active' else ''
                            sprintf(
                                '<button class="tab-button%s" data-tab="%s-tab-%d">%s</button>',
                                active_class, unique_id, i, tab$title
                            )
                        }), collapse = ""),
                        '</div>',
                        '<div class="tab-content-container" style="margin-top: 10px;">',
                        paste0(lapply(seq_along(tabs), function(i) {
                            tab <- tabs[[i]]
                            display_style <- ifelse(i == 1, "block", "none")
                            sprintf(
                                '<div id="%s-tab-%d" class="tab-content" style="display: %s;">%s</div>',
                                unique_id, i, display_style, tab$content
                            )
                        }), collapse = ""),
                        '</div>'
                    )
                } else {
                    ""
                },
                '</div>',
                '</div>'
            )
            return(html_content)
        }
    ),
    private = list(
        counter = 0  # Contatore statico per gli identificatori unici
    )
)