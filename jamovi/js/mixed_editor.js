// Here we define the editor, which is largely taken from Rj code.

const ace = require('brace');

require('brace/mode/r');
require('brace/ext/language_tools');
require('./mixed_editor_css');
var fun=require('./functions');

const events = {
    	editor_creating(ui) {
    	  
    

	    this.editSessions = { };

	  //  let $contents = ui.view.$el;
   	  let $contents=ui.syntax.$el;
   	  console.log("creating editor");
	    $contents.css('display', 'flex');
	    $contents.css('flex-direction', 'column');
      
		$contents.append(`
		    <div id="editor-box">
		        <div id="toolbar">
		            <div id="config" title="Configuration"></div>
		            <div id="run" title="Run"></div>
		        </div>
		        <div id="editor"></div>
                <div id="info">Ctrl + Shift + Enter to run</div>
            </div>`);

        let $config = $contents.find('#config');
        $config.append(`
            <div id="menu">
                <label id="fonts-label">Fonts size</label>
                <select id="fonts">
                    <option value="small">Small</option>
                    <option value="medium">Medium</option>
                    <option value="large">Large</option>
                </select>
            </div>`);

		this.$editor = $contents.find('#editor');
		this.$run = $contents.find('#run');
		this.$menu = $contents.find('#menu');

		this.$run.on('click', () => this.run(ui));

		this.$fonts = $config.find('#fonts');

		this.$fonts.on('change', (event) => {
		    var size=this.$fonts.children("option:selected").val();
		    this.$editor.css("font-size",size);
		    ui.fonts.setValue(size);
		});



		this.$menu.find('input').on('keyup', (event) => {
		    if (event.keyCode == 13)
		        this.run(ui);
		});

		$config.on('click', (event) => {
		    if (event.target === $config[0])
		        this.toggleMenu(ui);
		});

		this.$editor.on('click', () => {
            this.hideMenu(ui);
		});

		if (navigator.platform === 'MacIntel') {
		    let $info = $contents.find('#info');
		    $info.text('\u2318 + Shift + Enter to run');
		}
		


},

     loaded(ui) {
        console.log("loaded code");

        this.editor = ace.edit('editor');
        this.editor.$blockScrolling = Infinity; // disable a warning
        this.editor.setShowPrintMargin(false);
        this.editor.setHighlightActiveLine(false);
        this.editor.focus();
//        this.editor.setOptions({
//            enableBasicAutocompletion: true,
//        });
        
        
        
        this.toggleMenu = (ui) => {
            if ( ! this.$menu.hasClass('visible'))
		        this.showMenu(ui);
		    else
		        this.hideMenu(ui);
        };

        this.showMenu = (ui) => {
		    this.$menu.addClass('visible');
        };

        this.hideMenu = (ui) => {
	        this.$menu.removeClass('visible');
	        ui.view.model.options.beginEdit();
	        ui.view.model.options.endEdit();
        };

        this.run = (ui) => {
          
               console.log("mixed editor run")
            


            let script = this.currentSession.getDocument().getValue();

              ui.view.model.options.beginEdit();
              ui.code.setValue(script);
              ui.run.setValue(true);

                // toggle toggle so the analysis *always* reruns
                // even if nothing has changed
                ui.toggle.setValue( ! ui.toggle.value());

                ui.view.model.options.endEdit();

                this.editor.focus();
            }
    	

        this.$editor.on('keydown', (event) => {

            if (event.keyCode === 13 && (event.metaKey || event.ctrlKey) && event.shiftKey) {
                // ctrl+shift+enter
                this.run(ui);
                event.stopPropagation();
            }
            else if (event.keyCode === 65 && event.metaKey) {
                // ctrl+a
                this.$editor.select();
            }
            else if (event.keyCode === 67 && (event.metaKey || event.ctrlKey) && event.shiftKey)             {
                // ctrl+shift+c
                this.editor.toggleCommentLines();
            }
            else if (event.keyCode === 191 && (event.metaKey || event.ctrlKey)) {
                // ctrl+/
                this.editor.toggleCommentLines();
            }
        });


	},


	onDataChanged(ui, event) {

    return;


	},

    update(ui, event) {
        console.log("update ");
        let id = event.id;
        this.currentSession = this.editSessions[id];

        if (this.currentSession === undefined) {

            let code = ui.code.value();
            this.currentSession = ace.createEditSession(code, 'ace/mode/r');
            this.editSessions[id] = this.currentSession;
        }
        this.$editor.css("font-size",ui.fonts.value());
        this.$fonts.val(ui.fonts.value());

        this.editor.setSession(this.currentSession);
    },
};


module.exports = events;
