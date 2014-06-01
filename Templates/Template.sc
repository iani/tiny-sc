/* 
Template class

Holds a template, its name, and tags for search in multiple categories. 
Loaded at StartUp.
See AboutTemplates.org

IZ Wed, Mar 26 2014, 17:32 EET
SynthTemplate.gui;

Library.at(SynthTemplate, '---ALL---', 'auco01');

*/

Template {

	classvar <>showGuiAtStartup = false;

	var <name;
	var <template;
	var <tags;
	var <path;
	
	*initClass {
		StartUp add: {
			// Load template files in same folder as Template class
			(PathName(this.filenameSymbol.asString).pathOnly ++ "*.scd").pathMatch
			do: _.load;
			// Load template files from user app suppor dir
			(Platform.userAppSupportDir +/+ Templates +/+ "*.scd").pathMatch
			do: _.load;
			if (showGuiAtStartup) { this.subclasses do: _.gui; };
		}
	}

	*new { | name, template, tags |
		^super.newCopyArgs(name.asSymbol, template, tags).store;
	}

	store {
		var key;
		key = this.key;
		Library.put(key, '---ALL---', name, this);
		tags do: { | tag |
			Library.put(key, tag, name, this);
		}
	}

	key { ^this.class.key }
	*key { this.asSymbol }

	*gui {
		^Windows.for(
			this, 
			\list,
			{ | window |
				var tagsView, namesView, sourceView, tagsItems, namesItems;
				window.view.layout = VLayout(
					HLayout(tagsView = ListView(), namesView = ListView()),
					sourceView = DragSource()
				);
				tagsItems = Library.at(this.key);
				if (tagsItems.notNil) {
					tagsView.items = tagsItems.keys.asArray.sort;
				};
				tagsView.action = { | me |
					if (me.items.size > 0) {
						namesItems = Library.at(this.key, me.item);
						if (namesItems.notNil) {
							namesView.items = namesItems.keys.asArray.sort;
							if (namesView.items.size > 0) {
								namesView.value = 0;
								namesView.doAction;
								namesView.focus(true);
							}
						}
					}
				};
				namesView.action = { | me |
					if (me.items.size > 0) {
						sourceView.object = Library.at(this.key, tagsView.item, me.item)
					}
				};
				namesView.keyDownAction = { | view, char, modifiers, unicode, keycode, key |
					// modifiers.postln;
					switch (modifiers,
						0, { // no modifier (just one key typed)
							switch (char,
								13.asAscii, { // return key: chuck into selected ST
									if (~st.isNil) {
										sourceView.object.template => 
										format("st%", UniqueID.next - 1001).asSymbol;
									}{
										sourceView.object.template => ~st;
									}
								},
								Char.space, { // space key: toggle selected ST
									~st !? { ~st.toggle }
								},
								{ view.defaultKeyDownAction(
									char, modifiers, unicode, keycode, key) 
								}
							)
						},
						131072, { // shift key
							switch (char, // KeyFunc
								13.asAscii, { // shift+return: chuck into new ST
									sourceView.object.template => 
									format("st%", UniqueID.next - 1001).asSymbol;}
							)
						},
						262144, { // control key
							switch (key,
								16777220, { // return key: add input to selected ST
									if (~fx.isNil) {
										sourceView.object.template => 
										format("st%", UniqueID.next - 1001).asSymbol;
									}{
										~fx =<
										(sourceView.object.template ==>
											sourceView.object.makeSynthTreeName
										);
									};
								},
								70, { SynthTree.faders } // $f : Show faders
							)
						}
					);
				};
				
				sourceView.object = "THIS TEMPLATE LIST IS EMPTY";
				tagsView.doAction;
				namesView.doAction;
			}
		);
	}

	asString { ^format("% (%)", name, template.class); }

	makeSynthTreeName { | server |
		if (SynthTree.onServer(server ?? { SynthTree.server })[name].isNil) {
			^name;
		}{
			^(name ++ (UniqueID.next - 1000)).asSymbol
		};
	}
}

SynthTemplate : Template {
	*gui { ^super.gui.shiftTo(200, 600) }
	asSynthTemplate { ^template }
}

/*
PatternTemplate : Template {
	*gui { ^super.gui.shiftTo(200, 180) }
}
*/

// Utility classes: create lists of templates.

Templates {

	*new { | specArray ... tags |
		var templateClass;
		templateClass = this.templateClass;
		specArray keysValuesDo: { | name, templateSpec |
			templateClass.new(name, templateSpec.asTemplate, tags);
		}
	}
}

SynthTemplates : Templates {
	*templateClass { ^SynthTemplate }
}

PatternTemplates : Templates {
	*templateClass { ^PatternTemplate }
}

+ Function {
	asTemplate { ^this }
}

+ SynthDef {
	asTemplate { ^this.add }
}

+ Association {
	asTemplate { ^PatternTask(key, value) }
}
