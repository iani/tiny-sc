/* 
Template class

Holds a template, its name, and tags for search in multiple categories. 
Loaded at StartUp.
See AboutTemplates.org

IZ Wed, Mar 26 2014, 17:32 EET
SynthTemplate.gui;

Library.at(SynthTemplate);

*/

Template {
	var <name;
	var <template;
	var <tags;
	
	*initClass {
		StartUp add: {
			(PathName(this.filenameSymbol.asString).pathOnly ++ "*.scd").pathMatch
			do: _.load;
		}
	}

	
	*new { | name, template, tags |
		^super.newCopyArgs(name, template, tags).store;
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
						}
					}
				};
				namesView.action = { | me |
					if (me.items.size > 0) {
						sourceView.object = Library.at(this.key, tagsView.item, me.item)
					}
				};
				namesView.keyDownAction = { | view, char, modifiers, unicode, keycode, key |
					switch (char,
						13.asAscii, { // return key
							if (modifiers == 0) {
								sourceView.object.template => 
								sourceView.object.makeSynthTreeName;
							}{
								
							}
						},
						{ view.defaultKeyDownAction(
							char, modifiers, unicode, keycode, key) 
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
}

PatternTemplate : Template {
	*gui { ^super.gui.shiftTo(200, 180) }
}

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
	asTemplate { ^PatternPlayer(key, value) }
}