/*
Full width window at bottom of screen, filled with labeled knobs. 

Usage: 

An object that wants to use a knob can request it by Knobs.knob(myLabel). 
If a free (unlabeled) knob is available, its label is set to myLabel, 
and the knob view is returned. 

The object that wants to be controlled by the knob, can then do so by creating 
a ViewFunc on it: 

ViewFunc(aKnob, { value-action }, { onClose-action });

See ViewFunc for more. 

For the moment only one knob panel is used, stored in variable defaults. 
It is possible to extend the class to provide more panels. 

IZ Wed, Mar 12 2014, 15:02 EET

*/

Knobs {
	
	classvar all;
	var <name;
	var <window;
	var <knobs;
	
	*initClass { all = IdentityDictionary() }

	*knob { | label, panelName = \Knobs |
		^this.getKnob(panelName, label);
	}

	*getKnob { | panelName, label |
		^this.getPanel(panelName).knobLabeled(label);
	}

	*getPanel { | panelName |
		var panel;
		panelName = panelName.asSymbol;
		panel = all[panelName];
		if (panel.isNil) {
			panel = this.new(panelName);
			all[panelName] = panel;
		};
		^panel.front;
	}

	*new { | panelName |
		^this.newCopyArgs(panelName).init;
	}

	init {
		var margin = 200, width;
		width = Window.screenBounds.width - margin;
		window = Window(name.asString, 
			Rect(margin, Window.availableBounds.height - 80, width, 80));
		knobs = { KnobWithLabel() } ! (width - 50 / 60);
		window.view.layout = HLayout(
			*knobs.collect(_.layout)
		);
		window.onClose = { | w |
			all[name.asSymbol] = nil;
			w.objectClosed;
		};
		window.front;
	}

	knobLabeled { | string |
		var knob, message;
		message = string.asString;
		knob = knobs detect: { | k | k.label.string == message };
		knob ?? { knob = this.allocateKnob(string); };
		^knob !? { knob.knob };
	}
		
	allocateKnob { | string |
		var knob;
		knob = knobs detect: { | k | k.label.string == "" };
		if (knob.isNil) {
			postf("Could not allocate new knob for %\n", string);
		}{
			knob setLabel: string;
		};
		^knob;
	}

	front { window.front }
}

KnobWithLabel {

	var <layout;
	var <knob;
	var <label;

	*new { ^super.new.init; }

	init {
		knob = Knob();
		label = StaticText().string_("").align_(\center).canFocus_(true);
		layout = VLayout([knob, s: 3], label);
	}

	setLabel { | string | label.string = string; }
}
