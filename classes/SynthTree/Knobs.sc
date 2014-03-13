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
	
	classvar default;
	var <window;
	var <knobs;
	
	*knob { | label |
		var knob;
		knob = this.default.knobLabeled(label);
		^knob !? { knob.knob };
	}

	*default { ^default ?? { default = this.new }; }

	*new { ^super.new.init; }

	init {
		var width;
		width = Window.screenBounds.width;
		window = Window("Knobs", Rect(0, 0, width, 100));
		knobs = { KnobWithLabel() } ! (width - 50 / 70);
		window.view.layout = HLayout(
			*knobs.collect(_.layout)
		);
		//	window.onClose = { | w | w.objectClosed };
		window.front;
	}

	knobLabeled { | string |
		var knob, message;
		knob = knobs detect: { | k | k.label.string == message };
		knob ?? { knob = this.allocateKnob(string); };
		^knob;
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
}

KnobWithLabel {

	var <layout;
	var <knob;
	var <label;

	*new { ^super.new.init; }

	init {
		knob = Knob();
		label = StaticText().string_("").align_(\center);
		layout = VLayout([knob, s: 3], label);
	}

	setLabel { | string | label.string = string; }
}
