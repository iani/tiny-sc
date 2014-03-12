/*
Full width window at bottom of screen, filled with labeled knobs. 
The knobs are allocated dynamically to receiving elements, 
and labeled accordingly.

IZ Wed, Mar 12 2014, 15:02 EET

Knobs.default.knobs.first.knob.parent.window;
Knobs.connect(\test.asSynthTree);

*/

Knobs {
	
	classvar default;
	var <window;
	var <knobs;
	
	*connect { | object, label |
		var knob;
		label = label ?? { object.asString };
		knob = this.default.knobLabeled(label);
		knob !? { knob connect: object; }
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
		message = string.asSymbol;
		knob = knobs detect: { | k | k.message === message };
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
	
	// var <window; 
	/* receiving onClosed from the window is more economical: only one onClosed is sent.
		But it makes the code a little more complicated.  Therefore avoided for now */
	var <layout;
	var <knob;
	var <label;
	var <message;

	*new { ^super.new.init; }

	init {
		knob = Knob();
		knob.action = { | me | this.changed(message, me.value); };
		knob.onClose = { this.objectClosed; };
		label = StaticText().string_("").align_(\center);
		layout = VLayout([knob, s: 3], label);
	}

	setLabel { | string |
		label.string = string;
		message = string.asSymbol;
	}

	connect { | object, action, onClose |
		action = action ?? {{ | value |
			object.mapSet(message, value);
		}};
		object.addNotifier(this, message, action);
		onClose = onClose ?? {{ | sender |
			object.senderClosed(sender);
		}};
		object.addNotifier(this, \objectClosed, onClose);
	}
}
