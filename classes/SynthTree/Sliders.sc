/*
Full width window at bottom of screen, filled with labeled sliders. 

Usage: 

An object that wants to use a slider can request it by Sliders.slider(myLabel). 
If a free (unlabeled) slider is available, its label is set to myLabel, 
and the slider view is returned. 

The object that wants to be controlled by the slider, can then do so by creating 
a ViewFunc on it: 

ViewFunc(aSlider, { value-action }, { onClose-action });

See ViewFunc for more. 

For the moment only one slider panel is used, stored in variable defaults. 
It is possible to extend the class to provide more panels. 

IZ Wed, Mar 12 2014, 15:02 EET

*/

Sliders {
	
	classvar <all;
	var <name;
	var <window;
	var <sliders;
	
	*initClass { all = IdentityDictionary() }

	*slider { | label, panelName = \Sliders |
		^this.getSlider(panelName, label);
	}

	*getSlider { | panelName, label |
		^this.getPanel(panelName).sliderLabeled(label);
	}

	*getPanel { | panelName |
		var panel;
		panelName = panelName.asSymbol;
		panel = all[panelName];
		if (panel.isNil) {
			panel = this.new(panelName);
			all[panelName] = panel;
		};
		^panel;
	}

	*new { | panelName |
		^this.newCopyArgs(panelName).init;
	}

	*at { | key | ^all.at(key); }

	init {
		var height;
		height = Window.screenBounds.height;
		window = Window(name, Rect(0, height - 200, 200, height - 200));
		sliders = { SliderWithLabel() } ! (height - 50 / 35);
		window.view.layout = VLayout(
			*sliders.collect(_.layout)
		);
		window.onClose = { | w |
			all[name.asSymbol] = nil;
			w.objectClosed;
			this.objectClosed;
		};
		window.front;
	}

	sliderLabeled { | string |
		var slider, message;
		message = string.asString;
		slider = sliders detect: { | k | k.label.object == message };
		slider ?? { slider = this.allocateSlider(string); };
		^slider !? { slider.slider };
	}
		
	allocateSlider { | string |
		var slider;
		slider = sliders detect: { | k | k.label.object == "" };
		if (slider.isNil) {
			postf("Could not allocate new slider for %\n", string);
		}{
			slider setLabel: string;
		};
		^slider;
	}
}

SliderWithLabel {

	var <layout;
	var <slider;
	var <label;

	*new { ^super.new.init; }

	init {
		slider = Slider().orientation_(\horizontal).fixedWidth_(70).canFocus_(false);
		label = DragBoth().object_("").font_(Font.default.size_(10));
		layout = HLayout(label, slider);
		label.keyDownAction = { | view, char, modifiers, unicode, keycode, key |
			slider.keyDownAction.(slider, char, modifiers, unicode, keycode, key)
		};
	}

	setLabel { | string | label.object = string.asString; }
}
