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
New version using ScrollView: Sat, Mar 29 2014, 13:51 EET

*/

Sliders : UniqueWindow {

	var <layout, <sliders;
	var <selection;  // currently selected slider/label widget
 
	initUniqueWindow {
		layout = VLayout();
		sliders = IdentityDictionary();
		super.initUniqueWindow;
	}

	*getPanel { | model, panelName |
		^this.for(model ?? { SynthTree.server }, panelName ? \faders, { | self |
			var scroll, canvas, layout, window;
			window = self.window;
			window.bounds = Rect(0, 500, 200, Window.availableBounds.height - 500);
			scroll = ScrollView(window, window.view.bounds).canFocus_(false).resize_(5);
			canvas = View();
			canvas.layout = self.layout;
			scroll.canvas = canvas;
		});
	}

	*slider { | object, model, panelName |
		^this.getPanel(model, panelName).sliderFor(object);
	}

	sliderFor { | object | ^this.widgetFor(object).slider; }

	widgetFor { | object | ^sliders[object] ?? { this.newWidget(object) }; }

	newWidget { | object |
		var widget;
		widget = SliderWithLabel(this);
		widget.setObject(object);
		sliders[object] = widget;
		layout insert: widget.layout;
		^widget;
	}

	setSelection { | label, object |
		selection !? { selection.background = Color.gray.alpha_(0) };
		label.background = Color(0.1, 0.8, 0.9);
		label.string = object.asString;
		selection = label;
		label focus: true;
	}
}

SliderWithLabel {

	var <panel;
	var <layout;
	var <slider;
	var <label;  // note: The object is stored in label.object.

	*new { | panel | ^this.newCopyArgs(panel).init; }

	init {
		slider = Slider().orientation_(\horizontal).fixedWidth_(70).canFocus_(false);
		slider.onClose = { slider.objectClosed };
		label = DragBoth().object_("").font_(Font.default.size_(11));
		label.onClose = { label.objectClosed };
		layout = HLayout(label, slider);
		label.keyDownAction = { | view, char, modifiers, unicode, keycode, key |
			slider.keyDownAction.(slider, char, modifiers, unicode, keycode, key)
		};
	}

	setObject { | object | 
		label.object = object;
		label.string = object.asString;
	}

	object { ^label.object }
}
