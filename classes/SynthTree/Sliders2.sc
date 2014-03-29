/*
Draft for new Sliders class using SrollView.
Only the window creation and the allocateSlider methods need to change. 

IZ Sat, Mar 29 2014, 05:43 EET


a = Sliders2.getPanel;

a.testNewWidget;

{ a.sliderFor(UniqueID.next); } ! 10;

*/

Sliders2 : UniqueWindow {

	var <layout, <sliders;
 
	init {
		layout = VLayout();
		sliders = IdentityDictionary();
		super.init;
	}

	*getPanel { | model, panelName |
		^this.for(model ?? { SynthTree.server }, panelName ? \faders, { | self |
			var scroll, canvas, layout, window;
			window = self.window;
			window.bounds = Rect(0, 200, 200, Window.availableBounds.height - 200);
			scroll = ScrollView(window, window.view.bounds);
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
		widget = SliderWithLabel();
		widget.setObject(object);
		sliders[object] = widget;
		layout insert: widget.layout;
		^widget;
	}
}