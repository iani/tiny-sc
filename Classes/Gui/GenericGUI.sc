
/*
Any object should be able to register a window in Registry.

The window may have a default layout that allows to add or remove widgets 
horizontally or vertically. 

Draft.
*/

//: This is how to make a window with a scroll view so that objects can be added

/*
w = Window().front;
~scroll = ScrollView(w, w.view.bounds).canFocus_(false).resize_(5);
~canvas = View();
~canvas.layout = HLayout();
~scroll.canvas = ~canvas;
~canvas.layout.insert(Slider());

//: add some more sliders
{~canvas.layout.insert(Slider())} ! 10;
*/

+ Object {

	w { | name = \gui, makeFunc |
		makeFunc ?? { makeFunc = 
			{ this.makeDefaultWindow(format("%:%", this, name)) }
		};
		^Registry(this, name, makeFunc);
	}

	makeDefaultWindow { | name |
		var window, scroll, canvas;
		window = Window(name);
		scroll = ScrollView(window, window.view.bounds).canFocus_(false).resize_(5);
		canvas = View();
		canvas.layout = VLayout();
		scroll.canvas = canvas;
		window.onClose = { window.objectClosed }; 
		^window.front;
	}

	slider { | name = \slider, windowName = \gui, windowFunc |
		var window, parentLayout, layout, slider;
		window = this.w(windowName, windowFunc).front;
		^Registry(window, name, {
				parentLayout = window.view.children.first.canvas.layout;
				if (parentLayout isKindOf: HLayout) {
					layout = VLayout(
						StaticText().string_(name.asString),
						slider = Slider().orientation_(\vertical)
					);
				}{
					layout = HLayout(
						StaticText().string_(name.asString),
						slider = Slider().orientation_(\horizontal)
					);
				};
			parentLayout.insert(layout);
			slider.action = { | me | me.changed(\value, me.value) };
			slider.keyDownAction = { | ... args | slider.changed(\keydown, *args); };
			slider.onClose = { slider.objectClosed };
			slider;
		})
	}
}

/*
+ Symbol {
	slider { 

	}
}
*/