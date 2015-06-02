+ Symbol {
	w { | x = 100, y = 100, width = 400, height = 200 |
		^Registry(Window, this, { Window(this.asString, Rect(x, y, width, height))}).front;
	}
}

/*
\test.w;
\test.w.view.layout = HLayout();
\test.w.view.layout add: Slider();
*/