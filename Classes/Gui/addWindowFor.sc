/*
Create a unique window for a tuple object-key.  

Register the window in the global Library under [object, key].
If the window already exists, bring it to front, else create it. 
When the window closes, remove it from the object-key path.

Fri, May 30 2014, 14:20 EEST
*/

+ Window {
	*for { | object key = \gui, initFunc |
		^Registry(object, key, {
			var window;
			window = Window(format("%:%", object, key));
			initFunc.(window, object, key);
			window.onClose = {
				// NameSpace.remove(object, key);
				window.objectClosed;
			};
		}).front;
	}

	width_ { | width = 200 | this.bounds = this.bounds.width = width }

	height_ { | height = 200 | this.bounds = this.bounds.height = height }

	top { | height = 20 |
		var available;
		available = Window.availableBounds;
		this.bounds = Rect(0, available.height - height, available.width, height);
	}

	left { | width = 200 |
		var available;
		available = Window.availableBounds;
		this.bounds = Rect(0, 0, width, available.height);
	}

	bottom { | height = 100 |
		var available;
		available = Window.availableBounds;
		this.bounds = Rect(0, 0, available.width, height);
	}

	right { | width = 200 |
		var available;
		available = Window.availableBounds;
		this.bounds = Rect(available.width - width, 0, width, available.height);
	}

	topRight { | width = 400, height = 400 |
		var available;
		available = Window.availableBounds;
		this.bounds = 
		Rect(available.width - width, available.height - height, width, height);
	}

	shift { | x = 0, y = 0 |
		var bounds;
		bounds = this.bounds;
		this.bounds = bounds.left_(bounds.left + x).top_(bounds.top + y);
	}
	shiftTo { | x, y |
		var bounds;
		bounds = this.bounds;
		this.bounds = bounds.left_(x ?? { bounds.left }).top_(y ?? { bounds.top });
	}
}
