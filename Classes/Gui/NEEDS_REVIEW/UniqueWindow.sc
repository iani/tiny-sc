/*

Note Tue, Jun 24 2014, 12:56 EEST: This class can be replaced with Window.for,
using Registry.
================================================================
Variant of Windows class, to enable subclassing.

Enable any object to register windows under any number of keys.
If a window for that object and key exists, bring it to front.
If not, then create it with the function given by the object, 
and store it in a registry in classvar all.
When the window closes, remove it from the windows registry.

IZ Sat, Mar 29 2014, 06:27 EET

UniqueWindow.for(\a, \b);

*/

UniqueWindow {
	classvar <all;

	var <object, <key, <initFunc;
	var <window;

	*initClass {
		all = MultiLevelIdentityDictionary();
	}

	*for { | object, key, initFunc |
		var window;
		window = all.at(object, key);
		if (window.isNil) {
			window = this.newCopyArgs(object, key, initFunc).initUniqueWindow;
			all.put(object, key, window);
		};
		^window.front;
	}

	front { window.front }
	
	initUniqueWindow {
		window = Window(format("%:%", object, key));
		initFunc.(this);
		window.onClose = { window.objectClosed };
		this.addNotifierOneShot(window, \objectClosed, {
			all.removeEmptyAt(object, key);
			this.objectClosed;
		});
		all.put(object, key, this);
	}

	width_ { | width = 200 | window.bounds = window.bounds.width = width }

	height_ { | height = 200 | window.bounds = window.bounds.height = height }

	top { | height = 20 |
		var available;
		available = Window.availableBounds;
		window.bounds = Rect(0, available.height - height, available.width, height);
	}

	left { | width = 200 |
		var available;
		available = Window.availableBounds;
		window.bounds = Rect(0, 0, width, available.height);
	}

	bottom { | height = 100 |
		var available;
		available = Window.availableBounds;
		window.bounds = Rect(0, 0, available.width, height);
	}

	right { | width = 200 |
		var available;
		available = Window.availableBounds;
		window.bounds = Rect(available.width - width, 0, width, available.height);
	}

	topRight { | width = 400, height = 400 |
		var available;
		available = Window.availableBounds;
		window.bounds = 
		Rect(available.width - width, available.height - height, width, height);
	}
}