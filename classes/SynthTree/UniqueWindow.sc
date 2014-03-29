/*

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
			window = this.newCopyArgs(object, key, initFunc).init;
			all.put(object, key, window);
		};
		^window.front;
	}

	front { window.front }
	
	init {
		window = Window(format("%:%", object, key));
		initFunc.(this);
		window.onClose = { window.objectClosed };
		this.addNotifierOneShot(window, \objectClosed, {
			all.removeEmptyAt(object, key);
		});
		all.put(object, key, this);
	}
}