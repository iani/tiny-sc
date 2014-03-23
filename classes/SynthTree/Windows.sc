/*
Enable any object to register windows under any number of keys.
If a window for that object and key exists, bring it to front.
If not, then create it with the function given by the object, 
and store it in a registry in classvar all.
When the window closes, remove it from the windows registry.

IZ Sun, Mar 23 2014, 11:26 EET
*/


Windows {
	classvar <all;

	*initClass {
		all = MultiLevelIdentityDictionary();
	}

	*for { | object, key, initFunc |
		var window;
		window = all.at(object, key);
		if (window.notNil) {
			^window.front
		}{
			window = Window(format("%:%", object, key));
			initFunc.(window);
			window.onClose = { window.objectClosed };
			this.addNotifierOneShot(window, \objectClosed, {
				all.removeEmptyAt(object, key);
			});
			all.put(object, key, window);
			^window.front;
		}
	}

}