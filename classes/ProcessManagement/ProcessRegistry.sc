/* 

Keep track of running Nodes, Routines, Tasks and EventStreamPlayers.  
Issue notifications when an instance of one of the above stops running. 

Other objects can use the ProcessRegistry to update displays and lists of processes.

In this way, one may be able to build similar widgets as the Virtual Machine Monitor of Mini-Audicle in ChucK. 

UNDER CONSTRUCTION.

IZ 27 Feb 2014 15:35:58
*/

ProcessRegistry {

	classvar default;

	var processes;  // List of running processes.

	*default {
		if (default.isNil) { default = this.new };
		^default;
	}

	*new {
		^this.newCopyArgs(OrderedIdentitySet());
	}

	*register { | anObject |
		this.default register: anObject;
	}

	register { | object |
		// objects that can run add themselves to 
		object.registerProcess(this);
	}

	add { | object, message |
		// add object to the processes list
		processes add: object;
		this.addNotifierOneShot(object, message, { this.processEnded(object) });
		this.changed(\processAdded, object);
	}

	processEnded { | process |
		processes remove: process;
		{ this.changed(\processEnded, process); }.defer;
	}

	processes { ^processes.asArray }
}

ProcessRegistryGui {
	classvar guis;
	
	*guis {
		if (guis.isNil) { guis = IdentityDictionary() };
		^guis;
	}

	*gui { | processRegistry |
		processRegistry = processRegistry ?? { ProcessRegistry.default };
		^(this.guis[processRegistry] ?? { this.makeGui(processRegistry) }).front;
	}

	*makeGui { | processRegistry |
		var window, listView;
		window = Window("ProcessMonitor", 
			Rect(Window.screenBounds.width - 200, 0, 200, 200));
		this.guis[processRegistry] = window;
		listView = ListView(window, window.view.bounds);
		this.updateView(processRegistry, listView);
		listView.addNotifier(processRegistry, \processAdded,
			{ this.updateView(processRegistry, listView); }
		);
		listView.addNotifier(processRegistry, \processEnded,
			{ this.updateView(processRegistry, listView); }
		);
		window.onClose = {
			listView.removeNotifier(processRegistry);
			this.guis[processRegistry] = nil;
		};
		^window;
	}

	*updateView { | processRegistry, listView |
		listView.items = processRegistry.processes collect: _.asString;
	}
}

+ Object {
	registerProcess {}
}

+ Node {
	registerProcess { | processRegistry |
		// add myself to processRegistry, and prepare to notify when I end
		processRegistry.add(this, \n_end);
		NodeWatcher.register(this);
	}
	stop { this.free }
}

+ Routine {
	registerProcess { | processRegistry |
		// add myself to processRegistry
		processRegistry.add(this, \p_end);
	}
	stop {
		// Also notify ProcessRegistry.
		// still debugging this.
		[this.class, thisMethod.name, this.hash].postln;
		this.changed(\p_end);
		thisThread.changed(\p_end);		
		if (this === thisThread) { nil.alwaysYield } { this.prStop };

	}
}

+ EventStreamPlayer {
	registerProcess { | processRegistry |
		// add myself to processRegistry
		processRegistry.add(this, \stopped);
	}
}

+ Function {
	forkn { | clock, quant, stackSize |
		^Routine({
			this.value;
			thisThread.changed(\p_end);
		}, stackSize).play(clock, quant);
	}
}