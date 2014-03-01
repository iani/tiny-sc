/* 

Keep track of running Nodes, Routines, Tasks and EventStreamPlayers.  
Issue notifications when an instance of one of the above stops running. 

Other objects can use the ProcessRegistry to update displays and lists of processes.

In this way, one may be able to build similar widgets as the Virtual Machine Monitor of Mini-Audicle in ChucK. 

UNDER CONSTRUCTION.

Note: To kill all child-processes of a Routine one may add thisThread as notifier to a child process (Node or Routine or EventStreamPlayer) and notify the children when the thread stops.  Similarly for EventStreamPlayer.

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

	removeSelection { | selection |
		var processArray;
		processArray = this.processes;
		selection do: { | index |
			processArray[index].stop;
		};
	}
	clear { processes = OrderedIdentitySet(); }
}

ProcessRegistryGui {
	classvar guis;

	*initClass {
		StartUp.add({
			CmdPeriod.add({
				this.changed(\cmdPeriod);
			});
		});
	}

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
		listView.addNotifier(this, \cmdPeriod, {
			processRegistry.clear;
			this.updateView(processRegistry, listView);
		});
		listView.keyDownAction = { |  view, char, modifiers, unicode, keycode, key |
			switch (keycode, 51, { processRegistry.removeSelection(listView.selection); });
		};
		window.onClose = {
			listView.removeNotifier(processRegistry);
			listView.removeNotifier(this);
			this.guis[processRegistry] = nil;
		};
		^window;
	}

	*updateView { | processRegistry, listView |
		listView.items = processRegistry.processes collect: _.asString;
	}
}

// Shortcuts

Syn {
	*new { | ... args |
		^Synth(*args).rp;
	}
}

Gro {
	*new { | ... args |
		^Group(*args).rp;
	}
}

Pbi {
	*new { | ... args |
		^Pbind(*args).rp
	}
}

+ Node {
	rp { | processRegistry |
		// add myself to processRegistry, and prepare to notify when I end
		(processRegistry ?? { ProcessRegistry.default }).add(this, \n_end);
		NodeWatcher.register(this);
	}
	stop { this.free }
}

+ Routine {
	rp { | processRegistry |
		// add myself to processRegistry
		(processRegistry ?? { ProcessRegistry.default }).add(this, \p_end);
	}
	stop {
		// Also notify ProcessRegistry.
		this.changed(\p_end);
		//		thisThread.changed(\p_end);		
		if (this === thisThread) { nil.alwaysYield } { this.prStop };

	}
}

+ EventStreamPlayer {
	rp { | processRegistry |
		// add myself to processRegistry
		(processRegistry ?? { ProcessRegistry.default }).add(this, \stopped);
	}
}

+ Function {
	rp { | clock, quant, stackSize, processRegistry |
		^Routine({
			this.value;
			thisThread.changed(\p_end);
		}, stackSize).play(clock, quant).rp(processRegistry);
	}

	// in analogy to shortcuts below
	for { | ... args |
		^this.rp(*args);
	}

	pla { | ... args |
		^this.play(*args).rp;
	}
}