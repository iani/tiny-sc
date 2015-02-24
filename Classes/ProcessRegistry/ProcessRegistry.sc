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

	*register { | process |
		this.default register: process;
	}

	register { | process |
		// processss that can run add themselves to the registry
		process.registerProcess(this);
	}

	add { | process, message |
		// add process to the processes list
		process = NamedProcess(process);
		processes add: process;
		this.addNotifierOneShot(process.process, message,
			{ this.processEnded(process) });
		this.changed(\processAdded, process);
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

	*removeProcessesForID { | source_id, eval_id |
		this.default.removeProcessesForID(source_id, eval_id);
	}
	removeProcessesForID { | source_id, eval_id |
		// [this, thisMethod.name, "scr:", source_id, "eval:", eval_id].postln;
		processes.asArray do: { | p |
			//	[p, p.source_id, p.eval_id].postln;
			if (p.source_id === source_id) { p.stop };
		}
	}
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
		listView = ListView(window, window.view.bounds).resize = 5;
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
		{ listView.items = processRegistry.processes collect: _.asString; }.defer;
	}
}

NamedProcess {

	var <process, <source_id, <eval_id;

	*new { | process |
		^this.newCopyArgs(process, ~source_id, ~eval_id);
	}

	stop { process.stop }

	asString { ^format("% (%:%)", process.asString, eval_id ? "", source_id ? "") }
}

+ Node {
	rp { | processRegistry |
		// add myself to processRegistry, and prepare to notify when I end
		(processRegistry ?? { ProcessRegistry.default }).add(this, \n_end);
		NodeWatcher.register(this);
	}

	stop { this.free }

	onEnd { | listener, action |
		NodeWatcher.register(this);
		this.isPlaying = true; // dangerous
		listener.addNotifierOneShot(this, \n_end, action);
	}

	onStart { | listener, action |
		NodeWatcher.register(this);
		listener.addNotifierOneShot(this, \n_go, action);
	}

	// TODO:
	/* 
	onBegin { | lister, action |
		
	}
	*newNotifying { | listener, action |
		NodeWatcher.register(this);
		listener.addNotifierOneShot(this, \n_end, action);
	}
	*/
}

+ Routine {
	rp { | processRegistry |
		// add myself to processRegistry
		(processRegistry ?? { ProcessRegistry.default }).add(this, \p_end);
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
