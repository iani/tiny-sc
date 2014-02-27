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

	var <processes;  // List of running processes.

	default {
		if (default.isNil) { default = this.new };
		^default;
	}

	*new {
		^this.newCopyArgs(List());
	}

	*register { | anObject |
		this.default register: anObject;
	}

	register { | object |
		// objects that can run add themselves to 
		object.registerProcess(this);
	}

	add { | object |
		// add object to the processes list
		processes add: object;
	}

	processEnded { | process |
		processes remove: process;
		this.changed(\processEnded, process);
	}
}


+ Object {
	registerProcess { }
}

+ Node {
	registerProcess { | processRegistry |
		// add myself to processRegistry, and prepare to notify when I end
		processRegistry add: this;
		// TODO: complete this. NodeWatcher? Notification?
	}
}