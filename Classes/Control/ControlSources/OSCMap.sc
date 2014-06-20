/*

Obsolete.  See BroadcastResponder.

Experimental: 
Map a value received via osc message to range 0-1 and broadcast it with changed message. 

Wed, Jun 18 2014, 10:25 EEST

Note: Could be optimized by using own set of dependants. 

*/

/*
OSCMap : OSCFunc {
	var <>mapper;

	*new { | mapper, path, srcID, recvPort, argTemplate, dispatcher |
		^super.new({}, path, srcID, recvPort, argTemplate, dispatcher)
		.initOSCMapper(mapper = path.asSpec ? NullSpec);
	}

	initOSCMapper { | argMapper |
		mapper = argMapper;
		this.prFunc = { | msg | this.changed(\value, mapper.unmap(msg[1])) }
	}

	addListener { | listener, action |
		action ?? { action = { | value | listener.mapSet(value) } };
		listener.addNotifier(this, \value, action);
	}

	removeListener { | listener |
		listener.removeNotifier(this, \value);
	}
}

+ Symbol {
	osc { | mapper, path, srcID, recvPort, argTemplate, dispatcher |
		^Registry(\osc, this, {
			OSCMap(mapper, path ? this, srcID, recvPort, argTemplate, dispatcher)
		})
	}
}
*/