/*
Works in a similar way as OSCFunc or MIDIFunc, but simpler: 
Only listen to the updates received from view when its value changes, and when the view closes.
More actions may be added as vars for onKeyDown, onKeyUp, onMouseDown, onMouseUp. 

Uses Notification for broadcasting its value;
*/

ViewFunc {
	var <receiver, <view, <>action, <>onClose;

	*knob { | receiver, name = "knob",  action, onClose, enabled = true |
		^this.new(receiver, Knobs.knob(name), action, onClose, enabled)
	}

	*new { | receiver, view, action, onClose, enabled = true |
		^this.newCopyArgs(receiver, view, action, onClose).init(enabled);
	}

	init { | enabled = true |
		view.action = { | value |
			view.changed(\value, value); 
		};
		view.onClose = { view.objectClosed };
		if (enabled) { this.enable };
	}

	enable {
		receiver.addNotifier(view, \value, { | me | action.(me.value, this) });
		receiver.addNotifier(view, \objectClosed, {
			onClose.(this);
			// Nothing else is necessary: objectClosed disconnects view from ViewFunc
		});
	}

	disable {
		receiver.removeNotifier(this, \value);
		// this.objectClosed;
	}

	remove {
		[this, thisMethod.name, "not implemented"].postln;
		// TODO: Implement and check this with the code below:
		// this.disable;
		// receiver.removeNotifier(this.class, \closed);
	}

	value_ { | value |
		view.value = value;
	}
}


/* TODO: 
Replace notifier from this.class to view. 
*/
SimpleViewFunc {
	/* This version of ViewFunc does not require a receiver.  It allows to
		add multiple actions for the same receiver-view pair.  
		Applications that want to ensure that only one action is registered 
		for each receiver-view pair, must use class ViewFunc, or implement 
		the check for uniqueness outside the ViewFunc class */

	var <view, <>action, <>onClose;

	*new { | view, action, onClose, enabled = true |
		^this.newCopyArgs(view, action, onClose).init(enabled);
	}

	*knob { | name = "knob",  action, onClose, enabled = true |
		^this.new(Knobs.knob(name), action, onClose, enabled)
	}

	init { | enabled = true |
		var notifier = this.class;
		view.action = { | value |
			notifier.changed(\value, value); 
		};
		view.onClose = { | value |
			notifier.changed(\closed);
		};
		if (enabled) { this.enable };
	}

	enable {
		var notifier;
		notifier = this.class;
		this.addNotifier(notifier, \value, { | me | action.(me.value, this) });
		this.addNotifier(notifier, \closed, {
			onClose.(this);
			this.disable;
		});
	}

	disable {
		// this.removeNotifier(this.class, message);
		this.objectClosed;
	}
}