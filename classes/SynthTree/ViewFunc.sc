/*
Works in a similar way as OSCFunc or MIDIFunc, but simpler: 
Only listen to the updates received from view when its value changes, and when the view closes.
More actions may be added as vars for onKeyDown, onKeyUp, onMouseDown, onMouseUp. 

.  Dispatch and receipt of values sent by a View.

Uses Notification for broadcasting its value;
*/

ViewFunc {
	var <view, <>action, <>onClose;

	*new { | view, action, onClose, enabled = true |
		^this.newCopyArgs(view, action, onClose).init(enabled);
	}

	init { | enabled = true |
		var notifier = this.class;
		view.action = { | value |
			[this, thisMethod.name, "action", value].postln;
			notifier.changed(\value, value); 
		};
		view.onClose = { | value |
			[this, thisMethod.name, "closed", value].postln;
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