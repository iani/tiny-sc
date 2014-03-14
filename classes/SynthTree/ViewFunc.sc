/*
Works in a similar way as OSCFunc or MIDIFunc, but simpler: 
Only listen to the updates received from view when its value changes, and when the view closes.
More actions may be added as vars for onKeyDown, onKeyUp, onMouseDown, onMouseUp. 

.  Dispatch and receipt of values sent by a View.

Uses Notification for broadcasting its value;

Disadvantage: 
An object can add the same action to the same view many times by creating many ViewFunc for the same view with the same action.  Then each time that the view's action is activated by the user via mouse,  the view will send the same message many times to the same object.  To prevent this, care must be taken to create a ViewFunc only once for the same object, view and function.  An alternative is written below, UniqueViewFunc, which uses the object as listener (receiver) in the object.addNotifier method below, and thus only allows one object-view-action combination per object-view pair.  This has yet to be tested.  
*/

ViewFunc {
	var <view, <>action, <>onClose;

	*new { | view, action, onClose, enabled = true |
		^this.newCopyArgs(view, action, onClose).init(enabled);
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

/* Experimental variant:  UniqueViewFunc - not yet tested.
When tested, the ViewFunc above should be replaced with UniqueViewFunc. 
*/

UniqueViewFunc {
	var <receiver, <view, <>action, <>onClose;

	*new { | receiver, view, action, onClose, enabled = true |
		^this.newCopyArgs(receiver, view, action, onClose).init(enabled);
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
		receiver.addNotifier(notifier, \value, { | me | action.(me.value, this) });
		receiver.addNotifier(notifier, \closed, {
			onClose.(this);
			// todo: substitute single statement: "this.remove;" instead of the 
			// following three statements below:
			this.disable; // Necessary: remove receiver/class notification!
			receiver.removeNotifier(this.class, \closed);
			this.objectClosed; // Not needed? No notifications are added to self!
		});
	}

	disable {
		receiver.removeNotifier(this.class, \value);
		// this.objectClosed;
	}

	remove {
		[this, thisMethod.name, "not implemented"].postln;
		// TODO: Implement and check this with the code below:
		// this.disable;
		// receiver.removeNotifier(this.class, \closed);
	}
}
