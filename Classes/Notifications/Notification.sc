/*

Enable adding message-specific calls using the changed/update mechanism of Object.

Usage:

anObject.addNotifier(aNotifier, \message, action) :

Make anObject perform action when aNotifier calls:
aNotifier.changed(\message);

Arguments passed to the action are: The extra arguments given in the 'changed' call + the notification instance.

The notifier and the message are *not* passed as arguments to the action. This is for several reasons:

- to keep compatibility to NotificationCenter-type calls
- to keep the argument definition part of the action function shorter
- because the Notification instance that is always passed at the end contains all of:
  - the sender (notifier, changer)
  - the receiver (listener)
  - the message
  - the action.

So all the info is available there.

Example:
(
\listener.addNotifier(\notifier, \test, { | one, two, three, notifier |
	postf("one: %\n", one);
	postf("two: %\n", two);
	postf("three: %\n", three);
	postf("notifier: %\n", notifier);
	notifier.inspect;
});

\notifier.changed(\test, 1, 2, 3);
)

anObject.objectClosed : remove all notifiers and listeners from / to anObject.

*/


Notification {
	classvar <all;

	var <notifier, <message, <listener, <>action;

	*initClass {
		all = MultiLevelIdentityDictionary.new;
	}

	*new { | notifier, message, listener, action |
		// First remove previous notification at same address, if it exists:
		this.remove(notifier, message, listener);
		^this.newCopyArgs(notifier, message, listener, action).init;
	}

	init {
		notifier.addDependant(this);
		all.put(notifier, listener, message, this);
	}

	update { | sender, argMessage ... args |
		if (argMessage === message) {
			action.valueArray(if (args.size == 0) { [this] } { args add: this })
		}
	}

	*remove { | notifier, message, listener |
		all.at(notifier, listener, message).remove;
	}

	remove {
		notifier.removeDependant(this);
		all.removeEmptyAt(notifier, listener, message);
	}

	*removeMessage { | message, listener |
		all leafDo: { | path, notification |
			if (notification.message === message and: { notification.listener === listener }) {
				notification.remove;
			}
		}
	}

	*removeListenersOf { | notifier |
		// Corrected 140612 after MC because it breaks if the notifier is an empty envir ().
		// Because ().asArray is [] which matches the whole tree.  See leafDoFrom!
		all.leafDoFrom([notifier], { | path, notification |
			notification.notifier.removeDependant(notification);
		});
		all.put(notifier, nil);
	}

	*removeNotifiersOf { | listener |
		all do: { | listenerDict |
			listenerDict keysValuesDo: { | argListener, messageDict |
				if (argListener === listener) { messageDict do: _.remove; }
			}
		}
	}
}

+ Object {

	addNotifier { | notifier, message, action |
		Notification(notifier, message, this, action);
	}

	removeNotifier { | notifier, message |
		Notification.remove(notifier, message, this);
	}

	// Review? Possibly used by AppModel. 
	// But the correct form should be 
	// Notification.remove(notifier, message, this);
	// Maybe use another name for the correct form in order not to break
	// AppModel. INVESTIGATE!
	// First remove any previous notifiers that send me this message, 
	// then add the new notifier, message, and action.
	replaceNotifier { | notifier, message, action |
		this removeMessage: message;
		this.addNotifier(notifier, message, action);
	}

	// remove any notifiers that send me message
	removeMessage { | message |
		Notification.removeMessage(message, this);
	}

	objectClosed {
		this.changed(\objectClosed);
		Notification.removeNotifiersOf(this);
		Notification.removeListenersOf(this);
		this.releaseDependants;
	}

	onObjectClosed { | listener, action |
		listener.addNotifier(this, \objectClosed, action);
		if (this respondsTo: \onClose_) {
			this.onClose = { this.objectClosed };
		}
	}

	addNotifierOneShot { | notifier, message, action |
		Notification(notifier, message, this, { | ... args |
			action.(*args); //action.(args);
			args.last.remove;
		});
	}

	addNotifierAction { | notifier, message, action |
		var notification;
		notification = Notification.all.at(notifier, message, action);
		if (notification.isNil) {
			this.addNotifier(notifier, message, action);
		}{
			notification.action = notification.action addFunc: action;
		}
	}
}

+ Node {
    /* always release notified nodes when they are freed
        Note: any objects that want to be notified of the node's end, 
        can listen to it notifying 'n_end', which is triggered through NodeWatcher
        and which is the same message that makes the Node remove all its Notifications.
    */
    addNotifier { | notifier, message, action |
        super.addNotifier(notifier, message, action);
        NodeWatcher.register(this);
        this.addNotifierOneShot(this, 'n_end', {
			// remove notifiers only after all notifications have been issued!
			{ this.objectClosed; }.defer(0.1);
		});
    }

	
}

+ View {
	addNotifier { | notifier, message, action |
        super.addNotifier(notifier, message, action);
		this.onClose = this.objectClosed;
    }
}
