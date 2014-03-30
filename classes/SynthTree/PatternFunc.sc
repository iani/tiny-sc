/* 
IZ Sun, Mar 30 2014, 16:37 EEST
*/

PatternFunc {
	var <pattern, <receiver, <>action;
	*new { | pattern, receiver, action |
		^this.newCopyArgs(pattern, receiver, action).enable;
	}

	enable {
		this.addNotifier(pattern, \value, action);
	}

	disable {
		this.removeNotifier(pattern, \value);
	}

	remove { this.disable }

}