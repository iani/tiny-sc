/*
Named container for PatternTask, PatternEventPlayer

NOTE Fri, Jun 13 2014, 10:10 EEST: IS THIS REPLACED BY Edef?

IZ Wed, Apr 16 2014, 04:59 EEST
*/

Mdef {
	classvar <all;

	var <name, <player, <parent;
	var <patterns;

	*initClass { all = IdentityDictionary(); }

	*new { | name, patterns |
		var instance;
		instance = all[name];
		instance ?? {
			instance = this.newCopyArgs(name, PatternEventPlayer()).initMdef;
			all[name] = instance;
		};
		patterns !? { instance.setClear(patterns) };
		^instance;
	}

	initMdef {
		patterns = ();
		this.addNotifier(player, \values, { | values | this.updateFromPlayer(values) });
	}

	setClear { | argPatterns |
		patterns = argPatterns;
		this.updateValues;
	}

	updateValues {
		// get values from filters + parent
		player.inheritValues(this.mergeParentValues);
		this.changed(\values);
	}

	mergeParentValues {
		if (parent.isNil) {
			^patterns;
		}{
			^parent.patterns.copy make: { | event |
				patterns ?? { [] } keysValuesDo: { | par val | event[par] = val.value }
			}
		}
	}

	updateFromPlayer { | values |
		// update values when EventPatternTask is changed from another source
		values keysValuesDo: { | param, value | patterns[param] = value; };
		// do not update to player, since this came from it
		this.changed(\values);
	}

	set { | values |
		// add values in array or event to existing valuePattern array
		values keysValuesDo: { | param, pattern |  patterns[param] = pattern; };
		this.updateValues;
	}

	get { | key | ^player.get(key) }

	clone { | argName | ^this.class.new(argName).parent_(this); }

	parent_ { | argParent |
		parent = argParent;
		this.updateValues;
		this.addNotifier(parent, \values, { this.updateValues });
	}

	start { player.start }
	stop { player.stop }
	monitor { | active = true |
		if (active) {
			this.addNotifier(player, \value, { | values | postf("%: %\n", name, values) })
		}{	
			this.removeNotifier(player, \value)
		}
	}

	asSynthTemplate { ^PatternInstrument(player, name) }
}
