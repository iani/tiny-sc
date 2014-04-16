/*
Named container for PatternPlayer, PatternEventPlayer

IZ Wed, Apr 16 2014, 04:59 EEST
*/

Mdef {
	classvar <all;

	var <name, <player, <parent;

	*initClass { all = IdentityDictionary(); }

	*new { | name, template, durations = 1 |
		var instance;
		instance = all[name];
		instance ?? {
			instance = this.newCopyArgs(name);
			all[name] = instance;
		};
		template !? { instance.initPlayer(template, durations) };
		^instance;
	}

	initPlayer { | template, durations |
		if (player.isNil) {
			player = template.asPatternPlayer(durations);
		}{
			player.setPattern(template, durations);
		}
	}

	durations_ { | durations |
		player.durations = durations;
		this.changed(\values);
	}

	durations { ^player.durations }

	set { | values | 
		player.set(values);
		this.changed(\values);
	}

	get { | key | ^player.get(key) }

	clone { | argName, template, durations |
		var newMdef;
		newMdef = this.class.new(argName, template, durations);
		newMdef.parent = this;
		^newMdef;
	}

	parent_ { | argParent |
		parent = argParent;
		this.getParentValues;
		this.addNotifier(parent, \values, { this.getParentValues });
	}

	getParentValues {
		player !? { player.updateDataFromParent(parent) };
		this.changed(\values);
	}

	start { player.start }
	stop { player.stop }
	monitor { | onOffFlag = true |
		if (onOffFlag) {
			this.addNotifier(player, \value, { | values | values.postln; })
		}{	
			this.removeNotifier(player, \value)
		}
	}
}

+ Object {
	asPatternPlayer { | durations = 1 | ^PatternPlayer(this.asPattern, durations) }
}

+ SequenceableCollection {
	asPatternPlayer { | durations = 1 | ^PatternEventPlayer(this, durations) }
}

+ PatternPlayer {
	updeteDataFromParent {}
}

+ PatternEventPlayer {
	updeteDataFromParent {}
}
