/*
Named container for PatternPlayer, PatternEventPlayer

IZ Wed, Apr 16 2014, 04:59 EEST
*/

Mdef {
	classvar <all;

	var <name, <player, <parent;
	var <valueFilter, <durationFilter;

	*initClass { all = IdentityDictionary(); }

	*new { | name, template, durations, valueFilter, durationFilter |
		var instance;
		instance = all[name];
		instance ?? {
			instance = this.newCopyArgs(name);
			all[name] = instance;
		};
		instance.initPlayer(template, durations, valueFilter, durationFilter);
		^instance;
	}

	initPlayer { | template, durations, argValueFilter, argDurationFilter |
		template !? {
			if (player.isNil) {
				player = template.asPatternPlayer(durations);
			}{
				player.values_(template).durations_(durations);
			};
		};
		valueFilter = argValueFilter ?? { { | x | x } };
		/* // TODO: implement this: 
			valueFilter = argValueFilter ?? { player.valueFilter };
		*/
		durationFilter = argDurationFilter ?? { { | x | x } };
		this.addNotifier(player, \values, { this.updateValueFilterFromPlayer });
		this.changed(\values);
	}

	updateValueFilterFromPlayer {
		[this, thisMethod.name, "NOT YET IMPLEMENTED"].postln;
	}

	durations_ { | durations |
		// when durations are set, inheritance from parent is cancelled.
		durationFilter = durations; // block inheritance from parent
		player.durations = durations;
		this.changed(\values);
	}

	durations { ^player.durations }

	setClear { | values |
		// replace valuePattern array by new values
		player.values = values;
		/*
			valueFilter.clear.addValues(values);
		*/
		this.changed(\values);
	}

	set { | values |
		// add values in array to existing valuePattern array
		// TODO: valueFilter update:
		/*
			valueFilter.addValues(values);
		*/
		player.set(values);
		this.changed(\values);
	}

	get { | key | ^player.get(key) }

	clone { | argName | ^this.class.new(argName).parent_(this); }

	parent_ { | argParent |
		parent = argParent;
		this.getParentValues;
		this.addNotifier(parent, \values, { this.getParentValues });
	}

	valueFilter_ { | argFilter |
		valueFilter = argFilter.asValueFilter;
		parent !? { this.getParentValues; };
	}

	durationFilter_ { | argFilter |
		durationFilter = argFilter;
		parent !? { this.getParentValues; };
	}

	getParentValues {
		if (parent.player.isNil) {
			player = nil;
		}{
			if (player.isNil) {
				player = this.inheritValues
				.asPatternPlayer(durationFilter.(parent.player.durationPattern))
			}{
				player.updateDataFromParent(
					this.inheritValues,
					durationFilter.(parent.player.durationPattern)
				);
			}
		};
		this.changed(\values);
	}

	inheritValues {
		if (parent.isNil) {
			^valueFilter;
		}{
			^parent.valueFilter.copy make: { | event |
				valueFilter keysValuesDo: { | param filter | event[param] = filter.value }
			}
		}
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
}

+ Object {
	asPatternPlayer { | durations = 1 delay = 0 clock | 
		^PatternPlayer(this.asPattern, durations, delay, clock)
	}

	asValueFilter { ^this }
}

+ SequenceableCollection {
	asPatternPlayer { | delay = 0, clock | ^PatternEventPlayer(this, delay, clock) }
}

+ Event {
	asValueFilter {
		^{ | synthPattern |
			/*
			var filter;
			SynthPattern(
				synthPattern.params.clump(2).collect({ | keyVal |
					filter = this[keyVal[0]];
					if (filter.isNil) {
						keyVal
					}{
						[keyVal[0], filter.(keyVal[1])]
					}
				}).flat
			)
			*/
			synthPattern.postln;
		}
	}
}

+ PatternPlayer {
	updateDataFromParent { | vals, durs |
		this.values = vals;
		this.durations = durs;
	}
}

+ PatternEventPlayer {
	updateDataFromParent { | vals | this.values = vals }
}