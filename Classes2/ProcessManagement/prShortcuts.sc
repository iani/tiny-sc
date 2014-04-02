/* Shortcuts for adding processes to ProcessRegistry
IZ Mon, 03 Mar 2014 06:38:17
*/

Syn {
	*new { | defName, args, target, addAction = \addToHead |
		if (args.isNil) {
			args = [\out, ~mixerIn ? 0];
		}{
			if (args.includes(\out).not) {
				args = args ++ [\out, ~mixerIn ? 0];
			}
		};
		^Synth(defName, args, target ?? { ~target }, addAction).rp;
	}
}

Gro {
	*new { | ... args |
		^Group(*args).rp;
	}
}

Pbi {
	*new { | ... args |
		^Pbind(*args).rp
	}
}
