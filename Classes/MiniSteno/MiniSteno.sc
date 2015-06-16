/*
Tue, Jun 16 2015, 03:08 EEST

Inspired by Steno of Julian Rohrhuber. 
https://github.com/telephon/Steno

A much simpler draft that only creates chucks and puts them in Groups and links them with Busses. 

a = "b[(abc)[ax]]".miniSteno;
a.pp;
a.inspect;

b = "[(abc)[ax]]".miniSteno;
b.pp;
b.inspect;

"a".miniSteno;
"[a]".miniSteno;

*/

MiniSteno {
	var <tree, <>parent;
	*fromString { | string |
		if ("[(".includes(string[0]).not) { string = "(" ++ string ++ ")" };
		string = string.inject(" ", { | a, x |
			a ++ (if ("[]()".includes(x)) { x } { format("'%', ", x) })
		});
		string = string.replace("(", "Ser( ")
		.replace("[", "Par( ")
		.replace(")", "), ")
		.replace("]", "), ")
		.replace(", )", ")");
		^string[0..string.size - 3].postln.interpret;
	}

	*new { | ... specs |
		^this.newCopyArgs(specs collect: { | s |
			if (s isKindOf: Symbol) { Chuck(s) } { s }
		}).init
	}

	init {
		tree do: { | x | if (x isKindOf: MiniSteno ) { x.parent = this } }
	}

	pp { | levels = "" | // prettyprint
		postf ("%( // ---- % ----\n", levels, this.class);
		tree do: { | x |
			if (x isKindOf: Chuck) {
				postf("% %\n", levels, x);
			}{
				//		x.postln;
				x.pp(levels ++ " ");
			};
		};
		postf("%)\n", levels);
	}

	inBus {
		if (parent.isNil) { ^BusLink.nullBus } { ^parent.outBus };
	}

	outBus { ^tree.last.outBus }
}

Par : MiniSteno {
	setBusses {
		var inBus, outBus;
		inBus = this.inBus;
		tree do: { | branch |
			branch.setInBus(inBus);
			branch.set
		}
	}
}

Ser : MiniSteno {
	setBusses {
		var inBus;
		inBus = super.connect;
		tree do: { | branch |
			inBus = branch.setBusses(inBus);
		}
	}	
}

+ String { miniSteno { ^MiniSteno.fromString(this) }}