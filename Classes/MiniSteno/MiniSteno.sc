/*
Tue, Jun 16 2015, 03:08 EEST

Inspired by Steno of Julian Rohrhuber. 
https://github.com/telephon/Steno

A much simpler draft that only creates chucks and puts them in Groups and links them with Busses. 

a = "b[(abc)[ax]]".miniSteno;
a.pp;
a.inspect;

b = "(abc)[ax]de(fgh[ij])".miniSteno;
b.pp;
 b.inspect;

"a".miniSteno;
"[a]".miniSteno;

*/

MiniSteno {
	var <tree; // , <>parent;
	*fromString { | string |
		string = string.inject(" ", { | a, x |
			a ++ (if ("[]()".includes(x)) { x } { format("'%', ", x) })
		});
		string = string.replace("(", "Ser( ")
		.replace("[", "Par( ")
		.replace(")", "), ")
		.replace("]", "), ")
		.replace(", )", ")");
		^format("Par( % )", string).postln.interpret;
	}

	*new { | ... specs |
		^this.newCopyArgs(specs collect: { | s |
			if (s isKindOf: Symbol) { Chuck(s) } { s }
		}); // .init
	}

	pp { | levels = "" | // prettyprint
		postf ("% (        // % % %\n", levels, levels, this.class, levels);
		tree do: { | x |
			if (x isKindOf: Chuck) {
				postf("  % % % % %\n", levels, x, x.target, x.inBus, x.outBus);
			}{
				x.pp(levels ++ "-");
			};
		};
		postf("% )\n", levels);
	}
}

Par : MiniSteno {
	setBussesAndGroups { | inBus, outBus, group |
		tree do: { | branch | branch.setBusses(inBus, outBus, group) }
	}
}

Ser : MiniSteno {
	setBussesAndGroups { | inBus, outBus, group |
		var busArray, groupArray;
		busArray = [inBus];
		groupArray = [group];
		tree.size - 1 do: {
			busArray = busArray add: ArLinkBus();
			groupArray = groupArray add: (group = group.getReaderGroup);
		};
		busArray = busArray add: outBus;
		tree do: { | branch, i |
			branch.setBussesAndGroups(busArray[i], busArray[i + 1], groupArray[i])
		};
	}	
}

+ String { miniSteno { ^MiniSteno.fromString(this) }}