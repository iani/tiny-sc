/*
Tue, Jun 16 2015, 03:08 EEST

Inspired by Steno of Julian Rohrhuber. 
https://github.com/telephon/Steno

A much simpler draft that only creates chucks and puts them in Groups and links them with Busses. 

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
		});
	}

	push {
		var nullGroup;
		nullGroup = GroupLink.nullGroup;
		Library.put(MiniSteno, \current, this);
		Chuck.initInactive;
		this.setBussesAndGroups(ArBusLink.nullBus, ArBusLink.nullBus, GroupLink.default);
		Chuck.inactive do: _.setTarget(nullGroup);
		
	}

	*current { ^Library.at(MiniSteno, \current) }

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

	// TODO: Changing the structure of the tree after it was created:
	addBefore { | chuck, asSibling = false |

	}

	addAfter { | chuck, asSibling = false |

	}

	addSibling { | newChuck oldChuck |

	}

	remove { | chuck, sendToNullGroup = false |
		/* rearrange tree by removing chuck from it, if it exists. */
		var container;
		container = tree findContainerOf: chuck;
		
	}

	findContainerOf { | element |
		var found, func;
		func = { | ms |
			if (ms.tree includes: element) { found = ms }; 
		};
		this.traverseDoing(func);
		^found;
	}

	traverseDoing { | func |
		this.postln;
		func.(this);
		tree do: { | x |
			if (x isKindOf: MiniSteno) { x.traverseDoing(func) }
		}
	}
}

Par : MiniSteno {
	setBussesAndGroups { | inBus, outBus, group |
		tree do: { | branch | branch.setBussesAndGroups(inBus, outBus, group) }
	}
}

Ser : MiniSteno {
	setBussesAndGroups { | inBus, outBus, group |
		var busArray, groupArray;
		busArray = [inBus];
		groupArray = [group];
		tree.size - 1 do: {
			busArray = busArray add: ArBusLink();
			groupArray = groupArray add: (group = group.getReaderGroup);
		};
		busArray = busArray add: outBus;
		tree do: { | branch, i |
			branch.setBussesAndGroups(busArray[i], busArray[i + 1], groupArray[i])
		};
	}	
}

+ String {
	miniSteno { ^MiniSteno.fromString(this) }
	arlink { ^this.miniSteno.push }

}