/* Tue, Jun 16 2015, 03:08 EEST

Inspired by Steno of Julian Rohrhuber. 
https://github.com/telephon/Steno

A simpler version that only creates chucks and puts them in Groups and links them with Busses. 
*/

MiniSteno {
	var <tree;
	classvar inactive;
	
	inactive { ^this.class.inactive }
	*inactive {
		inactive ?? { inactive = Set() };
		^inactive;
	}
   
	initInactive { inactive = Chuck.all }

	*fromString { | string |
		string = string.replace (".", "', '");
		string = string
		.replace("(", "', Ser('")
		.replace("[", "', Par('")
		.replace(")", "'), '")
		.replace("]", "'), '")
		.replace(", '')", ")");
		string = format("Par('%')", string);
		string.postln.interpret;
	}

	*new { | ... specs |
		^this.newCopyArgs(specs collect: _.asSteno);
 	}

	asSteno {}
   
	push { // use this instance as the global tree of linked Chucks
		Library.put(MiniSteno, \current, this);
		this.initRootTree;
	}

	initRootTree {
		var nullGroup;
		nullGroup = GroupLink.nullGroup;
		this.initInactive;
		this.setBussesAndGroups(ArBusLink.nullBus, ArBusLink.nullBus, GroupLink.default /*, 0 */);
		// numLinkChucks do: { | i | Chuck(i.asSymbol).playIfNotPlaying };
		this.inactive do: _.setTarget(nullGroup);
		"================================================================".postln;
		MiniSteno.current.pp;
		"================================================================".postln;	
	}

	*current { ^Library.at(MiniSteno, \current) }

	pp { | levels = "" | // prettyprint
		postf ("% (        // % % %\n", levels, levels, this.class, levels);
		tree do: { | x |
			if (x isKindOf: Chuck) {
				postf("*%* % % % % %\n", x.target.level,
					levels, x, x.target, x.inBus, x.outBus);
			}{
				x.pp(levels ++ "-");
			};
		};
		postf("% )\n", levels);
	}

	// ================================================================
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

	doIfFound { | element, foundFunc, missingFunc |
		var container;
		container = this.findContainerOf(element);
		if (container.isNil) {
			missingFunc.(element, this);
		}{
			foundFunc.(container, element, this);
		}
	}
	
	findContainerOf { | element |
		var found;
		this.traverseDoing({ | ms | if (ms.tree includes: element) { found = ms } });
		^found;
	}

	traverseDoing { | func |
		func.(this);
		tree do: { | x | if (x isKindOf: MiniSteno) { x.traverseDoing(func) } }
	}
}

Par : MiniSteno {

	setBussesAndGroups { | inBus, outBus, group |
		// TODO: Test this new order - should work with Par-Ser-Par-Ser nestings
		var outGroup, newOutGroup;
		outGroup = group;
		tree do: { | branch, i |
			newOutGroup = branch.setBussesAndGroups(inBus, outBus, group);
			if (newOutGroup isAfter: outGroup) { outGroup = newOutGroup };
		};
		^outGroup;
	}
}

Ser : MiniSteno {

	setBussesAndGroups { | inBus, outBus, group | // if parent is a Ser, end in 0
		// Here a nested Ser allows one to "branch out" of a ser, directly to root output.
		var end, busses, nextBus;
		busses = [];
		nextBus = inBus;
		end = tree.size - 1;
		tree do: { | branch, i |
			if (branch isKindOf: Ser) {
				busses = busses add: nextBus;
				busses = busses add: BusLink.nullBus;
			}{
				busses = busses add: nextBus;
				busses = busses add: if (i == end) {
					outBus
				}{
					nextBus = ArBusLink()
				}
			}
		};
		tree do: { | branch, i |
			group = branch.setBussesAndGroups(
				busses[i * 2],
				busses[i * 2 + 1],
				group);
		};
		^group;
	}
}

+ String {
	miniSteno { ^MiniSteno.fromString(this) }
	arlink { ^this.miniSteno.push }
}

+ Symbol {
	asSteno { ^Chuck (this) }
}