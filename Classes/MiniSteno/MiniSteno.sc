/*
Tue, Jun 16 2015, 03:08 EEST

Inspired by Steno of Julian Rohrhuber. 
https://github.com/telephon/Steno

A simpler version that only creates chucks and puts them in Groups and links them with Busses. 

*/

MiniSteno {
	var <tree;
	classvar <numLinkChucks; // for naming system-created link Chucks

	*fromString { | string |
		numLinkChucks = 0;
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
		var linkChuck;
		^this.newCopyArgs(specs collect: { | s |
			if (s isKindOf: Symbol) { Chuck(s) } { s }
		});
 	}

	push { // use this instance as the global tree of linked Chucks
		Library.put(MiniSteno, \current, this);
		this.initRootTree;
	}

	initRootTree {
		var nullGroup;
		nullGroup = GroupLink.nullGroup;
		Chuck.initInactive;
		tree do: _.insertSerInPar; // Indispensable. Keep.
		// Being TESTED: Groups are now ordered so that Ser nested in Par work for next Ser...
		// See draft in Par:setBussesAndGroups method below.
		// Note: Re-use of busses at re-allocation is not optimal, but close to optimal.
		this.setBussesAndGroups(ArBusLink.nullBus, ArBusLink.nullBus, GroupLink.default, 0);
		numLinkChucks do: { | i | Chuck(i.asSymbol).playIfNotPlaying };
		Chuck.inactive do: _.setTarget(nullGroup);
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
	insertSerInPar {
		tree = tree collect: { | el |
			// TODO: Test following commented version
			// Wed, Jun 17 2015, 22:01 EEST - still TODO:
			// Not needed to insert link-chucks, if this is already a Ser?
			// But what if the ser only contains one element?
			// So it is easier and safer to always insert the link chucks ...
			// if(el idKindOf: Ser) {
			///	el
			// }{
				Ser(this.makeLinkChuck, el, this.makeLinkChuck)
			//}
		};
	}

	makeLinkChuck {
		var linkChuck;
		linkChuck = Chuck(numLinkChucks.asSymbol);
		linkChuck.source = { Inp.ar };
		linkChuck.permanent;
		numLinkChucks = numLinkChucks + 1;
		^linkChuck;
	}
	
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
	insertSerInPar { tree do: _.insertSerInPar }

	setBussesAndGroups { | inBus, outBus, group, parent | // if parent is a Ser, end in 0
		var busArray;
		this.flatten; // TODO: Remove this when figuring out the bus linking for nested Ser:
		// TODO: the parent check below enables 
		// one to "branch out" of a ser, directly to root output ... Group order is OK now. 
		// But in the following node must read from the output of the previous one
		// skipping this one.  How to do this?
		busArray = [inBus];
		tree.size - 1 do: {
			busArray = busArray add: ArBusLink();
		};
		busArray = busArray add: if (parent isKindOf: Ser ) {
			BusLink.nullBus; // will occur in nested Ser, when we disable "flatten"
		}{
			outBus;
		};
		tree do: { | branch, i |
			group = branch.setBussesAndGroups(busArray[i], busArray[i + 1], group, this);
			// TODO : Test this for the not - flattened variety:
			// #inBus, group = branch.setBus ...
		};
		^group;
	}

	flatten {
		// will become obsolete when skipping busses for nested ser in ser is implemented
		var newTree;
		tree do: { | el |
			if (el isKindOf: Ser) {
				el.flatten;
				el.tree do: { | el2 | newTree = newTree add: el2 };
			}{
				newTree = newTree add: el;
			}
		};
		tree = newTree;
	}
	
}

+ String {
	miniSteno { ^MiniSteno.fromString(this) }
	arlink { ^this.miniSteno.push }
}