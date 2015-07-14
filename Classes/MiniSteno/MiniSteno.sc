//:
/* Tue, Jun 16 2015, 03:08 EEST

Inspired by Steno of Julian Rohrhuber. 
https://github.com/telephon/Steno

*/

MiniSteno {
	var <tree; // child branches contained in me: chucks, other MiniStenos
	var <>parent; // MiniSteno containing me.  Needed for remove.
	classvar <>verbose = true;

    addBranch { | name = \root, server |
		var oldBranch, oldChucks, newChucks, nullGroup;
		server = server.asTarget.server;
		oldBranch = this.named (name, server);
		oldBranch !? { oldChucks = oldBranch.chucks };
		nullGroup = GroupLink.nullGroup;
		this.setBussesAndGroups(ArBusLink.nullBus, ArBusLink.nullBus, GroupLink.default);
		newChucks = this.chucks;
		oldChucks do: { | p |
			if (newChucks.includes (p).not) { p.setTarget(nullGroup) } // also free busses? !
		};
		Library.put (MiniSteno, server, name, this);
		if (verbose) {
			"================================================================".postln;
			this.pp;
			"================================================================".postln;
		};	
	}
  
	named {  | name, server | ^Library.at (MiniSteno, server.asTarget.server, name ) }

	*includes { | chuck | ^this.root includes: chuck }
	includes { | chuck |
		^this.findParentOf (chuck).notNil;
	}

	findParentOf { | chuck |
		var found;
		tree do: { | l |
			if (l === chuck) {  // l.chuck === chuck !!!
				^this
			}{
				if (l isKindOf: MiniSteno) {
					found = l findParentOf: chuck;
					if (found.notNil) { ^found }
				}
			};
		};
		^nil;
	}

	chucks {
		^tree.collect (_.chucks).flat;
	}

	*fromString { | string |
		string = string
		.replace (".", "', '");
		string = string
		.replace("(", "', Ser('")
		.replace("[", "', Par('")
		.replace(")", "'), '")
		.replace("]", "'), '")
		.replace(", '')", ")")
		.replace ("@'", "'.alink")
		.replace ("@", "'.alink, '");
		if (string [..2] == "', ") {
			string = string [3..];
  			string = string [..(string.size-4)];
		};
		string = string
		.replace (", ''", "")
		.replace ("'', ", "");
		^string.postln.interpret;
	}

	*new { | ... specs |
		^this.newCopyArgs(specs collect: _.asSteno).init;
 	}

	init {
		tree do: { | b |
			if (b isKindOf: MiniSteno) { b.parent = this }
		}
	}
	
	asSteno {}

	*root { | server | ^Library.at(MiniSteno, server.asTarget.server, \root) }

	pp { | levels = "" | // prettyprint
		postf ("% (        // % % %\n", levels, levels, this.class, levels);
		tree do: { | x |
			if (x isKindOf: ChuckLink) {
				postf("*%* % % % % % %\n", x.target.level,
					levels, x.chuck.name, x.target, x.inputName, x.inBus, x.outBus);
			}{
				x.pp(levels ++ "-");
			};
		};
		postf("% )\n", levels);
	}

	// ================================================================
	// TODO: Changing the structure of the tree after it was created:
	removeChuck { | chuck |
		/* rearrange tree by removing chuck from it, if it exists. */
		var container;
		container = this findParentOf: chuck;
		container !? {
			tree remove: chuck;
			this.removeSelfIfNeeded;
		};
	}

	removeSelfIfNeeded {
		parent !? {
			switch (tree.size,
				0, {
					parent.tree remove: this;
					parent.removeSelfIfNeeded;
				},
				1, {
					parent.tree [parent.tree indexOf: this] = tree [0];
				}
			)
		}
	}

	insertAfter { | existingChuck, newChuck |

	}

	addSiblingAfter {  | existingChuck, newChuck |

	}

	addSibling { | newChuck oldChuck |

	}

	doIfFound { | element, foundFunc, missingFunc |
		var container;
		container = this.findParentOf(element);
		if (container.isNil) {
			missingFunc.(element, this);
		}{
			foundFunc.(container, element, this);
		}
	}
}

Par : MiniSteno {
	var <xIn, <xOut;   // optional custom in and out busses;

	init {
		if (tree [0] isKindOf: ArBusLink) {
			xIn = tree [0];
			tree = tree [1..];
		};
		if (tree.last isKindOf: ArBusLink) {
			xIn = tree.last;
			tree = tree [..tree.size - 2];			
		};
		super.init;
	}
	
	setBussesAndGroups { | inBus, outBus, group |
		var outGroup, newOutGroup;
		outGroup = group;
		xIn !? { inBus = xIn };
		xOut !? { outBus = xOut };
		tree do: { | branch, i |
			newOutGroup = branch.setBussesAndGroups(inBus, outBus, group);
			if (newOutGroup isAfter: outGroup) { outGroup = newOutGroup };
		};
		^outGroup;
	}

	makeOutBus { | inBus |
		^if (xIn.notNil or: { xOut.notNil }) {
			inBus
		} {
			ArBusLink ()
		}; 
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
					nextBus = branch.makeOutBus (nextBus);
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
	arlink { ^("[" ++ this ++ "]").miniSteno.addBranch }
	addBranch { ^this.miniSteno.addBranch }
}

+ Symbol {
	asSteno {
		/*
			var chuck;
			chuck = Chuck(this);
			if (MiniSteno.root.includes(chuck) {
			    silently remove from previous location
			    / or remove from previous location and issue warning
                / or issue error????
			}

		*/
		
		^ChuckLink(*this.asString.split($:));
		
		//	^Chuck (this)
	}
}
