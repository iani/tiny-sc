//:
/* Tue, Jun 16 2015, 03:08 EEST

Inspired by Steno of Julian Rohrhuber. 
https://github.com/telephon/Steno

*/

MiniSteno {
	var <tree; // child branches contained in me: chucks, other MiniStenos
	var <>parent; // MiniSteno containing me.  Needed for remove.
	classvar <previous; // chucks traced during build

	named {  | server | ^Library.at (MiniSteno, server.asTarget.server ) }
	*includes { | chuck | ^this.root includes: chuck }
	includes { | chuck |
		^this.findParentOf (chuck).notNil;
	}

	findParentOf { | Chuck |
		var found;
		tree do: { | l |
			if (l === chuck) {
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

	storePrevious { previous = this.chucks }

	chucks {
		^tree.collect (_.chucks).flat;
	}

	*fromString { | string |
		string = string.replace (".", "', '");
		string = string
		.replace("(", "', Ser('")
		.replace("[", "', Par('")
		.replace(")", "'), '")
		.replace("]", "'), '")
		.replace(", '')", ")");
		string = format("Par('%')", string)
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
	
	push { // make this instance the root, and initialize it
		Library.put(MiniSteno, Server.default, \root, this);
		this.initTree;
	}

	initTree {
		var nullGroup, current;
		nullGroup = GroupLink.nullGroup;
		this.storePrevious;
		this.setBussesAndGroups(ArBusLink.nullBus, ArBusLink.nullBus, GroupLink.default);
		current = this.chucks;
		previous do: { | p |
			if (current.includes (p).not) { p.setTarget(nullGroup) }
		};
		"================================================================".postln;
		this.pp;
		"================================================================".postln;	
	}

	*root { | server | ^Library.at(MiniSteno, server.asTarget.server, \root) }

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

	addTree {

		
	}
	insertAfter { | existingChuck, newChuck |

	}

	addSiblingAfter {  | existingChuck, newChuck |

	}

	addSibling { | newChuck oldChuck |

	}

	doIfFound { | element, foundFunc, missingFunc |
		var container;
		container = this.findPare
ntOf(element);
		if (container.isNil) {
			missingFunc.(element, this);
		}{
			foundFunc.(container, element, this);
		}
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
		
		^Chuck (this)
	}
}//:
/* Tue, Jun 16 2015, 03:08 EEST

Inspired by Steno of Julian Rohrhuber. 
https://github.com/telephon/Steno

*/

MiniSteno {
	var <tree; // child branches contained in me: chucks, other MiniStenos
	var <parent; // MiniSteno containing me.  Needed for remove.
	classvar <previous; // chucks traced during build

	named {  | server | ^Library.at (MiniSteno, server.asTarget.server ) }
	*includes { | chuck | ^this.root includes: chuck }
	includes { | chuck |
		^this.findParentOf (chuck).notNil;
	}

	findParentOf { | Chuck |
		var found;
		tree do: { | l |
			if (l === chuck) {
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

	storePrevious { previous = this.chucks }

	chucks {
		^tree.collect (_.chucks).flat;
	}

	*fromString { | string |
		string = string.replace (".", "', '");
		string = string
		.replace("(", "', Ser('")
		.replace("[", "', Par('")
		.replace(")", "'), '")
		.replace("]", "'), '")
		.replace(", '')", ")");
		string = format("Par('%')", string)
		.replace (", ''", "")
		.replace ("'', ", "");
		^string.postln.interpret;
	}

	*new { | ... specs |
		^this.newCopyArgs(specs collect: _.asSteno);
 	}

	asSteno {}
	
	push { // make this instance the root, and initialize it
		Library.put(MiniSteno, Server.default, \root, this);
		this.initRootTree;
	}

	initRootTree {
		var nullGroup, current;
		nullGroup = GroupLink.nullGroup;
		this.storePrevious;
		this.setBussesAndGroups(ArBusLink.nullBus, ArBusLink.nullBus, GroupLink.default);
		current = this.chucks;
		previous do: { | p |
			if (current.includes (p).not) { p.setTarget(nullGroup) }
		};
		"================================================================".postln;
		this.pp;
		"================================================================".postln;	
	}

	*root { | server | ^Library.at(MiniSteno, server.asTarget.server, \root) }

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

	*remove { | chuck |
		/* rearrange tree by removing chuck from it, if it exists. */
		var container;
		container = this.root findParentOf: chuck;
		container !? {
			
		}
	}

	doIfFound { | element, foundFunc, missingFunc |
		var container;
		container = this.findPare
ntOf(element);
		if (container.isNil) {
			missingFunc.(element, this);
		}{
			foundFunc.(container, element, this);
		}
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
		
		^Chuck (this)
	}
}