/*
+ Ser {
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
				busses = busses add: if (i == end) { // branch.getOutBus???
					outBus
				}{
					nextBus = ArBusLink() // branch.getOutBus
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
*/