/*

NOT TESTED! / NOT COMPLETED

Version of IdentityTree in which the last element in a path is no longer a branch, but a leaf.
Fri, Jun 13 2014, 02:46 EEST

Like a MultilevelIdentityDictionary, except that its leaves (last level nodes) are always instances of the same class as the branches.  As a result, one can add branches of different depth to one branch.  Subclasses of IdentityTree can introduce instance variables to store data. 

IZ Thu, Mar  6 2014, 20:11 EET

a = LeafyIdentityTree();

a.set([\a, \b])
a.set([\a, \b, \c, \d])
a.set([\a, \d])


a.keys;
a[\a].keys;
a.get([\a]).keys;
a.get([\a, \b]);
a.get([\a, \b, \c]);
a.get([\a, \d]);
a.get([\a, \asdf]);

*/

LeafyIdentityTree : IdentityDictionary {

	get { | path |
		var result;
		if (path.size == 1) { ^this[path[0]] };
		result = this[path[0]];
		if (result.isNil) {
			^nil
		}{
			^result.get(path[1..]);
		}
	}

	set { | path |
		var key, next;
		[this, thisMethod.name, 0, path].postln;
		if (path.size < 2) { ^this };
		[this, thisMethod.name, 1, path].postln;
		if (path.size == 2) {
					[this, thisMethod.name, 2, path].postln;
			this[path[0]] = path[1];
		}{
			[this, thisMethod.name, 3, path].postln;
			key = path[0];
			next = this[key];
			[this, thisMethod.name, 4, path, key, next].postln;
			if (next.isNil) {
				next = this.class.new;
				this[key] = next;
				[this, thisMethod.name, 5, path].postln;
			};
				[this, thisMethod.name, 6, path].postln;
			next.set(path[1..]);
				[this, thisMethod.name, 7, path, next, this].postln;
		}
	}
}
