/*

Like a MultilevelIdentityDictionary, except that its leaves (last level nodes) are always instances of the same class as the branches.  As a result, one can add branches of different depth to one branch.  Subclasses of IdentityTree can introduce instance variables to store data. 

IZ Thu, Mar  6 2014, 20:11 EET

*/

IdentityTree : IdentityDictionary {

	get { | path |
		var result;
		if (path.size < 1) { ^this };
		result = this[path[0]];
		if (result.isNil) {
			^nil
		}{
			^result.get(path[1..]);
		}
	}

	set { | path |
		var key, next;
		if (path.size < 1) { ^this };
		key = path[0];
		next = this[key];
		if (next.isNil) {
			next = this.class.new;
			this[key] = next;
		};
		next.set(path[1..]);
	}
}
