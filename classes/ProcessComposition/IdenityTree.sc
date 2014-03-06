/*

Like a MultilevelIdentityDictionary, except that its leaves (last level nodes) are always instances of the same class as the branches.  As a result, one can add branches of different depth to one branch.  Subclasses of IdentityTree can introduce instance variables to store data. 

IZ Thu, Mar  6 2014, 20:11 EET

b = IdentityDictionary();

b.put(\1, 1);

a = IdentityTree();
a.add([\a, \b])
a.add([\a, \b, \c])
a.add([\a, \d])

a.keys;
a[\a].keys;
a.get([\a]).keys;
a.get([\a, \b]);
a.get([\a, \b, \c]);
a.get([\a, \d]);
a.get([\a, \asdf]);

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
