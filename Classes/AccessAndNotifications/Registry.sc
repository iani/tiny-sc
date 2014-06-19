/* 
A generalized version of NameSpace that accepts any number of keys.

Should replace NameSpace.

Uses Library: Branches under one first key must have the same number of keys.

Thu, Jun 12 2014, 19:53 EEST
*/

Registry {

	*new { | ... pathAndFunc |
		var path, makeFunc, instance;
		makeFunc = pathAndFunc.last;
		path = pathAndFunc[0..pathAndFunc.size-2];
		instance = Library.global.atPath(path);
		if (instance.isNil) {
			instance = makeFunc.value;
			Library.global.putAtPath(path, instance);
			instance.onObjectClosed(this, {
				this.remove(*path)
			});
		};
		^instance;
	}

	*doIfFound { | ... pathAndFunc |
		var path, action, instance;
		action = pathAndFunc.last;
		path = pathAndFunc[0..pathAndFunc.size-2];
		instance = Library.global.atPath(path);
		instance !? { action.(instance) };
	}

	*remove { | ... path |
		Library.global.removeEmptyAt(*path);
	}
}