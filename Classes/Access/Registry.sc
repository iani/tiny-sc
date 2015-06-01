/* 
Access an object in Library under a path. 
If no object exists under that path, then create it using a function passed as argument. 
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
		if (instance.isNil) {
			postf("No instance found at path: %\n", path);
		} { action.(instance) };
		^instance;
	}

	*remove { | ... path |
		Library.global.removeEmptyAt(*path);
	}
}

+ Function {
	at { | ... path |
		^Registry(*(path add: this))
	}
}