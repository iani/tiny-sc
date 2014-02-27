/* iz Sun 14 October 2012  7:30 PM EEST

Create unique name for adding a branch to a MultiLevelIdentityDictionary

*/


+ MultiLevelIdentityDictionary {
	makeUniqueName { | path, name, index |
		var newName;
		newName = format("%%", name, index ? "").asSymbol;
		if (this.atPath(path.asArray.copy add: newName).isNil) {
			^newName;
		}{
			if (index.isNil) { index = 0 } { index = index + 1 };
			^this.makeUniqueName(path, name, index);
		}
	}
}