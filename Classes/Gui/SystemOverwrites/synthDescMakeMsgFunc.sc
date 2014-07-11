
+ SynthDesc {

	makeMsgFunc {
		var	string, comma = false;
		var	names = IdentitySet.new,
		suffix = this.hash.asHexString(8);
		// if a control name is duplicated, the msgFunc will be invalid
		// that "shouldn't" happen but it might; better to check for it
		// and throw a proper error
		controls.do({ | controlName |
			var	cname;
			if (controlName.name.asString.first.isAlpha) {
				cname = controlName.name.asSymbol;
				if(names.includes(cname)) {
					"Could not build msgFunc for SynthDesc '%': duplicate control name %"
					.format(name, cname).warn;
					comma = true;
				} {
					names.add(cname);
				};
			};
		});
		// reusing variable to know if I should continue or not
		if(comma) {
			"\nYour synthdef has been saved in the library and loaded on the server, if running.
Use of this synth in Patterns will not detect argument names automatically because of the duplicate name(s).".postln;
			msgFunc = nil;
			^this
		};
		comma = false;
		names = 0;	// now, count the args actually added to the func

		string = String.streamContents { | stream |
			stream << "#{ ";
			if (controlNames.size > 0) {
				stream << "arg " ;
			};
			controls.do { | controlName, i |
				var name1, name2;
				name1 = controlName.name.asString;
				if (name1 != "?") {
					if (name1 == "gate") {
						hasGate = true;
						if(msgFuncKeepGate) {
							if (comma) { stream << ", " } { comma = true };
							stream << name1;
							names = names + 1;
						}
					}{
						if (name1[1] == $_) { name2 = name1.drop(2) } { name2 = name1 };
						if (comma) { stream << ", " } { comma = true };
						stream << name2;
						names = names + 1;
					};
				};
			};
			if (controlNames.size > 0) {
				stream << ";\n" ;
			};
			stream << "\tvar\tx" << suffix << " = Array.new(" << (names*2) << ");\n";
			comma = false;
			controls.do {|controlName, i|
				var name1, name2;
				name1 = controlName.name.asString;
				if (name != "?") {
					if (msgFuncKeepGate or: { name1 != "gate" }) {
						if (name1[1] == $_) { name2 = name1.drop(2) } { name2 = name1 };
						stream << "\t" << name2 << " !? { x" << suffix
						<< ".add('" << name1 << "').add(" << name2 << ") };\n";
						names = names + 1;
					};
				};
			};
			stream << "\tx" << suffix << "\n}"
		};

		// do not compile the string if no argnames were added
		if (names > 0) { msgFunc = string.compile.value };
	}
}