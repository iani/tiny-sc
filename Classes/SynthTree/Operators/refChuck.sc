
+ Ref {
	=> { | st |
		[this, thisMethod.name, value, value.asSynthTemplate].postln;
 ^st.asSynthTree chuck: value.asSynthTemplate }
}

+ Symbol {
	asSynthTemplate {
		^Library.at(SynthTemplate, '---ALL---', this);
	}
}
