
+ Ref {
	=> { | st | ^st.asSynthTree chuck: value.asSynthTemplate }
}

+ Symbol {
	asSynthTemplate {
		^Library.at(SynthTemplate, '---ALL---', this);
	}
}
