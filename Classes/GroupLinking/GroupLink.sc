GroupLink {
	classvar defaultGroup;
	var <group, <readerGroup, <writerGroup, <members;
	// name? 
	*new { | group members | ^this.newCopyArgs(group, members) }

	defaultGroup {
		^this.class.defaultGroup;
	}
	*defaultGroup {
		defaultGroup ?? { defaultGroup = Group(Server.default) };
		^defaultGroup;
	}
}