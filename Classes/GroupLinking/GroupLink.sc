GroupLink {
	classvar default, <nullGroup;
	var <>group, /* <>members, */ <readerGroup, <writerGroup;

	*initClass {
		ServerBootCheck addStartup: { this.init; };
		CmdPeriod add: { this.remakeGroups; };
	}
	*init {
		Chuck.parentArgs[\target] = this.default;
		nullGroup = GroupLink();
		this.remakeGroups;
	}

	*remakeGroups {
		{
			0.1.wait;  // Must defer after CmdPeriod, else groups are still destroyed
			default.group = Group();
			nullGroup.group = Group.before(default.group.group);
			postf("Default group is now %\n", default.group);
			default.remakeReaderGroups;
			default.remakeWriterGroups;
			Server.default.queryAllNodes;
			"GroupLink inited - can start using tiny-sc".postln;
		}.fork;
	}

	*new { | group | ^this.newCopyArgs(group) }

	/* // not used
	*firstGroup { // get Group which is at the head of all other GroupLinks
		^this.default.writerGroups.last;
	}
	*/

	remakeReaderGroups {
		readerGroup !? {
			readerGroup.group = Group.after(group);
			readerGroup.remakeReaderGroups;
		}
	}

	remakeWriterGroups {
		writerGroup !? {
			writerGroup.group = Group.before(group);
			writerGroup.remakeWriterGroups;
		}
	}

	*default {
		default ?? { default = GroupLink() };
		^default;
	}

	readerGroups {
		if (readerGroup.isNil) {
			^[]
		} {
			^readerGroup.readerGroups add: readerGroup
		}
	}
	
	writerGroups {
		if (writerGroup.isNil) {
			^[]
		} {
			^writerGroup.writerGroups add: writerGroup
		}
	}
	
	asTarget { ^group }

	asControlInput { ^group.nodeID }

	getReaderGroup {
		readerGroup ?? { readerGroup = GroupLink(Group.after(group)) };
		^readerGroup;
	}

	/* // not used
	getWriterGroup {
		writerGroup ?? { writerGroup = GroupLink(Group.before(group)) };
		^writerGroup;
	}
	*/

	server { ^group.server }

	printOn { arg stream;
		stream << "GroupLink(" << group.nodeID << ")";
	}
	storeOn { arg stream;
		stream << "GroupLink(" << group.nodeID << ")";
	}

}