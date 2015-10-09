GroupLink {
	classvar <default, <nullGroup;
	var <>group, /* <>members, */ <readerGroup, <writerGroup;
	var <>level = 0;

	*initClass {
		ServerBootCheck addStartup: { this.init; };
		CmdPeriod add: { this.remakeGroups; };
	}
	*init {
		SynthPlayer.parentArgs[\target] = this.default;
		nullGroup = GroupLink();
		default = GroupLink();
		this.remakeGroups;
	}

	*remakeGroups {
		{
			0.1.wait;  // Must defer after CmdPeriod, else groups are still destroyed
			default.group = Group();
			nullGroup.group = Group.before(Server.default.defaultGroup);
			postf("Default group is now %\n", default.group);
			default.remakeReaderGroups;
			default.remakeWriterGroups;
			Server.default.queryAllNodes;
			0.1.wait;
			"GroupLink inited - can start using tiny-sc".postln;
			this.changed(\inited);
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
			readerGroup.group = Group.after(group).register;
			readerGroup.remakeReaderGroups;
		}
	}

	remakeWriterGroups {
		writerGroup !? {
			writerGroup.group = Group.before(group).register;
			writerGroup.remakeWriterGroups;
		}
	}

	/*
	*default {
		default ?? { default = GroupLink() };
		^default;
	}
	*/

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
		readerGroup ?? { readerGroup = GroupLink(Group.after(group).register) };
		readerGroup.level = level + 1;
		^readerGroup;
	}

	isAfter { | otherGroup |
		^level > otherGroup.level;
	}
	/* // not used
	getWriterGroup {
		writerGroup ?? { writerGroup = GroupLink(Group.before(group)) };
		^writerGroup;
	}
	*/

	server { ^group.server }

	printOn { arg stream;
		stream << "GroupLink *" << level << "* (" << group.nodeID << ")";
	}
	storeOn { arg stream;
		stream << "GroupLink *" << level << "* (" << group.nodeID << ")";
	}
}