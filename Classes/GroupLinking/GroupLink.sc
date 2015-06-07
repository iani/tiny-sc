GroupLink {
	classvar default;
	var <>group, <>members, <readerGroup, <writerGroup;

	*initClass {
		ServerBootCheck add: { this.init; };
		CmdPeriod add: { this.remakeGroups; };
	}
	*init {
		Chuck.parentArgs[\target] = this.default;
		this.remakeGroups;
	}

	*remakeGroups {
		{
			0.1.wait;  // Must defer after CmdPeriod, else groups are still destroyed
			default.group = Group();
			postf("Default group is now %\n", default.group);
			default.remakeReaderGroups;
			default.remakeWriterGroups;
			Server.default.queryAllNodes;
			"GroupLink inited - can start using tiny-sc".postln;
		}.fork;
	}

	*new { | group |
		^this.newCopyArgs(group, Set())
	}

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
	asTarget { ^group }

	asControlInput { ^group.nodeID }

	getReaderGroup {
		readerGroup ?? { readerGroup = GroupLink(Group.after(group)) };
		^readerGroup;
	}

	getWriterGroup {
		writerGroup ?? { writerGroup = GroupLink(Group.before(group)) };
		^writerGroup;
	}

	printOn { arg stream;
		stream << "GroupLink(" << group.nodeID << ")";
	}
	storeOn { arg stream;
		stream << "GroupLink(" << group.nodeID << ")";
	}

}