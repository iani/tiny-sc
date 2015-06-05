GroupLink {
	classvar default;
	var <members, <>group, <readerGroup, <writerGroup;
	// name?

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
			"GroupLink inited - can start using tiny-sc v2".postln;
		}.fork;
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

	asTarget { ^group }
}