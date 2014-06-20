/*
Save and load list of buffers from the Library.

IZ Sun, Mar 16 2014, 17:45 EET
*/

BufferList {

	classvar <>autoload = false;
	classvar <all;

	var <server;
	var <namesPaths;

	*initClass {
		all = IdentityDictionary();
		StartUp add: {
			if (autoload) { this.loadFolder };
		}
	}

	*new { | server |
		var instance;
		server = server ?? { SynthTree.server };
		instance = all[server];
		if (instance.isNil) {
			instance = this.newCopyArgs(server).init;
			all[server] = instance;
		};
		^instance;
	}

	init {
		var bufferDict, buffer;
		bufferDict = Library.at(server);
		namesPaths = bufferDict.keys.asArray.select({ | key |
			bufferDict[key].path.notNil;
		}).collect({ | key |
			buffer = bufferDict[key];
			[key, buffer.path]
		});
		}

	save {
		namesPaths.writeArchive(this.defaultPath);
	}

	defaultPath { ^this.class.defaultPath }

	*defaultPath {
		^Platform.userAppSupportDir +/+ "BufferList.sctxar";
	}

	*loadDialog {
		{ | path | this.loadFolder(path) }.doPath;
	}

	*loadFolder { | path |
		var pathname, extension, server;
		//		[this, thisMethod.name].postln;
		server = Server.default;
		path ?? { path = Platform.userAppSupportDir +/+ "sounds/*" };
		//	path.pathMatch.postln;
		path.pathMatch do: { | filePath |
			pathname = PathName(filePath);
			extension = pathname.extension.asSymbol;
			if ([\aiff, \aif, \wav] includes: extension) {
				postf("Pre-loading: %\n", filePath);
				Library.put(server, pathname.fileNameWithoutExtension.asSymbol,
					BufferDummy(filePath);
				);
			}
		};
		if (server.serverRunning) {
			BufferFunc.initBuffers(server);
			{
				"=================== LOADED BUFFERS ARE: =================".postln;
				Library.at(server).keys.asArray.sort do: _.postln;
				"=================== END OF LOADED BUFFER LIST =================".postln;
			} defer: 1;
		}
	}

	*showList { | key, server |
		^this.new(server).showList(key);
	}

	showList { | key |
		var buffers, keys;
		buffers = Library.at(server);
		keys = buffers.keys.asArray.select({ | b | buffers[b].path.notNil }).sort;
		Windows.for(this, key ? \list, { | window |
			var list;
			window.view.layout = VLayout(
				list = ListView().items_(keys);
			);
			window.addNotifier(this, \buffers, {
				{ list.items = this.nameList; }.defer;
			});
			list.action = { | me | window.changed(\buffer, me.item); };
			list.keyDownAction = { | view, char, modifiers, unicode, keycode, key |
				switch (char,
					13.asAscii, { // return key
						this.toggleBuffer(view.item, modifiers != 0);
					},
					8.asAscii, { // backspace key
						this.free(view.item);
					},
					Char.space, { this.chuckBuffer(view.item, modifiers != 0) },
					$n, { this.newStChuckBuffer(view.item, modifiers != 0) },
					$x, { this.setCurrentStBuffer(view.item) },
					$f, { SynthTree.faders; },
					$l, { this.loadBufferDialog; },
					$s, { this.saveListDialog; },
					$o, { this.openListDialog; },
					{ view.defaultKeyDownAction(
						char, modifiers, unicode, keycode, key)
					}
				)
			};
		});
	}

	newStChuckBuffer { | bufName, loop = true |
		//		BufferList.getBuffer(bufName).postln;
		
		`bufName
		+> format("%%", bufName, UniqueID.next - 1001).asSymbol.asSynthTree
		.set(\amp, 1)
		.set(\loop, if (loop) { 1 } { 0 });
	}

 	chuckBuffer { | bufName, loop = true |
		var synthTree;
		//		BufferList.getBuffer(bufName).postln;
				synthTree = ~st;
		if (synthTree.isNil) {
			synthTree = format("buf%", UniqueID.next - 1001).asSymbol.asSynthTree;
		};
		`bufName +> synthTree
		.set(\amp, 1)
		.set(\loop, if (loop) { 1 } { 0 });
		
	}

	setCurrentStBuffer { | bufName |
		~st.buf(bufName);
	}

	toggleBuffer { | bufName, loop = true |
		if (bufName.asSynthTree.isPlaying) {
			bufName.asSynthTree.stop;
		}{
			`bufName +> bufName
			.set(\amp, 1)
			.set(\loop, if (loop) { 1 } { 0 });
		}
	}

	saveListDialog {
		Dialog.savePanel( { | path |
			Library.at(server).asArray.collect(_.path).select(_.notNil)
			.writeArchive(path);
		} );
	}

	openListDialog {
		var newPaths, alreadyLoadedPaths;
		alreadyLoadedPaths = Library.at(server).asArray collect: _.path;
		Dialog.openPanel( { | path |
			newPaths = Object.readArchive(path);
			newPaths do: { | newPath |
				if (alreadyLoadedPaths.detect({ | oldPath | oldPath == newPath }).isNil) {
					this.loadBuffer(newPath);
				};
			};
		});
	}

	free { | bufferName |
		var theBuffer;
		theBuffer = Library.at(server, bufferName);
		if (theBuffer.notNil) {
			theBuffer.free;
			Library.global.removeEmptyAt(server, bufferName);
			this.changed(\buffers, theBuffer);
		};
	}

	*selectPlay { | argServer |
		/* Select a buffer from the buffer list on server, using ido menu in Emacs,
			and play it in a SynthTree with the same name. */
		TinyEmacs.selectEval(
			this.nameList(argServer),
			"{ 'buf'.playBuf } => '%s'.buf",
			"Play buffer in SynthTree (default: %s): "
		)
	}

	*selectFree { | argServer |
		/* Select a buffer from the buffer list on server, using ido menu in Emacs,
			and free it.  Also free SynthTree of same name. */
		TinyEmacs.selectEval(
			this.nameList(argServer),
			"'%s'.freeBuffer;",
			"Free buffer (default: %s): "
		)
	}

	*nameList { | server |
		^this.new(server).nameList;
	}

	nameList {
		var buffers;
		buffers = Library.at(server);
		if (buffers.isNil) { ^[] };
		^buffers.keys.asArray.select({ | b | buffers[b].path.notNil }).sort;
	}

	*loadBufferDialog { | server |
		this.new(server ?? { SynthTree.server }).loadBufferDialog;
	}

	loadBufferDialog {
		var newBuffer;
		"Opening buffer load dialog".postln;
		Dialog.openPanel({ | path | this loadBuffer: path })
	}

	*loadBuffer { | path server |
		^this.new(server ?? { SynthTree.server }).loadBuffer(path);
	}

	loadBuffer { | path |
		var newBuffer, bufName;
		bufName = PathName(path).fileNameWithoutExtension.asSymbol;
		if (Library.at(server, bufName).isNil) {
			newBuffer = Buffer.read(server, path, action: {
				Library.put(server, bufName, newBuffer);
				postf("Loaded: %\n", newBuffer);
				this.changed(\buffers, server);
			})
		};
		^newBuffer;
	}

	asString {
		^format("BufferList(%)", server);
	}

	*getBuffer { | name, server |
		^Library.global.at(server ?? { SynthTree.server }, name);
	}
}

BufferDummy {
	var <path;
	*new { | path | ^this.newCopyArgs(path) }
	server { ^SynthTree.server }
}