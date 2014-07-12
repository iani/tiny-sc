/*
Configure folders in Platform.userAppSupportDir/Extensions

Wed, Jul  9 2014, 22:25 EEST

- Store a list of named configurations
- Each configuration is a list of folders
- To install a configuration, move all folders in its list 
  from Extensions-disabled to extensions
- To un-install a configuration, do the reverse.

Later: Install a new folder from github: 
- Clone the repository from github to a chosen folder.
- Create an alias of the new cloned folder into Extensions-disabled.

*/

LibConfig : List {

	*new { | name |
		^this.notifyNew(Registry(this, \libs, name, { super.new; }));
	}

	*notifyNew { | newInstance |
		this.changed(\list, this.sortedNames);
		^newInstance;
	}

	*save {
		Library.at(this, \libs) writeArchive: this.savePath;
	}

	*savePath { ^Platform.userAppSupportDir +/+ "LibConfig.sctxar" }

	*load {
		Library.put(this, \libs, Object readArchive: this.savePath);
		this.changed(\list, this.sortedNames)
	}

	*gui {
		var libsList, libNameField, foldersLabel, foldersList;
		Window.for(this, \gui, { | w |
			w.view.layout = VLayout(
				StaticText().string_("Libs:"),
				libsList = ListView(),
				HLayout(
					Button().states_([["New Lib:"]])
					.action_({ this.addLib(libNameField.string) }),
					libNameField = TextField()
				),
				foldersLabel = StaticText().string_("Folders for -"),
				foldersList = ListView(),
				Button().states_([["Add Folder"]]).action_({ this.addFolder(libsList.item) })
			);
			libsList addInterface: this;
			this.addNotifier(libsList, \items, {
				//				this.findInstalledConfig;
				/*
				libsList.items do: { | i |
					this.getConfig(i).postln;
					this.getConfig(i).installed.postln;
				}
				*/
			});
			this.addNotifier(libsList, \return, {
				this.getConfig(libsList.item).install
			});
			this.addNotifier(libsList, \delete, {
				this.getConfig(libsList.item).uninstall
			});
			foldersLabel.addNotifier(libsList, \items, {
				foldersLabel.string = format("Folders for %", libsList.item ? "-");
			});
			foldersList.addNotifier(libsList, \items, { | notification |
				foldersList.items = this.getConfigFolders(notification.notifier.item);
			});
			this.changed(\list, this.sortedNames);
		})
	}

	*getConfigFolders { | item | ^this.getConfig(item).array; }

	*getConfig { | item | ^Library.at(this, \libs, item.asSymbol) ?? { NullConfig } }

	*sortedNames {
		var names;
		names = Library.at(this, \libs);
		if (names.isNil) { ^[] };
		^names.keys.asArray.sort;
	}

	*installedConfig {
		var folders, installedConfig;
		folders  = PathName(Platform.userAppSupportDir +/+ "Extensions").folders 
		collect: { | p | p.folderName.asSymbol };
		folders remove: \quarks;
		this.sortedNames do: { | cfig |
			cfig = this.getConfig(cfig);
			if (cfig == folders) { installedConfig = cfig }
		};
		^installedConfig;
	}

	install {
		this do: {

		}
	}

	uninstall {
		this do: {

		}
	}
}

NullConfig {
	*array { ^[] }
	*install { }
	*uninstall { }
}