/*
Create a general interface to facilitate interacting with a list via ListView

Thu, Jul 10 2014, 08:12 EEST
*/

+ QListView {

	addInterface { | model |
		this.addNotifier(model, \list, { | items |
			this.items = items;
			this.changed(\items);
		});
		this.action = { this.changed(\items) };
		this.keyDownAction = { | me, key |
			switch (key.ascii,
				13, { this.changed(\return) },
				8, { this.changed(\delete) }
			);
		}
	}
}