(
var scroll = ScrollView(bounds:Rect(0,0,300,300).center_(Window.availableBounds.center));
var canvas = View();
var layout;

layout = VLayout();

canvas.layout = layout;
10.do { canvas.layout.add( makeEntry.() ) };
canvas.layout.add(nil); // stretch remaining empty space

scroll.canvas = canvas;
scroll.front;
)

var i = 0;

var makeEntry = {
	var view = View().background_(Color.rand).layout_(
		HLayout(
			TextField().string_( ("This is entry number " + i.asString) ),
			Button().states_([["Delete"]]).action_({view.remove; i = i - 1;})
			)
		);
	i = i + 1;
	view;
};

layout.add ( View().background_(Color.black).layout_(
	HLayout(
		Button().states_([["Add"]]).action_({ layout.insert(makeEntry.(), i) }),
		nil // stretch remaining empty space
		)
));
