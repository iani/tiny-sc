/*
Test behavior of HLayout when adding - removing widgets 

	IZ Wed, Mar  5 2014, 15:43 EET */

//:
~window = Window();

~widgets = { Knob() } ! 10;

//:
~refresh = {
	//	~window.view.children do: _.remove;
	~window.layout = HLayout(*~widgets);
};
~refresh.value;
~window.front;
//:

~widgets add: Knob();
~refresh.value;
~widgets[1].visible = 1;
//:
~widgets;
//:

~widgets remove: ~widgets.last;
~refresh.value;

~window.refresh;

~window.view.refresh;
//:
~window.layout = HLayout();
~window.views;
//:
~window.view.children;
~window.view.children do: _.remove;
