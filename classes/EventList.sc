/*
UNDER DEVELOPMENT - VERY SLOWLY

Instead of holding the lists (streams) of value events and durations separately,
each event holds its own duration together with the data in one object.  This is
good for editing event chains by cutting-pasting, inserting events or event
lists to change lists or to create other lists.  May be good also for displaying
event lists as a graphic score.

IZ Mon, Mar 31 2014, 23:19 EEST

*/

EventList {

	var <event, <dur;
	var <previous, <next;
	var <task, <nodes;

	*start {}

	*stop {}

	*isPlaying {}
}