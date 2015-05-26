
/* 
Access an object in Library under a path. 
If no object exists under that path, then create it using a function passed as argument. 
*/

Registry(\myWindow, { Window("My window!").front });

// Shortcut:

{ Window("My second window!").front }.at(\myWindow2);

