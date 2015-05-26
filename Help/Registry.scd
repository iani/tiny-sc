
/* 
Access an object in Library under a path. 
If no object exists under that path, then create it using a function passed as argument. 
*/

Registry(\myWindow, { Window("My WINDOW!").front.onClose() });

// Shortcut:

{ Window("My WINDOW!").front }.at(\myWindow);