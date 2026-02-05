package cs.system;

/** Specifies constants that define foreground and background colors for the console. */
@:native("System.ConsoleColor")
extern enum abstract ConsoleColor(Int) {
	var Black = 0;
	var Blue = 9;
	var Cyan = 11;
	var DarkBlue = 1;
	var DarkCyan = 3;
	var DarkGray = 8;
	var DarkGreen = 2;
	var DarkMagenta = 5;
	var DarkRed = 4;
	var DarkYellow = 6;
	var Gray = 7;
	var Green = 10;
	var Magenta = 13;
	var Red = 12;
	var White = 15;
	var Yellow = 14;
}
