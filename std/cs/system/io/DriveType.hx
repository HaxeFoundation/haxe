package cs.system.io;

/** Defines constants for drive types, including CDRom, Fixed, Network, NoRootDirectory, Ram, Removable, and Unknown. */
@:native("System.IO.DriveType")
extern enum abstract DriveType(Int) {
	var CDRom = 5;
	var Fixed = 3;
	var Network = 4;
	var NoRootDirectory = 1;
	var Ram = 6;
	var Removable = 2;
	var Unknown = 0;
}
