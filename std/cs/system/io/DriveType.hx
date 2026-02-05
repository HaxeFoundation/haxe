package cs.system.io;

/** Defines constants for drive types, including CDRom, Fixed, Network, NoRootDirectory, Ram, Removable, and Unknown. */
@:native("System.IO.DriveType")
extern enum DriveType {
	CDRom;
	Fixed;
	Network;
	NoRootDirectory;
	Ram;
	Removable;
	Unknown;
}
