package cs.system.io;

@:native('System.IO.FileAttributes') extern enum FileAttributes 
{
	ReadOnly;
	Hidden;
	System;
	Directory;
	Archive;
	Device;
	Normal;
	Temporary;
	SparseFile;
	ReparsePoint;
	Compressed;
	Offline;
	NotContentIndexed;
	Encrypted;
}