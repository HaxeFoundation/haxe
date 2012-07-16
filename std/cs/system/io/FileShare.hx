package cs.system.io;

@:native('System.IO.FileShare') extern enum FileShare 
{
	None;
	Read;
	Write;
	ReadWrite;
}