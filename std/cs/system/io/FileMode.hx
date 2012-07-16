package cs.system.io;

@:native('System.IO.FileMode') extern enum FileMode 
{
	CreateNew;
	Create;
	Open;
	OpenOrCreate;
	Truncate;
	Append;
}