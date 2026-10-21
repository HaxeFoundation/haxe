
enum SkipReason {
	DependencyDirty(path:String);
	Tainted(cause:String);
	FileChanged(file:String);
	Shadowed(file:String);
	LibraryChanged;
	ReadChanged(node:String);
}