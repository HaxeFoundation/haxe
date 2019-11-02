package asys;

import haxe.io.FilePath;

/**
	Events emitted by the `changeSignal` of a `sys.FileWatcher`. Any file change
	consists of a name change (`Rename`), a content change (`Change`), or both
	(`RenameChange`).
**/
enum FileWatcherEvent {
	Rename(newPath:FilePath);
	Change(path:FilePath);
	RenameChange(path:FilePath);
}
