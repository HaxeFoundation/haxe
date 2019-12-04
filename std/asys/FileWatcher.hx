package asys;

import asys.uv.UVError;
import haxe.NoData;
import haxe.async.*;
import haxe.io.FilePath;

/**
	File watchers can be obtained with the `asys.FileSystem.watch` method.
	Instances of this class will emit signals whenever any file in their watched
	path is modified.
**/
class FileWatcher {
	/**
		Emitted when a watched file is modified.
	**/
	public final changeSignal:Signal<FileWatcherEvent> = new ArraySignal();

	/**
		Emitted when `this` watcher is fully closed. No further signals will be
		emitted.
	**/
	public final closeSignal:Signal<NoData> = new ArraySignal();

	/**
		Emitted when an error occurs.
	**/
	public final errorSignal:Signal<UVError> = new ArraySignal();

	private var native:FileWatcherNative;

	private function new(filename:FilePath, recursive:Bool) {}

	/**
		Closes `this` watcher. This operation is asynchronous and will emit the
		`closeSignal` once done. If `listener` is given, it will be added to the
		`closeSignal`.
	**/
	public function close(?listener:Listener<NoData>):Void {
		if (listener != null)
			closeSignal.once(listener);
	}

	extern public function ref():Void;

	extern public function unref():Void;
}
