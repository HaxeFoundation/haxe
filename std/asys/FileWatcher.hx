package asys;

import haxe.signals.Signal;
import haxe.signals.ArraySignal;
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
	public var changeSignal(get,never):Signal<FileWatcherEvent>;
	final _changeSignal = new ArraySignal<FileWatcherEvent>();
	inline function get_changeSignal():Signal<FileWatcherEvent>
		return _changeSignal;

	/**
		Emitted when `this` watcher is fully closed. No further signals will be
		emitted.
	**/
	public var closeSignal(get,never):Signal<NoData>;
	final _closeSignal = new ArraySignal<NoData>();
	inline function get_closeSignal():Signal<NoData>
		return _closeSignal;

	/**
		Emitted when an error occurs.
	**/
	public var errorSignal(get,never):Signal<UVError>;
	final _errorSignal= new ArraySignal<UVError>();
	inline function get_errorSignal():Signal<UVError>
		return _errorSignal;

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
