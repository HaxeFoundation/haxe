package asys;

import haxe.Error;
import haxe.NoData;
import haxe.async.*;
import haxe.io.FilePath;

typedef FileWatcherNative =
	#if doc_gen
	{function ref():Void; function unref():Void;};
	#elseif eval
	eval.uv.FileWatcher;
	#elseif hl
	hl.uv.FileWatcher;
	#elseif neko
	neko.uv.FileWatcher;
	#else
	#error "file watcher not supported on this platform"
	#end

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
	public final errorSignal:Signal<Error> = new ArraySignal();

	private var native:FileWatcherNative;

	private function new(filename:FilePath, recursive:Bool) {
		#if !doc_gen
		native = new FileWatcherNative(filename, recursive, (err, event) -> {
			if (err != null)
				return errorSignal.emit(err);
			changeSignal.emit(event);
		});
		#end
	}

	/**
		Closes `this` watcher. This operation is asynchronous and will emit the
		`closeSignal` once done. If `listener` is given, it will be added to the
		`closeSignal`.
	**/
	public function close(?listener:Listener<NoData>):Void {
		if (listener != null)
			closeSignal.once(listener);
		#if doc_gen
		var err:haxe.Error = null;
		({
		#else
		native.close((err, _) -> {
		#end
			if (err != null)
				errorSignal.emit(err);
			closeSignal.emit(new NoData());
		});
	}

	public function ref():Void {
		native.ref();
	}

	public function unref():Void {
		native.unref();
	}
}
