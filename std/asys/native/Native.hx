package asys.native;

import haxe.IJobExecutor;
import asys.native.filesystem.FileSystem;
import asys.native.filesystem.IFileSystem;

/**
	Allows to run all IO operations through the same instance of `haxe.IJobExecutor`
**/
class Native implements INative {
	@:allow(asys.native)
	static var defaultExecutor:IJobExecutor =
		#if php
			new php.DefaultJobExecutor()
		#elseif java
			new java.DefaultJobExecutor(java.lang.Runtime.getRuntime().availableProcessors() + 1)
		#elseif eval
			new eval.DefaultJobExecutor(8)
		#else
			#error 'Not implemented for this target'
		#end;

	/** Access `asys.native.filesystem.FileSystem` API **/
	public var filesystem(get,never):IFileSystem;
	var _filesystem:Null<IFileSystem>;
	function get_filesystem():IFileSystem {
		switch _filesystem {
			case null:
				var fs = FileSystem.create(jobs);
				_filesystem = fs;
				return fs;
			case fs:
				return fs;
		}
	}

	final jobs:IJobExecutor;

	/**
		Returns an object which allows to run all IO operations through the
		given job `executor`.

		Default executor implementation depends on a target platform.
	**/
	static function create(executor:IJobExecutor = null):INative {
		return new Native(executor == null ? defaultExecutor : executor);
	}

	function new(executor:IJobExecutor) {
		jobs = executor;
	}
}