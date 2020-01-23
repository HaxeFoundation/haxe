package asyncio.system;

import asyncio.system.SystemUser;
import asyncio.filesystem.FilePath;

/**
	Options to configure new processes spawned via `asyncio.system.Process.execute`
**/
typedef ProcessOptions = {
	/**
		Working directory for a new process.
		By default current process working directory is used.
	**/
	var ?cwd:FilePath;
	/**
		Environment variables for a new process.
		By default current process environment is used.
	**/
	var ?env:Map<String,String>;
	/**
		Run new process with `user` identity.
		By default: the owner of the current process.
	**/
	var ?user:SystemUser;
	/**
		Run new process on behalf of `group`.
		By default: the group of the current process.
	**/
	var ?group:SystemGroup;
	/**
		When `true`, creates a detached process which can continue running after
		the current process exits.
		By default: `false`.
	**/
	var ?detached:Bool;
}

abstract IO(Array<>)