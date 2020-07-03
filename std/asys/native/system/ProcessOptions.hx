package asys.native.system;

import asys.native.system.StdioConfig;
import asys.native.system.SystemUser;
import asys.native.filesystem.FilePath;

/**
	Options to configure new processes spawned via `asys.native.system.Process.execute`
**/
typedef ProcessOptions = {
	/**
		Command line arguments.
	**/
	var ?args:Array<String>;
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
		Setup standard IO streams for a new process.
		First three indices are always used as follows:
		- 0 - stdin
		- 1 - stdout
		- 2 - stderr
		Indices from 3 and higher can be used to setup additional IO streams.
		If the array has less than three items, then default setup will be used
		for missing items.
		If `stdio` contains less than 3 items, default behavior will be used for
		missing ones.
		If `stdio` field is not specified at all, three anonymous pipes will be
		initiated for stdin, stdout and stderr of a new process.
		@see asys.native.system.StdioConfig
	**/
	var ?stdio:Array<StdioConfig>;
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