package python.lib.io;

import python.lib.Exceptions.IOError;

@:pythonImport("io", "BlockingIOError")
class BlockingIOError extends IOError {

	var characters_written:Int;

}