package rust.run;
import rust.io.*;
@:native("run.Program") extern class Program {
	/** Returns the process id of the program */
	public function get_id():Int {}
	/** Returns an io.writer that can be used to write to stdin */
	public function input():Writer {}
	/** Returns an io.reader that can be used to read from stdout */
	public function output():Reader {}
	/** Returns an io.reader that can be used to read from stderr */
	public function error():Reader {}
	/** Closes the handle to the child processes standard input */
	public function close_input():Void {}
	/** Waits for the child process to terminate. Closes the handle to stdin if necessary. */
	public function finish():Int {}
	/** Closes open handles */
	public function destroy():Void {}
	/** Spawns a process, waits for it to exit, and returns the exit code, and contents of stdout and stderr. */
	public static function program_output(prog:String, args:Array<String>):ProgramOutput {}
	public static function readclose(fd:Int):String {}
	/** Spawns a process and waits for it to terminate */
	public static function run_program(prog:String, args:Array<String>):NativeObject<String>;
	/** Run a program, providing stdin, stdout and stderr handles */
	public static function spawn_process(prog:String, args:Array<String, env:Tuple2, dir:String, in_fd:Int, out_fd:Int, err_fd:Int):Int;
	public static function start_program(prog:String, args:Array<String):Program {}
	/** Waits for a process to exit and returns the exit code */
	public static function wait_pid(pid):Int {}
	public static function writeclose(fd:Int, s:String):Void {}
}