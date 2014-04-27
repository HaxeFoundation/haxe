
package python.io;

import python.io.IInput;
import sys.io.FileSeek;



interface IFileInput extends IInput {
	public function seek( p : Int, pos : FileSeek ) : Void;

	public function tell() : Int;
	public function eof() : Bool;
}