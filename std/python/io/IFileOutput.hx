
package python.io;

import python.io.IOutput;
import sys.io.FileSeek;

interface IFileOutput extends IOutput {

	public function seek( p : Int, pos : FileSeek ) : Void;
	public function tell() : Int;

}