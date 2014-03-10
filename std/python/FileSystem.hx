
package python;

import python.lib.Os;
import python.lib.os.Path;

class FileSystem {

	public static function exists( path : String ) : Bool {
		return Path.exists(path);
	}

	public static function stat( path : String ) : sys.FileStat {
		var s = Os.stat(path);
		return {
	        gid : s.st_gid,
	        uid : s.st_uid,
	        atime : Date.fromTime(s.st_atime),
	        mtime : Date.fromTime(s.st_mtime),
	        ctime : Date.fromTime(s.st_ctime),
	        size : s.st_size,
	        dev : s.st_dev,
	        ino : s.st_ino,
	        nlink : s.st_nlink,
	        rdev : s.st_rdev,
	        mode : s.st_mode
		}
	}

	public static function rename( path : String, newPath : String ) : Void {
		Os.rename(path, newPath);
	}

	public static function fullPath( relPath : String ) : String {
		return Path.abspath(relPath);
	}

	public static function isDirectory( path : String ) : Bool
	{
		return Path.isdir(path);
	}
	public static function createDirectory( path : String ) : Void
	{
		Os.mkdir(path);
	}
	public static function deleteFile( path : String ) : Void
	{
		Os.remove(path);
	}
	public static function deleteDirectory( path : String ) : Void
	{
		Os.rmdir(path);
	}
	public static function readDirectory( path : String ) : Array<String>
	{
		return Os.listdir(path);
	}

}