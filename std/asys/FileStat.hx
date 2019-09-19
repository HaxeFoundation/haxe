package asys;

typedef FileStatData = {
    // sys.FileStat compatibility
    var atime:Date;
    var ctime:Date;
    var dev:Int;
    var gid:Int;
    var ino:Int;
    var mode:Int;
    var mtime:Date;
    var nlink:Int;
    var rdev:Int;
    var size:Int;
    var uid:Int;
    
    // node compatibility
    var blksize:Int;
    var blocks:Int;
    var atimeMs:Float;
    var ctimeMs:Float;
    var mtimeMs:Float;
    var birthtime:Date;
    var birthtimeMs:Float;
  };

@:forward
abstract FileStat(FileStatData) from FileStatData {
  public function isBlockDevice():Bool return false;
  public function isCharacterDevice():Bool return false;
  public function isDirectory():Bool return false;
  public function isFIFO():Bool return false;
  public function isFile():Bool return false;
  public function isSocket():Bool return false;
  public function isSymbolicLink():Bool return false;
}
