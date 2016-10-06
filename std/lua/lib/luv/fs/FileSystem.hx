package lua.lib.luv.fs;
import lua.lib.luv.fs.Open;

@:luaRequire("luv")
extern class FileSystem {
  @:native("fs_close")
  @:overload(function(file : FileDescriptor, cb : String->Bool->Void) : Request {})
  static function close(file : FileDescriptor) : Bool;

  @:native("fs_open")
  @:overload(function(path : String, flags : Open, mode : Int, ?cb : String->FileDescriptor->Void) : Request {})
  static function open(path : String, flags : Open, mode : Int) : FileDescriptor;

  @:native("fs_read")
  @:overload(function(file : FileDescriptor, len : Int, offset : Int, ?cb : String->String->Void) : Request {} )
  static function read(file : FileDescriptor, len : Int, offset : Int) : String;

  @:native("fs_unlink")
  @:overload(function(file : FileDescriptor, ?cb : String->String->Void) : Request {} )
  static function unlink(file : FileDescriptor, content : String) : String;

  @:native("fs_write")
  @:overload(function(file : FileDescriptor, content : String, offset : Int, ?cb : String->Bool->Void) : Int {})
  static function write(file : FileDescriptor, content : String, offset : Int) : Bool;

  @:native("fs_mkdir")
  @:overload(function(path : String, mode : Int, cb : String->Bool->Void) : Request {})
  static function mkdir(path : String, mode :Int) : Bool;

  @:native("fs_mkdtemp")
  @:overload(function(data : String, cb : String->Bool->Void) : Request {})
  static function mkdtemp(data : String) : Bool;

  @:native("fs_rmdir")
  @:overload(function(path : String, cb : String->Bool->Void) : Request {})
  static function rmdir(path : String) : LuvStatus<Int>;

  @:native("fs_scandir")
  @:overload(function(path : String, cb : String->Bool->Void) : Request {})
  static function scandir(path : String) : ScanDirMarker;

  @:native("fs_scandir_next")
  static function scandir_next(scandir : ScanDirMarker) : ScandirNext;

  @:native("fs_stat")
  @:overload(function(path : String, cb : String->Stat->Void) : Request {})
  static function stat(path : String) : Stat;

  @:native("fs_fstat")
  @:overload(function(descriptor : FileDescriptor, cb : String->Stat->Void) : Request {})
  static function fstat(descriptor : FileDescriptor) : Stat;

  @:native("fs_lstat")
  @:overload(function(path : String, cb : String->Stat->Void) : Request {})
  static function lstat(path : String) : Stat;

  @:native("fs_rename")
  @:overload(function(path : String, newpath : String, cb : String->Bool->Void) : Request {})
  static function rename(path : String, newpath : String) : Bool;

  @:native("fs_fsync")
  @:overload(function(descriptor : FileDescriptor, cb : String->Bool->Void) : Request {})
  static function fsync(descriptor : FileDescriptor) : Bool;

  @:native("fs_fdatasync")
  @:overload(function(descriptor : FileDescriptor, cb : String->Bool->Void) : Request {})
  static function fdatasync(descriptor : FileDescriptor) : Bool;

  @:native("fs_ftruncate")
  @:overload(function(descriptor : FileDescriptor, offset : Int, cb : String->Bool->Void) : Request {})
  static function ftruncate(descriptor : FileDescriptor, offset : Int) : Bool;

  @:native("fs_sendfile")
  @:overload(function(fin : FileDescriptor, fout : FileDescriptor, cb : String->Int->Void) : Request {})
  static function sendfile(fin : FileDescriptor, fout : FileDescriptor) : Int;

  @:native("fs_access")
  @:overload(function(path : String, mode : Int, cb : String->Bool->Void) : Request {})
  static function access(path : String, mode :Int) : Bool;

  @:native("fs_chmod")
  @:overload(function(path : String, mode : Int, cb : String->Bool->Void) : Request {})
  static function chmod(path : String, mode :Int) : Bool;

  @:native("fs_fchmod")
  @:overload(function(descriptor : FileDescriptor, mode : Int, cb : String->Bool->Void) : Request {})
  static function fchmod(descriptor : FileDescriptor, mode :Int) : Bool;

  @:native("fs_futime")
  @:overload(function(descriptor : FileDescriptor, actime : Int, modtime : Int, cb : String->Bool->Void) : Request {})
  static function futime(descriptor : FileDescriptor, actime : Int, modtime : Int) : Bool;

  @:native("fs_utime")
  @:overload(function(path : String, actime : Int, modtime : Int, cb : String->Bool->Void) : Request {})
  static function utime(path : String, actime : Int, modtime : Int) : Bool;

  @:native("fs_link")
  @:overload(function(oldpath : String, newpath : String, cb : String->Bool->Void) : Request {})
  static function link(oldpath : String, newpath : String) : Bool;

  @:native("fs_symlink")
  @:overload(function(oldpath : String, newpath : String, flags : Int, cb : String->Bool->Void) : Request {})
  static function symlink(oldpath : String, newpath : String, flags : Int) : Bool;

  // @:native("fs_readlink")
  // @:overload(function(path : String, cb : String->String->Void) : Request {})
  // static function readlink(path : String) : String;

  @:native("fs_realpath")
  @:overload(function(path : String, cb : String->String->Void) : Request{})
  static function realpath(path : String) : String;

  @:native("fs_chown")
  @:overload(function(path : String, uid : Int, gid : Int, cb : String->Bool->Void) : Request {})
  static function chown(path : String, uid : Int, gid : Int) : Bool;

  @:native("fs_fchown")
  @:overload(function(descriptor : FileDescriptor, uid : Int, gid : Int, cb : String->Bool->Void) : Request {})
  static function fchown(descriptor : FileDescriptor, uid : Int, gid : Int) : Bool;
}

extern class ScanDirMarker {}

@:multiReturn
extern class ScandirNext {
  var name : String;
  var type : String;
}

typedef Stat = {
  ino       : Int,
  ctime     : TimeStamp,
  uid       : Int,
  dev       : Int,
  nlink     : Int,
  mode      : Int,
  size      : Int,
  birthtime : TimeStamp,
  gid       : Int,
  type      : String,
  rdev      : Int,
  gen       : Int,
  blocks    : Int,
  mtime     : TimeStamp,
  atime     : TimeStamp,
  blksize   : Int,
  flags     : Int
}

typedef TimeStamp = {
  sec  : Int,
  nsec : Int
}

@:multiReturn extern class LuvStatus<T> {
  var status : T;
  var message : String;
}
