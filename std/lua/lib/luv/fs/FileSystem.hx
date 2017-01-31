/*
 * Copyright (C)2005-2017 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package lua.lib.luv.fs;
import lua.lib.luv.fs.Open;
import lua.Result;

@:luaRequire("luv")
extern class FileSystem {
  @:native("fs_close")
  @:overload(function(file : FileDescriptor, cb : String->Bool->Void) : Request {})
  static function close(file : FileDescriptor) : Result<Bool>;

  @:native("fs_open")
  @:overload(function(path : String, flags : Open, mode : Int, ?cb : String->FileDescriptor->Void) : Request {})
  static function open(path : String, flags : Open, mode : Int) : Result<FileDescriptor>;

  @:native("fs_read")
  @:overload(function(file : FileDescriptor, len : Int, offset : Int, ?cb : String->String->Void) : Request {} )
  static function read(file : FileDescriptor, len : Int, offset : Int) : Result<String>;

  @:native("fs_unlink")
  @:overload(function(file : FileDescriptor, ?cb : String->String->Void) : Request {} )
  static function unlink(file : FileDescriptor, content : String) : Result<String>;

  @:native("fs_write")
  @:overload(function(file : FileDescriptor, content : String, offset : Int, ?cb : String->Bool->Void) : Int {})
  static function write(file : FileDescriptor, content : String, offset : Int) : Result<Bool>;

  @:native("fs_mkdir")
  @:overload(function(path : String, mode : Int, cb : String->Bool->Void) : Request {})
  static function mkdir(path : String, mode :Int) : Result<Bool>;

  @:native("fs_mkdtemp")
  @:overload(function(data : String, cb : String->Bool->Void) : Request {})
  static function mkdtemp(data : String) : Result<Bool>;

  @:native("fs_rmdir")
  @:overload(function(path : String, cb : String->Bool->Void) : Request {})
  static function rmdir(path : String) : Result<Int>;

  @:native("fs_scandir")
  @:overload(function(path : String, cb : String->Bool->Void) : Request {})
  static function scandir(path : String) : ScanDirMarker;

  @:native("fs_scandir_next")
  static function scandir_next(scandir : ScanDirMarker) : ScandirNext;

  @:native("fs_stat")
  @:overload(function(path : String, cb : String->Stat->Void) : Request {})
  static function stat(path : String) : Result<Stat>;

  @:native("fs_fstat")
  @:overload(function(descriptor : FileDescriptor, cb : String->Stat->Void) : Request {})
  static function fstat(descriptor : FileDescriptor) : Result<Stat>;

  @:native("fs_lstat")
  @:overload(function(path : String, cb : String->Stat->Void) : Request {})
  static function lstat(path : String) : Result<Stat>;

  @:native("fs_rename")
  @:overload(function(path : String, newpath : String, cb : String->Bool->Void) : Request {})
  static function rename(path : String, newpath : String) : Result<Bool>;

  @:native("fs_fsync")
  @:overload(function(descriptor : FileDescriptor, cb : String->Bool->Void) : Request {})
  static function fsync(descriptor : FileDescriptor) : Result<Bool>;

  @:native("fs_fdatasync")
  @:overload(function(descriptor : FileDescriptor, cb : String->Bool->Void) : Request {})
  static function fdatasync(descriptor : FileDescriptor) : Result<Bool>;

  @:native("fs_ftruncate")
  @:overload(function(descriptor : FileDescriptor, offset : Int, cb : String->Bool->Void) : Request {})
  static function ftruncate(descriptor : FileDescriptor, offset : Int) : Result<Bool>;

  @:native("fs_sendfile")
  @:overload(function(fin : FileDescriptor, fout : FileDescriptor, cb : String->Int->Void) : Request {})
  static function sendfile(fin : FileDescriptor, fout : FileDescriptor) : Result<Int>;

  @:native("fs_access")
  @:overload(function(path : String, mode : Int, cb : String->Bool->Void) : Request {})
  static function access(path : String, mode :Int) : Result<Bool>;

  @:native("fs_chmod")
  @:overload(function(path : String, mode : Int, cb : String->Bool->Void) : Request {})
  static function chmod(path : String, mode :Int) : Result<Bool>;

  @:native("fs_fchmod")
  @:overload(function(descriptor : FileDescriptor, mode : Int, cb : String->Bool->Void) : Request {})
  static function fchmod(descriptor : FileDescriptor, mode :Int) : Result<Bool>;

  @:native("fs_futime")
  @:overload(function(descriptor : FileDescriptor, actime : Int, modtime : Int, cb : String->Bool->Void) : Request {})
  static function futime(descriptor : FileDescriptor, actime : Int, modtime : Int) : Result<Bool>;

  @:native("fs_utime")
  @:overload(function(path : String, actime : Int, modtime : Int, cb : String->Bool->Void) : Request {})
  static function utime(path : String, actime : Int, modtime : Int) : Result<Bool>;

  @:native("fs_link")
  @:overload(function(oldpath : String, newpath : String, cb : String->Bool->Void) : Request {})
  static function link(oldpath : String, newpath : String) : Result<Bool>;

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

