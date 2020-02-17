#if sys
var filename = '.sys.io.file.testfile';
if (sys.FileSystem.exists(filename)) sys.FileSystem.deleteFile(filename);
sys.FileSystem.exists(filename) == false;

// test file write
var fw = sys.io.File.write(filename);
sys.FileSystem.exists(filename) == true;
fw.writeString("apple\n");
fw.close();
sys.io.File.getContent(filename) == "apple\n";

// overwrite
var fw = sys.io.File.write(filename);
fw.writeString("banana\n");
fw.close();
sys.io.File.getContent(filename) == "banana\n";

// test file append
var fa = sys.io.File.append(filename);
fa.writeString("apple\n");
fa.close();
sys.io.File.getContent(filename) == "banana\napple\n";

// test file update
var fu = sys.io.File.update(filename);
fu.writeString("cherry\n");
fu.close();
sys.io.File.getContent(filename) == "cherry\napple\n";
var fu = sys.io.File.update(filename);
fu.seek(7, sys.io.FileSeek.SeekBegin);
fu.writeString("banana\n");
fu.close();
sys.io.File.getContent(filename) == "cherry\nbanana\n";

// File.update should create the file if it doesn't exist
sys.FileSystem.deleteFile(filename);
var fu = sys.io.File.update(filename);
fu.close();
sys.FileSystem.exists(filename) == true;

sys.FileSystem.deleteFile(filename);
#else
1 == 1;
#end
