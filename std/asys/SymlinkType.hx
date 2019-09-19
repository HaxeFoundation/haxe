package asys;

enum abstract SymlinkType(Int) {
  var SymlinkFile = 0;
  var SymlinkDir = 1;
  var SymlinkJunction = 2; // Windows only
  
  inline function get_raw():Int return this;
}
