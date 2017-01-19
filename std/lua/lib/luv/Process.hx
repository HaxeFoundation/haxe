package lua.lib.luv;

@:luaRequire("luv")
extern class Process extends Handle {
  static function disable_stdio_inheritance() : Void;
  static function spawn(path : String, options : Dynamic, cb : Int->Signal->Void ) : LuvSpawn;
  function kill(sig:String) : Int;
}

typedef ProcessOptions = {
  args : Table<Int,String>,
  stdio : Table<Int,Pipe>
}

@:multiReturn extern class LuvSpawn {
  var handle : Process;
  var pid : Int;
}
