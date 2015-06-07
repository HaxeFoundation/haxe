package lua;

/**
  These are all global static methods within lua
**/

@:native("_G")
extern Lua {
	public static function unpack : Dynamic->Table<Int,Dynamic>; 
	public static function setmetatable : Table->Table->Void; 
	public static function setfenv : Int->Table->Void;
}
