package cpp;

@:buildXml("<include name=\"${HXCPP}/src/hx/libs/std/Build.xml\"/>")
extern class NativeFile
{
   @:extern @:native("_hx_std_file_open")
   public static function file_open(fname:String,r:String) : Dynamic return null;

   @:extern @:native("_hx_std_file_close")
   public static function file_close(handle:Dynamic) : Void { }


   @:extern @:native("_hx_std_file_write")
   public static function file_write(handle:Dynamic,s:haxe.io.BytesData,p:Int,n:Int) : Int return 0;


   @:extern @:native("_hx_std_file_write_char")
   public static function file_write_char(handle:Dynamic,c:Int) : Void { }


   @:extern @:native("_hx_std_file_read")
   public static function file_read(handle:Dynamic,s:haxe.io.BytesData,p:Int,n:Int) : Int return 0;


   @:extern @:native("_hx_std_file_read_char")
   public static function file_read_char(handle:Dynamic) : Int return 0;


   @:extern @:native("_hx_std_file_seek")
   public static function file_seek(handle:Dynamic,pos:Int,kind:Int) : Void { }


   @:extern @:native("_hx_std_file_tell")
   public static function file_tell(handle:Dynamic) : Int return 0;


   @:extern @:native("_hx_std_file_eof")
   public static function file_eof(handle:Dynamic) : Bool return false;


   @:extern @:native("_hx_std_file_flush")
   public static function file_flush(handle:Dynamic) : Void return null;


   @:extern @:native("_hx_std_file_contents_string")
   public static function file_contents_string(name:String) : String return null;


   @:extern @:native("_hx_std_file_contents_bytes")
   public static function file_contents_bytes(name:String) : haxe.io.BytesData return null;


   @:extern @:native("_hx_std_file_stdin")
   public static function file_stdin() : Dynamic return null;


   @:extern @:native("_hx_std_file_stdout")
   public static function file_stdout() : Dynamic return null;


   @:extern @:native("_hx_std_file_stderr")
   public static function file_stderr() : Dynamic return null;

}
