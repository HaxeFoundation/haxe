package cpp;

@:buildXml("<include name=\"${HXCPP}/src/hx/libs/std/Build.xml\"/>")
extern class NativeSys
{
   @:native("__hxcpp_print")
   public static function print( v : Dynamic ) : Void { }

   @:native("__hxcpp_println")
   public static function println( v : Dynamic ) : Void { }

   @:extern @:native("_hx_std_get_env")
   public static function get_env(v:String) : String return null;


   @:extern @:native("_hx_std_put_env")
   public static function put_env(e:String,v:String) : Void { }


   @:extern @:native("_hx_std_sys_sleep")
   public static function sys_sleep(f:Float) : Void { }


   @:extern @:native("_hx_std_set_time_locale")
   public static function set_time_locale(l:String) : Bool return false;


   @:extern @:native("_hx_std_get_cwd")
   public static function get_cwd() : String return null;


   @:extern @:native("_hx_std_set_cwd")
   public static function set_cwd(d:String) : Void { }


   @:extern @:native("_hx_std_sys_string")
   public static function sys_string() : String return null;


   @:extern @:native("_hx_std_sys_is64")
   public static function sys_is64() : Bool return false;


   @:extern @:native("_hx_std_sys_command")
   public static function sys_command(cmd:String) : Int return 0;


   @:extern @:native("_hx_std_sys_exit")
   public static function sys_exit(code:Int) : Void { }


   @:extern @:native("_hx_std_sys_exists")
   public static function sys_exists(path:String) : Bool return false;


   @:extern @:native("_hx_std_file_delete")
   public static function file_delete(path:String) : Void { }


   @:extern @:native("_hx_std_sys_rename")
   public static function sys_rename(path:String,newname:String) : Bool return false;


   @:extern @:native("_hx_std_sys_stat")
   public static function sys_stat(path:String) : Dynamic return null;


   @:extern @:native("_hx_std_sys_file_type")
   public static function sys_file_type(path:String) : String return null;


   @:extern @:native("_hx_std_sys_create_dir")
   public static function sys_create_dir(path:String,mode:Int) : Bool return false;


   @:extern @:native("_hx_std_sys_remove_dir")
   public static function sys_remove_dir(path:String) : Void { }


   @:extern @:native("_hx_std_sys_time")
   public static function sys_time() : Float return 0;


   @:extern @:native("_hx_std_sys_cpu_time")
   public static function sys_cpu_time() : Float return 0;


   @:extern @:native("_hx_std_sys_read_dir")
   public static function sys_read_dir(p:String) : Array<String> return null;


   @:extern @:native("_hx_std_file_full_path")
   public static function file_full_path(path:String) : String return null;


   @:extern @:native("_hx_std_sys_exe_path")
   public static function sys_exe_path() : String return null;


   @:extern @:native("_hx_std_sys_env")
   public static function sys_env() : Array<String> return null;


   @:extern @:native("_hx_std_sys_getch")
   public static function sys_getch(b:Bool) : Int return 0;


   @:extern @:native("_hx_std_sys_get_pid")
   public static function sys_get_pid() : Int return 0;

}


