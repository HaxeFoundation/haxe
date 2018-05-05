package cpp;

@:buildXml('<include name="${HXCPP}/src/hx/libs/std/Build.xml"/>')
extern class NativeSys
{
   @:native("__hxcpp_print")
   public static function print( v : Dynamic ) : Void { }

   @:native("__hxcpp_println")
   public static function println( v : Dynamic ) : Void { }

   @:native("_hx_std_get_env")
   extern public static function get_env(v:String) : String return null;


   @:native("_hx_std_put_env")
   extern public static function put_env(e:String,v:String) : Void { }


   @:native("_hx_std_sys_sleep")
   extern public static function sys_sleep(f:Float) : Void { }


   @:native("_hx_std_set_time_locale")
   extern public static function set_time_locale(l:String) : Bool return false;


   @:native("_hx_std_get_cwd")
   extern public static function get_cwd() : String return null;


   @:native("_hx_std_set_cwd")
   extern public static function set_cwd(d:String) : Void { }


   @:native("_hx_std_sys_string")
   extern public static function sys_string() : String return null;


   @:native("_hx_std_sys_is64")
   extern public static function sys_is64() : Bool return false;


   @:native("_hx_std_sys_command")
   extern public static function sys_command(cmd:String) : Int return 0;


   @:native("_hx_std_sys_exit")
   extern public static function sys_exit(code:Int) : Void { }


   @:native("_hx_std_sys_exists")
   extern public static function sys_exists(path:String) : Bool return false;


   @:native("_hx_std_file_delete")
   extern public static function file_delete(path:String) : Void { }


   @:native("_hx_std_sys_rename")
   extern public static function sys_rename(path:String,newname:String) : Bool return false;


   @:native("_hx_std_sys_stat")
   extern public static function sys_stat(path:String) : Dynamic return null;


   @:native("_hx_std_sys_file_type")
   extern public static function sys_file_type(path:String) : String return null;


   @:native("_hx_std_sys_create_dir")
   extern public static function sys_create_dir(path:String,mode:Int) : Bool return false;


   @:native("_hx_std_sys_remove_dir")
   extern public static function sys_remove_dir(path:String) : Void { }


   @:native("_hx_std_sys_time")
   extern public static function sys_time() : Float return 0;


   @:native("_hx_std_sys_cpu_time")
   extern public static function sys_cpu_time() : Float return 0;


   @:native("_hx_std_sys_read_dir")
   extern public static function sys_read_dir(p:String) : Array<String> return null;


   @:native("_hx_std_file_full_path")
   extern public static function file_full_path(path:String) : String return null;


   @:native("_hx_std_sys_exe_path")
   extern public static function sys_exe_path() : String return null;


   @:native("_hx_std_sys_env")
   extern public static function sys_env() : Array<String> return null;


   @:native("_hx_std_sys_getch")
   extern public static function sys_getch(b:Bool) : Int return 0;


   @:native("_hx_std_sys_get_pid")
   extern public static function sys_get_pid() : Int return 0;

}


