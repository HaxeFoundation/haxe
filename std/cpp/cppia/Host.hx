package cpp.cppia;


@:build(cpp.cppia.HostClasses.include())
class Host
{
   public static function run(source:String)
   {
      untyped __global__.__scriptable_load_cppia(source);
   }

   public static function main()
   {
      var script = Sys.args()[0];
      #if (!scriptable && !doc_gen)
      #error "Please define scriptable to use cppia"
      #end
      if (script==null)
      {
         Sys.println("Usage : Cppia scriptname");
      }
      else
      {
         var source = sys.io.File.getContent(script);
         run(source);
      }
   }
}
