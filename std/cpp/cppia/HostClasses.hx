package cpp.cppia;

import haxe.macro.Compiler;
import haxe.macro.Context;
import haxe.macro.Type;

#if !macro
  #if cppia
  @:build(cpp.cppia.HostClasses.exclude())
  #end
class HostClasses { }
#else

import Sys;
import haxe.Constraints;

@:noPackageRestrict
class HostClasses
{
   static var classes = [
   "cpp.Lib",
   "cpp.NativeArray",
   "cpp.NativeString",
   "cpp.vm.Debugger",
   "cpp.vm.Deque",
   "cpp.vm.ExecutionTrace",
   "cpp.vm.Gc",
   "cpp.vm.Lock",
   "cpp.vm.Mutex",
   "cpp.vm.Profiler",
   "cpp.vm.Thread",
   "cpp.vm.Tls",
   "cpp.vm.Unsafe",
   "cpp.vm.WeakRef",
   "cpp.Object",
   "cpp.Int64",
   "Std",
   "StringBuf",
   "sys.db.Mysql",
   "sys.db.Sqlite",
   "sys.db.Object",
   "sys.db.Manager",
   "sys.db.Connection",
   "sys.FileSystem",
   "sys.io.File",
   "sys.io.FileInput",
   "sys.net.Socket",
   "Enum",
   "EnumValue",
   //"Sys",
   "Type",
   "Xml",
   "Date",
   "DateTools",
   "List",
   "Math",
   "Reflect",
   "StringBuf",
   "StringTools",

   "haxe.ds.IntMap",
   "haxe.ds.ObjectMap",
   "haxe.ds.StringMap",
   "haxe.CallStack",
   "haxe.Serializer",
   "haxe.Unserializer",
   "haxe.Resource",
   "haxe.Template",
   "haxe.Utf8",
   "haxe.Log",
   "haxe.zip.Uncompress",

   "haxe.crypto.BaseCode",
   "haxe.crypto.Sha256",
   "haxe.crypto.Hmac",
   "haxe.crypto.Crc32",
   "haxe.crypto.Base64",
   "haxe.crypto.Adler32",
   "haxe.crypto.Md5",
   "haxe.crypto.Sha1",
 
   "haxe.io.BufferInput",
   "haxe.io.Bytes",
   "haxe.io.BytesBuffer",
   "haxe.io.BytesData",
   "haxe.io.BytesInput",
   "haxe.io.BytesOutput",
   "haxe.io.Eof",
   "haxe.io.Error",
   "haxe.io.FPHelper",
   "haxe.io.Input",
   "haxe.io.Output",
   "haxe.io.Path",
   "haxe.io.StringInput",
   "haxe.xml.Parser",
   "haxe.Json",

   "haxe.CallStack",
   "haxe.Resource",
   "haxe.Utf8",
   "haxe.Int64",
   "haxe.Int32",
   "haxe.Serializer",
   "haxe.Unserializer",


   "haxe.ds.ArraySort",
   "haxe.ds.GenericStack",
   "haxe.ds.ObjectMap",
   "haxe.ds.Vector",
   "haxe.ds.BalancedTree",
   "haxe.ds.HashMap",
   "haxe.ds.Option",
   "haxe.ds.WeakMap",
   "haxe.ds.EnumValueMap",
   "haxe.ds.IntMap",
   "haxe.ds.StringMap",

   "StdTypes",
   "Array",
   "Class",
   "Date",
   "EReg",
   "Enum",
   "EnumValue",
   "IntIterator",
   "List",
   "Map",
   "String",


   ];


   static function onGenerateCppia(types:Array<Type>):Void
   {
      var externs = new Map<String,Bool>();
      externs.set("Sys",true);
      externs.set("haxe.IMap",true);
      externs.set("haxe.crypto.HashMethod",true);
      externs.set("haxe._Int64.Int64_Impl_",true);
      externs.set("haxe._Int64.___Int64",true);
      externs.set("haxe._Int32.Int32_Impl_",true);
      externs.set("haxe._Int32.___Int32",true);
      for(e in classes)
         externs.set(e,true);
      for(path in Context.getClassPath())
      {
         var filename = path + "/export_classes.info";
         if (sys.FileSystem.exists(filename))
         {
            try
            {
               var contents = sys.io.File.getContent(filename);
               contents = contents.split("\r").join("");
               for(cls in contents.split("\n"))
               {
                  if (cls!="")
                  {
                     var parts = cls.split("|");
                     if (parts.length==1)
                        externs.set(cls,true);
                  }
               }
            } catch( e : Dynamic ) { }
         }
      }
      for(type in types)
      {
         switch(type)
         {
            case TInst(classRef, params):
               if (externs.exists(classRef.toString()))
                    classRef.get().exclude();
            case TEnum(enumRef, params):
               if (externs.exists(enumRef.toString()))
                    enumRef.get().exclude();
            default:
         }
      }
   }

   // Exclude the standard classes, and any described in 'export_classes.info' files found in the classpath
   public static function exclude()
   {
      if (Context.defined("cppia"))
         Context.onGenerate(onGenerateCppia);
      else
         Context.error("cpp.cppia.excludeHostFiles is only for cppia code", Context.currentPos());
      return Context.getBuildFields();
   }


   // Ensure that the standard classes are included in the host
   public static function include()
   {
      Compiler.keep("haxe.IMap");
      Compiler.keep("haxe.crypto.HashMethod");
      Compiler.keep("haxe._Int64.Int64_Impl_");
      Compiler.keep("haxe._Int32.Int32_Impl_");
      Compiler.keep("haxe._Int64.___Int64");
      Compiler.keep("haxe._Int32.___Int32");
      for(cls in classes)
      {
         Context.getModule(cls);
         Compiler.keep(cls);
      }
      return Context.getBuildFields();
   }
}
#end


