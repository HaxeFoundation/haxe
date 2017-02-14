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
   "cpp.Finalizable",
   "Std",
   "StringBuf",
   "sys.db.Mysql",
   "sys.db.Sqlite",
   "sys.FileSystem",
   "sys.io.File",
   "sys.io.FileInput",
   "sys.net.UdpSocket",
   "sys.net.Socket",
   "sys.ssl.Certificate",
   "sys.ssl.Digest",
   "sys.ssl.Key",
   "sys.ssl.Socket",
   "Enum",
   "EnumValue",
   //"Sys",
   "Type",
   "Xml",
   "Date",
   "Lambda",
   "DateTools",
   "List",
   "Math",
   "Reflect",
   "StringBuf",
   "StringTools",

   "haxe.ds.IntMap",
   "haxe.ds.ObjectMap",
   "haxe.ds.StringMap",
   "haxe.ds.BalancedTree",
   "haxe.CallStack",
   "haxe.Serializer",
   "haxe.Unserializer",
   "haxe.Resource",
   "haxe.Template",
   "haxe.Utf8",
   "haxe.Log",
   "haxe.zip.Compress",
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
   // "IntIterator",
   "List",
   "Map",
   "String",


   ];


   static function parseClassInfo(externs:Map<String,Bool>, filename:String)
   {
      if (sys.FileSystem.exists(filename))
      {
         var file = sys.io.File.read(filename);
         try
         {
            while(true)
            {
               var line = file.readLine();
               var parts = line.split(" ");
               if (parts[0]=="class" || parts[0]=="interface" || parts[0]=="enum")
                  externs.set(parts[1],true);
            }
         } catch( e : Dynamic ) { }
         if (file!=null)
            file.close();
      }
   }


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
      // Hidded in implementation classes
      //externs.set("sys.db.RecordType",true);
      externs.set("sys.db._Sqlite.SqliteConnection",true);
      externs.set("sys.db._Sqlite.SqliteResultSet",true);
      externs.set("sys.db._Mysql.MysqlConnection",true);
      externs.set("sys.db._Mysql.MysqlResultSet",true);
      externs.set("sys.net._Socket.SocketInput",true);
      externs.set("sys.net._Socket.SocketOutput",true);
      externs.set("sys.ssl._Socket.SocketInput",true);
      externs.set("sys.ssl._Socket.SocketOutput",true);
      externs.set("haxe.ds.TreeNode",true);
      externs.set("haxe.xml.XmlParserException",true);
      for(e in classes)
         externs.set(e,true);


      var define = Context.defined("dll_import") ? Context.definedValue("dll_import") : "1";
      if (define!="1")
         parseClassInfo(externs,define);
      else
      {
         var tried = new Map<String, Bool>();
         for(path in Context.getClassPath())
            if (!tried.exists(path))
            {
                tried.set(path,true);
                parseClassInfo(externs,path + "/export_classes.info");
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


