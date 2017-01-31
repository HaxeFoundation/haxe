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

import java.lang.System;
import sys.io.Process;
using haxe.Int64;

@:coreApi class Sys {
	private static var _args:java.NativeArray<String>;
	private static var _env:haxe.ds.StringMap<String>;
	private static var _sysName:String;

	public static inline function print( v : Dynamic ) : Void
	{
		java.lang.System.out.print(v);
	}

	public static inline function println( v : Dynamic ) : Void
	{
		java.lang.System.out.println(v);
	}

	public static function args() : Array<String>
	{
		if (_args == null)
			return [];
		return java.Lib.array(_args);
	}

	public static function getEnv( s : String ) : String
	{
		return java.lang.System.getenv(s);
	}

	public static function putEnv( s : String, v : String ) : Void
	{
		//java offers no support for it (!)
		throw "Not implemented in this platform";
	}

	public static function environment() : Map<String,String>
	{
		if (_env != null)
			return _env;
		var _env = _env = new haxe.ds.StringMap();
		for (mv in java.lang.System.getenv().entrySet())
		{
			_env.set(mv.getKey(), mv.getValue());
		}

		return _env;
	}

	public static function sleep( seconds : Float ) : Void
	{
		try
			java.lang.Thread.sleep(cast seconds * 1000)
		catch (e:Dynamic)
			throw e;
	}

	public static function setTimeLocale( loc : String ) : Bool
	{
		return false;
	}

	public static function getCwd() : String
	{
		return new java.io.File(".").getAbsolutePath().substr(0,-1);
	}

	public static function setCwd( s : String ) : Void
	{
		//java offers no support for it (!)
		throw "not implemented";
	}

	public static function systemName() : String
	{
		if (_sysName != null) return _sysName;
		var sname = System.getProperty("os.name").toLowerCase();
		if (sname.indexOf("win") >= 0)
			return _sysName = "Windows";
		if (sname.indexOf("mac") >= 0)
			return _sysName = "Mac";
		if (sname.indexOf("nux") >= 0)
			return _sysName = "Linux";
		if (sname.indexOf("nix") >= 0)
			return _sysName = "BSD";

		return _sysName = System.getProperty("os.name");
	}

	public static function command( cmd : String, ?args : Array<String> ) : Int
	{
		var pb = Process.createProcessBuilder(cmd, args);
#if java6
		pb.redirectErrorStream(true);
#else
		pb.redirectOutput(java.lang.ProcessBuilder.ProcessBuilder_Redirect.INHERIT);
		pb.redirectError(java.lang.ProcessBuilder.ProcessBuilder_Redirect.INHERIT);
#end
		var proc = pb.start();
#if java6
		var reader = new java.io.NativeInput(proc.getInputStream());
		try
		{
			while(true) {
				var ln = reader.readLine();
				Sys.println(ln);
			}
		}
		catch(e:haxe.io.Eof) {}
#end
		proc.waitFor();
		var exitCode = proc.exitValue();
		proc.destroy();
		return exitCode;
	}

	public static function exit( code : Int ) : Void
	{
		System.exit(code);
	}

	public static function time() : Float
	{
		return cast(System.currentTimeMillis(), Float) / 1000;
	}

	public static function cpuTime() : Float
	{
		return cast(System.nanoTime(), Float) / 1000000000;
	}

	@:deprecated("Use programPath instead") public static function executablePath() : String
	{
		return getCwd();
	}

	public static function programPath() : String {
		return java.Lib.toNativeType(Sys)
			.getProtectionDomain()
			.getCodeSource()
			.getLocation().toURI().getPath();
	}

	public static function getChar( echo : Bool ) : Int
	{
		//TODO
		return throw "Not implemented";
	}

	public static function stdin() : haxe.io.Input
	{
		var _in:java.io.InputStream = Reflect.field(System, "in");
		return new java.io.NativeInput(_in);
	}

	public static function stdout() : haxe.io.Output
	{
		return new java.io.NativeOutput(System.out);
	}

	public static function stderr() : haxe.io.Output
	{
		return new java.io.NativeOutput(System.err);
	}
}
