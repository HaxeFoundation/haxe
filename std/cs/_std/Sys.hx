/*
 * Copyright (C)2005-2012 Haxe Foundation
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
import sys.io.Process;
import cs.system.Environment;
import cs.system.threading.Thread;
/*
 * Copyright (c) 2005-2012, The Haxe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

@:coreApi
class Sys {
	private static var _env:haxe.ds.StringMap<String>;
	private static var _args:Array<String>;

	public static function print( v : Dynamic ) : Void
	{
		cs.system.Console.Write(v);
	}

	public static function println( v : Dynamic ) : Void
	{
		cs.system.Console.WriteLine(v);
	}

	public static function args() : Array<String>
	{
		if (_args == null)
		{
			var ret = cs.Lib.array(Environment.GetCommandLineArgs());
			ret.shift();
			_args = ret;
		}
		return _args.copy();
	}

	public static function getEnv( s : String ) : String
	{
		return Environment.GetEnvironmentVariable(s);
	}

	public static function putEnv( s : String, v : String ) : Void
	{
		Environment.SetEnvironmentVariable(s, v);
		if (_env != null)
			_env.set(s, v);
	}

	public static function environment() : Map<String,String>
	{
		if (_env == null)
		{
			var e = _env = new haxe.ds.StringMap();
			var nenv = Environment.GetEnvironmentVariables().GetEnumerator();
			while (nenv.MoveNext())
			{
				e.set(nenv.Key, nenv.Value);
			}
		}

		return _env;
	}

	public static function sleep( seconds : Float ) : Void
	{
		Thread.Sleep( Std.int(seconds * 1000) );
	}

	public static function setTimeLocale( loc : String ) : Bool
	{
		//TODO C#
		return false;
	}

	public static function getCwd() : String
	{
		return cs.system.io.Directory.GetCurrentDirectory();
	}

	public static function setCwd( s : String ) : Void
	{
		cs.system.io.Directory.SetCurrentDirectory(s);
	}

	public static function systemName() : String
	{
		//doing a switch with strings since MacOS might not be available
		switch(Environment.OSVersion.Platform + "")
		{
			case "Unix": return "Linux";
			case "Xbox": return "Xbox";
			case "MacOSX": return "Mac";
			default:
				var ver = cast(Environment.OSVersion.Platform, Int);
				if (ver == 4 || ver == 6 || ver == 128)
					return "Linux";
				return "Windows";
		}
	}

	public static function command( cmd : String, ?args : Array<String> ) : Int
	{
		var proc:Process = new Process(cmd, args == null ? [] : args);
		var ret = proc.exitCode();
		proc.close();

		return ret;
	}

	public static function exit( code : Int ) : Void
	{
		Environment.Exit(code);
	}

	public static function time() : Float
	{
		return Date.now().getTime() / 1000;
	}

	public static function cpuTime() : Float
	{
		return Environment.TickCount / 1000;
	}

	public static inline function executablePath() : String
	{
		return cs.system.reflection.Assembly.GetExecutingAssembly().GetName().CodeBase;
	}

	public static function getChar( echo : Bool ) : Int
	{
		#if !(Xbox || CF || MF) //Xbox, Compact Framework, Micro Framework
		return cast(cs.system.Console.ReadKey(!echo).KeyChar, Int);
		#else
		return -1;
		#end
	}

	public static function stdin() : haxe.io.Input
	{
#if !(Xbox || CF || MF)
		return new cs.io.NativeInput(cs.system.Console.OpenStandardInput());
#else
		return null;
#end
	}

	public static function stdout() : haxe.io.Output
	{
#if !(Xbox || CF || MF)
		return new cs.io.NativeOutput(cs.system.Console.OpenStandardOutput());
#else
		return null;
#end
	}

	public static function stderr() : haxe.io.Output
	{
#if !(Xbox || CF || MF)
		return new cs.io.NativeOutput(cs.system.Console.OpenStandardError());
#else
		return null;
#end
	}

}
