/*
 * Copyright (C)2005-2016 Haxe Foundation
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
package cpp;

@:include("stdio.h")
extern class Stdio
{
   @:native("printf")
   @:overload(function(format:ConstCharStar,a0:VarArg):Void { })
   @:overload(function(format:ConstCharStar,a0:VarArg,a1:VarArg):Void { })
   @:overload(function(format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg):Void { })
   @:overload(function(format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg,a3:VarArg):Void { })
   @:overload(function(format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg,a3:VarArg,a4:VarArg):Void { })
   @:overload(function(format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg,a3:VarArg,a4:VarArg,a5:VarArg):Void { })
   @:overload(function(format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg,a3:VarArg,a4:VarArg,a5:VarArg,a6:VarArg):Void { })
   @:overload(function(format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg,a3:VarArg,a4:VarArg,a5:VarArg,a6:VarArg,a7:VarArg):Void { })
   @:overload(function(format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg,a3:VarArg,a4:VarArg,a5:VarArg,a6:VarArg,a7:VarArg,a8:VarArg):Void { })
   @:overload(function(format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg,a3:VarArg,a4:VarArg,a5:VarArg,a6:VarArg,a7:VarArg,a8:VarArg,a8:VarArg):Void { })
   @:overload(function(format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg,a3:VarArg,a4:VarArg,a5:VarArg,a6:VarArg,a7:VarArg,a8:VarArg,a9:VarArg,a10:VarArg):Void { })
   @:overload(function(format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg,a3:VarArg,a4:VarArg,a5:VarArg,a6:VarArg,a7:VarArg,a8:VarArg,a9:VarArg,a10:VarArg,a11:VarArg):Void { })
   @:overload(function(format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg,a3:VarArg,a4:VarArg,a5:VarArg,a6:VarArg,a7:VarArg,a8:VarArg,a9:VarArg,a10:VarArg,a11:VarArg,a12:VarArg):Void { })
   public static function printf(format:ConstCharStar):Void;

   @:native("fopen")
   public static function fopen(filename:ConstCharStar, mode:ConstCharStar) : FILE;

   @:native("fwrite")
   public static function fwrite<T>(data:RawPointer<T>, elemSize:SizeT, elemCount:SizeT, file:FILE ) : SizeT;

   @:native("fclose")
   public static function fclose(file:FILE) : Int;


   @:native("fprintf")
   @:overload(function(file:FILE,format:ConstCharStar,a0:VarArg):Void { })
   @:overload(function(file:FILE,format:ConstCharStar,a0:VarArg,a1:VarArg):Void { })
   @:overload(function(file:FILE,format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg):Void { })
   @:overload(function(file:FILE,format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg,a3:VarArg):Void { })
   @:overload(function(file:FILE,format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg,a3:VarArg,a4:VarArg):Void { })
   @:overload(function(file:FILE,format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg,a3:VarArg,a4:VarArg,a5:VarArg):Void { })
   @:overload(function(file:FILE,format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg,a3:VarArg,a4:VarArg,a5:VarArg,a6:VarArg):Void { })
   @:overload(function(file:FILE,format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg,a3:VarArg,a4:VarArg,a5:VarArg,a6:VarArg,a7:VarArg):Void { })
   @:overload(function(file:FILE,format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg,a3:VarArg,a4:VarArg,a5:VarArg,a6:VarArg,a7:VarArg,a8:VarArg):Void { })
   @:overload(function(file:FILE,format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg,a3:VarArg,a4:VarArg,a5:VarArg,a6:VarArg,a7:VarArg,a8:VarArg,a8:VarArg):Void { })
   @:overload(function(file:FILE,format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg,a3:VarArg,a4:VarArg,a5:VarArg,a6:VarArg,a7:VarArg,a8:VarArg,a9:VarArg,a10:VarArg):Void { })
   @:overload(function(file:FILE,format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg,a3:VarArg,a4:VarArg,a5:VarArg,a6:VarArg,a7:VarArg,a8:VarArg,a9:VarArg,a10:VarArg,a11:VarArg):Void { })
   @:overload(function(file:FILE,format:ConstCharStar,a0:VarArg,a1:VarArg,a2:VarArg,a3:VarArg,a4:VarArg,a5:VarArg,a6:VarArg,a7:VarArg,a8:VarArg,a9:VarArg,a10:VarArg,a11:VarArg,a12:VarArg):Void { })
   public static function fprintf(file:FILE,format:ConstCharStar):Void;




}

