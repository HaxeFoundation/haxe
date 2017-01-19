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
package python;

import haxe.extern.Rest;

@:native("BaseException")
extern class BaseException
{
	function new(args:Rest<Dynamic>):Void;
}



@:native("BufferError")
extern class BufferError extends BaseException
{

}

@:native("GeneratorExit")
extern class GeneratorExit extends BaseException
{

}

@:native("KeyboardInterrupt")
extern class KeyboardInterrupt extends BaseException
{

}

@:native("Exception")
extern class Exception extends BaseException
{

}

@:native("SyntaxError")
extern class SyntaxError extends Exception
{

}

@:native("StopIteration")
extern class StopIteration extends Exception
{

}

@:native("RuntimeError")
extern class RuntimeError extends Exception
{

}

@:native("NotImplementedError")
extern class NotImplementedError extends RuntimeError
{

}

@:native("IndentationError")
extern class IndentationError extends SyntaxError
{

}

@:native("EnvironmentError")
extern class EnvironmentError extends Exception
{

}

@:native("OSError")
extern class OSError extends EnvironmentError
{

}

@:native("BlockingIOError")
extern class BlockingIOError extends OSError
{

}

@:native("ChildProcessError")
extern class ChildProcessError extends OSError
{

}

@:native("ConnectionError")
extern class ConnectionError extends OSError
{

}

@:native("BrokenPipeError")
extern class BrokenPipeError extends ConnectionError
{

}

@:native("ConnectionAbortedError")
extern class ConnectionAbortedError extends ConnectionError
{

}
@:native("ConnectionRefusedError")
extern class ConnectionRefusedError extends ConnectionError
{

}
@:native("ConnectionResetError")
extern class ConnectionResetError extends ConnectionError
{

}

@:native("FileExistsError")
extern class FileExistsError extends OSError
{

}
@:native("FileNotFoundError")
extern class FileNotFoundError extends OSError
{

}
@:native("InterruptedError")
extern class InterruptedError extends OSError
{

}
@:native("IsADirectoryError")
extern class IsADirectoryError extends OSError
{

}
@:native("NotADirectoryError")
extern class NotADirectoryError extends OSError
{

}
@:native("PermissionError")
extern class PermissionError extends OSError
{

}
@:native("ProcessLookupError")
extern class ProcessLookupError extends OSError
{

}
@:native("TimeoutError")
extern class TimeoutError extends OSError
{

}


@:native("NameError")
extern class NameError extends Exception
{

}

@:native("UnboundLocalError")
extern class UnboundLocalError extends NameError
{

}

@:native("MemoryError")
extern class MemoryError extends Exception
{

}

@:native("AssertionError")
extern class AssertionError extends Exception
{

}

@:native("AttributeError")
extern class AttributeError extends Exception
{

}

@:native("EOFError")
extern class EOFError extends Exception
{

}

@:native("ArithmeticError")
extern class ArithmeticError extends Exception
{

}



@:native("FloatingPointError")
extern class FloatingPointError extends ArithmeticError
{

}

@:native("OverflowError")
extern class OverflowError extends ArithmeticError
{

}


@:native("ZeroDivisionError")
extern class ZeroDivisionError extends ArithmeticError
{

}



@:native("ImportError")
extern class ImportError extends Exception
{

}

@:native("LookupError")
extern class LookupError extends Exception
{

}

@:native("IndexError")
extern class IndexError extends LookupError
{

}

@:native("KeyError")
extern class KeyError extends LookupError
{

}

@:native("IOError")
extern class IOError extends EnvironmentError
{

}

@:native("VMSError")
extern class VMSError extends OSError
{

}

@:native("WindowsError")
extern class WindowsError extends OSError
{

}






@:native("ValueError")
extern class ValueError extends Exception
{

}

@:native("UnicodeError")
extern class UnicodeError extends ValueError
{

}
@:native("UnicodeDecodeError")
extern class UnicodeDecodeError extends UnicodeError
{

}
@:native("UnicodeEncodeError")
extern class UnicodeEncodeError extends UnicodeError
{

}
@:native("UnicodeTranslateError")
extern class UnicodeTranslateError extends UnicodeError
{

}

@:native("Warning")
extern class Warning extends Exception
{

}

@:native("DeprecationWarning")
extern class DeprecationWarning extends Warning
{

}
@:native("PendingDeprecationWarning")
extern class PendingDeprecationWarning extends Warning
{

}
@:native("RuntimeWarning")
extern class RuntimeWarning extends Warning
{

}
@:native("SyntaxWarning")
extern class SyntaxWarning extends Warning
{

}
@:native("UserWarning")
extern class UserWarning extends Warning
{

}
@:native("FutureWarning")
extern class FutureWarning extends Warning
{

}
@:native("ImportWarning")
extern class ImportWarning extends Warning
{

}
@:native("UnicodeWarning")
extern class UnicodeWarning extends Warning
{

}
@:native("BytesWarning")
extern class BytesWarning extends Warning
{

}
@:native("ResourceWarning")
extern class ResourceWarning extends Warning
{

}
