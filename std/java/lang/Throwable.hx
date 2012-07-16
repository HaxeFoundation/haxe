package java.lang;
import java.NativeArray;

extern class Throwable 
{
	
	function new(message:String, cause:Throwable):Void;
	function fillInStackTrace():Throwable;
	function getCause():Throwable;
	function getLocalizedMessage():String;
	function getMessage():String;
	function getStackTrace():NativeArray<StackTraceElement>;
	function setStackTrace(stackTrace:NativeArray<StackTraceElement>):Void;
	function initCause(cause:Throwable):Throwable;
	function printStackTrace():Void;
	
}

extern class Exception extends Throwable { }

extern class RuntimeException extends Exception { }

extern class Error extends Throwable { }

extern class StackTraceElement
{
	function new(declaringClass:String, methodName:String, fileName:String, lineNumber:Int):Void;
	
	function getClassName():String;
	function getFileName():String;
	function getLineNumber():Int;
	function getMethodName():String;
	function isNativeMethod():Bool;
}