package java.lang;
import java.NativeArray;

extern class Process
{
	function destroy():Void;
	function exitValue():Int;
	function getErrorStream():java.io.InputStream;
	function getInputStream():java.io.InputStream;
	function getOutputStream():java.io.OutputStream;
	function waitFor():Int;
}