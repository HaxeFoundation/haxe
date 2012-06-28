package cpp.vm;

import haxe.Stack;
import cpp.vm.DebugBase;
import cpp.net.Socket;


class DebugSocket extends DebugStdio
{
 	var socket:Socket;
   var socketOut:haxe.io.Output;
	var host:String;
	var port:Int;


   public function new(inHost:String, inPort:Int, inCreateStooped:Bool=false)
   {
		host = inHost;
		port = inPort;
		super(inCreateStooped);
	}

	override function init()
	{
      try
      {
         socket = new Socket();
         trace("Connect " + host + ":" + port + "...");
         try
			{
         	socket.connect( new cpp.net.Host(host), port );
         } catch (e:Dynamic)
			{
             trace("Could not CONNECT!");
             socket = null;
         }

         trace("connected.");
			if (socket!=null)
			{
         	input = socket.input;
         	socketOut = socket.output;
			}
      }
      catch(e:Dynamic)
      {
         if (socket!=null)
            socket.close();
         socket = null;
         trace("Socket error:" + e);
      }

		stillDebugging = socket!=null;
   }

	override function getNextCommand() : String
	{
		if (input==null)
			return "bye";
		try
		{
			return input.readLine();
		}
		catch(e:Dynamic)
		{
			trace("getNextCommand - socket closed");
			input.close();
			input = null;
		}
		return "bye";
	}

	override function sendOutput(inString:String)
	{
		// TODO
		//Sys.println(inString);
   }

}


