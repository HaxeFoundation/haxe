package cpp.vm;

import haxe.Stack;
import cpp.vm.DebugBase;


class DebugStdio extends DebugBase
{
	var input:haxe.io.Input;

   public function new(inCreateStooped:Bool=false)
   {
		super();
		init();
		if (inCreateStooped && stillDebugging)
			Debugger.breakBad();
   }

	function init()
	{
		input = Sys.stdin();
	}


	override function getNextCommand() : String
	{
		Sys.print("debug>");
		return input.readLine();
	}


	function sendOutput(inString:String)
	{
		Sys.println(inString);
   }

	function sendStatus(inString:String)
	{
		sendOutput(inString);
   }

	override function onRunning()
	{
		sendStatus("running");
	}


   override function onStopped()
   {
      sendStatus("stopped.");
   }

   override function showWhere()
   {
      var idx = 0;
      for(item in stack)
      {
         idx++;
         sendOutput((idx==frame ? "*" : " ") + idx + ":" + item);
      }
   }

   override function showFiles()
   {
 		if (files!=null)
			for(idx in 0...files.length)
            sendOutput("file " + idx + " : " + files[idx] );
   }

   override function showBreakpoints()
   {
      var bps = Debugger.getBreakpoints();
 		if (bps!=null)
			for(idx in 0...bps.length)
            sendOutput("breakpoint " + idx + " : " + bps[idx] );
   }

	override function onPrint(result:String)
	{
		sendOutput(result);
	}

	override function onResult(inResult:String)
	{
		sendOutput(inResult);
	}

	override function onHelp()
	{
 		sendOutput("help  - print this message");
      sendOutput("break [file line] - pause execution of one thread [when at certain point]");
      sendOutput("breakpoints - list breakpoints");
      sendOutput("delete N - delete breakpoint N");
      sendOutput("cont  - continue execution");
      sendOutput("where - print call stack");
      sendOutput("files - print file list that may be used with breakpoints");
      sendOutput("vars - print local vars for frame");
      sendOutput("exit  - exit programme");
      sendOutput("bye  - stop debugging, keep running");
	}

}


