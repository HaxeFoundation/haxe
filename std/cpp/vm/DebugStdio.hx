package cpp.vm;


class DebugStdio
{
   var inDebugger:Bool;
   var inputThread:Thread;
   var debugQueue:Deque<Dynamic>;
   var files:Array<String>;
   

   public function new()
   {
		files = Debugger.getFiles();
      inDebugger = false;
      Debugger.setHandler(onDebug);
      debugQueue= new Deque<Dynamic>();
      inputThread = Thread.create(inputLoop);
   }

   function onDebug()
   {
      Sys.println("stopped.");
      inDebugger = true;
      while(inDebugger)
      {
         var job = debugQueue.pop(true);
         job();
      }
   }

   
   function waitDebugger()
   {
      debugQueue.add( function() inputThread.sendMessage("Ok")  );
      var result = Thread.readMessage(true);
      Sys.println(result);
   }

   function where()
   {
      var stack = haxe.Stack.callStack();
      var idx = 0;
      for(item in stack)
      {
         idx++;
         Sys.println(idx + ":" + item);
      }
   }

   function vars(inI:Int)
   {
      var result = Debugger.getStackVars(inI);
      Sys.println("Frame " + inI + " : " + result );
   }

   function showFiles()
   {
 		if (files!=null)
			for(idx in 0...files.length)
            Sys.println("file " + idx + " : " + files[idx] );
   }

   function showBreakpoints()
   {
      var bps = Debugger.getBreakpoints();
 		if (bps!=null)
			for(idx in 0...bps.length)
            Sys.println("breakpoint " + idx + " : " + bps[idx] );
   }


   function addBreakpoint(inFile:String, inLine:String)
	{
		var id = Std.parseInt(inFile);
		if (id==null)
		{
			for(idx in 0...files.length)
				if (files[idx]==inFile)
				{
					id = idx;
					break;
				}
		}

 		if (id==null)
         Sys.println("Could not find file for " + inFile );
		else
			Debugger.addBreakpoint(id,Std.parseInt(inLine));
			
	}


   function inputLoop()
   {
      var input = Sys.stdin();
      while(true)
      {
         Sys.print("debug >");
         var command = input.readLine();
         var words = command.split(" ");
         switch(words[0])
         {
            case "":
               // Do nothing

            case "exit","quit":
               Debugger.exit();

            case "break","b":
					if (words.length==1)
					{
               	if (inDebugger)
                  	Sys.println("already stopped.");
               	else
               	{
                  	Debugger.setBreak(Debugger.BRK_ASAP,inputThread);
                  	waitDebugger();
               	}
					}
					else if (words.length==3)
					{
						addBreakpoint(words[1],words[2]);
					}
					else
						Sys.println("Usage: break [file line] - pause execution of one thread [when at certain point]");


            case "cont","c":
               if (!inDebugger)
                  Sys.println("Already running.");
               else
                  debugQueue.add( function() inDebugger = false );

            case "vars","v":
               if (!inDebugger)
                  Sys.println("Must break first.");
               else
               {
                  var n = Std.parseInt(words[1]);
                  debugQueue.add( function() vars(n) );
                  waitDebugger();
               }
              

            case "where","w":
               if (!inDebugger)
                  Sys.println("Must break first.");
               else
               {
                  debugQueue.add( function() where() );
                  waitDebugger();
               }

            case "files","f":
               showFiles();

            case "breakpoints","bp":
               showBreakpoints();

            case "delete","d":
               if (words[1]==null)
					{
						Sys.println("Usage : delete N");
 					}
					else
					{
                  var i = Std.parseInt(words[1]);
						Debugger.deleteBreakpoint(i);
					}

            case "help","h","?":
               Sys.println("help  - print this message");
               Sys.println("break [file line] - pause execution of one thread [when at certain point]");
               Sys.println("breakpoints - list breakpoints");
               Sys.println("delete N - delete breakpoint N");
               Sys.println("cont  - continue execution");
               Sys.println("where - print call stack");
               Sys.println("files - print file list that may be used with breakpoints");
               Sys.println("vars N - print local vars for frame N");
               Sys.println("exit  - exit programme");


            default:
               Sys.println("Unknown command:" + words);
         }
      }
   }

}


