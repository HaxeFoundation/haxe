package cpp.vm;


class DebugStdio
{
   var inDebugger:Bool;
   var inputThread:Thread;
   var debugQueue:Deque<Dynamic>;
   

   public function new()
   {
      inDebugger = false;
      Debugger.setHandler(onDebug);
      debugQueue= new Deque<Dynamic>();
      inputThread = Thread.create(inputLoop);
   }

   function onDebug()
   {
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

   function inputLoop()
   {
      var input = Sys.stdin();
      while(true)
      {
         Sys.print("debug >");
         var command = input.readLine();
         switch(command)
         {
            case "exit":
               Debugger.exit();

            case "break":
               if (inDebugger)
                  Sys.println("already stopped.");
               else
               {
                  Debugger.setBreak(Debugger.BRK_ASAP,inputThread);
                  waitDebugger();
               }

            case "cont":
               if (!inDebugger)
                  Sys.println("Already running.");
               else
                  debugQueue.add( function() inDebugger = false );

            case "where":
               if (!inDebugger)
                  Sys.println("Must break first.");
               else
               {
                  debugQueue.add( function() where() );
                  waitDebugger();
               }


               debugQueue.add( function() inDebugger = false );

            case "": // ignore

            case "help":
               Sys.println("help  - print this message");
               Sys.println("break - pause execution of one thread");
               Sys.println("cont  - continue execution");
               Sys.println("where - print call stack");
               Sys.println("exit  - exit programme");

            default:
               Sys.println("Unknown command:" + command);
         }
      }
   }

}


