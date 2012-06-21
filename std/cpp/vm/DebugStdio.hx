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

   function vars(inI:Int)
   {
      var result = Debugger.getStackVars(inI);
      Sys.println("Frame " + inI + " : " + result );
   }


   function inputLoop()
   {
      var input = Sys.stdin();
      while(true)
      {
         Sys.print("debug >");
         var command = input.readLine();
         var words = command.split(" ");
         if (words.length>0) switch(words[0])
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

            case "vars":
               if (!inDebugger)
                  Sys.println("Must break first.");
               else
               {
                  var n = Std.parseInt(words[1]);
                  debugQueue.add( function() vars(n) );
                  waitDebugger();
               }
              

            case "where":
               if (!inDebugger)
                  Sys.println("Must break first.");
               else
               {
                  debugQueue.add( function() where() );
                  waitDebugger();
               }

            case "help":
               Sys.println("help  - print this message");
               Sys.println("break - pause execution of one thread");
               Sys.println("cont  - continue execution");
               Sys.println("where - print call stack");
               Sys.println("vars N - print local vars for frame N");
               Sys.println("exit  - exit programme");

            default:
               Sys.println("Unknown command:" + command);
         }
      }
   }

}


