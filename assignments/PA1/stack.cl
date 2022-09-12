(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

class Main {
   interpreter: Interpreter <- new Interpreter;

   main() : Object {
      interpreter.readInput()
   };
};

class Interpreter inherits IO {
   a2i: A2I <- new A2I;
   curLine: String;
   cur: Node;
   prev: Node;

   c_DEBUG: Bool <- false;
   c_STOP_TOKEN: String <- "x";
   c_DISPLAY_TOKEN: String <- "d";

   readInput(): Object {
      while not curLine = c_STOP_TOKEN loop
      {
         out_string("> ");
         curLine <- in_string();

         if c_DEBUG then {
            out_string("curLine: ");
            out_string(curLine.concat("\n\n"));
         } else
            0
         fi;

         prev <- cur;
         if curLine = c_STOP_TOKEN then
           0
         else if curLine = c_DISPLAY_TOKEN then
           cur.printAll()
         else
           cur <- new IntNode.init(a2i.a2i(curLine), prev)
         fi fi;
      } pool
   };
};

-- Abstract class: 
class Node inherits IO {
   abstractErrorStr: String <- "Node abstract method not implemented";
   prev: Node;


   printSelf(): Object { 
      { 
         out_string(abstractErrorStr);
         abort();
      }
   };

   printAll(): Object {
      {
         printSelf();

         if not isvoid prev then
            prev.printAll()
         else
            0
         fi;
      }
   };

   -- TODO: implement swap operator
   swap(): Object {
      0
   };

   evaluate(): Object { 0 };
};

class IntNode inherits Node {
   a2i: A2I <- new A2I;
   value: Int;

   init(initValue: Int, initPrev: Node): Node {
      {
         prev <- initPrev;
         value <- initValue;
         self;
      }
   };

   printSelf(): Object {
      {
         out_string("IntNode out_string: ");
         out_string(a2i.i2a(value).concat("\n"));
      }
   };
};