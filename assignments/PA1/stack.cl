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
   cur: Node <- new Node;
   prev: Node;

   c_DEBUG: Bool <- false;
   c_STOP_TOKEN: String <- "x";
   c_DISPLAY_TOKEN: String <- "d";
   c_EVALUATE_TOKEN: String <- "e";
   c_PLUS_TOKEN: String <- "+";
   c_SWAP_TOKEN: String <-"s";

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
         else if curLine = c_EVALUATE_TOKEN then
           cur <- cur.evaluate()
         else if curLine = c_PLUS_TOKEN then
           cur <- new PlusNode.init(prev)
         else if curLine = c_SWAP_TOKEN then
           cur <- new SwapNode.init(prev)
         else
           cur <- new IntNode.init(a2i.a2i(curLine), prev)
         fi fi fi fi fi;
      } pool
   };
};

-- Abstract class: 
class Node inherits IO {
   abstractErrorStr: String <- "Node abstract method not implemented";
   prev: Node;

   printSelf(): Object { 
      { 
         0;
         -- out_string(abstractErrorStr);
         -- abort();
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

   prev(): Node { prev };

   setPrev(newPrev: Node): Node { 
      {
         prev <- newPrev;
         self;
      }
   };


   setIntValue(newValue: Int): Node { self };
   intValue(): Int { 0 };

   evaluate(): Node { self };
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
         -- out_string("IntNode out_string: ");
         out_string(a2i.i2a(value).concat("\n"));
      }
   };

   setIntValue(newValue: Int): Node {
      {
         value <- newValue;
         self;
      }
   };

   intValue(): Int {
      value
   };
};

class PlusNode inherits Node {
   init(initPrev: Node): Node {
      {
         prev <- initPrev;
         self;
      }
   };

   printSelf(): Object {
      -- out_string("PlusNode out_string: +\n")
      out_string("+\n")
   };

   first: Int;
   second: Int;
   evaluate(): Node {
      {
         first <- self.prev().intValue();
         second <- self.prev().prev().intValue();
         self.prev().prev().setIntValue(first + second);
      }
   };
};

class SwapNode inherits Node {
   init(initPrev: Node): Node {
      {
         prev <- initPrev;
         self;
      }
   };

   printSelf(): Object {
      -- out_string("SwapNode out_string: s\n")
      out_string("s\n")
   };

   first: Node;
   second: Node;
   evaluate(): Node {
      {
         first <- self.prev();
         second <- self.prev().prev();
         first.setPrev(second.prev());
         second.setPrev(first);
      }
   };
};