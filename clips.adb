-------------------------------------------------------------------------------
-- Copyright (C) 1999 Ted Dennison
--
-- This source file is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 2, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
-- As a special exception,  if other files  instantiate  generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License.  This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--
--
-- Maintainer: Ted Dennison (dennison@telepath.com)
--
-- This software was originally developed as part of an AI project for class
-- at the University of Tulsa as part of my graduate studies.
--
--
-------------------------------------------------------------------------------

with Interfaces.C;
with Interfaces.C.Strings;
with Unchecked_Conversion;

use type Interfaces.C.Int;
use type Interfaces.C.Long;
------------------------------------------------------------------------------
-- This package implements an Ada binding to the CLIPS AI shell.
------------------------------------------------------------------------------
package body Clips is

   --
   -- Clips hash nodes
   type Mod_32 is mod 32;
   for Mod_32'Size use 32;

   type Generic_Hash_Node;
   type Generic_Hash_Node_Ptr is access Generic_Hash_Node;
   type Generic_Hash_Node is record
      Next        : Generic_Hash_Node_Ptr;
      Count       : Interfaces.C.Long;
      Depth       : Interfaces.C.Int;
      Bucket_Plus : Mod_32; -- Actually contains 3 fields, but we don't care...
   end record;
   pragma Convention (C, Generic_Hash_Node);

   type Symbol_Hash_Node;
   type Symbol_Hash_Node_Ptr is access Symbol_Hash_Node;
   type Symbol_Hash_Node is record
      GHN      : Generic_Hash_Node;
      Contents : Interfaces.C.Strings.Chars_Ptr;
   end record;
   pragma Convention (C, Symbol_Hash_Node);

   function To_Symbol_Hash_Ptr is new Unchecked_Conversion
     (Source => Generic_Hash_Node_Ptr,
      Target => Symbol_Hash_Node_Ptr
      );

   type Integer_Hash_Node;
   type Integer_Hash_Node_Ptr is access Integer_Hash_Node;
   type Integer_Hash_Node is record
      GHN      : Generic_Hash_Node;
      Contents : Interfaces.C.Long;
   end record;
   pragma Convention (C, Integer_Hash_Node);

   function To_Integer_Hash_Ptr is new Unchecked_Conversion
     (Source => Generic_Hash_Node_Ptr,
      Target => Integer_Hash_Node_Ptr
      );


   --
   -- Clips generic Data Object structure
   type Data_Object;
   type Data_Object_Ptr is access all Data_Object;
   type Data_Object is record
      SubblementalInfo : Void_Ptr;
      DO_Type          : Clips_Type_Description;
      Value            : Generic_Hash_Node_Ptr;
      DO_Begin         : Interfaces.C.Long;
      DO_End           : Interfaces.C.Long;
      Next             : Data_Object_Ptr;
   end record;
   pragma Convention (C, Data_Object);

   --
   -- Access conversion routines (for getting values out of Data_Objects)
   type Int_Ptr is access all Interfaces.C.Int;
   function To_Int_Ptr is new Unchecked_Conversion
     (Source => Void_Ptr,
      Target => Int_Ptr
      );
   function To_Chars_Ptr is new Unchecked_Conversion
     (Source => Void_Ptr,
      Target => Interfaces.C.Strings.Chars_Ptr
      );

   --
   -- Clips Routines
   function Clips_Assert (Pattern : in Interfaces.C.Char_Array)
                          return Fact_Ptr;
   pragma Import (C, Clips_Assert, "AssertString");

   procedure InitializeEnvironment;
   pragma Import (C, InitializeEnvironment, "InitializeEnvironment");

   function Clips_Load (FileName : in Interfaces.C.Char_Array)
                        return Interfaces.C.Int;
   pragma Import (C, Clips_Load, "Load");

   procedure Clips_Reset;
   pragma Import (C, Clips_Reset, "Reset");

   function Clips_Retract (FactPtr : in Fact_Ptr) return Interfaces.C.Int;
   pragma Import (C, Clips_Retract, "Retract");

   function Clips_Run (RunLimit : in Interfaces.C.Long)
                       return Interfaces.C.Long;
   pragma Import (C, Clips_Run, "Run");

   function Clips_Define_Function_Void_2
     (FunctionName         : in Interfaces.C.Char_Array;
      FunctionType         : in Interfaces.C.Char;
      FunctionPointer      : in Void_Function_Ptr;
      ActualFunctionName   : in Interfaces.C.Char_Array;
      FunctionRestrictions : in Interfaces.C.Char_Array
     ) return Interfaces.C.Int;
   pragma Import (C, Clips_Define_Function_Void_2, "DefineFunction2");

   function RtnArgCount return Interfaces.C.Int;
   pragma Import (C, RtnArgCount, "RtnArgCount");

   function ArgRangeCheck
     (FunctionName : in Interfaces.C.Char_Array;
      Min          : in Interfaces.C.Int;
      Max          : in Interfaces.C.Int
     ) return Interfaces.C.Int;
   pragma Import (C, ArgRangeCheck, "ArgRangeCheck");

   function ArgTypeCheck
     (FunctionName     : in Interfaces.C.Char_Array;
      ArgumentPosition : in Interfaces.C.Int;
      ExpectedType     : in Clips_Type_Description;
      Argument         : in Void_Ptr := null
     ) return Interfaces.C.Int;
   pragma Import (C, ArgTypeCheck, "ArgTypeCheck");

   function RtnLexeme
     (ArgumentPosition : in Interfaces.C.Int
     ) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import (C, RtnLexeme, "RtnLexeme");

   function RtnLong
     (ArgumentPosition : in Interfaces.C.Int
     ) return Interfaces.C.Long;
   pragma Import (C, RtnLong, "RtnLong");

   function GetNextFact (FactPtr : Fact_Ptr) return Fact_Ptr;
   pragma Import (C, GetNextFact, "GetNextFact");

   function GetFactSlot
     (FactPtr  : in Fact_Ptr;
      SlotName : in Interfaces.C.Char_Array;
      TheValue : in Data_Object_Ptr
     ) return Interfaces.C.Int;
   pragma Import (C, GetFactSlot, "GetFactSlot");

   function GetFactSlot2
     (FactPtr  : in Fact_Ptr;
      SlotName : in Interfaces.C.Strings.Chars_Ptr;
      TheValue : in Data_Object_Ptr
     ) return Interfaces.C.Int;
   pragma Import (C, GetFactSlot2, "GetFactSlot");

   function CL_Watch
     (Item : in Interfaces.C.Char_Array) return Interfaces.C.Int;
   pragma Import (C, CL_Watch, "Watch");

   procedure CL_Agenda
     (LogicalName : in Interfaces.C.Char_Array := Interfaces.C.To_C("stdout");
      Module      : in Module_Ptr);
   pragma Import (C, CL_Agenda, "Agenda");

   procedure Clips_Facts
     (LogicalName : in Interfaces.C.Char_Array := Interfaces.C.To_C("stdout");
      TheModule   : in Module_Ptr;
      Start       : in Interfaces.C.Long := -1;
      Fact_End    : in Interfaces.C.Long := -1;
      Max         : in Interfaces.C.Long := -1
      );
   pragma Import (C, Clips_Facts, "Facts");

   function GetFocus return Module_Ptr;
   pragma Import (C, GetFocus, "GetFocus");

   function FindDefmodule (DefmoduleName : in Interfaces.C.Char_Array)
                           return Module_Ptr;
   pragma Import (C, FindDefmodule, "FindDefmodule");

   -----------------------------------------------------------------
   -- Asserts a fact into the CLIPS fact-list. The function version
   -- returns the Fact_Pointer required by Retract
   -----------------------------------------------------------------
   function Assert (Pattern : in String) return Fact_Ptr is
   begin
      return Clips_Assert (Interfaces.C.To_C(Pattern));
   end Assert;
   procedure Assert (Pattern : in String) is
      Fact : Fact_Ptr;
   begin
      Fact := Clips_Assert (Interfaces.C.To_C(Pattern));
   end Assert;

   ----------------
   -- Initialize --
   ----------------

   ---------------------------------------------------------------------------
   -- Initialize the CLIPS environment.
   ---------------------------------------------------------------------------
   procedure Initialize is
   begin
      InitializeEnvironment;
   end Initialize;

   ---------------------------------------------------------------------------
   -- Load the given file into the CLIPS environment
   ---------------------------------------------------------------------------
   procedure Load (File_Name : in String) is
   begin
      if Clips_Load (Interfaces.C.To_C(File_Name)) < 0 then
         raise Failure;
      end if;
   end Load;

   ---------------------------------------------------------------------------
   -- Reset the CLIPS environment.
   ---------------------------------------------------------------------------
   procedure Reset is
   begin
      Clips_Reset;
   end Reset;

   -----------------------------------------------------------------
   -- Causes a fact asserted by the ASSERT_FACT function to be retracted.
   -- Returns: false if fact has already been retracted, else true.
   -- Input of any value not returned by ASSERT will
   -- cause CLIPS to abort.
   -----------------------------------------------------------------
   function Retract (Assertion : in Fact_Ptr) return Boolean is
   begin
      return Clips_Retract (Assertion) = 1;
   end Retract;

   -----------------------------------------------------------------
   -- Allow Run_Limit rules to fire (execute).
   -- -1 allows rules to fire until the agenda is empty.
   -- Returns: Number of rules that were fired.
   -----------------------------------------------------------------
   function Run (Run_Limit : in Integer := -1) return integer is
   begin
      return Integer(Clips_Run (Interfaces.C.Long(Run_Limit)));
   end Run;

   procedure Run (Run_Limit : in Integer := -1) is
      Rules_Fired : Interfaces.C.Long;
   begin
      Rules_Fired := Clips_Run (Interfaces.C.Long(Run_Limit));
   end Run;

   -----------------------------------------------------------------
   -- Define a new function for CLIPS.
   -- This version is for functions that return no value
   -- (procedures).
   -----------------------------------------------------------------
   procedure Define_Function
     (Name                  : in String;
      Routine               : in Void_Function_Ptr;
      Argument_Restrictions : in String := "**"
     ) is
   begin
      if Integer
        (Clips_Define_Function_Void_2
         (FunctionName         => Interfaces.C.To_C (Name),
          FunctionType         => Interfaces.C.To_C ('v'),
          FunctionPointer      => Routine,
          ActualFunctionName   => Interfaces.C.To_C (Name),
          FunctionRestrictions => Interfaces.C.To_C (Argument_Restrictions)
          )) = 0
      then
         raise Failure;
      end if;
   end Define_Function;

   -----------------------------------------------------------------
   -- Return the number of arguments that the current user function
   -- was invoked with.
   -----------------------------------------------------------------
   function Argument_Count return Natural is
   begin
      return Natural(RtnArgCount);
   end Argument_Count;

   -----------------------------------------------------------------
   -- Return whether the number of arguments that the current user
   -- function was invoked with is within the given range.
   -----------------------------------------------------------------
   function Argument_Range_Check
     (Name : in String;
      Min  : in Natural;
      Max  : in Natural
     ) return Boolean is
   begin
      return
        ArgRangeCheck
          (FunctionName => Interfaces.C.To_C (Name),
           Min          => Interfaces.C.Int(Min),
           Max          => Interfaces.C.Int(Max)
           ) /= -1;
   end Argument_Range_Check;

   -----------------------------------------------------------------
   -- Return whether the argument of the current user function with
   -- the given index is of the given type.
   -- If the check fails, an error message will be outputted using
   -- the given Name, and Clips will halt when the user function
   -- returns.
   -----------------------------------------------------------------
   function Argument_Type_Check
     (Name         : in String;
      Arg_Position : in Positive;
      Arg_Type     : in Clips_Type_Description
     ) return Boolean is
   begin
      return
        ArgTypeCheck
        (FunctionName     => Interfaces.C.To_C(Name),
         ArgumentPosition => Interfaces.C.Int(Arg_Position),
         ExpectedType     => Arg_Type
         ) /= 0;
   end Argument_Type_Check;

   -----------------------------------------------------------------
   -- Retreive the lexeme value of the given argument to the current
   -- user function.
   -----------------------------------------------------------------
   function Retrieve_Lexeme (Argument_Number : in Positive)
                             return String is
   begin
      return Interfaces.C.Strings.Value
        (RtnLexeme (Interfaces.C.Int(Argument_Number)));
   end Retrieve_Lexeme;

   -----------------------------------------------------------------
   -- Retreive the integer value of the given argument to the current
   -- user function.
   -----------------------------------------------------------------
   function Retrieve_Long (Argument_Number : in Positive)
                           return Integer is
   begin
      return Integer(RtnLong(Interfaces.C.Int(Argument_Number)));
   end Retrieve_Long;

   ---------------------------------------------------------------------------
   -- Retrireve the next fact on the fact list after the given one. If
   -- Null_Fact is given, then the *first* fact will be returned.
   ---------------------------------------------------------------------------
   function Get_Next_Fact (Last_Fact : in Fact_Ptr) return Fact_Ptr is
   begin
      return GetNextFact(Last_Fact);
   end Get_Next_Fact;

   ---------------------------------------------------------------------------
   -- Retrieve the value of the named slot in the given fact.
   ---------------------------------------------------------------------------
   function Get_Fact_Slot
     (Fact : in Fact_Ptr;
      Slot : in String
     ) return Integer is

      Slot_Object : aliased Data_Object;
   begin
      if GetFactSlot
        (FactPtr  => Fact,
         SlotName => Interfaces.C.To_C(Slot),
         TheValue => Slot_Object'Unchecked_Access
         ) = 0
      then
         raise Failure;
      end if;

      if Slot_Object.DO_Type /= Integer_Type then
         raise Failure;
      end if;

      return Integer(To_Integer_Hash_Ptr (Slot_Object.Value).Contents);

   end Get_Fact_Slot;

   function Get_Fact_Slot
     (Fact : in Fact_Ptr;
      Slot : in String
     ) return String is

      Slot_Object : aliased Data_Object;
   begin
      if GetFactSlot
        (FactPtr  => Fact,
         SlotName => Interfaces.C.To_C(Slot),
         TheValue => Slot_Object'Unchecked_Access
         ) = 0
      then
         raise Failure;
      end if;

      if Slot_Object.DO_Type /= String_Type then
         raise Failure;
      end if;

      return Interfaces.C.Strings.Value
        (To_Symbol_Hash_Ptr(Slot_Object.Value).Contents);

   end Get_Fact_Slot;

   ---------------------------------------------------------------------------
   -- Return whether the fact has a slot with the given name.
   ---------------------------------------------------------------------------
   function Has_Slot
     (Fact : in Fact_Ptr;
      Slot : in String
     ) return Boolean is
      Slot_Object : aliased Data_Object;
   begin
      return GetFactSlot
        (FactPtr  => Fact,
         SlotName => Interfaces.C.To_C(Slot),
         TheValue => Slot_Object'Unchecked_Access
         ) /= 0;
   end Has_Slot;

   ---------------------------------------------------------------------------
   -- Watch the given item
   -- The item name should be one of the following strings: facts, rules,
   -- activations, focus, compilations, statistics, globals, deffunctions,
   -- instances, slots, messages, message-handlers, generic-functions,
   -- methods, or all. If all is se-lected, all possible watch items will be
   -- traced.
   ---------------------------------------------------------------------------
   procedure Watch (Item : in String) is
   begin
      if CL_Watch (Interfaces.C.To_C(Item)) = 0 then
         raise Failure;
      end if;
   end Watch;


   ---------------------------------------------------------------------------
   -- Print the agenda for the given module (all modules if null).
   ---------------------------------------------------------------------------
   procedure Agenda (Module : in Module_Ptr := Null_Module) is
   begin
      CL_Agenda (Module => Module);
   end Agenda;

   ---------------------------------------------------------------------------
   -- Get the current focus
   ---------------------------------------------------------------------------
   function Get_Focus return Module_Ptr is
   begin
      return GetFocus;
   end Get_Focus;

   ---------------------------------------------------------------------------
   -- Get the module with the given name
   ---------------------------------------------------------------------------
   function Find_Defmodule (Name : in String) return Module_Ptr is
   begin
      return FindDefmodule (Interfaces.C.To_C(Name));
   end Find_Defmodule;

   ---------------------------------------------------------------------------
   -- Prints the list of all facts currently in the fact-list of the given
   -- module (or all if Null_Module is specified).
   ---------------------------------------------------------------------------
   procedure Facts (Module : in Module_Ptr := Null_Module)is
   begin
      Clips_Facts (TheModule => Module);
   end Facts;


end Clips;


