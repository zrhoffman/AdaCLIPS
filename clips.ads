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

------------------------------------------------------------------------------
-- This package implements an Ada binding to the CLIPS AI shell.
------------------------------------------------------------------------------
package Clips is

   Failure : exception;

   type Fact_Ptr is private;
   Null_Fact : constant Fact_Ptr;

   type Module_Ptr is private;
   Null_Module : constant Module_Ptr;

   type Clips_Type_Description is
      (Float_Type,
       Integer_Type,
       Symbol_Type,
       String_Type,
       Multifield_Type,
       External_Address_Type,
       Fact_Address_Type,
       Instance_Address_Type,
       Instance_Name_Type,
       Integer_Or_Float,
       Symbol_Or_String,
       Instance_Or_Instance_Name
       );
   for Clips_Type_Description use
      (Float_Type                => 0,
       Integer_Type              => 1,
       Symbol_Type               => 2,
       String_Type               => 3,
       Multifield_Type           => 4,
       External_Address_Type     => 5,
       Fact_Address_Type         => 6,
       Instance_Address_Type     => 7,
       Instance_Name_Type        => 8,
       Integer_Or_Float          => 110,
       Symbol_Or_String          => 111,
       Instance_Or_Instance_Name => 112
       );
   for Clips_Type_Description'Size use Interfaces.C.Int'Size;

   ---------------------------------------------------------------------------
   -- Initialize the CLIPS environment.
   ---------------------------------------------------------------------------
   procedure Initialize;

   ---------------------------------------------------------------------------
   -- Reset the CLIPS environment.
   ---------------------------------------------------------------------------
   procedure Reset;

   ---------------------------------------------------------------------------
   -- Halt the CLIPS environment.
   ---------------------------------------------------------------------------
   procedure Halt;

   ---------------------------------------------------------------------------
   -- Load the given file into the CLIPS environment
   ---------------------------------------------------------------------------
   procedure Load (File_Name : in String);

   -----------------------------------------------------------------
   -- Allow Run_Limit rules to fire (execute).
   -- -1 allows rules to fire until the agenda is empty.
   -- Returns: Number of rules that were fired.
   -----------------------------------------------------------------
   function Run (Run_Limit : in Integer := -1) return integer;
   procedure Run (Run_Limit : in Integer := -1);

   -----------------------------------------------------------------
   -- Asserts a fact into the CLIPS fact-list. The function version
   -- returns the Fact_Pointer required by Retract
   -----------------------------------------------------------------
   function Assert (Pattern : in String) return Fact_Ptr;
   procedure Assert (Pattern : in String);

   -----------------------------------------------------------------
   -- Causes a fact asserted by the ASSERT_FACT function to be
   -- retracted. Returns: false if fact has already been retracted,
   -- else true. Input of any value not returned by ASSERT will
   -- cause CLIPS to abort.
   -----------------------------------------------------------------
   function Retract (Assertion : in Fact_Ptr) return Boolean;

   -----------------------------------------------------------------
   -- Define Function routines
   --
   -- User-defined functions are searched before system functions.
   -- If the user defines a function which is the same as one of
   -- the defined functions already provided, the user function will
   -- be executed in its place.
   --
   -- The restriction string parameter may be used to tell Clips
   -- what arguments are valid to pass into the function.
   -- The syntax format for the restriction string is
   -- <min-args> <max-args> [<default-type> <types>*]
   -- An '*' for min or max args indicates any number is appropriate.
   -- The type codes allowed are:
   --      a    External Address
   --      d    Float
   --      e    Instance Address, Instance Name, or Symbol
   --      f    Float
   --      g    Integer, Float, or Symbol
   --      h    Instance Address, Instance Name, Fact Address, Integer,
   --           or Symbol
   --      i    Integer
   --      j    Symbol, String, or Instance Name
   --      k    Symbol or String
   --      l    Integer
   --      m    Multifield
   --      n    Integer or Float
   --      o    Instance Name
   --      p    Instance Name or Symbol
   --      q    Symbol, String, or Multifield
   --      s    String
   --      u    Any Data Type
   --      w    Symbol
   --      x    Instance Address
   --      y    Fact Address
   --      z    Fact address, Integer, or Symbol
   --
   -- It would be wise to perform a "pragma interface (C," on
   -- the function you pass in.
   -----------------------------------------------------------------

   type Void_Function_Ptr is access procedure;
   pragma Convention (C, Void_Function_Ptr);

   --------------------------------------------------------------------------
   -- Define a new function for CLIPS.
   -- This version is for functions that return no value
   -- (procedures).
   --------------------------------------------------------------------------
   procedure Define_Function
     (Name                  : in String;
      Routine               : in Void_Function_Ptr;
      Argument_Restrictions : in String := "**"
      );

   --------------------------------------------------------------------------
   -- Return the number of arguments that the current user function
   -- was invoked with.
   --------------------------------------------------------------------------
   function Argument_Count return Natural;

   --------------------------------------------------------------------------
   -- Return whether the number of arguments that the current user
   -- function was invoked with is within the given range.
   -- If the check fails, an error message will be outputted using
   -- the given Name, and Clips will halt when the user function
   -- returns.
   --------------------------------------------------------------------------
   function Argument_Range_Check
     (Name : in String;
      Min  : in Natural;
      Max  : in Natural
     ) return Boolean;

   --------------------------------------------------------------------------
   -- Return whether the argument of the current user function with
   -- the given index is of the given type.
   -- If the check fails, an error message will be outputted using
   -- the given Name, and Clips will halt when the user function
   -- returns.
   --------------------------------------------------------------------------
   function Argument_Type_Check
     (Name         : in String;
      Arg_Position : in Positive;
      Arg_Type     : in Clips_Type_Description
     ) return Boolean;

   --------------------------------------------------------------------------
   -- Retreive the lexeme value of the given argument to the current
   -- user function.
   --------------------------------------------------------------------------
   function Retrieve_Lexeme (Argument_Number : in Positive)
                             return String;

   --------------------------------------------------------------------------
   -- Retreive the integer value of the given argument to the current
   -- user function.
   --------------------------------------------------------------------------
   function Retrieve_Long (Argument_Number : in Positive)
                           return Integer;

   ---------------------------------------------------------------------------
   -- Retrireve the next fact on the fact list after the given one. If
   -- Null_Fact is given, then the *first* fact will be returned.
   ---------------------------------------------------------------------------
   function Get_Next_Fact (Last_Fact : in Fact_Ptr) return Fact_Ptr;

   ---------------------------------------------------------------------------
   -- Return whether the fact has a slot with the given name.
   ---------------------------------------------------------------------------
   function Has_Slot
     (Fact : in Fact_Ptr;
      Slot : in String
     ) return Boolean;

   ---------------------------------------------------------------------------
   -- Retrieve the value of the named slot in the given fact.
   ---------------------------------------------------------------------------
   function Get_Fact_Slot
     (Fact : in Fact_Ptr;
      Slot : in String
     ) return Integer;
   function Get_Fact_Slot
     (Fact : in Fact_Ptr;
      Slot : in String
     ) return String;

   ---------------------------------------------------------------------------
   -- Watch the given item
   -- The item name should be one of the following strings: facts, rules,
   -- activations, focus, compilations, statistics, globals, deffunctions,
   -- instances, slots, messages, message-handlers, generic-functions,
   -- methods, or all. If all is se-lected, all possible watch items will be
   -- traced.
   ---------------------------------------------------------------------------
   procedure Watch (Item : in String);

   ---------------------------------------------------------------------------
   -- Print the agenda for the given module (all modules if null).
   ---------------------------------------------------------------------------
   procedure Agenda (Module : in Module_Ptr := Null_Module);

   ---------------------------------------------------------------------------
   -- Get the current focus
   ---------------------------------------------------------------------------
   function Get_Focus return Module_Ptr;

   ---------------------------------------------------------------------------
   -- Get the module with the given name
   ---------------------------------------------------------------------------
   function Find_Defmodule (Name : in String) return Module_Ptr;

   ---------------------------------------------------------------------------
   -- Prints the list of all facts currently in the fact-list of the given
   -- module (or all if Null_Module is specified).
   ---------------------------------------------------------------------------
   procedure Facts (Module : in Module_Ptr := Null_Module);

private
   -- Make a dummy type to use for VOID ptrs
   type Void_Ptr is access Boolean;

   type Fact_Ptr is new Void_Ptr;
   Null_Fact : constant Fact_Ptr := null;

   type Module_Ptr is new Void_Ptr;
   Null_Module : constant Module_Ptr := null;
end Clips;
