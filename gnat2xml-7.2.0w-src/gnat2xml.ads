------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                              G N A T 2 X M L                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2012, AdaCore, Inc.                     --
--                                                                          --
-- Gnat2xml is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Gnat2xml is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
-- The gnat2xml tool was derived from the Avatox sources.                   --
------------------------------------------------------------------------------

--  Root package for the Gnat2xml tools. There are two main procedures: the
--  schema (XSD) generator Gnat2xml.Gnat2xsd and the XML generator
--  Gnat2xml.Gnat2xml. The corresponding executables are gnat2xsd[.exe] and
--  gnat2xml[.exe].

with A4G.Queries; use A4G;

with Asis.Text;
with Asis.Extensions.Flat_Kinds; use Asis.Extensions.Flat_Kinds;

with Strings; use Strings;

package Gnat2xml is

   subtype Classes is Flat_Element_Kinds'Base
     range Not_An_Element .. A_Statement_Class;
   --  All classes, including the abstract ones, the lists, and the singleton
   --  element kinds

   subtype Opt_ASIS_Elems is Flat_Element_Kinds'Base
     range Not_An_Element .. A_Compilation_Unit;

   function Ekind (Element : Asis.Element) return Opt_ASIS_Elems;

   function Num_Queries (Kind : Opt_ASIS_Elems) return A4G.Queries.Query_Count;
   use type A4G.Queries.Query_Index;
   function Is_Leaf (E : Asis.Element) return Boolean is
      (Num_Queries (Ekind (E)) = 0);

   function Span (Element : Asis.Element) return Asis.Text.Span;
   --  Return Nil_Element for gnat2xml-specific kinds

   subtype ASIS_Elems is Opt_ASIS_Elems with
     Predicate => ASIS_Elems /= Not_An_Element;

   subtype Def_Names is Flat_Defining_Name_Kinds;
   --  Defining occurences, such as A_Defining_Identifier

   subtype Usage_Names is Flat_Usage_Name_Kinds;
   --  References to defining occurrences, such as An_Identifier

   subtype Name_Elems is ASIS_Elems with
     Predicate => Name_Elems in Def_Names | Usage_Names;

   subtype Boolean_Elems is ASIS_Elems'Base range An_Aliased .. An_Is_Prefix_Notation;

   subtype Other_Elems is ASIS_Elems with
     Predicate => Other_Elems not in Name_Elems | Boolean_Elems;

   Main_Done : Boolean := False;
   --  This is set True at the end of each main procedure. The purpose is so
   --  assertions in Finalize operations can tell whether the main procedure
   --  exited normally. See, for example, Generic_Formatted_Output.Finalize,
   --  which insists that when we reach the end of the main procedure, the
   --  indentation level should be zero. But if an exception propagates out of
   --  the main procedure, that's just a bug which should be reported normally.

   subtype Unit_Kinds is Asis.Unit_Kinds;
   subtype Unit_Classes is Asis.Unit_Classes;
   subtype Unit_Origins is Asis.Unit_Origins;
   use all type Unit_Kinds, Unit_Classes, Unit_Origins;

   Debug_Mode : Boolean := False;

end Gnat2xml;
