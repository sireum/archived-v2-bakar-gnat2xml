------------------------------------------------------------------------------
--                                                                          --
--                          GNAT2XML COMPONENTS                             --
--                           AVATOX COMPONENTS                              --
--                                                                          --
--                        VATOX (Via Asis To Xml)                           --
--                                                                          --
--                                                                          --
--                Copyright (C) 2006, McKae Technologies.                   --
--                Copyright (C) 2012, AdaCore, Inc.                         --
--                                                                          --
-- Avatox is free software; you can redistribute it and/or modify it        --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Avatox is distributed in the hope  that it will be useful,      --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- Avatox is based off the Display_Source software distributed as part of   --
-- the ASIS implementation for GNAT, and therefore inherits its GPL         --
-- licensing.  Ada Core Technologies maintains the Display_Source program   --
-- and its copyright is held by the Free Software Foundation.               --
--                                                                          --
-- Avatox is now maintained by McKae Technologies (http://www.mckae.com)    --
-- The gnat2xml tool was derived from the Avatox sources.                   --
------------------------------------------------------------------------------

with Ada.Containers.Vectors;

with Asis; use Asis;
with Asis.Elements;
with Asis.Extensions.Iterator; use Asis.Extensions.Iterator;
with Text_Io;
with Gnat2xml.Xsl_Transformation;

package Gnat2xml.Xml is

   type Info_Node is
      record
         XML_File      : Text_Io.File_Access;
         Krunch        : Boolean      := False;
         Xml_Style     : Boolean      := False;
         Xsl_Info      : Xsl_Transformation.Xsl_Information;
         Last_Element  : Asis.Element := Asis.Nil_Element;
         Verbose       : Boolean      := False;
      end record;

   procedure Start_Representation
     (State : in out Info_Node);

   procedure Process_Unit
     (The_Unit : in     Asis.Compilation_Unit;
      State    : in out Info_Node);

end Gnat2xml.Xml;
