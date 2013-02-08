------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                              G N A T 2 X M L                             --
--                                                                          --
--                                 B o d y                                  --
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

with Asis.Set_Get;

package body Gnat2xml is

   function Ekind (Element : Asis.Element) return Opt_ASIS_Elems is
   begin
      return Opt_ASIS_Elems (Asis.Set_Get.Int_Kind (Element));
   end Ekind;

   function Num_Queries (Kind : Opt_ASIS_Elems) return A4G.Queries.Query_Count is
   begin
      return A4G.Queries.Appropriate_Queries (Kind)'Length;
   end Num_Queries;

   function Span (Element : Asis.Element) return Asis.Text.Span is
   begin
      if Ekind (Element) in Flat_Element_Kinds then
         return Asis.Text.Element_Span (Element);
      else
         return Asis.Text.Nil_Span;
      end if;
   end Span;

end Gnat2xml;
