--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package Func_Ptr
is

   procedure P1;
   --  Dummy procedure to be called by Call (0)

   procedure P2;
   --  Dummy procedure to be called by Call (112)

   procedure Call (Offset : Integer);
   --  This procedure will call a procedure at an offset relative
   --  to P1. The offset 0 will call P1 and the offset 112 will
   --  call P2. Any other offset will try to call whatever code
   --  is located at P1'Address + Offset.

end Func_Ptr;
