--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package Rop_Test
is

   --  This procedure prints a secret number if the correct input is
   --  given. It also contains a stack overflow that will trigger
   --  printing the secret number if the correct address is provided
   --  in the input.
   procedure Run (Input : String);

end Rop_Test;
