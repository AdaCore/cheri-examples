--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package Secret
is

   --  Print_Secret prints a secret number if the correct password
   --  is provided. The goal of the tests is to circumvent the
   --  password check in this procedure to make it print the
   --  secret number without the correct password.
   procedure Print_Secret (Password : String);

end Secret;
