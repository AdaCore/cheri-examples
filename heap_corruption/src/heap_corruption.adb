with Ada.Text_IO;

procedure Heap_Corruption
is
   type String_Access is access String;
   procedure Malicious_Lib with
      Import,
      Convention => C,
      External_Name => "malicious_lib";
   Data : String_Access := new String'("blubb");
begin
   Ada.Text_IO.Put_Line ("Data: " & Data.all);
   Malicious_Lib;
   Ada.Text_IO.Put_Line ("Data: " & Data.all);
end Heap_Corruption;
