package body WNM.Factory_Reset is

   -----------
   -- Reset --
   -----------

   procedure Reset (FS : aliased in out Littlefs.LFS_T) is
   begin
      null;
      --  if Littlefs.Mkdir (FS, "samples") /= 0 then
      --     raise Program_Error with "Mkdir error...";
      --  end if;
      --
      --  if Littlefs.Mkdir (FS, "projects") /= 0 then
      --     raise Program_Error with "Mkdir error...";
      --  end if;
   end Reset;

end WNM.Factory_Reset;
