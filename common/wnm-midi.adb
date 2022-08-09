pragma Ada_2012;
package body WNM.MIDI is

   ------------------
   -- Make_Note_On --
   ------------------

   function Make_Note_On (C : MIDI_Channel;
                          K : MIDI_Key;
                          V : MIDI_Data)
                          return MIDI_USB_Event
   is
      pragma Unreferenced (C, K, V);
   begin
      return 0;
   end Make_Note_On;

   ------------------
   -- Make_Note_On --
   ------------------

   function Make_Note_On (C : MIDI_Channel;
                          N : Notes;
                          O : Octaves;
                          V : MIDI_Data)
                          return MIDI_USB_Event
   is
                           pragma Unreferenced (C, O, N, V);
   begin
      return raise Program_Error with "Unimplemented function Make_Note_On";
   end Make_Note_On;

   -------------------
   -- Make_Note_Off --
   -------------------

   function Make_Note_Off (C : MIDI_Channel;
                           K : MIDI_Key;
                           V : MIDI_Data)
                           return MIDI_USB_Event
   is
                           pragma Unreferenced (C, K, V);
   begin
      return raise Program_Error with "Unimplemented function Make_Note_Off";
   end Make_Note_Off;

   -------------------
   -- Make_Note_Off --
   -------------------

   function Make_Note_Off (C : MIDI_Channel;
                           N : Notes;
                           O : Octaves;
                           V : MIDI_Data)
                           return MIDI_USB_Event
   is
                           pragma Unreferenced (C, N, O, V);
   begin
      return raise Program_Error with "Unimplemented function Make_Note_Off";
   end Make_Note_Off;

   -------------
   -- Make_CC --
   -------------

   function Make_CC (C : MIDI_Channel;
                     Controller : MIDI_Data;
                     Value : MIDI_Data)
                     return MIDI_USB_Event
   is
                           pragma Unreferenced (C, Controller, Value);
   begin
      return raise Program_Error with "Unimplemented function Make_CC";
   end Make_CC;

end WNM.MIDI;
