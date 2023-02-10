with Tresses;            use Tresses;
with Tresses.Interfaces; use Tresses.Interfaces;

private with Tresses.Excitation;
private with Tresses.Random;
private with Tresses.Filters.SVF;
private with Tresses.Envelopes.AR;

private package WNM.Synth.Snare_Voice is

   type Instance
   is new Four_Params_Voice
   with private;

   type Snare_Engine is (Snare, Clap);

   function Engine (This : Instance) return Snare_Engine;
   procedure Set_Engine (This : in out Instance; E : Snare_Engine);

   function Img (E : Snare_Engine) return String;

   procedure Init (This : in out Instance);

   procedure Render (This   : in out Instance;
                     Buffer :    out Tresses.Mono_Buffer);

   --  Interfaces --

   overriding
   function Param_Label (This : Instance; Id : Param_Id) return String;

   overriding
   function Param_Short_Label (This : Instance; Id : Param_Id)
                               return Short_Label;

private

   type Instance
   is new Four_Params_Voice
   with record

      Engine : Snare_Engine := Snare_Engine'First;

      Pulse0, Pulse1, Pulse2, Pulse3 : Excitation.Instance;
      Filter0, Filter1, Filter3 : Filters.SVF.Instance;
      Rng : Tresses.Random.Instance;
      Env0 : Tresses.Envelopes.AR.Instance;
      Re_Trig : Tresses.U32;

      Do_Init : Boolean := True;
   end record;

end WNM.Synth.Snare_Voice;
