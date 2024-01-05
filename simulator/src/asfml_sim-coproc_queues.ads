with WNM_HAL;

package ASFML_Sim.Coproc_Queues is

   procedure Push (Target : WNM_HAL.Coproc_Target;
                   D      : WNM_HAL.Coproc_Data);

   procedure Pop (Target  :     WNM_HAL.Coproc_Target;
                  D       : out WNM_HAL.Coproc_Data;
                  Success : out Boolean);

end ASFML_Sim.Coproc_Queues;
